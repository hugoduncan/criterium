(ns criterium.stats.kde
  "Kernel Density Estimation utilities.

  Provides ISJ (Improved Sheather-Jones) bandwidth selection, Gaussian kernel
  density estimation, bootstrap confidence bands, and mode finding.

  All functions require typed arrays (DoubleArray, LongArray)."
  (:require
   [criterium.array :as arr]
   [criterium.primitive-fn :as pf]
   [criterium.random.interface :as random]
   [criterium.stats.core :as core]
   [criterium.stats.fft :as fft]
   [criterium.stats.sampling :as sampling]
   [criterium.transducer :as xd]
   [criterium.utils.interface :refer [have?]])
  (:import
   [criterium.array DoubleArray LongArray]
   [criterium.random.well WellRng1024a]))

(defn- data-min-max
  "Returns [min max] for typed array data."
  [^DoubleArray data]
  [(xd/reduce pf/dmin Double/POSITIVE_INFINITY data)
   (xd/reduce pf/dmax Double/NEGATIVE_INFINITY data)])

(defn- ensure-double-array
  "Ensures data is a DoubleArray. Converts LongArray to DoubleArray."
  ^DoubleArray [data]
  (cond
    (instance? DoubleArray data) data
    (instance? LongArray data)
    (let [^longs a (.array ^LongArray data)
          len (alength a)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (double (aget a i))))
      (DoubleArray. out))
    :else
    (throw (ex-info "ensure-double-array requires a typed array"
                    {:type (type data)}))))

(defn- ensure-sorted-doubles
  "Returns a sorted primitive double array from typed array data."
  ^doubles [data]
  (let [sorted-arr (arr/sorted data)]
    (.array ^DoubleArray sorted-arr)))

(defn- quantile-primitive
  "Calculate quantile from a sorted primitive double array."
  ^double [^double q ^doubles sorted-arr]
  (let [n (dec (alength sorted-arr))
        x (* q n)
        f (Math/floor x)
        i (long f)
        p (- x f)]
    (cond
      (zero? p) (aget sorted-arr i)
      (= 1.0 p) (aget sorted-arr (inc i))
      :else (+ (* p (aget sorted-arr (inc i)))
               (* (- 1.0 p) (aget sorted-arr i))))))

;;; Excess Mass computation (Müller-Sawitzki 1991)

(defn- add-jitter
  "Add small uniform jitter to handle ties in data.
  Perturbs each point by ±mindist/2 where mindist is the smallest
  non-zero distance between points.
  rng is a mutable WellRng1024a instance."
  ^doubles [^doubles sorted-data ^WellRng1024a rng]
  (let [n (alength sorted-data)]
    (if (< n 2)
      sorted-data
      (let [;; Find minimum non-zero distance
            min-dist-raw (loop [i (long 1)
                                md Double/MAX_VALUE]
                           (if (< i n)
                             (let [d (- (aget sorted-data i) (aget sorted-data (dec i)))]
                               (recur (inc i)
                                      (if (> d 0.0) (Math/min md d) md)))
                             md))
            min-dist (double (if (= min-dist-raw Double/MAX_VALUE) 1e-10 min-dist-raw))
            half-dist (/ min-dist 2.0)
            result (double-array n)]
        (dotimes [i n]
          (let [r (random/next-double! rng)
                jitter (- (* 2.0 r half-dist) half-dist)]
            (aset result i (+ (aget sorted-data i) jitter))))
        result))))

(defn- build-distance-matrix
  "Build upper-triangular distance matrix.
  A[i,j] = data[j] - data[i] for j > i.
  Returns a flat array in row-major order for upper triangle."
  ^doubles [^doubles sorted-data]
  (let [n (long (alength sorted-data))
        ;; Number of entries in upper triangle (excluding diagonal)
        size (long (/ (* n (dec n)) 2))
        result (double-array size)]
    (loop [i (long 0)
           idx (long 0)]
      (when (< i (dec n))
        (loop [j (long (inc i))
               idx2 idx]
          (when (< j n)
            (aset result idx2 (- (aget sorted-data j) (aget sorted-data i)))
            (recur (inc j) (inc idx2))))
        (recur (inc i) (+ idx (- n 1 i)))))
    result))

(defn- distance-at
  "Get distance A[i,j] from flat upper triangular array.
  Requires i < j."
  ^double [^doubles dist-arr ^long n ^long i ^long j]
  (let [;; Index in flattened upper triangular array
        ;; For row i, entries start at: i*n - i*(i+1)/2
        row-start (- (* i n) (long (/ (* i (inc i)) 2)))
        ;; Column offset within row (j - i - 1)
        col-offset (- j i 1)]
    (aget dist-arr (+ row-start col-offset))))

(defn- compute-interval-distances-k1
  "Compute minimum cumulative distances for k=1 (single interval).
  For each span s, finds the minimum interval width covering s+1 points.
  Returns array where element i is the min distance for span (i+1)."
  ^doubles [^doubles dist-arr ^long n]
  (let [result (double-array (dec n))]
    (dotimes [span (dec n)]
      (let [interval-size (long (inc span))
            min-d (double
                   (loop [start (long 0)
                          md Double/MAX_VALUE]
                     (if (< (+ start interval-size) n)
                       (let [end (+ start interval-size)
                             d (distance-at dist-arr n start end)]
                         (recur (inc start) (Math/min md d)))
                       md)))]
        (aset result (int span) min-d)))
    result))

(defn- compute-interval-distances-next
  "Compute minimum cumulative distances for k intervals given k-1 results.
  Returns array where element i is the min distance for span (i+k)."
  ^doubles [^doubles dist-arr ^long n ^long k ^doubles prev-mins]
  (let [prev-len (long (alength prev-mins))
        result-len (- n k)
        result (double-array result-len)]
    (dotimes [span result-len]
      (let [total-span (long (+ span k 1))
            min-d (double
                   (loop [new-int-size (long 2)
                          md Double/MAX_VALUE]
                     (if (<= new-int-size (- total-span (* 2 (dec k))))
                       (let [prev-span (long (- total-span new-int-size))
                             prev-idx (long (- prev-span k))
                             prev-d (if (and (>= prev-idx 0) (< prev-idx prev-len))
                                      (aget prev-mins prev-idx)
                                      Double/MAX_VALUE)
                             new-d (loop [start (long (- total-span new-int-size))
                                          nd Double/MAX_VALUE]
                                     (if (>= start prev-span)
                                       (if (< (+ start new-int-size) n)
                                         (let [end (+ start (dec new-int-size))
                                               d (distance-at dist-arr n start end)]
                                           (recur (dec start) (Math/min nd d)))
                                         (recur (dec start) nd))
                                       nd))]
                         (recur (inc new-int-size) (Math/min md (+ prev-d (double new-d)))))
                       md)))]
        (aset result (int span) min-d)))
    result))

(defn- compute-all-interval-distances
  "Compute minimum cumulative distances for 1 to max-k disjoint intervals.
  Bottom-up computation that avoids redundant recursive calls.
  Returns a vector of arrays, where element i is the result for k=i+1."
  [^doubles dist-arr ^long n ^long max-k]
  (loop [k 1
         results []]
    (if (> k max-k)
      results
      (let [result (if (= k 1)
                     (compute-interval-distances-k1 dist-arr n)
                     (compute-interval-distances-next
                      dist-arr n k (nth results (- k 2))))]
        (recur (inc k) (conj results result))))))

(defn excess-mass
  "Compute excess mass statistic for testing k modes.

  The excess mass test statistic is max_λ{E_{n,k+1}(P_n,λ) - E_{n,k}(P_n,λ)}
  where E_{n,k}(P_n,λ) = sup{∑P_n(C_m) - λ|C_m|} over k disjoint intervals
  with endpoints at data points.

  Parameters:
  - data: sample values (will be sorted internally)
  - k: number of modes to test (tests H0: at most k modes)
  - opts: optional map with:
    - :rng-factory: 0-arity fn returning WellRng1024a for jitter (default: make-well-rng-1024a)

  Returns map with:
  - :statistic - the excess mass test statistic
  - :k - number of modes tested
  - :n - sample size

  Reference: Müller, D.W. and Sawitzki, G. (1991) 'Excess Mass Estimates
  and Tests for Multimodality' JASA 86, 738-746"
  ([data k] (excess-mass data k {}))
  ([data k {:keys [rng-factory]
            :or {rng-factory #(random/make-well-rng-1024a)}}]
   {:pre [(have? arr/typed-array? data)]}
   (let [n (arr/length data)
         k (long k)]
     (when (< n 3)
       (throw (ex-info "Need at least 3 data points for excess mass"
                       {:error :excess-mass/insufficient-data
                        :n n})))
     (when (< k 1)
       (throw (ex-info "k must be at least 1"
                       {:error :excess-mass/invalid-k
                        :k k})))
     (let [sorted-data (ensure-sorted-doubles data)
           ;; Check for ties and add jitter if needed
           has-ties? (loop [i (long 1)]
                       (if (< i n)
                         (if (= (aget sorted-data i) (aget sorted-data (dec i)))
                           true
                           (recur (inc i)))
                         false))
           sorted-data (if has-ties?
                         (let [jittered (add-jitter sorted-data (rng-factory))]
                           (java.util.Arrays/sort jittered)
                           jittered)
                         sorted-data)
           ;; Build distance matrix
           dist-arr (build-distance-matrix sorted-data)
           nd (double n)
           ;; Compute min distances for spans of 1 to k+1 intervals in one pass
           all-dists (compute-all-interval-distances dist-arr n (inc k))
           ^doubles min-dist-k (nth all-dists (dec k))
           ^doubles min-dist-k1 (nth all-dists k)
           len-k (long (alength min-dist-k))
           len-k1 (long (alength min-dist-k1))
           ;; Find all possible lambda values where transitions occur
           lambdas (atom #{})
           _ (dotimes [i (dec len-k)]
               (let [i (long i)
                     p1 (/ (double (+ i k 1)) nd)
                     p2 (/ (double (+ i k 2)) nd)
                     d1 (aget min-dist-k i)
                     d2 (aget min-dist-k (inc i))
                     dd (double (- d2 d1))]
                 (when (> (Math/abs dd) 1e-15)
                   (let [lam (/ (- p2 p1) dd)]
                     (when (pos? lam)
                       (swap! lambdas conj lam))))))
           _ (dotimes [i (dec len-k1)]
               (let [i (long i)
                     p1 (/ (double (+ i k 2)) nd)
                     p2 (/ (double (+ i k 3)) nd)
                     d1 (aget min-dist-k1 i)
                     d2 (aget min-dist-k1 (inc i))
                     dd (double (- d2 d1))]
                 (when (> (Math/abs dd) 1e-15)
                   (let [lam (/ (- p2 p1) dd)]
                     (when (pos? lam)
                       (swap! lambdas conj lam))))))
           lambda-vec (vec (sort @lambdas))
           ;; For each lambda, compute excess mass difference
           compute-em (fn ^double [^double lam ^long num-k ^doubles min-dists ^long len]
                        (loop [span (long 0)
                               max-em Double/NEGATIVE_INFINITY]
                          (if (< span len)
                            (let [p (/ (double (+ span num-k 1)) nd)
                                  d (aget min-dists span)
                                  em (- p (* lam d))]
                              (recur (inc span) (Math/max max-em em)))
                            max-em)))
           ;; Compute max difference over all lambdas
           max-diff (double
                     (if (empty? lambda-vec)
                       (let [lam 1.0
                             ^double em-k (compute-em lam k min-dist-k len-k)
                             ^double em-k1 (compute-em lam (inc k) min-dist-k1 len-k1)]
                         (- em-k1 em-k))
                       (loop [idx (long 0)
                              max-d Double/NEGATIVE_INFINITY]
                         (if (< idx (count lambda-vec))
                           (let [lam (nth lambda-vec idx)
                                 ^double em-k (compute-em lam k min-dist-k len-k)
                                 ^double em-k1 (compute-em lam (inc k) min-dist-k1 len-k1)
                                 d (- em-k1 em-k)]
                             (recur (inc idx) (Math/max max-d d)))
                           max-d))))]
       {:statistic (Math/max 0.0 max-diff)
        :k k
        :n n}))))

;;; DCT-II implementation

(defn dct-ii-direct
  "Discrete Cosine Transform Type II.
  Direct O(n²) implementation. Retained for testing/reference.

  DCT-II formula: X_k = sum_{n=0}^{N-1} x_n * cos(π/N * (n + 0.5) * k)

  Returns vector of N DCT coefficients."
  ^doubles [^doubles data]
  (let [n (alength data)
        result (double-array n)
        pi-n (/ Math/PI (double n))]
    (dotimes [k n]
      (let [kd (double k)
            sum (double
                 (loop [i 0
                        acc 0.0]
                   (if (< i n)
                     (recur (inc i)
                            (+ acc (* (aget data i)
                                      (Math/cos (* pi-n (+ (double i) 0.5) kd)))))
                     acc)))]
        (aset result k sum)))
    result))

(defn dct-via-fft
  "Discrete Cosine Transform Type II via FFT.
  O(n log n) implementation using 4N-point FFT.

  Algorithm: Place input at odd positions in 4N array,
  compute FFT, extract real parts.

  Returns array of n DCT-II coefficients."
  ^doubles [^doubles data]
  (let [n (alength data)
        n4 (* 4 n)
        ;; Create 4N-length interleaved complex array
        ;; Place x[k] at position 2k+1 for k = 0..N-1
        ^doubles extended (double-array (* 2 n4))
        _ (dotimes [k n]
            ;; Position 2k+1 in the signal = index (2k+1)*2 in interleaved array
            (aset extended (* 2 (inc (* 2 k))) (aget data k)))
        ;; Make it even-symmetric: extended[4N-k] = extended[k] for k = 1..2N-1
        ;; In interleaved format: position 4N-k has real index 2*(4N-k)
        _ (dotimes [k (dec (* 2 n))]
            (let [src-pos (inc k)  ; positions 1 to 2N-1
                  dst-pos (- n4 src-pos)]
              (aset extended (* 2 dst-pos) (aget extended (* 2 src-pos)))))
        ;; Compute 4N-point FFT
        _ (fft/fft! extended)
        ;; Extract DCT coefficients: real parts at positions 0..N-1, scaled by 0.5
        result (double-array n)]
    (dotimes [k n]
      (aset result k (* 0.5 (aget extended (* 2 k)))))
    result))

(defn dct-ii
  "Discrete Cosine Transform Type II.
  O(n log n) implementation using FFT.

  DCT-II formula: X_k = sum_{n=0}^{N-1} x_n * cos(π/N * (n + 0.5) * k)

  Returns array of n DCT-II coefficients."
  ^doubles [^doubles data]
  (dct-via-fft data))

;;; Linear binning

(defn linear-bin
  "Bin data onto a regular grid using linear interpolation.
  Returns vector of bin weights that sum to 1.0.

  Each data point contributes to two adjacent bins proportionally
  to its distance from bin centers.

  Requires a typed array (DoubleArray or LongArray)."
  ^doubles [data ^doubles grid]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (alength grid)
        weights (double-array n)
        x-min (aget grid 0)
        x-max (aget grid (dec n))
        dx (/ (- x-max x-min) (dec n))
        n-data (arr/length data)
        add-weight! (fn [^double x]
                      (let [x (max x-min (min x-max x))
                            pos (/ (- x x-min) dx)
                            i-low (long (Math/floor pos))
                            i-low (min i-low (- n 2))
                            i-high (inc i-low)
                            w-high (- pos i-low)
                            w-low (- 1.0 w-high)]
                        (aset weights i-low (+ (aget weights i-low) w-low))
                        (aset weights i-high (+ (aget weights i-high) w-high))))]
    (arr/dfold data
               (fn [_ ^double x]
                 (add-weight! x)
                 nil)
               nil)
    ;; Normalize to sum to 1
    (let [total (double n-data)]
      (dotimes [i n]
        (aset weights i (/ (aget weights i) total))))
    weights))

;;; ISJ bandwidth selection

(defn- fixed-point
  "Fixed-point function for ISJ bandwidth selection.
  Equation from Botev et al. 'Kernel Density Estimation via Diffusion'."
  ^double [^double t ^long n ^doubles i-sq ^doubles a2]
  (let [n-terms (alength i-sq)
        f (double
           (loop [i 0
                  sum 0.0]
             (if (< i n-terms)
               (let [i-val (aget i-sq i)
                     a-val (aget a2 i)
                     exp-t (* i-val i-val Math/PI Math/PI t)]
                 (recur (inc i)
                        (+ sum (* i-val i-val a-val
                                  (Math/exp (* -2.0 exp-t))))))
               sum)))]
    (- (Math/pow
        (/ (* 2.0 (double n) (Math/sqrt Math/PI) f)
           1.0)
        -0.4)
       t)))

(defn- isj-solve
  "Solve for optimal ISJ bandwidth using bisection.
  Returns t* parameter or nil if no solution found."
  [^long n ^doubles i-sq ^doubles a2]
  (let [tol 1e-12
        max-t 1.0]
    (loop [lo 1e-10
           hi max-t
           its 0]
      (when (< its 100)
        (let [mid (* 0.5 (+ lo hi))
              f-mid (fixed-point mid n i-sq a2)]
          (cond
            (< (Math/abs f-mid) tol) mid
            (< (Math/abs (- hi lo)) tol) mid
            (neg? f-mid) (recur lo mid (inc its))
            :else (recur mid hi (inc its))))))))

(defn silverman-bandwidth
  "Silverman's rule of thumb bandwidth selector.
  h = 0.9 * min(σ, IQR/1.34) * n^(-1/5)

  A simple fallback when ISJ doesn't converge.
  Requires a typed array (DoubleArray or LongArray)."
  ^double [data]
  {:pre [(have? arr/typed-array? data)]}
  (let [n      (arr/length data)
        sigma  (Math/sqrt (core/variance data))
        sorted (arr/sorted data)
        q1     (core/quantile 0.25 sorted)
        q3     (core/quantile 0.75 sorted)
        iqr    (- q3 q1)
        spread (min sigma (/ iqr 1.34))]
    (* 0.9 spread (Math/pow (double n) -0.2))))

(defn isj-bandwidth
  "Improved Sheather-Jones bandwidth selector.

  Uses DCT-based algorithm from Botev et al. for optimal bandwidth
  selection that works well for multimodal distributions.

  Falls back to Silverman's rule if ISJ doesn't converge or gives
  an unreasonable result (bandwidth > half the data range).

  Requires a typed array (DoubleArray or LongArray)."
  ^double [data]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (arr/length data)
        n-grid 1024
        [x-min x-max] (data-min-max data)
        x-min (double x-min)
        x-max (double x-max)
        r (- x-max x-min)]
    (if (zero? r)
      1e-10
      (let [grid (double-array n-grid)
            _ (dotimes [i n-grid]
                (aset grid i (+ x-min (* r (/ (double i) (double (dec n-grid)))))))
            binned (linear-bin data grid)
            dct (dct-ii binned)
            i-sq (double-array (dec n-grid))
            a2 (double-array (dec n-grid))
            _ (dotimes [i (dec n-grid)]
                (let [k (inc i)]
                  (aset i-sq i (double (* k k)))
                  (aset a2 i (let [d (aget dct k)] (* d d)))))
            t-star (isj-solve n i-sq a2)
            h-isj (when t-star (* (Math/sqrt (double t-star)) r))
            h-silv (silverman-bandwidth data)]
        (if (and h-isj (< (double h-isj) (* 0.5 r)))
          h-isj
          h-silv)))))

;;; Gaussian kernel density estimation

(defn- gaussian-kernel
  "Standard Gaussian kernel K(u) = (1/√2π) * exp(-u²/2)."
  ^double [^double u]
  (* (/ 1.0 (Math/sqrt (* 2.0 Math/PI)))
     (Math/exp (* -0.5 u u))))

(defn gaussian-kde-direct
  "Compute Gaussian kernel density estimate at grid points.
  Direct O(n×m) implementation. Retained for testing/reference.

  Parameters:
  - data: typed array of sample values (DoubleArray or LongArray)
  - bandwidth: kernel bandwidth (h)
  - grid: vector of evaluation points

  Returns vector of density values at each grid point.
  Requires a typed array (DoubleArray or LongArray)."
  ^doubles [data ^double bandwidth ^doubles grid]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (arr/length data)
        n-grid (alength grid)
        density (double-array n-grid)
        h bandwidth
        nd (double n)
        data-a (.array ^DoubleArray (ensure-double-array data))]
    (dotimes [i n-grid]
      (let [x (aget grid i)
            sum (double
                 (loop [j 0
                        acc 0.0]
                   (if (< j n)
                     (let [xj (aget ^doubles data-a j)
                           u (/ (- x xj) h)]
                       (recur (inc j)
                              (+ acc (gaussian-kernel u))))
                     acc)))]
        (aset density i (/ sum (* nd h)))))
    density))

(defn gaussian-kde-fft
  "Compute Gaussian kernel density estimate using FFT-based convolution.
  O(m log m) implementation where m is grid size.

  Algorithm:
  1. Extend grid by 4h on each side to avoid circular convolution edge effects
  2. Bin data onto extended grid using linear interpolation
  3. FFT the binned histogram
  4. Multiply by Gaussian kernel in frequency domain
  5. IFFT to get density estimate
  6. Extract the central portion corresponding to original grid

  Parameters:
  - data: typed array of sample values (DoubleArray or LongArray)
  - bandwidth: kernel bandwidth (h)
  - grid: array of evaluation points

  Returns array of density values at each grid point.
  Requires a typed array (DoubleArray or LongArray)."
  ^doubles [data ^double bandwidth ^doubles grid]
  {:pre [(have? arr/typed-array? data)]}
  (let [m (alength grid)
        x-min (aget grid 0)
        x-max (aget grid (dec m))
        dx (/ (- x-max x-min) (double (dec m)))
        ;; Extend grid by 4h on each side to avoid edge effects from circular convolution
        ;; 4h covers >99.99% of Gaussian kernel support
        extension (* 4.0 bandwidth)
        ext-points (long (Math/ceil (/ extension dx)))
        ext-min (- x-min (* ext-points dx))
        m-ext (long (+ m (* 2 ext-points)))
        ;; Pad extended size to next power of 2 for FFT
        m-fft (fft/next-power-of-2 m-ext)
        ;; Create extended grid
        ^doubles ext-grid (double-array m-ext)
        _ (dotimes [i m-ext]
            (aset ext-grid i (+ ext-min (* dx (double i)))))
        ;; Bin data onto extended grid (weights sum to 1.0)
        ^doubles binned (linear-bin data ext-grid)
        ;; Zero-pad to power of 2 and convert to interleaved complex
        ^doubles binned-complex (fft/zero-pad-real binned m-fft)
        ;; FFT the binned histogram
        _ (fft/fft! binned-complex)
        ;; Compute Gaussian kernel in frequency domain and multiply
        ;; For a Gaussian with bandwidth h, its FFT is also Gaussian
        ;; The standard result: FT of (1/√(2π)h) exp(-x²/(2h²)) is exp(-2π²h²f²)
        h-scaled (/ bandwidth dx)  ; bandwidth in grid units
        m-fft-half (bit-shift-right m-fft 1)
        _ (dotimes [k m-fft]
            (let [;; Frequency index (handles wrap-around for DFT)
                  freq (if (< k m-fft-half)
                         (double k)
                         (- (double k) (double m-fft)))
                  ;; Gaussian in frequency domain: exp(-2π²σ²f²)
                  ;; where σ = h (in grid units) and f = freq/m-fft (normalized)
                  exponent (* -2.0 Math/PI Math/PI h-scaled h-scaled
                              (/ (* freq freq) (* (double m-fft) (double m-fft))))
                  kernel-val (Math/exp exponent)
                  idx (* 2 k)
                  ;; Multiply in-place (kernel is real)
                  binned-re (aget binned-complex idx)
                  binned-im (aget binned-complex (inc idx))]
              (aset binned-complex idx (* binned-re kernel-val))
              (aset binned-complex (inc idx) (* binned-im kernel-val))))
        ;; IFFT to get density
        _ (fft/ifft! binned-complex)
        ;; Extract real parts for original grid points (skip ext-points at start)
        ;; Scale by 1/dx to get proper density (integral = 1)
        ^doubles density (double-array m)
        scale (/ 1.0 dx)
        ;; Small threshold to eliminate numerical noise (prevents spurious modes)
        noise-threshold 1e-14]
    (dotimes [i m]
      (let [ext-idx (+ i ext-points)
            val (* scale (aget binned-complex (* 2 ext-idx)))]
        ;; Clamp tiny values to zero to eliminate numerical noise
        (aset density i (if (< val noise-threshold) 0.0 val))))
    density))

(defn gaussian-kde
  "Compute Gaussian kernel density estimate at grid points.

  Parameters:
  - data: typed array of sample values (DoubleArray or LongArray)
  - bandwidth: kernel bandwidth (h)
  - grid: vector of evaluation points

  Returns vector of density values at each grid point.
  Requires a typed array (DoubleArray or LongArray)."
  ^doubles [data ^double bandwidth ^doubles grid]
  (gaussian-kde-fft data bandwidth grid))

;;; Mode finding

(defn find-modes
  "Find modes (local maxima) in a density estimate.

  Returns vector of maps with :location and :density for each mode,
  sorted by density (highest first)."
  [^doubles grid ^doubles density]
  (let [n (alength density)]
    (->> (loop [i 1
                result []]
           (if (< i (dec n))
             (let [d-prev (aget density (dec i))
                   d-curr (aget density i)
                   d-next (aget density (inc i))]
               (if (and (> d-curr d-prev)
                        (> d-curr d-next))
                 (recur (inc i)
                        (conj result {:location (aget grid i)
                                      :density d-curr
                                      :index i}))
                 (recur (inc i) result)))
             result))
         (sort-by :density >)
         vec)))

;;; Bootstrap confidence bands

(defn kde-bootstrap-sample
  "Generate a bootstrap sample of KDE density at fixed grid points.
  Uses the same bandwidth for all bootstrap iterations.
  rng is a mutable WellRng1024a instance.
  Requires a typed array (DoubleArray or LongArray)."
  [data ^double bandwidth ^doubles grid ^WellRng1024a rng]
  {:pre [(have? arr/typed-array? data)]}
  (let [resampled (sampling/sample-doubles (ensure-double-array data) rng)]
    (gaussian-kde resampled bandwidth grid)))

(defn kde-confidence-bands
  "Compute bootstrap confidence bands for KDE.

  Parameters:
  - data: original sample data
  - bandwidth: kernel bandwidth
  - grid: evaluation grid points
  - n-bootstrap: number of bootstrap samples (default 200)
  - alpha: confidence level (default 0.05 for 95% CI)
  - rng-factory: 0-arity fn returning WellRng1024a (default: make-well-rng-1024a)

  Returns map with :lower and :upper vectors."
  ([data bandwidth grid]
   (kde-confidence-bands data bandwidth grid {}))
  ([data bandwidth ^doubles grid {:keys [n-bootstrap alpha rng-factory]
                                  :or {n-bootstrap 200
                                       alpha 0.05
                                       rng-factory #(random/make-well-rng-1024a)}}]
   {:pre [(have? arr/typed-array? data)]}
   (let [n-grid (alength grid)
         samples (vec (for [_ (range n-bootstrap)]
                        (kde-bootstrap-sample data bandwidth grid (rng-factory))))
         alpha-low (/ (double alpha) 2.0)
         alpha-hi (- 1.0 alpha-low)
         lower (double-array n-grid)
         upper (double-array n-grid)]
     (dotimes [i n-grid]
       (let [point-samples (double-array (mapv #(aget ^doubles % i) samples))]
         (java.util.Arrays/sort point-samples)
         (aset lower i (quantile-primitive alpha-low point-samples))
         (aset upper i (quantile-primitive alpha-hi point-samples))))
     {:lower lower
      :upper upper})))

;;; Mode confidence intervals

(defn mode-confidence-intervals
  "Compute bootstrap confidence intervals for mode locations.

  Parameters:
  - data: original sample data (typed array)
  - bandwidth: kernel bandwidth
  - grid: evaluation grid
  - n-modes: number of modes to track (default 3)
  - n-bootstrap: number of bootstrap samples (default 200)
  - alpha: confidence level (default 0.05)
  - rng-factory: 0-arity fn returning WellRng1024a (default: make-well-rng-1024a)

  Returns vector of mode CIs, each with :location, :ci-lower, :ci-upper.

  Requires a typed array (DoubleArray or LongArray)."
  ([data bandwidth grid n-modes]
   (mode-confidence-intervals data bandwidth grid n-modes {}))
  ([data bandwidth ^doubles grid n-modes
    {:keys [n-bootstrap alpha rng-factory]
     :or {n-bootstrap 200
          alpha 0.05
          rng-factory #(random/make-well-rng-1024a)}}]
   {:pre [(have? arr/typed-array? data)]}
   (let [density (gaussian-kde data bandwidth grid)
         orig-modes (vec (take n-modes (find-modes grid density)))
         boot-modes (vec (for [_ (range n-bootstrap)]
                           (let [boot-density (kde-bootstrap-sample
                                               data bandwidth grid (rng-factory))]
                             (vec (take n-modes (find-modes grid boot-density))))))
         alpha-low (/ (double alpha) 2.0)
         alpha-high (- 1.0 alpha-low)]
     (vec
      (for [[i mode] (map-indexed vector orig-modes)]
        (let [boot-locs (keep #(when-let [m (get % i)]
                                 (:location m))
                              boot-modes)
              sorted-arr (double-array (sort boot-locs))]
          (if (pos? (alength sorted-arr))
            (assoc mode
                   :ci-lower (quantile-primitive alpha-low sorted-arr)
                   :ci-upper (quantile-primitive alpha-high sorted-arr))
            mode)))))))

;;; Silverman's test for multimodality

(defn- count-modes-with-grid
  "Count modes using a pre-computed grid. Internal helper for critical-bandwidth.
  Avoids recomputing grid bounds when same data is used with different bandwidths."
  ^long [data ^double bandwidth ^doubles grid]
  (let [density (gaussian-kde data bandwidth grid)]
    (count (find-modes grid density))))

(defn- make-mode-counting-grid
  "Create grid for mode counting from data bounds.
  Grid extends 10% beyond data range on each side."
  ^doubles [^double x-min ^double x-max ^long n-points]
  (let [margin (/ (- x-max x-min) 10.0)
        g-min (- x-min margin)
        g-max (+ x-max margin)
        g-range (- g-max g-min)
        grid (double-array n-points)]
    (dotimes [i n-points]
      (aset grid i (+ g-min (* g-range (/ (double i) (double (dec n-points)))))))
    grid))

(defn count-modes
  "Count number of modes in KDE with given bandwidth.
  Creates a grid and counts local maxima in the density estimate.
  Requires a typed array (DoubleArray or LongArray)."
  ^long [data ^double bandwidth ^long n-points]
  {:pre [(have? arr/typed-array? data)]}
  (let [[x-min x-max] (data-min-max data)
        grid (make-mode-counting-grid x-min x-max n-points)]
    (count-modes-with-grid data bandwidth grid)))

(defn critical-bandwidth
  "Find smallest bandwidth giving at most k modes via binary search.

  Returns the critical bandwidth h_k, which is the smallest bandwidth
  such that the KDE has at most k modes.

  Parameters:
  - data: typed array of sample values
  - k: maximum number of modes
  - opts: optional map with :tol (tolerance, default 1e-6),
          :n-points (grid size, default 512),
          :h-max (optional upper bound, useful when computing h_k knowing h_{k-1})

  Requires a typed array (DoubleArray or LongArray)."
  ^double [data ^long k {:keys [tol n-points h-max]
                         :or {tol 1e-6
                              n-points 512}}]
  {:pre [(have? arr/typed-array? data)]}
  (let [n-pts (long n-points)
        tol (double tol)
        sigma (Math/sqrt (core/variance data))
        ;; Pre-compute grid once for all binary search iterations
        [x-min x-max] (data-min-max data)
        grid (make-mode-counting-grid x-min x-max n-pts)
        ;; Start with range from very small to provided h-max or Silverman bandwidth * 2
        h-upper (double (or h-max (* 2.0 (silverman-bandwidth data))))
        h-min (/ sigma 100.0)]
    ;; Binary search for smallest h with <= k modes
    (loop [lo h-min
           hi h-upper
           its 0]
      (if (or (>= its 100) (< (- hi lo) (* tol (+ hi lo) 0.5)))
        hi
        (let [mid (* 0.5 (+ lo hi))
              n-modes (count-modes-with-grid data mid grid)]
          (if (<= n-modes k)
            ;; Can achieve <= k modes, try smaller bandwidth
            (recur lo mid (inc its))
            ;; Too many modes, need larger bandwidth
            (recur mid hi (inc its))))))))

(defn critical-bandwidths
  "Compute critical bandwidths for k=1 to max-k efficiently.

  Uses h_{k-1} as upper bound for h_k since h_k < h_{k-1} (more modes
  require less smoothing). This avoids redundant binary search iterations
  when testing multiple k values.

  Parameters:
  - data: typed array of sample values
  - max-k: maximum number of modes to compute bandwidth for
  - opts: optional map with :tol (tolerance), :n-points (grid size)

  Returns a map from k to critical bandwidth h_k."
  [data ^long max-k opts]
  {:pre [(have? arr/typed-array? data)]}
  (loop [k 1
         prev-h nil
         results {}]
    (if (> k max-k)
      results
      (let [h-k (critical-bandwidth data k (if prev-h
                                             (assoc opts :h-max prev-h)
                                             opts))]
        (recur (inc k) h-k (assoc results k h-k))))))

(defn- find-antimodes
  "Find antimodes (local minima) in a density estimate.
  Returns vector of maps with :location and :density for each antimode,
  sorted by location (left to right)."
  [^doubles grid ^doubles density]
  (let [n (alength density)]
    (->> (loop [i 1
                result []]
           (if (< i (dec n))
             (let [d-prev (aget density (dec i))
                   d-curr (aget density i)
                   d-next (aget density (inc i))]
               (if (and (< d-curr d-prev)
                        (< d-curr d-next))
                 (recur (inc i)
                        (conj result {:location (aget grid i)
                                      :density d-curr}))
                 (recur (inc i) result)))
             result))
         (sort-by :location)
         vec)))

(defn locate-modes
  "Find mode and antimode locations using critical bandwidth.

  Given target number of modes k, finds the critical bandwidth h_k
  (smallest bandwidth with at most k modes) and locates both modes
  (local maxima) and antimodes (local minima) at that bandwidth.

  Parameters:
  - data: typed array of sample values
  - k: target number of modes
  - opts: optional map with :n-points (grid size, default 512),
          :tol (tolerance, default 1e-6),
          :cached-critical-bandwidth (optional pre-computed bandwidth)

  Returns:
  {:modes [{:location, :density} ...] - sorted by location
   :antimodes [{:location, :density} ...] - sorted by location
   :critical-bandwidth h_k}

  Requires a typed array (DoubleArray or LongArray)."
  [data ^long k {:keys [n-points tol cached-critical-bandwidth]
                 :or {n-points 512
                      tol 1e-6}}]
  {:pre [(have? arr/typed-array? data)]}
  (let [n-pts (long n-points)
        h (double (or cached-critical-bandwidth
                      (critical-bandwidth data k {:n-points n-pts :tol tol})))
        [x-min x-max] (data-min-max data)
        x-min (double x-min)
        x-max (double x-max)
        margin (/ (- x-max x-min) 10.0)
        g-min (- x-min margin)
        g-max (+ x-max margin)
        g-range (- g-max g-min)
        grid (double-array n-pts)]
    (dotimes [i n-pts]
      (aset grid i (+ g-min (* g-range (/ (double i) (double (dec n-pts)))))))
    (let [density (gaussian-kde data h grid)
          modes (->> (find-modes grid density)
                     (map #(dissoc % :index))
                     (sort-by :location)
                     vec)
          antimodes (find-antimodes grid density)]
      {:modes modes
       :antimodes antimodes
       :critical-bandwidth h})))

(defn silverman-bootstrap-sample
  "Generate a smoothed bootstrap sample for Silverman's test.

  Uses rescaled bootstrap from Silverman (1981):
  y_i = mean + (X*_i - mean + h * epsilon_i) / sqrt(1 + h²/σ²)

  This ensures the bootstrap sample has the same variance as the original.
  Returns a DoubleArray.
  rng is a mutable WellRng1024a instance.
  Requires a typed array (DoubleArray or LongArray)."
  ^DoubleArray [data ^double bandwidth ^WellRng1024a rng]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (arr/length data)
        sigma-sq (core/variance data)
        mean-val (core/mean data)
        scale (Math/sqrt (+ 1.0 (/ (* bandwidth bandwidth) sigma-sq)))
        ;; Sample with replacement - mutates rng state
        ^DoubleArray resampled (sampling/sample-doubles (ensure-double-array data) rng)
        ^doubles resampled-a (.array resampled)
        ;; Generate smoothed sample using Box-Muller for normal random
        result (double-array n)]
    (dotimes [i n]
      (let [x-star (aget resampled-a i)
            ;; Box-Muller for normal random - consume 2 uniform values per iteration
            u1 (max 1e-10 (random/next-double! rng))
            u2 (random/next-double! rng)
            epsilon (* (Math/sqrt (* -2.0 (Math/log u1)))
                       (Math/cos (* 2.0 Math/PI u2)))
            y (+ mean-val (/ (+ (- x-star mean-val) (* bandwidth epsilon)) scale))]
        (aset result i y)))
    (DoubleArray. result)))

(defn- hall-york-lambda
  "Hall-York asymptotic correction factor for k=1.

  Approximation from Hall & York (2001) Table 2.
  λ_α ≈ a + b*α + c*α² where coefficients fit the asymptotic distribution."
  ^double [^double alpha]
  ;; Approximate formula based on Hall-York (2001)
  ;; For α = 0.05, λ ≈ 1.06; for α = 0.10, λ ≈ 1.05
  (let [a 1.07
        b -0.11
        c 0.08]
    (+ a (* b alpha) (* c alpha alpha))))

(defn silverman-test
  "Silverman's bootstrap test for H0: at most k modes.

  Tests the null hypothesis that the underlying density has at most k modes.
  For k=1, applies Hall-York asymptotic correction for better calibration.
  For k>1, uses standard bootstrap (may be conservative).

  Parameters:
  - data: typed array of sample values
  - k: number of modes under H0
  - opts: optional map with:
    - :n-bootstrap (default 200)
    - :n-points (default 512)
    - :alpha (for Hall-York correction, default 0.05)
    - :tol (for critical bandwidth search, default 1e-6)
    - :cached-critical-bandwidth (optional pre-computed bandwidth, skips search if provided)
    - :rng-factory: 0-arity fn returning WellRng1024a (default: make-well-rng-1024a)

  Returns map with:
  - :k - number of modes tested
  - :critical-bandwidth - bandwidth giving exactly k modes
  - :p-value - proportion of bootstrap samples with > k modes
  - :corrected? - whether Hall-York correction was applied

  Requires a typed array (DoubleArray or LongArray)."
  [data ^long k {:keys [n-bootstrap n-points alpha tol cached-critical-bandwidth rng-factory]
                 :or {n-bootstrap 200
                      n-points 512
                      alpha 0.05
                      tol 1e-6
                      rng-factory #(random/make-well-rng-1024a)}}]
  {:pre [(have? arr/typed-array? data)]}
  (let [n-pts (long n-points)
        ;; Use pre-computed critical bandwidth or compute it
        h-crit (double (or cached-critical-bandwidth
                           (critical-bandwidth data k {:tol tol :n-points n-pts})))
        ;; Bootstrap: count how many times we get > k modes
        exceeds (atom 0)]
    (dotimes [_ n-bootstrap]
      (let [rng (rng-factory)
            ;; Generate smoothed bootstrap sample using the helper function
            boot-sample (silverman-bootstrap-sample data h-crit rng)
            ;; Count modes with critical bandwidth
            boot-modes (count-modes boot-sample h-crit n-pts)]
        (when (> boot-modes k)
          (swap! exceeds inc))))
    ;; Calculate p-value
    (let [raw-p (/ (double @exceeds) (double n-bootstrap))
          ;; Apply Hall-York correction for k=1
          corrected? (= k 1)
          p-value (if corrected?
                    (let [lambda (hall-york-lambda alpha)]
                      ;; Correction adjusts the critical bandwidth
                      ;; Here we approximate by scaling the p-value
                      (min 1.0 (* raw-p lambda)))
                    raw-p)]
      {:k k
       :critical-bandwidth h-crit
       :p-value p-value
       :raw-p-value raw-p
       :corrected? corrected?})))

(defn acr-test
  "ACR test for H0: at most k modes.

  Combines critical bandwidth and excess mass approaches from
  Ameijeiras-Alonso, Crujeiras, and Rodríguez-Casal (2019).

  Unlike Silverman's test which uses mode count as test statistic,
  ACR uses excess mass which provides better calibration.

  Parameters:
  - data: typed array of sample values
  - k: number of modes under H0
  - opts: optional map with:
    - :n-bootstrap (default 200)
    - :n-points (default 512)
    - :tol (for critical bandwidth search, default 1e-6)
    - :cached-critical-bandwidth (optional pre-computed bandwidth, skips search if provided)
    - :rng-factory: 0-arity fn returning WellRng1024a (default: make-well-rng-1024a)

  Returns map with:
  - :k - number of modes tested
  - :excess-mass - observed excess mass statistic
  - :critical-bandwidth - bandwidth giving exactly k modes
  - :p-value - proportion of bootstrap excess masses >= observed

  Reference: Ameijeiras-Alonso et al. (2019) 'Mode testing, critical
  bandwidth and excess mass' TEST 28, 900-919

  Requires a typed array (DoubleArray or LongArray)."
  [data ^long k {:keys [n-bootstrap n-points tol cached-critical-bandwidth rng-factory]
                 :or {n-bootstrap 200
                      n-points 512
                      tol 1e-6
                      rng-factory #(random/make-well-rng-1024a)}}]
  {:pre [(have? arr/typed-array? data)]}
  (let [n-pts (long n-points)
        ;; Use pre-computed critical bandwidth or compute it
        h-crit (double (or cached-critical-bandwidth
                           (critical-bandwidth data k {:tol tol :n-points n-pts})))
        ;; Compute observed excess mass statistic
        observed-em (:statistic (excess-mass data k {:rng-factory rng-factory}))
        observed-em (double observed-em)
        ;; Bootstrap: count how many times bootstrap excess mass >= observed
        exceeds (atom 0)]
    (dotimes [_ n-bootstrap]
      (let [rng (rng-factory)
            ;; Generate smoothed bootstrap sample at critical bandwidth
            boot-sample (silverman-bootstrap-sample data h-crit rng)
            ;; Compute excess mass for bootstrap sample
            boot-em (:statistic (excess-mass boot-sample k {:rng-factory rng-factory}))]
        (when (>= (double boot-em) observed-em)
          (swap! exceeds inc))))
    ;; Calculate p-value
    (let [p-value (/ (double @exceeds) (double n-bootstrap))]
      {:k k
       :excess-mass observed-em
       :critical-bandwidth h-crit
       :p-value p-value})))

;;; Main KDE function

(defn kde
  "Compute KDE analysis on sample data.

  Parameters:
  - data: typed array of sample values (DoubleArray or LongArray)
  - opts: optional map with:
    - :n-points: grid size (default 512)
    - :bandwidth: override bandwidth (default: ISJ selection)
    - :n-bootstrap: bootstrap samples for confidence bands (default 200)
    - :alpha: confidence level (default 0.05)
    - :rng-factory: 0-arity fn returning WellRng1024a (default: make-well-rng-1024a)

  Returns map with:
  - :type :criterium/kde
  - :bandwidth: selected or provided bandwidth
  - :grid: evaluation points
  - :density: density values at grid points
  - :lower-band: lower confidence band
  - :upper-band: upper confidence band
  - :n: sample size

  Note: Mode detection is now a separate analysis step. Use silverman-test
  and mode-confidence-intervals for statistical mode analysis.

  Requires a typed array (DoubleArray or LongArray)."
  ([data] (kde data {}))
  ([data {:keys [n-points bandwidth n-bootstrap alpha rng-factory]
          :or   {n-points    512
                 n-bootstrap 200
                 alpha       0.05
                 rng-factory #(random/make-well-rng-1024a)}}]
   {:pre [(have? arr/typed-array? data)]}
   (let [n (arr/length data)]
     (when (zero? n)
       (throw (ex-info "Input data cannot be empty"
                       {:error :kde/no-data})))
     (let [[^double x-min ^double x-max] (data-min-max data)]
       (when (= x-min x-max)
         (throw (ex-info "All values are the same - cannot compute KDE"
                         {:error :kde/constant-data
                          :value x-min})))
       (let [margin  (/ (- x-max x-min) 10.0)
             g-min   (- x-min margin)
             g-max   (+ x-max margin)
             g-range (- g-max g-min)
             n-pts   (long n-points)
             grid    (double-array n-pts)
             _       (dotimes [i n-pts]
                       (aset grid i (+ g-min (* g-range
                                                (/ (double i) (double (dec n-pts)))))))
             h       (double (or bandwidth (isj-bandwidth data)))
             density (gaussian-kde data h grid)
             bands   (kde-confidence-bands data h grid
                                           {:n-bootstrap n-bootstrap
                                            :alpha       alpha
                                            :rng-factory rng-factory})]
         {:type       :criterium/kde
          :bandwidth  h
          :grid       (vec grid)
          :density    (vec density)
          :lower-band (vec (:lower bands))
          :upper-band (vec (:upper bands))
          :n          n})))))
