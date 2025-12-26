(ns criterium.util.kde
  "Kernel Density Estimation utilities.

  Provides ISJ (Improved Sheather-Jones) bandwidth selection, Gaussian kernel
  density estimation, bootstrap confidence bands, and mode finding."
  (:require
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]))

;;; DCT-II implementation

(defn dct-ii
  "Discrete Cosine Transform Type II.
  Direct O(n²) implementation without FFT dependency.

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

;;; Linear binning

(defn linear-bin
  "Bin data onto a regular grid using linear interpolation.
  Returns vector of bin weights that sum to 1.0.

  Each data point contributes to two adjacent bins proportionally
  to its distance from bin centers."
  ^doubles [data ^doubles grid]
  (let [n (alength grid)
        weights (double-array n)
        x-min (aget grid 0)
        x-max (aget grid (dec n))
        dx (/ (- x-max x-min) (dec n))
        n-data (count data)]
    (doseq [x data]
      (let [x (double x)
            ;; Clamp to grid range
            x (max x-min (min x-max x))
            ;; Find position in grid
            pos (/ (- x x-min) dx)
            i-low (long (Math/floor pos))
            i-low (min i-low (- n 2))
            i-high (inc i-low)
            ;; Linear interpolation weights
            w-high (- pos i-low)
            w-low (- 1.0 w-high)]
        (aset weights i-low (+ (aget weights i-low) w-low))
        (aset weights i-high (+ (aget weights i-high) w-high))))
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

  A simple fallback when ISJ doesn't converge."
  ^double [data]
  (let [n (count data)
        sigma (Math/sqrt (stats/variance data))
        sorted (sort data)
        q1 (double (stats/quantile 0.25 sorted))
        q3 (double (stats/quantile 0.75 sorted))
        iqr (- q3 q1)
        spread (min sigma (/ iqr 1.34))]
    (* 0.9 spread (Math/pow (double n) -0.2))))

(defn isj-bandwidth
  "Improved Sheather-Jones bandwidth selector.

  Uses DCT-based algorithm from Botev et al. for optimal bandwidth
  selection that works well for multimodal distributions.

  Falls back to Silverman's rule if ISJ doesn't converge or gives
  an unreasonable result (bandwidth > half the data range)."
  ^double [data]
  (let [data (vec data)
        n (count data)
        n-grid 1024
        x-min (double (reduce min data))
        x-max (double (reduce max data))
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

(defn gaussian-kde
  "Compute Gaussian kernel density estimate at grid points.

  Parameters:
  - data: vector of sample values
  - bandwidth: kernel bandwidth (h)
  - grid: vector of evaluation points

  Returns vector of density values at each grid point."
  ^doubles [data ^double bandwidth ^doubles grid]
  (let [n (long (count data))
        n-grid (alength grid)
        density (double-array n-grid)
        h bandwidth
        nd (double n)
        data-a (double-array data)]
    (dotimes [i n-grid]
      (let [x (aget grid i)
            sum (double
                 (loop [j 0
                        acc 0.0]
                   (if (< j n)
                     (let [xj (aget data-a j)
                           u (/ (- x xj) h)]
                       (recur (inc j)
                              (+ acc (gaussian-kernel u))))
                     acc)))]
        (aset density i (/ sum (* nd h)))))
    density))

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
  rng is a lazy sequence of random doubles in [0,1)."
  [data ^double bandwidth ^doubles grid rng]
  (let [resampled (stats/sample data rng)]
    (gaussian-kde resampled bandwidth grid)))

(defn kde-confidence-bands
  "Compute bootstrap confidence bands for KDE.

  Parameters:
  - data: original sample data
  - bandwidth: kernel bandwidth
  - grid: evaluation grid points
  - n-bootstrap: number of bootstrap samples (default 200)
  - alpha: confidence level (default 0.05 for 95% CI)
  - rng-factory: function returning RNG (default: WELL RNG)

  Returns map with :lower and :upper vectors."
  ([data bandwidth grid]
   (kde-confidence-bands data bandwidth grid {}))
  ([data bandwidth ^doubles grid {:keys [n-bootstrap alpha rng-factory]
                                  :or {n-bootstrap 200
                                       alpha 0.05
                                       rng-factory #(well/well-rng-1024a)}}]
   (let [n-grid (alength grid)
         samples (vec (for [_ (range n-bootstrap)]
                        (kde-bootstrap-sample data bandwidth grid (rng-factory))))
         alpha-low (/ (double alpha) 2.0)
         alpha-hi (- 1.0 alpha-low)
         lower (double-array n-grid)
         upper (double-array n-grid)]
     (dotimes [i n-grid]
       (let [point-samples (mapv #(aget ^doubles % i) samples)
             sorted (sort point-samples)]
         (aset lower i (double (stats/quantile alpha-low sorted)))
         (aset upper i (double (stats/quantile alpha-hi sorted)))))
     {:lower lower
      :upper upper})))

;;; Mode confidence intervals

(defn mode-confidence-intervals
  "Compute bootstrap confidence intervals for mode locations.

  Parameters:
  - data: original sample data
  - bandwidth: kernel bandwidth
  - grid: evaluation grid
  - n-modes: number of modes to track (default 3)
  - n-bootstrap: number of bootstrap samples (default 200)
  - alpha: confidence level (default 0.05)
  - rng-factory: function returning RNG

  Returns vector of mode CIs, each with :location, :ci-lower, :ci-upper."
  ([data bandwidth grid n-modes]
   (mode-confidence-intervals data bandwidth grid n-modes {}))
  ([data bandwidth ^doubles grid n-modes
    {:keys [n-bootstrap alpha rng-factory]
     :or {n-bootstrap 200
          alpha 0.05
          rng-factory #(well/well-rng-1024a)}}]
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
              sorted (sort boot-locs)]
          (if (seq sorted)
            (assoc mode
                   :ci-lower (stats/quantile alpha-low sorted)
                   :ci-upper (stats/quantile alpha-high sorted))
            mode)))))))

;;; Silverman's test for multimodality

(defn count-modes
  "Count number of modes in KDE with given bandwidth.
  Creates a grid and counts local maxima in the density estimate."
  ^long [data ^double bandwidth ^long n-points]
  (let [data (vec data)
        x-min (double (reduce min data))
        x-max (double (reduce max data))
        margin (/ (- x-max x-min) 10.0)
        g-min (- x-min margin)
        g-max (+ x-max margin)
        g-range (- g-max g-min)
        grid (double-array n-points)]
    (dotimes [i n-points]
      (aset grid i (+ g-min (* g-range (/ (double i) (double (dec n-points)))))))
    (let [density (gaussian-kde data bandwidth grid)]
      (count (find-modes grid density)))))

(defn critical-bandwidth
  "Find smallest bandwidth giving at most k modes via binary search.
  
  Returns the critical bandwidth h_k, which is the smallest bandwidth
  such that the KDE has at most k modes.
  
  Parameters:
  - data: sample values
  - k: maximum number of modes
  - opts: optional map with :tol (tolerance, default 1e-6), 
          :n-points (grid size, default 512)"
  ^double [data ^long k {:keys [tol n-points]
                         :or {tol 1e-6
                              n-points 512}}]
  (let [data (vec data)
        n-pts (long n-points)
        tol (double tol)
        sigma (Math/sqrt (double (stats/variance data)))
        ;; Start with range from very small to Silverman bandwidth * 2
        h-max (* 2.0 (silverman-bandwidth data))
        h-min (/ sigma 100.0)]
    ;; Binary search for smallest h with <= k modes
    (loop [lo h-min
           hi h-max
           its 0]
      (if (or (>= its 100) (< (- hi lo) (* tol (+ hi lo) 0.5)))
        hi
        (let [mid (* 0.5 (+ lo hi))
              n-modes (count-modes data mid n-pts)]
          (if (<= n-modes k)
            ;; Can achieve <= k modes, try smaller bandwidth
            (recur lo mid (inc its))
            ;; Too many modes, need larger bandwidth
            (recur mid hi (inc its))))))))

(defn silverman-bootstrap-sample
  "Generate a smoothed bootstrap sample for Silverman's test.
  
  Uses rescaled bootstrap from Silverman (1981):
  y_i = mean + (X*_i - mean + h * epsilon_i) / sqrt(1 + h²/σ²)
  
  This ensures the bootstrap sample has the same variance as the original."
  [data ^double bandwidth rng]
  (let [n (count data)
        sigma-sq (double (stats/variance data))
        mean-val (double (stats/mean data))
        scale (Math/sqrt (+ 1.0 (/ (* bandwidth bandwidth) sigma-sq)))
        ;; Sample with replacement - this consumes n random values
        resampled (vec (stats/sample data rng))
        ;; Get 2*n more random values for Box-Muller pairs
        rng-rest (drop n rng)
        u-pairs (take (* 2 n) rng-rest)
        u-vec (vec u-pairs)
        ;; Generate smoothed sample
        result (double-array n)]
    (dotimes [i n]
      (let [x-star (double (nth resampled i))
            ;; Box-Muller for normal random
            u1 (max 1e-10 (double (nth u-vec (* 2 i))))
            u2 (double (nth u-vec (inc (* 2 i))))
            epsilon (* (Math/sqrt (* -2.0 (Math/log u1)))
                       (Math/cos (* 2.0 Math/PI u2)))
            y (+ mean-val (/ (+ (- x-star mean-val) (* bandwidth epsilon)) scale))]
        (aset result i y)))
    (vec result)))

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
  - data: sample values
  - k: number of modes under H0
  - opts: optional map with:
    - :n-bootstrap (default 200)
    - :n-points (default 512)
    - :alpha (for Hall-York correction, default 0.05)
    - :tol (for critical bandwidth search, default 1e-6)
    - :rng-factory (default: WELL RNG)
  
  Returns map with:
  - :k - number of modes tested
  - :critical-bandwidth - bandwidth giving exactly k modes  
  - :p-value - proportion of bootstrap samples with > k modes
  - :corrected? - whether Hall-York correction was applied"
  [data ^long k {:keys [n-bootstrap n-points alpha tol rng-factory]
                 :or {n-bootstrap 200
                      n-points 512
                      alpha 0.05
                      tol 1e-6
                      rng-factory #(well/well-rng-1024a)}}]
  (let [data (vec data)
        n-pts (long n-points)
        ;; Find critical bandwidth
        h-crit (double (critical-bandwidth data k {:tol tol :n-points n-pts}))
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

;;; Main KDE function

(defn kde
  "Compute KDE analysis on sample data.

  Parameters:
  - data: vector of sample values
  - opts: optional map with:
    - :n-points: grid size (default 512)
    - :bandwidth: override bandwidth (default: ISJ selection)
    - :n-bootstrap: bootstrap samples for confidence bands (default 200)
    - :alpha: confidence level (default 0.05)
    - :rng-factory: RNG factory function

  Returns map with:
  - :type :criterium/kde
  - :bandwidth: selected or provided bandwidth
  - :grid: evaluation points
  - :density: density values at grid points
  - :lower-band: lower confidence band
  - :upper-band: upper confidence band
  - :n: sample size
  
  Note: Mode detection is now a separate analysis step. Use silverman-test
  and mode-confidence-intervals for statistical mode analysis."
  ([data] (kde data {}))
  ([data {:keys [n-points bandwidth n-bootstrap alpha rng-factory]
          :or {n-points 512
               n-bootstrap 200
               alpha 0.05
               rng-factory #(well/well-rng-1024a)}}]
   (when (empty? data)
     (throw (ex-info "Input data cannot be empty"
                     {:error :kde/no-data})))
   (let [data (vec data)
         n (count data)
         x-min (double (reduce min data))
         x-max (double (reduce max data))]
     (when (= x-min x-max)
       (throw (ex-info "All values are the same - cannot compute KDE"
                       {:error :kde/constant-data
                        :value x-min})))
     (let [margin (/ (- x-max x-min) 10.0)
           g-min (- x-min margin)
           g-max (+ x-max margin)
           g-range (- g-max g-min)
           n-pts (long n-points)
           grid (double-array n-pts)
           _ (dotimes [i n-pts]
               (aset grid i (+ g-min (* g-range
                                        (/ (double i) (double (dec n-pts)))))))
           h (double (or bandwidth (isj-bandwidth data)))
           density (gaussian-kde data h grid)
           bands (kde-confidence-bands data h grid
                                       {:n-bootstrap n-bootstrap
                                        :alpha alpha
                                        :rng-factory rng-factory})]
       {:type :criterium/kde
        :bandwidth h
        :grid (vec grid)
        :density (vec density)
        :lower-band (vec (:lower bands))
        :upper-band (vec (:upper bands))
        :n n}))))
