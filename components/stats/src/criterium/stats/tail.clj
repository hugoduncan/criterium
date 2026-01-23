(ns criterium.stats.tail
  "Tail statistics for extreme value analysis.

  Provides functions for analyzing distribution tails, including:
  - Hill estimator for tail index estimation
  - Generalized Pareto Distribution (GPD) fitting and functions
  - Mean residual life for threshold selection
  - Tail ratios from percentiles

  All functions requiring sample data accept typed arrays (ITypedArray).

  References:
  - Hill (1975), A Simple General Approach to Inference About the Tail of a Distribution
  - Grimshaw (1993), Computing Maximum Likelihood Estimates for the GPD
  - Coles (2001), An Introduction to Statistical Modeling of Extreme Values"
  (:require
   [criterium.array :as arr]
   [criterium.utils.interface :refer [have?]]))

;;; Exceedances

(defn exceedances-over-threshold
  "Extract excesses over the given threshold.
  Returns a new DoubleArray containing (y - threshold) for each y > threshold.

  In extreme value theory, the 'exceedance' or 'excess' over a threshold u
  is defined as Y = X - u for observations X > u. These excesses are modeled
  by the Generalized Pareto Distribution (GPD).

  Parameters:
    samples - typed array of sample values
    threshold - threshold value u

  Returns DoubleArray of excesses (y - threshold for y > threshold)."
  [samples ^double threshold]
  {:pre [(have? arr/typed-array? samples)]}
  ;; Two-pass: count exceeding values, then collect them
  (let [n (arr/length samples)
        ;; First pass: count
        count-exceed
        (loop [i 0
               c 0]
          (if (>= i n)
            c
            (if (> (arr/get-double samples i) threshold)
              (recur (inc i) (inc c))
              (recur (inc i) c))))
        ;; Second pass: collect excesses (value - threshold) into array
        result (double-array count-exceed)]
    (loop [i 0
           j 0]
      (when (< i n)
        (let [v (arr/get-double samples i)]
          (if (> v threshold)
            (do
              (aset result j (- v threshold))
              (recur (inc i) (inc j)))
            (recur (inc i) j)))))
    (arr/->double-array result)))

;;; Hill Estimator

(defn hill-estimator
  "Compute the Hill estimator for tail index across a range of k values.

  The Hill estimator for the k largest order statistics is:
    H_k = (1/k) * Σᵢ₌₁ᵏ log(X_{(n-i+1)} / X_{(n-k)})

  where X_{(i)} is the i-th order statistic (sorted ascending).

  The tail index α is estimated as 1/H_k. Heavy tails have small α (< 2).

  Parameters:
    sorted-samples - typed array of samples sorted in ascending order
    k-range - sequence of k values to compute estimates for
              (each k uses the k largest observations)

  Returns vector of maps {:k k :estimate H_k :tail-index (1/H_k)}
  for each k in k-range where computation is valid.

  Notes:
  - k must be >= 1 and < n (sample size)
  - Returns empty vector if samples has fewer than 2 elements
  - Requires sorted input (ascending order)"
  [sorted-samples k-range]
  {:pre [(have? arr/typed-array? sorted-samples)]}
  (let [n (arr/length sorted-samples)]
    (if (< n 2)
      []
      (let [;; Pre-compute log of all values for efficiency
            log-vals (arr/dmap sorted-samples (fn ^double [^double x] (Math/log x)))]
        (into []
              (comp
               (filter (fn [k] (let [k (long k)] (and (>= k 1) (< k n)))))
               (map (fn [k]
                      (let [k (long k)
                            ;; X_{(n-k)} is the (k+1)-th largest, at index n-k-1
                            threshold-idx (- n k 1)
                            log-threshold (arr/get-double log-vals threshold-idx)
                            ;; Sum log(X_{(n-i+1)}) - log(X_{(n-k)}) for i=1..k
                            ;; X_{(n-i+1)} for i=1..k are the k largest values
                            ;; at indices n-1, n-2, ..., n-k
                            sum-log-diff
                            (double
                             (loop [i (long 0)
                                    acc 0.0]
                               (if (>= i k)
                                 acc
                                 (let [idx (- n 1 i)
                                       log-xi (arr/get-double log-vals idx)]
                                   (recur (inc i) (+ acc (- log-xi log-threshold)))))))
                            h-k (/ sum-log-diff (double k))]
                        {:k k
                         :estimate h-k
                         :tail-index (if (pos? h-k) (/ 1.0 h-k) Double/POSITIVE_INFINITY)}))))
              k-range)))))

(defn hill-estimator-default-k-range
  "Compute default k range for Hill estimator.
  Uses k from 10 to min(n/2, 500) with step size based on n.

  Parameters:
    n - sample size

  Returns sequence of k values."
  [^long n]
  (when (> n 10)
    (let [k-max (min (quot n 2) 500)
          k-min 10
          step (max 1 (quot (- k-max k-min) 50))]
      (range k-min (inc k-max) step))))

;;; Generalized Pareto Distribution Functions

(defn gpd-pdf
  "Probability density function for the Generalized Pareto Distribution.

  For exceedances y > 0:
    f(y; ξ, σ) = (1/σ) * (1 + ξy/σ)^(-1/ξ - 1)  if ξ ≠ 0
    f(y; 0, σ) = (1/σ) * exp(-y/σ)               if ξ = 0

  Support:
    y ≥ 0           if ξ ≥ 0
    0 ≤ y ≤ -σ/ξ    if ξ < 0

  Parameters:
    xi - shape parameter ξ (can be negative, zero, or positive)
    sigma - scale parameter σ (must be positive)

  Returns a function f(y) that computes the density at y."
  [^double xi ^double sigma]
  {:pre [(pos? sigma)]}
  (if (< (Math/abs xi) 1e-10)
    ;; Exponential case (ξ = 0)
    (fn ^double [^double y]
      (if (neg? y)
        0.0
        (/ (Math/exp (- (/ y sigma))) sigma)))
    ;; General GPD case
    (let [inv-xi (/ 1.0 xi)
          exp-term (- (- inv-xi) 1.0)]
      (fn ^double [^double y]
        (cond
          (neg? y) 0.0
          ;; Check upper bound for ξ < 0
          (and (neg? xi) (> y (- (/ sigma xi)))) 0.0
          :else
          (let [z (+ 1.0 (/ (* xi y) sigma))]
            (if (<= z 0.0)
              0.0
              (/ (Math/pow z exp-term) sigma))))))))

(defn gpd-cdf
  "Cumulative distribution function for the Generalized Pareto Distribution.

  For exceedances y > 0:
    F(y; ξ, σ) = 1 - (1 + ξy/σ)^(-1/ξ)  if ξ ≠ 0
    F(y; 0, σ) = 1 - exp(-y/σ)           if ξ = 0

  Parameters:
    xi - shape parameter ξ
    sigma - scale parameter σ (must be positive)

  Returns a function F(y) that computes P(Y ≤ y)."
  [^double xi ^double sigma]
  {:pre [(pos? sigma)]}
  (if (< (Math/abs xi) 1e-10)
    ;; Exponential case (ξ = 0)
    (fn ^double [^double y]
      (cond
        (neg? y) 0.0
        :else (- 1.0 (Math/exp (- (/ y sigma))))))
    ;; General GPD case
    (let [inv-xi (/ -1.0 xi)]
      (fn ^double [^double y]
        (cond
          (neg? y) 0.0
          ;; Upper bound for ξ < 0
          (and (neg? xi) (>= y (- (/ sigma xi)))) 1.0
          :else
          (let [z (+ 1.0 (/ (* xi y) sigma))]
            (if (<= z 0.0)
              1.0
              (- 1.0 (Math/pow z inv-xi)))))))))

(defn gpd-quantile
  "Quantile function (inverse CDF) for the Generalized Pareto Distribution.

  For probability p ∈ [0, 1]:
    Q(p; ξ, σ) = (σ/ξ) * ((1-p)^(-ξ) - 1)  if ξ ≠ 0
    Q(p; 0, σ) = -σ * log(1-p)              if ξ = 0

  Parameters:
    xi - shape parameter ξ
    sigma - scale parameter σ (must be positive)

  Returns a function Q(p) that computes the p-th quantile."
  [^double xi ^double sigma]
  {:pre [(pos? sigma)]}
  (if (< (Math/abs xi) 1e-10)
    ;; Exponential case (ξ = 0)
    (fn ^double [^double p]
      (cond
        (<= p 0.0) 0.0
        (>= p 1.0) Double/POSITIVE_INFINITY
        :else (- (* sigma (Math/log (- 1.0 p))))))
    ;; General GPD case
    (fn ^double [^double p]
      (cond
        (<= p 0.0) 0.0
        (>= p 1.0) (if (neg? xi)
                     (- (/ sigma xi))  ; Finite upper bound for ξ < 0
                     Double/POSITIVE_INFINITY)
        :else
        (let [one-minus-p (- 1.0 p)]
          (* (/ sigma xi) (- (Math/pow one-minus-p (- xi)) 1.0)))))))

;;; GPD Maximum Likelihood Estimation

(defn- gpd-log-likelihood
  "Compute GPD log-likelihood for given parameters.
  Returns -∞ if parameters are invalid for the data."
  ^double [exceedances ^double xi ^double sigma]
  (let [n (arr/length exceedances)]
    (if (or (<= sigma 0.0) (zero? n))
      Double/NEGATIVE_INFINITY
      (if (< (Math/abs xi) 1e-10)
        ;; Exponential case: -n*log(σ) - (1/σ)*Σyᵢ
        (let [sum-y (arr/fold-double exceedances
                                     (fn ^double [^double acc ^double y] (+ acc y))
                                     0.0)]
          (- (* (- n) (Math/log sigma)) (/ sum-y sigma)))
        ;; General case: -n*log(σ) - (1/ξ + 1)*Σlog(1 + ξyᵢ/σ)
        (let [coef (+ (/ 1.0 xi) 1.0)
              sum-log
              (arr/fold-double exceedances
                               (fn ^double [^double acc ^double y]
                                 (let [z (+ 1.0 (/ (* xi y) sigma))]
                                   (if (<= z 0.0)
                                     Double/NEGATIVE_INFINITY
                                     (+ acc (Math/log z)))))
                               0.0)]
          (if (Double/isInfinite sum-log)
            Double/NEGATIVE_INFINITY
            (- (* (- n) (Math/log sigma)) (* coef sum-log))))))))

(defn- gpd-mle-grimshaw
  "Grimshaw's algorithm for GPD MLE.
  Uses profile likelihood: for each ξ, optimal σ is computed in closed form,
  then we search for the ξ that maximizes the profile likelihood.

  Returns {:xi xi :sigma sigma :log-likelihood ll :converged? bool}"
  [exceedances opts]
  (let [n (arr/length exceedances)
        {:keys [max-iter tol xi-min xi-max]
         :or {max-iter 100 tol 1e-8 xi-min -0.5 xi-max 2.0}} opts
        ^long max-iter max-iter
        ^double tol tol
        ^double xi-min xi-min
        ^double xi-max xi-max
        ;; Sample statistics
        sum-y (arr/fold-double exceedances
                               (fn ^double [^double acc ^double y] (+ acc y))
                               0.0)
        mean-y (/ sum-y (double n))
        max-y (arr/fold-double exceedances
                               (fn ^double [^double acc ^double y] (Math/max acc y))
                               Double/NEGATIVE_INFINITY)
        ;; For ξ given, optimal σ satisfies: σ = (1 + ξ) * mean(y) / (1 + ξ*n*mean_log)
        ;; where mean_log = (1/n) * Σ log(1 + ξyᵢ/σ)
        ;; This requires iterative solution, but we can use grid + refinement

        ;; Profile likelihood function for a given ξ
        profile-ll
        (fn ^double [^double xi]
          (if (< (Math/abs xi) 1e-10)
            ;; Exponential: σ = mean(y), ll = -n*log(σ) - n
            (- (* (- n) (Math/log mean-y)) n)
            ;; For given ξ, find optimal σ iteratively
            (let [;; Initial σ from method of moments
                  init-sigma mean-y
                  ;; Newton iteration to find σ
                  sigma
                  (loop [sigma (double init-sigma)
                         iter (long 0)]
                    (if (>= iter 50)
                      sigma
                      (let [;; Compute Σlog(1 + ξyᵢ/σ)
                            sum-log
                            (arr/fold-double exceedances
                                             (fn ^double [^double acc ^double y]
                                               (let [z (+ 1.0 (/ (* xi y) sigma))]
                                                 (if (<= z 0.0)
                                                   Double/NEGATIVE_INFINITY
                                                   (+ acc (Math/log z)))))
                                             0.0)]
                        (if (Double/isInfinite sum-log)
                          sigma
                          ;; Profile equation: n/σ - (1/ξ + 1) * Σ(ξyᵢ/(σ(σ + ξyᵢ))) = 0
                          ;; Simplified: σ = (1 + ξ) * Σyᵢ / (n + ξ*Σlog(1 + ξyᵢ/σ))
                          (let [new-sigma (/ (* (+ 1.0 xi) sum-y)
                                             (+ (double n) (* xi sum-log)))]
                            (if (or (<= new-sigma 0.0)
                                    (< (Math/abs (- new-sigma sigma)) (* tol sigma)))
                              (if (pos? new-sigma) new-sigma sigma)
                              (recur new-sigma (inc iter))))))))
                  sigma (double sigma)]
              (if (pos? sigma)
                (gpd-log-likelihood exceedances xi sigma)
                Double/NEGATIVE_INFINITY))))

        ;; Grid search for ξ, then refine
        ;; Constrain ξ so that all data is valid: 1 + ξ*max_y/σ > 0
        ;; For ξ < 0: ξ > -σ/max_y, approximately ξ > -mean_y/max_y
        effective-xi-min (Math/max xi-min (- (/ mean-y max-y) 0.1))

        ;; Grid search
        grid-points (long 20)
        xi-step (/ (- xi-max effective-xi-min) (double grid-points))
        best-xi
        (double
         (loop [xi (double effective-xi-min)
                best-xi 0.0
                best-ll Double/NEGATIVE_INFINITY]
           (if (> xi xi-max)
             best-xi
             (let [ll #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                   (double (profile-ll xi))]
               (if (> ll best-ll)
                 (recur (+ xi xi-step) xi ll)
                 (recur (+ xi xi-step) best-xi best-ll))))))

        ;; Golden section refinement around best-xi
        final-xi
        (let [golden (/ (- (Math/sqrt 5.0) 1.0) 2.0)
              refine-width (* 2.0 xi-step)]
          (loop [a (double (Math/max (double effective-xi-min) (- best-xi refine-width)))
                 b (double (Math/min xi-max (+ best-xi refine-width)))
                 iter (long 0)]
            (if (or (>= iter max-iter) (< (- b a) tol))
              (/ (+ a b) 2.0)
              (let [c (- b (* golden (- b a)))
                    d (+ a (* golden (- b a)))
                    fc #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                    (double (profile-ll c))
                    fd #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                    (double (profile-ll d))]
                (if (> fc fd)
                  (recur a d (inc iter))
                  (recur c b (inc iter)))))))

        ;; Compute final σ for the optimal ξ
        final-xi (double final-xi)
        final-sigma
        (if (< (Math/abs final-xi) 1e-10)
          mean-y
          ;; Iterate to get correct σ
          (loop [sigma (double mean-y)
                 iter (long 0)]
            (if (>= iter 50)
              sigma
              (let [sum-log-curr
                    (arr/fold-double exceedances
                                     (fn ^double [^double acc ^double y]
                                       (let [z (+ 1.0 (/ (* final-xi y) sigma))]
                                         (if (<= z 0.0)
                                           acc
                                           (+ acc (Math/log z)))))
                                     0.0)
                    new-sigma (/ (* (+ 1.0 final-xi) sum-y)
                                 (+ (double n) (* final-xi sum-log-curr)))]
                (if (or (<= new-sigma 0.0)
                        (< (Math/abs (- new-sigma sigma)) (* tol sigma)))
                  (if (pos? new-sigma) new-sigma sigma)
                  (recur new-sigma (inc iter)))))))]
    {:xi final-xi
     :sigma final-sigma
     :log-likelihood (gpd-log-likelihood exceedances final-xi final-sigma)
     :converged? true
     :n n}))

(defn gpd-mle
  "Maximum likelihood estimation for the Generalized Pareto Distribution.

  Uses Grimshaw's (1993) algorithm with profile likelihood optimization.

  Parameters:
    exceedances - typed array of exceedance values (values above threshold)
                  All values must be positive (exceedances, not raw data)
    opts - optional map with:
      :max-iter - maximum iterations (default 100)
      :tol - convergence tolerance (default 1e-8)
      :xi-min - minimum ξ to search (default -0.5)
      :xi-max - maximum ξ to search (default 2.0)

  Returns map with:
    :xi - shape parameter estimate
    :sigma - scale parameter estimate
    :log-likelihood - maximized log-likelihood
    :converged? - whether optimization converged
    :n - number of exceedances

  Throws if exceedances is empty or contains non-positive values."
  ([exceedances] (gpd-mle exceedances {}))
  ([exceedances opts]
   {:pre [(have? arr/typed-array? exceedances)]}
   (let [n (arr/length exceedances)]
     (when (zero? n)
       (throw (IllegalArgumentException. "exceedances cannot be empty")))
     ;; Validate all positive
     (arr/fold-double exceedances
                      (fn ^double [^double _ ^double y]
                        (when (<= y 0.0)
                          (throw (IllegalArgumentException.
                                  (str "exceedances must be positive, got: " y))))
                        0.0)
                      0.0)
     (gpd-mle-grimshaw exceedances opts))))

;;; Mean Residual Life

(defn mean-residual-life
  "Compute mean residual life (mean excess) over a range of thresholds.

  The mean residual life at threshold u is:
    e(u) = E[X - u | X > u]

  For GPD data, e(u) is linear in u with slope ξ/(1-ξ).
  A threshold where e(u) becomes approximately linear suggests
  a good choice for POT analysis.

  Parameters:
    sorted-samples - typed array of samples sorted in ascending order
    threshold-range - sequence of threshold values to evaluate

  Returns vector of maps {:threshold u :mrl e(u) :n-exceed count}
  where n-exceed is the number of observations exceeding u."
  [sorted-samples threshold-range]
  {:pre [(have? arr/typed-array? sorted-samples)]}
  (let [n (arr/length sorted-samples)]
    (if (zero? n)
      []
      (into []
            (comp
             (map (fn [u]
                    (let [u (double u)
                          ;; Find first index where sample > u
                          ;; Since sorted, we can binary search
                          exceed-start
                          (long
                           (loop [lo (long 0)
                                  hi n]
                             (if (>= lo hi)
                               lo
                               (let [mid (quot (+ lo hi) 2)
                                     v (arr/get-double sorted-samples mid)]
                                 (if (<= v u)
                                   (recur (inc mid) hi)
                                   (recur lo mid))))))
                          n-exceed (- n exceed-start)]
                      (if (zero? n-exceed)
                        {:threshold u :mrl Double/NaN :n-exceed 0}
                        (let [;; Sum of exceedances: Σ(xᵢ - u) for xᵢ > u
                              sum-excess
                              (double
                               (loop [i exceed-start
                                      acc 0.0]
                                 (if (>= i n)
                                   acc
                                   (recur (inc i)
                                          (+ acc (- (arr/get-double sorted-samples i) u))))))
                              mrl (/ sum-excess (double n-exceed))]
                          {:threshold u :mrl mrl :n-exceed n-exceed}))))))
            threshold-range))))

(defn mean-residual-life-default-thresholds
  "Compute default threshold range for MRL plot.
  Uses quantiles from 50th to 95th percentile.

  Parameters:
    sorted-samples - typed array of samples sorted in ascending order
    n-points - number of threshold points (default 20)

  Returns sequence of threshold values."
  ([sorted-samples] (mean-residual-life-default-thresholds sorted-samples 20))
  ([sorted-samples ^long n-points]
   {:pre [(have? arr/typed-array? sorted-samples)]}
   (let [n (arr/length sorted-samples)]
     (when (pos? n)
       (let [q-min 0.5
             q-max 0.95
             q-step (/ (- q-max q-min) (double (dec n-points)))]
         (for [i (range n-points)]
           (let [i (long i)
                 q (+ q-min (* (double i) q-step))
                 idx (min (dec n) (long (* q (double (dec n)))))]
             (arr/get-double sorted-samples idx))))))))

;;; Tail Ratios

(defn tail-ratios
  "Compute tail ratios from percentile values.

  Tail ratios indicate how heavy the distribution tail is.
  Higher ratios suggest heavier tails.

  Parameters:
    percentiles - map of percentile values with keys like :p95, :p99, :p999
                  or numeric keys like 0.95, 0.99, 0.999

  Returns map with:
    :p99-p95 - ratio of 99th to 95th percentile
    :p999-p99 - ratio of 99.9th to 99th percentile
    :p999-p95 - ratio of 99.9th to 95th percentile (if all present)

  Returns nil for ratios where required percentiles are missing."
  [percentiles]
  (let [;; Look up percentile by trying common key formats
        p95 (or (get percentiles :p95)
                (get percentiles 0.95))
        p99 (or (get percentiles :p99)
                (get percentiles 0.99))
        p999 (or (get percentiles :p999)
                 (get percentiles 0.999))]
    (cond-> {}
      (and p99 p95 (pos? (double p95)))
      (assoc :p99-p95 (/ (double p99) (double p95)))

      (and p999 p99 (pos? (double p99)))
      (assoc :p999-p99 (/ (double p999) (double p99)))

      (and p999 p95 (pos? (double p95)))
      (assoc :p999-p95 (/ (double p999) (double p95))))))
