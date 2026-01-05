(ns stats.mle
  "Maximum Likelihood Estimation for statistical distributions.

  Provides MLE fitting functions that return both parameter estimates
  and log-likelihood values for model comparison via AIC/BIC.

  Distributions supported:
  - Gamma: Minka's fast fixed-point approximation for shape
  - Log-normal: Closed-form MLE
  - Inverse Gaussian: Closed-form MLE

  All functions return maps with :params and :log-likelihood keys."
  (:require
   [stats.probability :as probability]))

;;; Log-normal MLE (closed-form)

(defn lognormal-mle
  "Maximum likelihood estimation for the log-normal distribution.

  The MLE for log-normal has a closed-form solution:
    μ = mean(log(x))
    σ = sqrt(variance(log(x)))  ; using population variance (n denominator)

  Parameters:
    samples - sequence of positive sample values

  Returns map with:
    :params {:mu μ, :sigma σ}
    :log-likelihood - the maximized log-likelihood value

  Throws if any sample is non-positive."
  [samples]
  (let [samples (vec samples)
        n (long (count samples))
        _ (when (zero? n)
            (throw (IllegalArgumentException. "samples cannot be empty")))
        ;; Transform to log space
        log-samples (mapv #(let [x (double %)]
                             (when (<= x 0.0)
                               (throw (IllegalArgumentException.
                                       (str "lognormal requires positive samples, got: " x))))
                             (Math/log x))
                          samples)
        ;; MLE estimates
        mu (/ ^double (reduce + 0.0 log-samples) (double n))
        ;; Use population variance (divide by n, not n-1) for MLE
        sum-sq (double
                (reduce (fn [^double acc ^double lx]
                          (let [diff (- lx (double mu))]
                            (+ acc (* diff diff))))
                        0.0
                        log-samples))
        sigma (Math/sqrt (/ sum-sq (double n)))
        ;; Log-likelihood: Σ[-log(x) - log(σ) - 0.5*log(2π) - 0.5*((log(x)-μ)/σ)²]
        half-log-2pi (* 0.5 (Math/log (* 2.0 Math/PI)))
        log-likelihood (reduce (fn [^double acc ^double lx]
                                 (let [z (/ (- lx (double mu)) (double sigma))]
                                   (- acc lx (Math/log (double sigma)) half-log-2pi
                                      (* 0.5 z z))))
                               0.0
                               log-samples)]
    {:params {:mu mu :sigma sigma}
     :log-likelihood log-likelihood}))

;;; Inverse Gaussian MLE (closed-form)

(defn inverse-gaussian-mle
  "Maximum likelihood estimation for the inverse Gaussian distribution.

  The MLE for inverse Gaussian has a closed-form solution:
    μ = mean(x)
    λ = n / Σ(1/xᵢ - 1/μ)

  Parameters:
    samples - sequence of positive sample values

  Returns map with:
    :params {:mu μ, :lambda λ}
    :log-likelihood - the maximized log-likelihood value

  Throws if any sample is non-positive."
  [samples]
  (let [samples (vec samples)
        n (long (count samples))
        _ (when (zero? n)
            (throw (IllegalArgumentException. "samples cannot be empty")))
        ;; Validate and compute sum and sum of reciprocals
        [sum sum-recip]
        (reduce (fn [[^double s ^double sr] x]
                  (let [x (double x)]
                    (when (<= x 0.0)
                      (throw (IllegalArgumentException.
                              (str "inverse-gaussian requires positive samples, got: " x))))
                    [(+ s x) (+ sr (/ 1.0 x))]))
                [0.0 0.0]
                samples)
        ;; MLE estimates
        mu (/ (double sum) (double n))
        ;; λ = n / Σ(1/xᵢ - 1/μ) = n / (Σ(1/xᵢ) - n/μ)
        lambda (/ (double n) (- (double sum-recip) (/ (double n) (double mu))))
        ;; Log-likelihood: Σ[0.5*log(λ/(2πx³)) - λ(x-μ)²/(2μ²x)]
        half-log-lambda-2pi (* 0.5 (- (Math/log (double lambda)) (Math/log (* 2.0 Math/PI))))
        mu-sq (* (double mu) (double mu))
        log-likelihood (reduce (fn [^double acc x]
                                 (let [x (double x)
                                       diff (- x (double mu))
                                       term1 (- half-log-lambda-2pi (* 1.5 (Math/log x)))
                                       term2 (/ (* (double lambda) diff diff) (* 2.0 mu-sq x))]
                                   (+ acc term1 (- term2))))
                               0.0
                               samples)]
    {:params {:mu mu :lambda lambda}
     :log-likelihood log-likelihood}))

;;; Gamma MLE (Minka's fixed-point approximation)

(defn- gamma-log-likelihood
  "Compute log-likelihood for gamma distribution with given shape and scale."
  ^double [samples ^double shape ^double scale]
  (let [log-normalizer (+ (* shape (Math/log scale))
                          (probability/log-gamma shape))]
    (reduce (fn [^double acc x]
              (let [x (double x)]
                (+ acc
                   (* (- shape 1.0) (Math/log x))
                   (- (/ x scale))
                   (- log-normalizer))))
            0.0
            samples)))

(defn gamma-mle
  "Maximum likelihood estimation for the gamma distribution.

  Uses Minka's fast fixed-point iteration for shape parameter:
    k_new = k_old × (log(k_old) - ψ(k_old) + log(mean(x)) - mean(log(x)))⁻¹ × (log(k_old) - ψ(k_old))

  Simplified form (Newton-like):
    k_new = k_old + (log(mean) - mean(log) - log(k) + ψ(k)) / (1/k - ψ'(k))

  Scale is then: θ = mean(x) / k

  Parameters:
    samples - sequence of positive sample values
    opts - optional map with:
      :max-iter - maximum iterations (default 100)
      :tol - convergence tolerance (default 1e-10)
      :init-shape - initial shape estimate (default: method of moments)

  Returns map with:
    :params {:shape k, :scale θ}
    :log-likelihood - the maximized log-likelihood value
    :iterations - number of iterations used

  Throws if any sample is non-positive.

  Reference: Minka (2002), Estimating a Gamma distribution"
  ([samples] (gamma-mle samples {}))
  ([samples {:keys [max-iter tol init-shape]
             :or {max-iter 100 tol 1e-10}}]
   (let [samples (vec samples)
         n (long (count samples))
         _ (when (zero? n)
             (throw (IllegalArgumentException. "samples cannot be empty")))
         max-iter (long max-iter)
         tol (double tol)
         ;; Compute sufficient statistics
         [sum sum-log]
         (reduce (fn [[^double s ^double sl] x]
                   (let [x (double x)]
                     (when (<= x 0.0)
                       (throw (IllegalArgumentException.
                               (str "gamma requires positive samples, got: " x))))
                     [(+ s x) (+ sl (Math/log x))]))
                 [0.0 0.0]
                 samples)
         mean-x (/ (double sum) (double n))
         mean-log-x (/ (double sum-log) (double n))
         log-mean-x (Math/log (double mean-x))
         ;; s = log(mean(x)) - mean(log(x)), always positive for valid data
         s (- log-mean-x (double mean-log-x))
         ;; Initial shape estimate using method of moments if not provided
         ;; k_init ≈ (3 - s + sqrt((s-3)² + 24s)) / (12s) from Minka
         init-k (double (or init-shape
                            (if (< (double s) 0.001)
                              ;; For very small s, shape is very large
                              100.0
                              (/ (+ 3.0 (- (double s)) (Math/sqrt (+ (* (- (double s) 3.0) (- (double s) 3.0))
                                                                     (* 24.0 (double s)))))
                                 (* 12.0 (double s))))))
         ;; Minka's fixed-point iteration
         [shape iterations]
         (loop [k (double init-k)
                iter 0]
           (if (>= iter max-iter)
             [k iter]
             (let [psi-k (probability/digamma k)
                   psi-prime-k (probability/trigamma k)
                   ;; Newton update: k_new = k + (s - log(k) + ψ(k)) / (1/k - ψ'(k))
                   ;; where s = log(mean) - mean(log)
                   numer (- (double s) (- (Math/log k) psi-k))
                   denom (- (/ 1.0 k) psi-prime-k)
                   k-new (+ k (/ numer denom))]
               ;; Ensure k stays positive
               (if (or (<= k-new 0.0)
                       (< (Math/abs (- k-new k)) tol))
                 [(Math/max 1e-10 k-new) (inc iter)]
                 (recur k-new (inc iter))))))
         shape (double shape)
         scale (/ (double mean-x) shape)
         log-lik (gamma-log-likelihood samples shape scale)]
     {:params {:shape shape :scale scale}
      :log-likelihood log-lik
      :iterations iterations})))
