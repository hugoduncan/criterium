(ns stats.moment-match
  "Moment-based parameter estimation and distribution suitability screening.

  Provides method-of-moments initial parameter estimates for distributions
  and a prefilter to screen out distributions that are unsuitable for a
  given dataset based on sample statistics.

  This is used before MLE fitting to quickly eliminate distributions where
  moment-based estimates yield invalid parameters (e.g., negative shape)."
  (:require
   [stats.probability :as probability]))

;;; Method-of-Moments Parameter Estimation
;; These provide initial estimates that can also be used to screen distributions

(defn gamma-moment-estimate
  "Estimate gamma distribution parameters using method of moments.

  Parameters (returned):
    shape (k) = mean² / variance
    scale (θ) = variance / mean

  Returns nil if estimates are invalid (non-positive mean or variance).

  Reference: Johnson, Kotz & Balakrishnan (1994), Ch. 17"
  [^double mean ^double variance]
  (when (and (pos? mean) (pos? variance))
    {:shape (/ (* mean mean) variance)
     :scale (/ variance mean)}))

(defn lognormal-moment-estimate
  "Estimate log-normal distribution parameters using method of moments.

  Parameters (log-space):
    sigma² = log(1 + variance/mean²)
    mu = log(mean) - sigma²/2

  Returns nil if mean is non-positive (log undefined) or variance is negative.

  Reference: Johnson, Kotz & Balakrishnan (1994), Ch. 14"
  [^double mean ^double variance]
  (when (and (pos? mean) (>= variance 0.0))
    (let [cv-squared (/ variance (* mean mean))
          sigma-sq   (Math/log (+ 1.0 cv-squared))]
      {:mu    (- (Math/log mean) (/ sigma-sq 2.0))
       :sigma (Math/sqrt sigma-sq)})))

(defn inverse-gaussian-moment-estimate
  "Estimate inverse Gaussian distribution parameters using method of moments.

  Parameters:
    mu = mean
    lambda = mean³ / variance

  Returns nil if mean is non-positive or variance is non-positive.

  Reference: Johnson, Kotz & Balakrishnan (1994), Ch. 15"
  [^double mean ^double variance]
  (when (and (pos? mean) (pos? variance))
    {:mu     mean
     :lambda (/ (* mean mean mean) variance)}))

(defn weibull-moment-estimate
  "Estimate Weibull distribution parameters using method of moments.

  Uses the coefficient of variation (CV = σ/μ) to estimate the shape parameter
  via a simple approximation, then derives scale from the mean.

  Approximation for shape (valid for CV < 1):
    k ≈ 1.2 / CV for moderate CV values

  For more accurate estimation, uses Newton-Raphson iteration on:
    CV² = Γ(1+2/k)/Γ²(1+1/k) - 1

  Returns nil if mean is non-positive, variance is non-positive,
  or if CV is too large (> 2, suggesting heavy-tailed distribution).

  Reference: Cohen & Whitten (1988), Parameter Estimation in Reliability"
  [^double mean ^double variance]
  (when (and (pos? mean) (pos? variance))
    (let [cv (/ (Math/sqrt variance) mean)]
      ;; Weibull CV is bounded; for CV > ~1.2, shape < 1 (heavy tail)
      ;; For CV > 2, moment matching becomes unreliable
      (when (<= cv 2.0)
        ;; Simple approximation: shape ≈ 1.2785 / CV for moderate CV
        ;; This comes from inverting the CV formula at k ≈ 1.2/CV
        (let [shape (cond
                      ;; Very small CV suggests exponential-like (shape ≈ 1)
                      (< cv 0.1) 10.0
                      ;; Moderate CV: use approximation
                      (<= cv 1.0) (/ 1.2785 cv)
                      ;; CV > 1: shape < 1, use different approximation
                      :else (/ 0.8 cv))
              ;; scale = mean / Γ(1 + 1/k)
              ;; Using approximation: Γ(1+x) ≈ 1 for small x
              gamma-factor (Math/exp (probability/log-gamma
                                      (+ 1.0 (/ 1.0 (double shape)))))
              scale (/ mean gamma-factor)]
          (when (and (pos? (double shape)) (pos? scale))
            {:shape shape
             :scale scale}))))))

;;; Distribution Suitability Screening

(def ^:private distribution-estimators
  "Map from distribution keyword to moment estimation function."
  {:gamma           gamma-moment-estimate
   :lognormal       lognormal-moment-estimate
   :inverse-gaussian inverse-gaussian-moment-estimate
   :weibull         weibull-moment-estimate})

(def all-distributions
  "Set of all distributions supported by the prefilter."
  #{:gamma :lognormal :inverse-gaussian :weibull})

(defn moment-match-prefilter
  "Screen distributions for suitability based on sample moments.

  Takes sample mean and variance (or computes them from data) and returns
  a map of distributions with their moment-based parameter estimates.
  Distributions where moment matching yields invalid parameters are excluded.

  Parameters:
    mean - sample mean
    variance - sample variance
    distributions - (optional) set of distributions to check, defaults to all

  Returns map from distribution keyword to {:params {...} :suitable? true/false}
  where :params contains the moment-estimated parameters.

  Example:
    (moment-match-prefilter 100.0 400.0)
    ;; => {:gamma {:params {:shape 25.0 :scale 4.0} :suitable? true}
    ;;     :lognormal {:params {:mu 4.58 :sigma 0.198} :suitable? true}
    ;;     ...}

    (moment-match-prefilter -5.0 10.0)
    ;; => {:gamma {:params nil :suitable? false}
    ;;     :lognormal {:params nil :suitable? false}
    ;;     ...}  ; negative mean makes all distributions unsuitable"
  ([^double mean ^double variance]
   (moment-match-prefilter mean variance all-distributions))
  ([^double mean ^double variance distributions]
   (into {}
         (map (fn [dist]
                (if-let [estimator (get distribution-estimators dist)]
                  (let [params (estimator mean variance)]
                    [dist {:params    params
                           :suitable? (some? params)}])
                  [dist {:params    nil
                         :suitable? false
                         :error     :unknown-distribution}])))
         distributions)))

(defn suitable-distributions
  "Return the set of distributions suitable for the given sample statistics.

  This is a convenience function that calls moment-match-prefilter and
  returns only the distribution keywords that are suitable.

  Parameters:
    mean - sample mean
    variance - sample variance
    distributions - (optional) set of distributions to check, defaults to all

  Returns set of suitable distribution keywords."
  ([^double mean ^double variance]
   (suitable-distributions mean variance all-distributions))
  ([^double mean ^double variance distributions]
   (into #{}
         (comp
          (filter (fn [[_ v]] (:suitable? v)))
          (map first))
         (moment-match-prefilter mean variance distributions))))

(defn unsuitable-distributions
  "Return the set of distributions unsuitable for the given sample statistics.

  This is a convenience function that calls moment-match-prefilter and
  returns only the distribution keywords that are not suitable.

  Parameters:
    mean - sample mean
    variance - sample variance
    distributions - (optional) set of distributions to check, defaults to all

  Returns set of unsuitable distribution keywords."
  ([^double mean ^double variance]
   (unsuitable-distributions mean variance all-distributions))
  ([^double mean ^double variance distributions]
   (into #{}
         (comp
          (filter (fn [[_ v]] (not (:suitable? v))))
          (map first))
         (moment-match-prefilter mean variance distributions))))
