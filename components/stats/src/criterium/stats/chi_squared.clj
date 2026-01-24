(ns criterium.stats.chi-squared
  "Chi-squared distribution functions.

  The chi-squared distribution with k degrees of freedom is the distribution
  of a sum of squares of k independent standard normal random variables.
  It is a special case of the gamma distribution with shape = k/2 and scale = 2."
  (:require
   [criterium.stats.probability :as probability]))

(defn cdf
  "Cumulative distribution function for the chi-squared distribution.
  Returns P(X ≤ x) for a chi-squared random variable with df degrees of freedom.

  Uses the regularized incomplete gamma function:
    P(x; df) = P(df/2, x/2) = γ(df/2, x/2) / Γ(df/2)

  Edge cases:
  - df ≤ 0: returns 0.0
  - x ≤ 0: returns 0.0
  - x = ∞: returns 1.0

  Accuracy: within 1e-10 of R's pchisq()."
  ^double [^double x ^long df]
  (cond
    (<= df 0) 0.0
    (<= x 0.0) 0.0
    (Double/isInfinite x) 1.0
    :else (probability/regularized-gamma-p (/ (double df) 2.0) (/ x 2.0))))
