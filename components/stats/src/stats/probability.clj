(ns stats.probability
  "Probability functions: log-gamma, error function, normal distribution,
  and common statistical distributions (gamma, weibull, lognormal, inverse-gaussian).")

(defn polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
  descending order (so the last element of coefficients is the constant term)."
  ^double [^double x ^doubles coefficients]
  (reduce
   #(+ (* x ^double %1) ^double %2)
   (first coefficients)
   (rest coefficients)))

;;; Log-gamma (Lanczos approximation)

(def ^:private ^:const log-sqrt-2pi
  "Precomputed log(sqrt(2*pi)) for Lanczos approximation."
  (Math/log (Math/sqrt (* 2.0 Math/PI))))

(def ^:private ^"[D" lanczos-g7-coefficients
  "Lanczos coefficients for g=7, n=9.
  Source: Numerical Recipes 3rd edition, section 6.1.
  These provide ~15 significant digits for the gamma function."
  (double-array
   [0.99999999999980993
    676.5203681218851
    -1259.1392167224028
    771.32342877765313
    -176.61502916214059
    12.507343278686905
    -0.13857109526572012
    9.9843695780195716e-6
    1.5056327351493116e-7]))

(defn log-gamma
  "Compute the natural logarithm of the gamma function using Lanczos approximation.
  Returns ln(Γ(x)) for x > 0.

  Uses the Lanczos approximation with g=7 and 9 coefficients, providing
  approximately 15 digits of precision. Matches R's lgamma() behavior.

  Special cases:
  - x ≤ 0: throws IllegalArgumentException
  - x = 1 or x = 2: returns 0.0 (since Γ(1) = Γ(2) = 1)

  Reference: Numerical Recipes 3rd ed., section 6.1"
  ^double [^double x]
  (when (<= x 0.0)
    (throw (IllegalArgumentException.
            (str "log-gamma requires positive argument, got: " x))))
  ;; Use reflection formula for x < 0.5 to improve accuracy
  (if (< x 0.5)
    ;; Reflection formula: Γ(x)Γ(1-x) = π/sin(πx)
    ;; So: log(Γ(x)) = log(π) - log(sin(πx)) - log(Γ(1-x))
    (- (Math/log Math/PI)
       (Math/log (Math/sin (* Math/PI x)))
       (log-gamma (- 1.0 x)))
    ;; Standard Lanczos for x >= 0.5
    (let [z  (- x 1.0)
          g  7.0
          ;; Compute the sum: c0 + c1/(z+1) + c2/(z+2) + ... + c8/(z+8)
          ag (loop [i   8
                    sum (aget lanczos-g7-coefficients 0)]
               (if (< i 1)
                 sum
                 (recur (dec i)
                        (+ sum (/ (aget lanczos-g7-coefficients i)
                                  (+ z (double i)))))))
          t  (+ z g 0.5)]
      ;; log(Γ(z+1)) = log(sqrt(2π)) + (z+0.5)*log(t) - t + log(ag)
      (+ (double log-sqrt-2pi)
         (* (+ z 0.5) (Math/log t))
         (- t)
         (Math/log ag)))))

;;; Error function

(def ^:private a-coeffs
  [1.061405429 -1.453152027 1.421413741 -0.284496736 0.254829592 0.0])

(defn erf
  "erf polynomial approximation.  Maximum error is 1.5e-7.
  Handbook of Mathematical Functions: with Formulas, Graphs, and Mathematical
  Tables. Milton Abramowitz (Editor), Irene A. Stegun (Editor), 7.1.26"
  ^double [^double x]
  (let [sign  (Math/signum x)
        x     (Math/abs x)
        a     a-coeffs
        p     0.3275911
        t     (/ (+ 1.0 (* p x)))
        value (- 1.0 (* (polynomial-value t a)
                        (Math/exp (- (* x x)))))]
    (* sign value)))

(defn normal-cdf
  "Probability p(X<x), for a normal distrubtion.  Uses the polynomial erf
  approximation above, and so is not super accurate."
  ^double [^double x]
  (* 0.5 (+ 1.0 (erf (/ x (Math/sqrt 2.0))))))

(def ^:private sqrt-2pi (Math/sqrt (* 2.0 Math/PI)))

(defn normal-pdf
  "Probability density function for the normal distribution."
  [^double mu ^double sigma]
  (let [d (* sigma (double sqrt-2pi))]
    (fn ^double [^double x]
      (let [e (/ (- x mu) sigma)]
        (/ (Math/exp (* -0.5 e e))
           d)))))

(defn normal-quantile
  "Normal quantile function. Given a quantile in (0,1), return the normal value
  for that quantile.

  Wichura, MJ. 'Algorithm AS241' The Percentage Points of the Normal
  Distribution. Applied Statistics, 37, 477-484 "
  ^double [^double x]
  (let [x (double x)
        a [2509.0809287301226727
           33430.575583588128105
           67265.770927008700853
           45921.953931549871457
           13731.693765509461125
           1971.5909503065514427
           133.14166789178437745
           3.3871328727963666080]
        b [5226.4952788528545610
           28729.085735721942674
           39307.895800092710610
           21213.794301586595867
           5394.1960214247511077
           687.18700749205790830
           42.313330701600911252
           1.0]
        c [0.000774545014278341407640
           0.0227238449892691845833
           0.241780725177450611770
           1.27045825245236838258
           3.64784832476320460504
           5.76949722146069140550
           4.63033784615654529590
           1.42343711074968357734]
        d [1.05075007164441684324e-9
           0.000547593808499534494600
           0.0151986665636164571966
           0.148103976427480074590
           0.689767334985100004550
           1.67638483018380384940
           2.05319162663775882187
           1.0]
        e [2.01033439929228813265e-7
           0.0000271155556874348757815
           0.00124266094738807843860
           0.0265321895265761230930
           0.296560571828504891230
           1.78482653991729133580
           5.46378491116411436990
           6.65790464350110377720]
        f [2.04426310338993978564e-15
           1.42151175831644588870e-7
           1.84631831751005468180e-5
           0.000786869131145613259100
           0.0148753612908506148525
           0.136929880922735805310
           0.599832206555887937690
           1.0]]
    (if (<= 0.075 x 0.925)
      (let [v (- x 0.5)
            r (- 180625e-6 (* v v))]
        (* v (/ (polynomial-value r a) (polynomial-value r b))))
      (let [r (if (< x 0.5) x (- 1.0 x))
            r (Math/sqrt (- (Math/log r)))]
        (if (<= r 5.0)
          (let [r (- r (double 16/10))]
            (* (Math/signum (double (- x 0.5)))
               (/ (polynomial-value r c) (polynomial-value r d))))
          (let [r (- r 5.0)]
            (* (Math/signum (double (- x 0.5)))
               (/ (polynomial-value r e) (polynomial-value r f)))))))))

;;; Regularized Incomplete Gamma Function
;; Required for gamma distribution CDF

(defn regularized-gamma-p
  "Regularized lower incomplete gamma function P(a, x) = γ(a,x) / Γ(a).
  Uses series expansion for small x, continued fraction for large x.

  This is the CDF of the gamma distribution with shape=a and scale=1.

  Reference: Numerical Recipes 3rd ed., section 6.2"
  ^double [^double a ^double x]
  (cond
    (< x 0.0)
    (throw (IllegalArgumentException. (str "x must be >= 0, got: " x)))

    (<= a 0.0)
    (throw (IllegalArgumentException. (str "a must be > 0, got: " a)))

    (== x 0.0)
    0.0

    ;; Use series expansion when x < a + 1
    (< x (+ a 1.0))
    (let [max-iter 200
          eps 1e-14
          log-gamma-a (log-gamma a)]
      (loop [n 1
             ap a
             sum (/ 1.0 a)
             del (/ 1.0 a)]
        (if (>= n max-iter)
          (* sum (Math/exp (- (* a (Math/log x)) x log-gamma-a)))
          (let [ap (+ ap 1.0)
                del (* del (/ x ap))
                sum (+ sum del)]
            (if (< (Math/abs del) (* (Math/abs sum) eps))
              (* sum (Math/exp (- (* a (Math/log x)) x log-gamma-a)))
              (recur (inc n) ap sum del))))))

    ;; Use continued fraction when x >= a + 1
    :else
    (let [max-iter 200
          eps 1e-14
          fpmin 1e-300
          log-gamma-a (log-gamma a)
          b (+ x 1.0 (- a))
          c (/ 1.0 fpmin)
          d (/ 1.0 b)
          h d]
      (loop [i 1
             d d
             c c
             h h]
        (if (>= i max-iter)
          (- 1.0 (* h (Math/exp (- (* a (Math/log x)) x log-gamma-a))))
          (let [an (* (- i) (- i a))
                b (+ b 2.0)
                d (+ (* an d) b)
                d (if (< (Math/abs d) fpmin) fpmin d)
                c (+ b (/ an c))
                c (if (< (Math/abs c) fpmin) fpmin c)
                d (/ 1.0 d)
                del (* d c)
                h (* h del)]
            (if (< (Math/abs (- del 1.0)) eps)
              (- 1.0 (* h (Math/exp (- (* a (Math/log x)) x log-gamma-a))))
              (recur (inc i) d c h))))))))

;;; Gamma Distribution

(defn gamma-pdf
  "Probability density function for the gamma distribution.

  Parameters:
    shape (k) - shape parameter, must be > 0
    scale (θ) - scale parameter, must be > 0

  Returns a function f(x) that computes the density at x.
  f(x) = x^(k-1) * e^(-x/θ) / (θ^k * Γ(k)) for x > 0"
  [^double shape ^double scale]
  (when (<= shape 0.0)
    (throw (IllegalArgumentException. (str "shape must be > 0, got: " shape))))
  (when (<= scale 0.0)
    (throw (IllegalArgumentException. (str "scale must be > 0, got: " scale))))
  (let [log-normalizer (+ (* shape (Math/log scale)) (log-gamma shape))]
    (fn ^double [^double x]
      (if (<= x 0.0)
        0.0
        (Math/exp (- (+ (* (- shape 1.0) (Math/log x))
                        (- (/ x scale)))
                     log-normalizer))))))

(defn gamma-cdf
  "Cumulative distribution function for the gamma distribution.

  Parameters:
    shape (k) - shape parameter, must be > 0
    scale (θ) - scale parameter, must be > 0

  Returns a function F(x) that computes P(X ≤ x).
  Uses the regularized incomplete gamma function."
  [^double shape ^double scale]
  (when (<= shape 0.0)
    (throw (IllegalArgumentException. (str "shape must be > 0, got: " shape))))
  (when (<= scale 0.0)
    (throw (IllegalArgumentException. (str "scale must be > 0, got: " scale))))
  (fn ^double [^double x]
    (if (<= x 0.0)
      0.0
      (regularized-gamma-p shape (/ x scale)))))

;;; Weibull Distribution

(defn weibull-pdf
  "Probability density function for the Weibull distribution.

  Parameters:
    shape (k) - shape parameter, must be > 0
    scale (λ) - scale parameter, must be > 0

  Returns a function f(x) that computes the density at x.
  f(x) = (k/λ) * (x/λ)^(k-1) * e^(-(x/λ)^k) for x > 0"
  [^double shape ^double scale]
  (when (<= shape 0.0)
    (throw (IllegalArgumentException. (str "shape must be > 0, got: " shape))))
  (when (<= scale 0.0)
    (throw (IllegalArgumentException. (str "scale must be > 0, got: " scale))))
  (fn ^double [^double x]
    (if (<= x 0.0)
      0.0
      (let [x-scaled (/ x scale)
            x-pow (Math/pow x-scaled (- shape 1.0))
            exp-term (Math/exp (- (Math/pow x-scaled shape)))]
        (* (/ shape scale) x-pow exp-term)))))

(defn weibull-cdf
  "Cumulative distribution function for the Weibull distribution.

  Parameters:
    shape (k) - shape parameter, must be > 0
    scale (λ) - scale parameter, must be > 0

  Returns a function F(x) that computes P(X ≤ x).
  F(x) = 1 - e^(-(x/λ)^k) for x > 0"
  [^double shape ^double scale]
  (when (<= shape 0.0)
    (throw (IllegalArgumentException. (str "shape must be > 0, got: " shape))))
  (when (<= scale 0.0)
    (throw (IllegalArgumentException. (str "scale must be > 0, got: " scale))))
  (fn ^double [^double x]
    (if (<= x 0.0)
      0.0
      (- 1.0 (Math/exp (- (Math/pow (/ x scale) shape)))))))

;;; Log-normal Distribution

(defn lognormal-pdf
  "Probability density function for the log-normal distribution.

  Parameters:
    mu (μ) - mean of the underlying normal distribution (log scale)
    sigma (σ) - standard deviation of the underlying normal, must be > 0

  Returns a function f(x) that computes the density at x.
  f(x) = 1/(x*σ*√(2π)) * e^(-(ln(x)-μ)²/(2σ²)) for x > 0"
  [^double mu ^double sigma]
  (when (<= sigma 0.0)
    (throw (IllegalArgumentException. (str "sigma must be > 0, got: " sigma))))
  (let [log-normalizer (+ (Math/log sigma) (* 0.5 (Math/log (* 2.0 Math/PI))))]
    (fn ^double [^double x]
      (if (<= x 0.0)
        0.0
        (let [log-x (Math/log x)
              z (/ (- log-x mu) sigma)]
          (Math/exp (- (- (* 0.5 z z))
                       (Math/log x)
                       log-normalizer)))))))

(defn lognormal-cdf
  "Cumulative distribution function for the log-normal distribution.

  Parameters:
    mu (μ) - mean of the underlying normal distribution (log scale)
    sigma (σ) - standard deviation of the underlying normal, must be > 0

  Returns a function F(x) that computes P(X ≤ x).
  F(x) = Φ((ln(x) - μ) / σ) for x > 0"
  [^double mu ^double sigma]
  (when (<= sigma 0.0)
    (throw (IllegalArgumentException. (str "sigma must be > 0, got: " sigma))))
  (fn ^double [^double x]
    (if (<= x 0.0)
      0.0
      (let [z (/ (- (Math/log x) mu) sigma)]
        (normal-cdf z)))))

;;; Inverse Gaussian (Wald) Distribution

(defn inverse-gaussian-pdf
  "Probability density function for the inverse Gaussian distribution.

  Parameters:
    mu (μ) - mean parameter, must be > 0
    lambda (λ) - shape parameter, must be > 0

  Returns a function f(x) that computes the density at x.
  f(x) = √(λ/(2πx³)) * e^(-λ(x-μ)²/(2μ²x)) for x > 0"
  [^double mu ^double lambda]
  (when (<= mu 0.0)
    (throw (IllegalArgumentException. (str "mu must be > 0, got: " mu))))
  (when (<= lambda 0.0)
    (throw (IllegalArgumentException. (str "lambda must be > 0, got: " lambda))))
  (let [mu-sq (* mu mu)
        half-log-lambda-2pi (* 0.5 (- (Math/log lambda)
                                      (Math/log (* 2.0 Math/PI))))]
    (fn ^double [^double x]
      (if (<= x 0.0)
        0.0
        (let [diff (- x mu)
              exponent (/ (* (- lambda) diff diff)
                          (* 2.0 mu-sq x))]
          (Math/exp (+ half-log-lambda-2pi
                       (* -1.5 (Math/log x))
                       exponent)))))))

(defn inverse-gaussian-cdf
  "Cumulative distribution function for the inverse Gaussian distribution.

  Parameters:
    mu (μ) - mean parameter, must be > 0
    lambda (λ) - shape parameter, must be > 0

  Returns a function F(x) that computes P(X ≤ x).
  F(x) = Φ(√(λ/x)*(x/μ - 1)) + e^(2λ/μ) * Φ(-√(λ/x)*(x/μ + 1))"
  [^double mu ^double lambda]
  (when (<= mu 0.0)
    (throw (IllegalArgumentException. (str "mu must be > 0, got: " mu))))
  (when (<= lambda 0.0)
    (throw (IllegalArgumentException. (str "lambda must be > 0, got: " lambda))))
  (let [two-lambda-over-mu (/ (* 2.0 lambda) mu)]
    (fn ^double [^double x]
      (if (<= x 0.0)
        0.0
        (let [sqrt-lambda-x (Math/sqrt (/ lambda x))
              x-over-mu (/ x mu)
              term1 (normal-cdf (* sqrt-lambda-x (- x-over-mu 1.0)))
              ;; For numerical stability, cap the exponent
              exp-factor (Math/min two-lambda-over-mu 700.0)
              term2 (* (Math/exp exp-factor)
                       (normal-cdf (* (- sqrt-lambda-x) (+ x-over-mu 1.0))))]
          (+ term1 term2))))))

;;; Information Criteria for Model Selection

(defn aic
  "Akaike Information Criterion.

  AIC = 2k - 2·ln(L)

  Parameters:
    k - number of estimated parameters
    log-likelihood - log-likelihood value (log(L))

  Lower AIC indicates better model fit (balances goodness of fit with parsimony).

  Reference: Akaike (1974), A new look at the statistical model identification."
  ^double [^long k ^double log-likelihood]
  (- (* 2.0 (double k)) (* 2.0 log-likelihood)))

(defn bic
  "Bayesian Information Criterion (Schwarz criterion).

  BIC = k·ln(n) - 2·ln(L)

  Parameters:
    k - number of estimated parameters
    n - sample size
    log-likelihood - log-likelihood value (log(L))

  Lower BIC indicates better model fit. BIC penalizes model complexity
  more heavily than AIC for n ≥ 8.

  Reference: Schwarz (1978), Estimating the dimension of a model."
  ^double [^long k ^long n ^double log-likelihood]
  (- (* (double k) (Math/log (double n)))
     (* 2.0 log-likelihood)))

(defn aicc
  "Corrected Akaike Information Criterion for small samples.

  AICc = AIC + (2k² + 2k) / (n - k - 1)
       = 2k - 2·ln(L) + (2k² + 2k) / (n - k - 1)

  Parameters:
    k - number of estimated parameters
    n - sample size
    log-likelihood - log-likelihood value (log(L))

  For small samples (n/k < 40), AICc should be used instead of AIC.
  As n → ∞, AICc → AIC.

  Requires n > k + 1 to avoid division by zero.

  Reference: Hurvich & Tsai (1989), Regression and time series model
             selection in small samples."
  ^double [^long k ^long n ^double log-likelihood]
  (let [k (double k)
        n (double n)
        denom (- n k 1.0)]
    (when (<= denom 0.0)
      (throw (IllegalArgumentException.
              (str "AICc requires n > k + 1, got n=" (long n) ", k=" (long k)))))
    (+ (aic (long k) log-likelihood)
       (/ (+ (* 2.0 k k) (* 2.0 k))
          denom))))
