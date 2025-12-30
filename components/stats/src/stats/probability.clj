(ns stats.probability
  "Probability functions: log-gamma, error function, normal distribution.")

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
