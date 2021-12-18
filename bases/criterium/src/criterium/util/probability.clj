(ns criterium.util.probability
  "probability functions")

(defn polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
  descending order (so the last element of coefficients is the constant term)."
  ^double [^double x ^doubles coefficients]
  (reduce
   #(+ (* x ^double %1) ^double %2)
   (first coefficients)
   (rest coefficients)))

(def a-coeffs
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

(def sqrt-2pi (Math/sqrt (* 2.0 Math/PI)))

(defn normal-pdf
  "Probability density function for the normal distribution."
  [^double mu ^double sigma]
  (let [d (* sigma (double sqrt-2pi))]
    (fn ^double [^double x]
      (let [e (/ (- x mu) sigma)]
        (/ (Math/exp (* -0.5 (* e e)))
           d)))))

((normal-pdf 1.0 1.0) 1.0)
((normal-pdf 100.0 100.0) 100.0)

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
