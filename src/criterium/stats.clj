;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.

;;;; A collection of statistical methods used by criterium

(ns criterium.stats)

;; (set! *warn-on-reflection* true)

;;; Utilities
(defn transpose
  "Transpose a vector of vectors."
  [data]
  (if (vector? (first data))
    (apply map vector data)
    data))

(defn sqr
  "Square of argument"
  [x] (* x x))

(defn cube
  "Square of argument"
  [x] (* x x x))


;;; Statistics
(defn mean
  "Arithmetic mean of data."
  [data]
  (/ (reduce + data) (count data)))

(defn sum
  "Sum of each data point."
  [data] (reduce + data))

(defn sum-of-squares
  "Sum of the squares of each data point."
  [data]
  (reduce
   (fn [s v]
     (+ s (* v v))) 0.0 data))

(defn variance
  "Sample variance. Returns variance.
   Ref: Chan et al. Algorithms for computing the sample variance: analysis and
        recommendations. American Statistician (1983)."
  ([data] (variance data 1))
  ([data df]
   ;; Uses a single pass, non-pairwise algorithm, without shifting.
   (letfn [(update-estimates [[m q k] x]
             [(+ m (/ (- x m) (inc k)))
              (+ q (/ (* k (sqr (- x m))) (inc k)))
              (inc k)])]
     (let [[m q k] (reduce update-estimates [0.0 0.0 0.0] data)]
       (/ q (- k df))))))

;; For the moment we take the easy option of sorting samples
(defn median
  "Calculate the median of a sorted data set
   References: http://en.wikipedia.org/wiki/Median"
  [data]
  (let [n (count data)
        i (bit-shift-right n 1)]
    (if (even? n)
      [(/ (+ (nth data (dec i)) (nth data i)) 2)
       (take i data)
       (drop i data)]
      [(nth data (bit-shift-right n 1))
       (take i data)
       (drop (inc i) data)])))

(defn quartiles
  "Calculate the quartiles of a sorted data set
   References: http://en.wikipedia.org/wiki/Quartile"
  [data]
  (let [[m lower upper] (median data)]
    [(first (median lower)) m (first (median upper))]))

(defn quantile
  "Calculate the quantile of a sorted data set
   References: http://en.wikipedia.org/wiki/Quantile"
  [quantile data]
  (let [n (dec (count data))
        interp (fn [x]
                 (let [f (Math/floor x)
                       i (long f)
                       p (- x f)]
                   (+ (* p (nth data (inc i))) (* (- 1.0 p) (nth data i)))))]
    (interp (* quantile n))))

(defn boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles."
  [q1 q3]
  (let [iqr (- q3 q1)
        severe (* iqr 3)
        mild (* iqr 1.5)]
    [(- q1 severe)
     (- q1 mild)
     (+ q3 mild)
     (+ q3 severe)]))


(defn uniform-distribution
  "Return uniformly distributed deviates on 0..max-val use the specified rng."
  [max-val rng]
  (map (fn [x] (* x max-val)) rng))

(defn sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val"
  [n max-val rng]
  (take n (uniform-distribution max-val rng)))

(defn sample
  "Sample with replacement."
  [x rng]
  (let [n (count x)]
    (map #(nth x %1) (sample-uniform n n rng))))

(defn bootstrap-sample
  "Bootstrap sampling of a statistic, using resampling with replacement."
  [data statistic size rng-factory]
  (transpose
   (for [_ (range size)] (statistic (sort (sample data (rng-factory)))))))

(defn confidence-interval
  "Find the significance of outliers gicen boostrapped mean and variance
   estimates. This uses the bootstrapped statistic's variance, but we should use
   BCa of ABC."
  [mean variance]
  (let [n-sigma 1.96                    ; use 95% confidence interval
        delta (* n-sigma (Math/sqrt variance))]
    [(- mean delta) (+ mean delta)]))

(defn bootstrap-estimate
  "Mean, variance and confidence interval. This uses the bootstrapped
  statistic's variance for the confidence interval, but we should use BCa of
  ABC."
  [sampled-stat]
  (let [stats ((juxt mean variance ) sampled-stat)]
    (conj stats (apply confidence-interval stats))))

(defn scale-bootstrap-estimate [estimate scale]
  [(* (first estimate) scale)
   (map #(* scale %1) (last estimate))])

(defn polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
descending order (so the last element of coefficients is the constant term)."
  [x coefficients]
  (reduce #(+ (* x %1) %2) (first coefficients) (rest coefficients)))

(defn erf
  "erf polynomial approximation.  Maximum error is 1.5e-7.
  Handbook of Mathematical Functions: with Formulas, Graphs, and Mathematical
  Tables. Milton Abramowitz (Editor), Irene A. Stegun (Editor), 7.1.26"
  [x]
  (let [x (double x)
        sign (Math/signum x)
        x (Math/abs x)
        a [1.061405429 -1.453152027 1.421413741 -0.284496736 0.254829592 0.0]
        p 0.3275911
        t (/ (+ 1.0 (* p x)))
        value (- 1.0 (* (polynomial-value t a) (Math/exp (- (* x x)))))]
    (* sign value)))

(defn normal-cdf
  "Probability p(X<x), for a normal distrubtion.  Uses the polynomial erf
  approximation above, and so is not super accurate."
  [x]
  (* 0.5 (+ 1.0 (erf (/ x (Math/sqrt 2.0))))))

(defn normal-quantile
  "Normal quantile function. Given a quantile in (0,1), return the normal value
  for that quantile.

  Wichura, MJ. 'Algorithm AS241' The Percentage Points of the Normal
  Distribution. Applied Statistics, 37, 477-484 "
  [x]
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
        e [
           2.01033439929228813265e-7
           0.0000271155556874348757815
           0.00124266094738807843860
           0.0265321895265761230930
           0.296560571828504891230
           1.78482653991729133580
           5.46378491116411436990
           6.65790464350110377720
           ]
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


(defn drop-at [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (concat (take n s) (next (drop n s))))))

(defn trunc
  "Round towards zero to an integeral value."
  [x] (if (pos? x)
        (Math/floor x)
        (Math/ceil x)))

(defn jacknife
  "Jacknife statistics on data."
  [data statistic]
  (transpose
   (map #(statistic (drop-at %1 data)) (range (count data)))))

(defn bca-nonparametric-eval
  "Calculate bootstrap values for given estimate and samples"
  [n size data z-alpha estimate samples jack-samples]
  (let [z0 (normal-quantile
            (/ (count (filter (partial > estimate) samples)) size))
        jack-mean (mean jack-samples)
        jack-deviation (map #(- jack-mean %1) jack-samples)
        acc (/ (reduce + 0.0 (map cube jack-deviation))
               (* 6.0 (Math/pow (reduce + 0.0 (map sqr jack-deviation)) 1.5)))
        tt (map
            #(normal-cdf (+ z0 (/ (+ z0 %1) (- 1.0 (* acc (+ z0 %1))))))
            z-alpha)
        ooo (map #(trunc (* %1 size)) tt)
        sorted-samples (sort samples)
        confpoints (map (partial nth sorted-samples) ooo)]
    [confpoints z0 acc jack-mean jack-samples]))

(defn bca-nonparametric
  "Non-parametric BCa estimate of a statistic on data. Size bootstrap samples
  are used. Confidence values are returned at the alpha normal
  quantiles. rng-factory is a method that returns a random number generator to
  use for the sampling.

  An introduction to the bootstrap.  Efron, B., & Tibshirani, R. J. (1993).

  See http://lib.stat.cmu.edu/S/bootstrap.funs for Efron's original
   implementation."
  [data statistic size alpha rng-factory]
  (let [n (count data)
        data (sort data)
        estimate (statistic data)
        samples (bootstrap-sample data statistic size rng-factory)
        jack-samples (jacknife data statistic)
        alpha (if (vector? alpha) alpha [alpha])
        z-alpha (map normal-quantile alpha)]
    (if (vector? estimate)
      (map
       (partial bca-nonparametric-eval n size data z-alpha)
       estimate samples jack-samples)
      (bca-nonparametric-eval
       n size data z-alpha estimate samples jack-samples))))

(defn bca-to-estimate [alpha bca-estimate]
  [(first (first bca-estimate)) (next (first bca-estimate))])



;;; Nonparametric assessment of multimodality for univariate data.
;;; Salgado-Ugarte IH, Shimizu M. 1998

;;; Maximum likelihood kernel density estimation: On the potential of convolution sieves.
;;; Jones and Henderson. Computational Statistics and Data Analysis (2009)

(defn modal-estimation-constant
  "Kernel function for estimation of multi-modality.
  h-k is the critical bandwidth, sample-variance is the observed sample variance.
  Equation 7, Nonparametric assessment of multimodality for univariate
  data. Salgado-Ugarte IH, Shimizu M"
  [h-k sample-variance]
  (Math/sqrt (+ 1 (/ (sqr h-k) sample-variance))))

(defn smoothed-sample
  "Smoothed estimation function."
  [c-k h-k data deviates]
  (lazy-seq
    (cons
     (* c-k (+ (take 1 data) (* h-k (take 1 deviates))))
     (if-let [n (next data)]
       (smoothed-sample c-k h-k n (next deviates))))))

(defn gaussian-weight
  "Weight function for gaussian kernel."
  [t]
  (let [k (Math/pow (* 2 Math/PI) -0.5)]
    (* k (Math/exp (/ (* t t) -2)))))

(defn kernel-density-estimator
  "Kernel density estimator for x, given n samples X, weights K and width h."
  [h K n X x]
  (/ (reduce #(+ %1 (K (/ (- x %2) h))) 0 X) (* n h)))
