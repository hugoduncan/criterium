(ns criterium.util.stats
  "A collection of statistical methods used by criterium"
  (:refer-clojure :exclude [min max])
  (:require
   [criterium.util.helpers :as util]))

;;; Utilities
(defn transpose
  "Transpose a vector of vectors."
  [data]
  (if (vector? (first data))
    (apply map vector data)
    data))

;;; Statistics

(defn min
  ([data]
   (reduce clojure.core/min data))
  ([data _count]
   (reduce clojure.core/min data)))

(defn max
  ([data]
   (reduce clojure.core/max data))
  ([data _count]
   (reduce clojure.core/max data)))

(defn unchecked-add-d
  ^double [^double a ^double b]
  (unchecked-add a b))

(defn mean
  "Arithmetic mean of data."
  (^double [data]
   (let [c (count data)]
     (when (pos? c)
       (/ (double (reduce unchecked-add-d 0.0 data)) c))))
  (^double [data ^long count]
   (/ (double (reduce unchecked-add-d 0.0 data)) count)))

(defn sum
  "Sum of each data point."
  [data] (reduce + data))

(defn sum-of-squares
  "Sum of the squares of each data point."
  [data]
  (reduce
   (fn ^double [^double s ^double v]
     (+ s (* v v))) 0.0 data))

(defn variance*
  "variance based on subtracting mean"
  ^double [data ^double mean ^long df]
  (/ (double
      (reduce
       (fn ^double [^double a ^double b]
         (+ a (util/sqr (- b mean))))
       0.0
       data))
     df))

(defn variance
  "Return the variance of data.

  By default returns the sample variance with (- (count data) 1) degrees
  of freedom.

  The population variance can be returned using (variance data 0), which uses
  (count data) degrees of freedom.

   Ref: Chan et al. Algorithms for computing the sample variance: analysis and
        recommendations. American Statistician (1983)."
  (^double [data] (variance data 1))
  (^double [data ^long df]
   ;; Uses a single pass, non-pairwise algorithm, without shifting.
   (letfn [(update-estimates [[^double m ^double q ^long k] ^double x]
             (let [kp1   (inc k)
                   delta (- x m)]
               [(+ m (/ delta kp1))
                (+ q (/ (* k (util/sqr delta)) kp1))
                kp1]))]
     (let [[_ ^double q ^long k] (reduce update-estimates [0.0 0.0 0] data)]
       (when (> k df)
         ;; (throw (ex-info
         ;;         "insufficient data to calculate variance"
         ;;         {:data data}))

         (/ q (- k df)))))))

;; For the moment we take the easy option of sorting samples
(defn median
  "Calculate the median of a sorted data set.
  Return [median, [vals less than median] [vals greater than median]]
  References: http://en.wikipedia.org/wiki/Median"
  [data]
  (let [n (count data)
        i (bit-shift-right n 1)]
    (if (even? n)
      [(/ (+ (double (nth data (dec i)))
             (double (nth data i)))
          2.0)
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
  [^double quantile data]
  (let [n      (dec (count data))
        interp (fn [^double x]
                 (let [f (Math/floor x)
                       i (long f)
                       p (- x f)]
                   (cond
                     (zero? p) (nth data i)
                     (= 1.0 p) (nth data (inc i))
                     :else     (+ (* p (double (nth data (inc i))))
                                  (* (- 1.0 p) (double (nth data i)))))))]
    (interp (* quantile n))))

(defn boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles."
  [^double q1 ^double q3]
  {:pre [(number? q1) (number? q3)]}
  (let [iqr    (- q3 q1)
        severe (* iqr 3.0)
        mild   (* iqr 1.5)]
    [(- q1 severe)
     (- q1 mild)
     (+ q3 mild)
     (+ q3 severe)]))

(defn uniform-distribution
  "Return uniformly distributed deviates on 0..max-val use the specified rng."
  [^double max-val rng]
  (map (fn [^double x] (* x max-val)) rng))

(defn sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val"
  [n max-val rng]
  (take n (uniform-distribution max-val rng)))

(defn sample
  "Sample with replacement."
  [x rng]
  (let [n (count x)]
    (map #(nth x %1) (sample-uniform n n rng))))

(defn confidence-interval
  "Find the significance of outliers given boostrapped mean and variance
   estimates. This uses the bootstrapped statistic's variance, but we should use
   BCa of ABC."
  [^double mean ^double variance]
  (let [n-sigma 1.96 ; use 95% confidence interval
        delta   (* n-sigma (Math/sqrt variance))]
    [(- mean delta) (+ mean delta)]))

;;; Nonparametric assessment of multimodality for univariate data.
;;; Salgado-Ugarte IH, Shimizu M. 1998

;;; Maximum likelihood kernel density estimation: On the potential of convolution sieves.
;;; Jones and Henderson. Computational Statistics and Data Analysis (2009)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn modal-estimation-constant
  "Kernel function for estimation of multi-modality.
  h-k is the critical bandwidth, sample-variance is the observed sample variance.
  Equation 7, Nonparametric assessment of multimodality for univariate
  data. Salgado-Ugarte IH, Shimizu M"
  [^double h-k ^double sample-variance]
  (Math/sqrt (+ 1 (/ (util/sqr h-k) sample-variance))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn smoothed-sample
  "Smoothed estimation function."
  [^double c-k ^double h-k data deviates]
  (lazy-seq
   (cons
    (* c-k (+ ^double (first data)
              (* h-k ^double (first deviates))))
    (when-let [n (next data)]
      (smoothed-sample c-k h-k n (next deviates))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn gaussian-weight
  "Weight function for gaussian kernel."
  [^double t]
  (let [k (Math/pow (* 2 Math/PI) -0.5)]
    (* k (Math/exp (/ (* t t) -2)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn kernel-density-estimator
  "Kernel density estimator for x, given n samples X, weights K and width h."
  [h K n X x]
  (/ ^double (reduce
              (fn ^double [^double a ^double b]
                (+ a
                   ^double (K (/ (- ^double x b) ^double h))))
              0.0 X)
     (* (long n) (double h))))

(defn sum-square-delta ^double [vs ^double mv]
  (reduce + (map (comp util/sqrd (fn [^double x] (- x mv))) vs)))

(defn- muld ^double [^double a ^double b] (* a b))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn linear-regression
  [xs ys]
  (let [n             (count xs)
        mx            (/ ^double (reduce + xs) n)
        my            (/ ^double (reduce + ys) n)
        nmxmy         (* n mx my)
        nmxmx         (* n mx mx)
        s1            (- ^double (reduce + (map muld xs ys))
                         nmxmy)
        s2            (- ^double (reduce + (map util/sqrd xs))
                         nmxmx)
        a1            (/ s1 s2)
        a0            (- my (* a1 mx))
        f             (fn ^double [^double x] (+ a0 (* a1 x)))
        pred-ys       (mapv f xs)
        sqr-residuals (mapv (comp util/sqrd -) ys pred-ys)
        ss-residuals  (double (reduce + sqr-residuals))
        variance      (/ ss-residuals (- n 2))
        ss-total      (sum-square-delta ys my)
        r-sqr         (- 1 (/ ss-residuals ss-total))]
    {:coeffs   [a0 a1]
     :variance variance
     :r-sqr    r-sqr}))

;; (= (linear-regression (range 10) (range 10)) [0 1 0 1])
;; (= (linear-regression (range 10) (range 1 11)) [1 1 0 1])

;; (let [[a0 a1] (linear-regression (range 10) (range 1 11))]
;;   (+ a0 (* a1 10)))
