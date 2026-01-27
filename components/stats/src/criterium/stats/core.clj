(ns criterium.stats.core
  "Core statistical functions: min, max, mean, sum, variance, median, quartiles, quantile.

  All functions require typed arrays (ITypedArray) as input.
  Primitive-optimized implementations avoid boxing overhead."
  (:refer-clojure :exclude [min max reduce])
  (:require
   [criterium.array :as arr]
   criterium.array.interfaces
   [criterium.primitive-fn :as prim]
   [criterium.transducer :as tr]
   [criterium.utils.interface :as utils :refer [have?]])
  (:import
   [criterium.array.interfaces IDoubleFold]
   [criterium.transducer.interfaces IDDDReducible]))

(defn transpose
  "Transpose a vector of vectors."
  [data]
  (if (vector? (first data))
    (apply map vector data)
    data))

(defn min
  "Minimum value in data.
  Requires a typed array (ITypedArray)."
  (^double [data]
   {:pre [(have? arr/typed-array? data)]}
   (tr/reduce prim/dmin Double/MAX_VALUE ^IDDDReducible data))
  (^double [data ^long _count]
   (min data)))

(defn max
  "Maximum value in data.
  Requires a typed array (ITypedArray)."
  (^double [data]
   {:pre [(have? arr/typed-array? data)]}
   (tr/reduce prim/dmax Double/MIN_VALUE ^IDDDReducible data))
  (^double [data ^long _count]
   (max data)))

(defn mean
  "Arithmetic mean of data.
  Requires a typed array (ITypedArray)."
  (^double [data]
   {:pre [(have? arr/typed-array? data)]}
   (let [c (arr/length data)]
     (when (pos? c)
       (/ (tr/reduce prim/dadd 0.0 ^IDDDReducible data) c))))
  (^double [data ^long count]
   {:pre [(have? arr/typed-array? data)]}
   (/ (tr/reduce prim/dadd 0.0 ^IDDDReducible data) count)))

(defn sum
  "Sum of each data point.
  Requires a typed array (ITypedArray)."
  ^double [data]
  {:pre [(have? arr/typed-array? data)]}
  (tr/reduce prim/dadd 0.0 ^IDDDReducible data))

(defn- reduce-dsquare
  "Primitive squaring function for transducer map."
  ^double [^double acc ^double x]
  (+ acc (* x x)))

(defn sum-of-squares
  "Sum of the squares of each data point.
  Requires a typed array (ITypedArray)."
  ^double [data]
  {:pre [(have? arr/typed-array? data)]}
  (tr/reduce reduce-dsquare 0.0 ^IDDDReducible data))

(defn variance*
  "Variance based on subtracting mean.
  Requires a typed array (ITypedArray)."
  ^double [data ^double mean ^long df]
  {:pre [(have? arr/typed-array? data)]}
  (let [f (fn ^double [^double a ^double b]
            (+ a (utils/sqr (- b mean))))]
    (/ (arr/fold-double data f 0.0) df)))

(defn- variance-typed-array
  "Single-pass variance computation for typed arrays."
  ^double [^IDDDReducible data ^long df]
  (let [^doubles mq (arr/doubles-fill 2 0.0)
        ^longs k    (arr/longs-fill 1 0)]
    ;; Accumulate in arrays to avoid boxing
    (tr/reduce
     (fn ^double [^double _ ^double x]
       (let [k-val (aget k 0)
             kp1   (unchecked-inc k-val)
             m     (aget mq 0)
             delta (- x m)
             new-m (+ m (/ delta kp1))
             new-q (+ (aget mq 1) (/ (* k-val (utils/sqr delta)) kp1))]
         (aset mq 0 new-m)
         (aset mq 1 new-q)
         (aset k 0 kp1)
         0.0))
     0.0
     data)
    (let [k-val (aget k 0)]
      (if (> k-val df)
        (/ (aget mq 1) (- k-val df))
        Double/NaN))))

(defn variance
  "Return the variance of data.

  By default returns the sample variance with (- (count data) 1) degrees
  of freedom.

  The population variance can be returned using (variance data 0), which uses
  (count data) degrees of freedom.

  Requires a typed array (ITypedArray).

  Ref: Chan et al. Algorithms for computing the sample variance: analysis and
       recommendations. American Statistician (1983)."
  (^double [data] (variance data 1))
  (^double [data ^long df]
   {:pre [(have? arr/typed-array? data)]}
   (variance-typed-array data df)))

(defn median-value
  "Calculate the median value of a sorted data set.
  Returns just the median value (not the lower/upper partitions).
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Median"
  ^double [data]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (arr/length data)
        i (bit-shift-right n 1)]
    (if (even? n)
      (/ (+ (arr/get-double data (dec i))
            (arr/get-double data i))
         2.0)
      (arr/get-double data (bit-shift-right n 1)))))

(defn median
  "Calculate the median of a sorted data set.
  Return [median nil nil] (partitions not supported for typed arrays).
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Median"
  [data]
  {:pre [(have? arr/typed-array? data)]}
  [(median-value data) nil nil])

(defn quartiles
  "Calculate the quartiles of a sorted data set.
  Returns [q1 median q3].
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Quartile"
  [data]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (arr/length data)
        q1-idx (quot n 4)
        q3-idx (quot (* 3 n) 4)
        i (bit-shift-right n 1)
        med (if (even? n)
              (/ (+ (arr/get-double data (dec i))
                    (arr/get-double data i))
                 2.0)
              (arr/get-double data i))]
    [(arr/get-double data q1-idx)
     med
     (arr/get-double data q3-idx)]))

(defn quantile
  "Calculate the quantile of a sorted data set.
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Quantile"
  ^double [^double quantile data]
  {:pre [(have? arr/typed-array? data)]}
  (let [n      (dec (arr/length data))
        interp (fn ^double [^double x]
                 (let [f (Math/floor x)
                       i (long f)
                       p (- x f)]
                   (cond
                     (zero? p) (arr/get-double data i)
                     (= 1.0 p) (arr/get-double data (inc i))
                     :else     (+ (* p (arr/get-double data (inc i)))
                                  (* (- 1.0 p) (arr/get-double data i))))))]
    (prim/invoke-dd interp (* quantile n))))

(defn central-moment
  "Compute the r-th central moment: (1/n) * Σ(xᵢ - μ)^r
  Requires a typed array (ITypedArray)."
  ^double [data ^double mean ^long r]
  {:pre [(have? arr/typed-array? data)]}
  (let [n (arr/length data)
        f (fn ^double [^double acc ^double x]
            (+ acc (Math/pow (- x mean) r)))]
    (/ (arr/fold-double data f 0.0) n)))

(defn skewness
  "Compute sample skewness using one of three methods.

  Type 1: g₁ = m₃ / m₂^(3/2) - typical textbook definition
  Type 2: G₁ = g₁ × √(n(n-1)) / (n-2) - unbiased under normality (SAS/SPSS)
  Type 3: b₁ = g₁ × ((n-1)/n)^(3/2) - used in MINITAB/BMDP

  Where m_r = sample central moment of order r: Σ(xᵢ - μ)^r / n

  Default is type 2 (unbiased under normality).
  Returns 0.0 for constant data (zero variance), since constant data is symmetric.
  Requires a typed array (ITypedArray).

  Reference: Joanes & Gill (1998), Comparing measures of sample skewness
             and kurtosis. The Statistician, 47, 183-189."
  (^double [data] (skewness data 2))
  (^double [data ^long type]
   (let [n  (arr/length data)
         mu (mean data)
         m2 (central-moment data mu 2)]
     (if (zero? m2)
       0.0
       (let [m3 (central-moment data mu 3)
             g1 (/ m3 (Math/pow m2 1.5))]
         (case (int type)
           1 g1
           2 (if (< n 3)
               Double/NaN
               (* g1 (/ (Math/sqrt (* n (dec n))) (- n 2))))
           3 (* g1 (Math/pow (/ (dec n) n) 1.5))
           (throw (ex-info "Invalid skewness type, must be 1, 2, or 3"
                           {:type type}))))))))

(defn kurtosis
  "Compute sample excess kurtosis using one of three methods.

  Type 1: g₂ = m₄ / m₂² - 3 - typical textbook definition
  Type 2: G₂ = ((n+1)g₂ + 6)(n-1) / ((n-2)(n-3)) - unbiased under normality (SAS/SPSS)
  Type 3: b₂ = (g₂ + 3)((n-1)/n)² - 3 - used in MINITAB/BMDP

  Where m_r = sample central moment of order r: Σ(xᵢ - μ)^r / n

  Default is type 2 (unbiased under normality). Returns excess kurtosis
  (normal distribution has excess kurtosis of 0).
  Returns 0.0 for constant data (zero variance).
  Requires a typed array (ITypedArray).

  Reference: Joanes & Gill (1998), Comparing measures of sample skewness
             and kurtosis. The Statistician, 47, 183-189."
  (^double [data] (kurtosis data 2))
  (^double [data ^long type]
   (let [n  (arr/length data)
         mu (mean data)
         m2 (central-moment data mu 2)]
     (if (zero? m2)
       0.0
       (let [m4 (central-moment data mu 4)
             g2 (- (/ m4 (* m2 m2)) 3.0)]
         (case (int type)
           1 g2
           2 (if (< n 4)
               Double/NaN
               (/ (* (+ (* (inc n) g2) 6) (dec n))
                  (* (- n 2) (- n 3))))
           3 (- (* (+ g2 3) (Math/pow (/ (dec n) n) 2)) 3)
           (throw (ex-info "Invalid kurtosis type, must be 1, 2, or 3"
                           {:type type}))))))))

(defn cv
  "Coefficient of variation (CV), also known as relative standard deviation.
  Computed as σ/μ (standard deviation divided by mean).

  Returns Double/NaN if mean is zero or data has fewer than 2 elements.
  CV is dimensionless and useful for comparing variability across datasets
  with different units or scales.
  Requires a typed array (ITypedArray)."
  ^double [data]
  (let [n (arr/length data)]
    (if (< n 2)
      Double/NaN
      (let [mu (mean data)]
        (if (zero? mu)
          Double/NaN
          (/ (Math/sqrt (variance data)) mu))))))
