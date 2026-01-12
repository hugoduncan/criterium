(ns criterium.stats.core
  "Core statistical functions: min, max, mean, sum, variance, median, quartiles, quantile.

  All functions require typed arrays (ITypedArray) as input.
  Primitive-optimized implementations avoid boxing overhead."
  (:refer-clojure :exclude [min max])
  (:require
   [criterium.array :as arr]
   criterium.array.interface
   [criterium.utils.interface :as utils])
  (:import
   [criterium.array.interface ITypedArray IDoubleFold IIndexed]))

(defn transpose
  "Transpose a vector of vectors."
  [data]
  (if (vector? (first data))
    (apply map vector data)
    data))

(defn- require-typed-array!
  "Throws if data is not a typed array."
  [data fn-name]
  (when-not (arr/typed-array? data)
    (throw (ex-info (str fn-name " requires a typed array, got: " (type data))
                    {:fn fn-name
                     :type (type data)
                     :data data}))))

(defn- typed-array-length
  "Returns the length of a typed array."
  ^long [^ITypedArray arr]
  (.length arr))

(defn- typed-array-fold-double
  "Fold over typed array with primitive double function."
  ^double [^IDoubleFold arr ^clojure.lang.IFn$DDD f ^double init]
  (.fold arr f init))

(defn- typed-array-get-double
  "Get element at index as double from typed array."
  ^double [^IIndexed arr ^long index]
  (.getDouble arr index))

(defn- dmin
  "Primitive double min function."
  ^double [^double a ^double b]
  (if (< a b) a b))

(defn- dmax
  "Primitive double max function."
  ^double [^double a ^double b]
  (if (> a b) a b))

(defn min
  "Minimum value in data.
  Requires a typed array (ITypedArray)."
  ([data]
   (require-typed-array! data "min")
   (typed-array-fold-double data dmin Double/MAX_VALUE))
  ([data _count]
   (min data)))

(defn max
  "Maximum value in data.
  Requires a typed array (ITypedArray)."
  ([data]
   (require-typed-array! data "max")
   (typed-array-fold-double data dmax Double/MIN_VALUE))
  ([data _count]
   (max data)))

(defn unchecked-add-d
  "Unchecked double addition."
  ^double [^double a ^double b]
  (unchecked-add a b))

(defn mean
  "Arithmetic mean of data.
  Requires a typed array (ITypedArray)."
  (^double [data]
   (require-typed-array! data "mean")
   (let [c (typed-array-length data)]
     (when (pos? c)
       (/ (typed-array-fold-double data unchecked-add-d 0.0) c))))
  (^double [data ^long count]
   (require-typed-array! data "mean")
   (/ (typed-array-fold-double data unchecked-add-d 0.0) count)))

(defn sum
  "Sum of each data point.
  Requires a typed array (ITypedArray)."
  [data]
  (require-typed-array! data "sum")
  (typed-array-fold-double data unchecked-add-d 0.0))

(defn sum-of-squares
  "Sum of the squares of each data point.
  Requires a typed array (ITypedArray)."
  [data]
  (require-typed-array! data "sum-of-squares")
  (let [f (fn ^double [^double s ^double v] (+ s (* v v)))]
    (typed-array-fold-double data f 0.0)))

(defn variance*
  "Variance based on subtracting mean.
  Requires a typed array (ITypedArray)."
  ^double [data ^double mean ^long df]
  (require-typed-array! data "variance*")
  (let [f (fn ^double [^double a ^double b]
            (+ a (utils/sqr (- b mean))))]
    (/ (typed-array-fold-double data f 0.0) df)))

(defn- variance-typed-array
  "Single-pass variance computation for typed arrays."
  ^double [^IDoubleFold data ^long df]
  (let [^doubles mq (double-array [0.0 0.0])
        ^longs k (long-array [0])]
    ;; Accumulate in arrays to avoid boxing
    (.fold data
           (fn ^double [^double _ ^double x]
             (let [k-val   (aget k 0)
                   kp1     (unchecked-inc k-val)
                   m       (aget mq 0)
                   delta   (- x m)
                   new-m   (+ m (/ delta kp1))
                   new-q   (+ (aget mq 1) (/ (* k-val (utils/sqr delta)) kp1))]
               (aset mq 0 new-m)
               (aset mq 1 new-q)
               (aset k 0 kp1)
               0.0))
           0.0)
    (let [k-val (aget k 0)]
      (when (> k-val df)
        (/ (aget mq 1) (- k-val df))))))

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
   (require-typed-array! data "variance")
   (variance-typed-array data df)))

(defn median-value
  "Calculate the median value of a sorted data set.
  Returns just the median value (not the lower/upper partitions).
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Median"
  ^double [data]
  (require-typed-array! data "median-value")
  (let [n (typed-array-length data)
        i (bit-shift-right n 1)]
    (if (even? n)
      (/ (+ (typed-array-get-double data (dec i))
            (typed-array-get-double data i))
         2.0)
      (typed-array-get-double data (bit-shift-right n 1)))))

(defn median
  "Calculate the median of a sorted data set.
  Return [median nil nil] (partitions not supported for typed arrays).
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Median"
  [data]
  (require-typed-array! data "median")
  [(median-value data) nil nil])

(defn quartiles
  "Calculate the quartiles of a sorted data set.
  Returns [q1 median q3].
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Quartile"
  [data]
  (require-typed-array! data "quartiles")
  (let [n (typed-array-length data)
        q1-idx (quot n 4)
        q3-idx (quot (* 3 n) 4)
        i (bit-shift-right n 1)
        med (if (even? n)
              (/ (+ (typed-array-get-double data (dec i))
                    (typed-array-get-double data i))
                 2.0)
              (typed-array-get-double data i))]
    [(typed-array-get-double data q1-idx)
     med
     (typed-array-get-double data q3-idx)]))

(defn quantile
  "Calculate the quantile of a sorted data set.
  Requires a typed array (ITypedArray).
  References: http://en.wikipedia.org/wiki/Quantile"
  [^double quantile data]
  (require-typed-array! data "quantile")
  (let [n (dec (typed-array-length data))
        interp (fn [^double x]
                 (let [f (Math/floor x)
                       i (long f)
                       p (- x f)]
                   (cond
                     (zero? p) (typed-array-get-double data i)
                     (= 1.0 p) (typed-array-get-double data (inc i))
                     :else     (+ (* p (typed-array-get-double data (inc i)))
                                  (* (- 1.0 p) (typed-array-get-double data i))))))]
    (interp (* quantile n))))

(defn central-moment
  "Compute the r-th central moment: (1/n) * Σ(xᵢ - μ)^r
  Requires a typed array (ITypedArray)."
  ^double [data ^double mean ^long r]
  (require-typed-array! data "central-moment")
  (let [n (typed-array-length data)
        f (fn ^double [^double acc ^double x]
            (+ acc (Math/pow (- x mean) r)))]
    (/ (typed-array-fold-double data f 0.0) n)))

(defn- data-count
  "Returns the count of elements in a typed array."
  ^long [data]
  (typed-array-length data))

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
   (let [n  (data-count data)
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
   (let [n  (data-count data)
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
  (let [n (data-count data)]
    (if (< n 2)
      Double/NaN
      (let [mu (mean data)]
        (if (zero? mu)
          Double/NaN
          (/ (Math/sqrt (variance data)) mu))))))
