(ns criterium.stats.core
  "Core statistical functions: min, max, mean, sum, variance, median, quartiles, quantile.

  Functions accept both sequences and typed arrays (ITypedArray).
  For typed arrays, primitive-optimized implementations are used."
  (:refer-clojure :exclude [min max])
  (:require
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

(defn- typed-array?
  "Returns true if data is a typed array."
  [data]
  (instance? ITypedArray data))

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
  Accepts sequences and typed arrays (ITypedArray)."
  ([data]
   (if (typed-array? data)
     (typed-array-fold-double data dmin Double/MAX_VALUE)
     (reduce clojure.core/min data)))
  ([data _count]
   (min data)))

(defn max
  "Maximum value in data.
  Accepts sequences and typed arrays (ITypedArray)."
  ([data]
   (if (typed-array? data)
     (typed-array-fold-double data dmax Double/MIN_VALUE)
     (reduce clojure.core/max data)))
  ([data _count]
   (max data)))

(defn unchecked-add-d
  "Unchecked double addition."
  ^double [^double a ^double b]
  (unchecked-add a b))

(defn mean
  "Arithmetic mean of data.
  Accepts sequences and typed arrays (ITypedArray)."
  (^double [data]
   (if (typed-array? data)
     (let [c (typed-array-length data)]
       (when (pos? c)
         (/ (typed-array-fold-double data unchecked-add-d 0.0) c)))
     (let [c (count data)]
       (when (pos? c)
         (/ (double (reduce unchecked-add-d 0.0 data)) c)))))
  (^double [data ^long count]
   (if (typed-array? data)
     (/ (typed-array-fold-double data unchecked-add-d 0.0) count)
     (/ (double (reduce unchecked-add-d 0.0 data)) count))))

(defn sum
  "Sum of each data point.
  Accepts sequences and typed arrays (ITypedArray)."
  [data]
  (if (typed-array? data)
    (typed-array-fold-double data unchecked-add-d 0.0)
    (reduce + data)))

(defn sum-of-squares
  "Sum of the squares of each data point.
  Accepts sequences and typed arrays (ITypedArray)."
  [data]
  (let [f (fn ^double [^double s ^double v] (+ s (* v v)))]
    (if (typed-array? data)
      (typed-array-fold-double data f 0.0)
      (reduce f 0.0 data))))

(defn variance*
  "Variance based on subtracting mean.
  Accepts sequences and typed arrays (ITypedArray)."
  ^double [data ^double mean ^long df]
  (let [f (fn ^double [^double a ^double b]
            (+ a (utils/sqr (- b mean))))]
    (/ (if (typed-array? data)
         (typed-array-fold-double data f 0.0)
         (double (reduce f 0.0 data)))
       df)))

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

  Accepts sequences and typed arrays (ITypedArray).

  Ref: Chan et al. Algorithms for computing the sample variance: analysis and
       recommendations. American Statistician (1983)."
  (^double [data] (variance data 1))
  (^double [data ^long df]
   (if (typed-array? data)
     (variance-typed-array data df)
     ;; Uses a single pass, non-pairwise algorithm, without shifting.
     (letfn [(update-estimates [[^double m ^double q ^long k] ^double x]
               (let [kp1   (inc k)
                     delta (- x m)]
                 [(+ m (/ delta kp1))
                  (+ q (/ (* k (utils/sqr delta)) kp1))
                  kp1]))]
       (let [[_ ^double q ^long k] (reduce update-estimates [0.0 0.0 0] data)]
         (when (> k df)
           (/ q (- k df))))))))

(defn median-value
  "Calculate the median value of a sorted data set.
  Returns just the median value (not the lower/upper partitions).
  Accepts sequences and typed arrays (ITypedArray).
  References: http://en.wikipedia.org/wiki/Median"
  ^double [data]
  (let [n (if (typed-array? data)
            (typed-array-length data)
            (count data))
        i (bit-shift-right n 1)
        get-val (if (typed-array? data)
                  typed-array-get-double
                  (fn ^double [data ^long idx] (double (nth data idx))))]
    (if (even? n)
      (/ (+ (get-val data (dec i))
            (get-val data i))
         2.0)
      (get-val data (bit-shift-right n 1)))))

(defn median
  "Calculate the median of a sorted data set.
  Return [median, [vals less than median] [vals greater than median]]
  For typed arrays, returns [median nil nil] (partitions not supported).
  References: http://en.wikipedia.org/wiki/Median"
  [data]
  (if (typed-array? data)
    [(median-value data) nil nil]
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
         (drop (inc i) data)]))))

(defn quartiles
  "Calculate the quartiles of a sorted data set.
   References: http://en.wikipedia.org/wiki/Quartile"
  [data]
  (let [[m lower upper] (median data)]
    [(first (median lower)) m (first (median upper))]))

(defn quantile
  "Calculate the quantile of a sorted data set.
  Accepts sequences and typed arrays (ITypedArray).
  References: http://en.wikipedia.org/wiki/Quantile"
  [^double quantile data]
  (let [n (dec (if (typed-array? data)
                 (typed-array-length data)
                 (count data)))
        get-val (if (typed-array? data)
                  typed-array-get-double
                  (fn ^double [data ^long idx] (double (nth data idx))))
        interp (fn [^double x]
                 (let [f (Math/floor x)
                       i (long f)
                       p (- x f)]
                   (cond
                     (zero? p) (get-val data i)
                     (= 1.0 p) (get-val data (inc i))
                     :else     (+ (* p (get-val data (inc i)))
                                  (* (- 1.0 p) (get-val data i))))))]
    (interp (* quantile n))))

(defn central-moment
  "Compute the r-th central moment: (1/n) * Σ(xᵢ - μ)^r
  Accepts sequences and typed arrays (ITypedArray)."
  ^double [data ^double mean ^long r]
  (let [n (if (typed-array? data)
            (typed-array-length data)
            (count data))
        f (fn ^double [^double acc ^double x]
            (+ acc (Math/pow (- x mean) r)))]
    (/ (if (typed-array? data)
         (typed-array-fold-double data f 0.0)
         (double (reduce f 0.0 data)))
       n)))

(defn- data-count
  "Returns the count of elements in data.
  Accepts sequences and typed arrays (ITypedArray)."
  ^long [data]
  (if (typed-array? data)
    (typed-array-length data)
    (count data)))

(defn skewness
  "Compute sample skewness using one of three methods.

  Type 1: g₁ = m₃ / m₂^(3/2) - typical textbook definition
  Type 2: G₁ = g₁ × √(n(n-1)) / (n-2) - unbiased under normality (SAS/SPSS)
  Type 3: b₁ = g₁ × ((n-1)/n)^(3/2) - used in MINITAB/BMDP

  Where m_r = sample central moment of order r: Σ(xᵢ - μ)^r / n

  Default is type 2 (unbiased under normality).
  Returns 0.0 for constant data (zero variance), since constant data is symmetric.
  Accepts sequences and typed arrays (ITypedArray).

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
  Accepts sequences and typed arrays (ITypedArray).

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
  Accepts sequences and typed arrays (ITypedArray)."
  ^double [data]
  (let [n (data-count data)]
    (if (< n 2)
      Double/NaN
      (let [mu (mean data)]
        (if (zero? mu)
          Double/NaN
          (/ (Math/sqrt (variance data)) mu))))))
