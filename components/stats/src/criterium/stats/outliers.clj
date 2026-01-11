(ns criterium.stats.outliers
  "Outlier detection using boxplot thresholds.

  Provides both standard symmetric boxplot and adjusted boxplot for
  skewed distributions using the medcouple statistic.

  Functions accept both sequences and typed arrays (ITypedArray)."
  (:require
   criterium.array.interface
   [criterium.stats.core :as core])
  (:import
   [criterium.array.interface ITypedArray IIndexed]))

(defn boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles.
  Returns [severe-low mild-low mild-high severe-high]."
  [^double q1 ^double q3]
  {:pre [(number? q1) (number? q3)]}
  (let [iqr    (- q3 q1)
        severe (* iqr 3.0)
        mild   (* iqr 1.5)]
    [(- q1 severe)
     (- q1 mild)
     (+ q3 mild)
     (+ q3 severe)]))

(defn adjusted-boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles adjusted for skewness.
  Returns [low-severe low-mild high-mild high-severe].

  Uses the adjusted boxplot method from Hubert & Vandervieren (2008)
  which accounts for skewness via the medcouple statistic.

  When mc = 0 (symmetric), reduces to standard boxplot thresholds.
  When mc > 0 (right-skewed), upper fence widens, lower fence narrows.
  When mc < 0 (left-skewed), lower fence widens, upper fence narrows."
  [^double q1 ^double q3 ^double mc]
  {:pre [(number? q1) (number? q3) (number? mc)]}
  (let [iqr (- q3 q1)
        a   -4.0
        b   3.0
        [^double lower-exp ^double upper-exp]
        (if (>= mc 0.0)
          [(Math/exp (* a mc)) (Math/exp (* b mc))]
          [(Math/exp (* (- b) mc)) (Math/exp (* (- a) mc))])
        mild-lower   (- q1 (* 1.5 lower-exp iqr))
        mild-upper   (+ q3 (* 1.5 upper-exp iqr))
        severe-lower (- q1 (* 3.0 lower-exp iqr))
        severe-upper (+ q3 (* 3.0 upper-exp iqr))]
    [severe-lower mild-lower mild-upper severe-upper]))

(defn medcouple-kernel
  "Compute the medcouple kernel h(x_i, x_j).
  For values not both at the median:
    h = ((x_j - median) - (median - x_i)) / (x_j - x_i)
  For values both at the median, returns 0."
  ^double [^double xi ^double xj ^double med]
  (let [diff (- xj xi)]
    (if (< (Math/abs diff) 1e-15)
      0.0
      (/ (- (- xj med) (- med xi)) diff))))

(defn- typed-array?
  "Returns true if data is a typed array."
  [data]
  (instance? ITypedArray data))

(defn- typed-array-length
  "Returns the length of a typed array."
  ^long [^ITypedArray arr]
  (.length arr))

(defn- typed-array-get-double
  "Get element at index as double from typed array."
  ^double [^IIndexed arr ^long index]
  (.getDouble arr index))

(defn- data-length
  "Returns the count of elements in data."
  ^long [data]
  (if (typed-array? data)
    (typed-array-length data)
    (count data)))

(defn- data-get-double
  "Get element at index as double."
  ^double [data ^long index]
  (if (typed-array? data)
    (typed-array-get-double data index)
    (double (nth data index))))

(defn medcouple
  "Compute the medcouple, a robust measure of skewness.
  Returns a value in [-1, 1] where positive indicates right-skew
  and negative indicates left-skew.

  The medcouple is the median of the kernel function h(x_i, x_j) evaluated
  over all pairs where x_i ≤ median ≤ x_j.

  Uses the naive O(n²) algorithm. For criterium's typical sample sizes
  (hundreds to low thousands), this is acceptable.

  Accepts sequences and typed arrays (ITypedArray).
  Takes sorted data as input. Returns 0.0 for constant data or n < 3."
  ^double [sorted-data]
  (let [n (data-length sorted-data)]
    (if (< n 3)
      0.0
      (let [med       (double (core/median-value sorted-data))
            first-val (data-get-double sorted-data 0)
            last-val  (data-get-double sorted-data (dec n))]
        (if (== first-val last-val)
          0.0
          (let [h-values (java.util.ArrayList.)]
            (dotimes [i n]
              (let [xi (data-get-double sorted-data i)]
                (when (<= xi med)
                  (loop [j i]
                    (when (< j n)
                      (let [xj (data-get-double sorted-data j)]
                        (when (>= xj med)
                          (.add h-values (medcouple-kernel xi xj med)))
                        (recur (inc j))))))))
            (let [h-vec (vec h-values)
                  h-sorted (sort h-vec)]
              (first (core/median h-sorted)))))))))
