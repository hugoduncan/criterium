(ns criterium.util.histogram
  "Histogram computation utilities using Freedman-Diaconis rule for binning."
  (:require
   [clojure.math :as math]))

(defn- quartiles
  "Calculate quartiles Q1 and Q3 from sorted data.
   Returns [q1 q3]"
  [^doubles sorted-data]
  (let [n (alength sorted-data)
        q1-idx (quot n 4)
        q3-idx (quot (* 3 n) 4)]
    [(aget sorted-data q1-idx)
     (aget sorted-data q3-idx)]))

(defn- compute-iqr
  "Compute Interquartile Range (IQR) from vector of values"
  ^double [values]
  (let [sorted  (double-array (sort values))
        [q1 q3] (quartiles sorted)]
    (- (double q3) (double q1))))

(def ^:private ^:const minus-one-third
  (/ -1.0 3.0))

(defn- compute-bin-width
  "Compute bin width using Freedman-Diaconis rule:
   width = 2 * IQR * n^(-1/3)"
  [values ^double iqr]
  (let [n (count values)]
    (* 2.0 iqr (math/pow n minus-one-third))))

(defn- generate-bins
  "Generate bin edges and centers based on data range and bin width"
  [^double min-val ^double max-val ^double bin-width]
  (let [data-range     (- max-val  min-val)
        ;; Ensure at least 2 bins for distinct values
        num-bins       (max 2 (int (math/ceil (/ data-range bin-width))))
        adjusted-width (/ data-range num-bins) ; Adjust to exactly cover range
        edges          (mapv #(+ min-val (* (double %) adjusted-width))
                             (range (inc num-bins)))
        centers        (mapv #(+ min-val (* (+ (double %) 0.5) adjusted-width))
                             (range num-bins))]
    {:edges    edges
     :centers  centers
     :width    adjusted-width
     :num-bins num-bins}))

(defn- count-values-in-bins
  "Count number of values falling into each bin"
  [values edges]
  (let [bins     (int-array (dec (count edges)))
        last-idx (dec (alength bins))]
    (doseq [v values]
      (loop [idx 0]
        (when (< idx (count bins))
          (let [v     (double v)
                lower (double (nth edges idx))
                upper (double  (nth edges (inc idx)))]
            (if (or (and (<= lower v) (< v upper))
                    (and (= idx last-idx) (<= lower v) (<= v upper)))
              (aset bins idx (inc (aget bins idx)))
              (when (< idx last-idx)
                (recur (inc idx))))))))
    (vec bins)))

(defn- compute-density
  "Compute probability density for each bin"
  [counts ^long total-samples]
  (mapv #(double (/ (long %) total-samples)) counts))

(defn histogram
  "Compute histogram from vector of numeric values using Freedman-Diaconis rule.
   Optional pre-computed IQR can be provided.
   Returns map containing:
   - :counts - vector of bin counts
   - :centers - vector of bin centers
   - :width - bin width
   - :density - vector of probability density values
   - :n - total number of samples
   - :min - minimum value
   - :max - maximum value

   Throws:
   - ex-info {:error :histogram/no-values} for empty input
   - ex-info {:error :histogram/same-values} when all values are the same"
  ([values]
   (histogram values nil))
  ([values precomputed-iqr]
   (when (empty? values)
     (throw (ex-info
             "Input vector cannot be empty"
             {:error :histogram/no-values})))
   (let [min-val (reduce min values)
         max-val (reduce max values)]
     (when (= min-val max-val)
       (throw (ex-info
               "All values are the same - cannot create histogram"
               {:error   :histogram/same-values
                :min-val min-val
                :max-val max-val})))
     (let [iqr       (or precomputed-iqr (compute-iqr values))
           bin-width (compute-bin-width values iqr)
           {:keys [edges centers width num-bins]}
           (generate-bins min-val max-val bin-width)
           counts    (count-values-in-bins values edges)
           n         (count values)
           density   (compute-density counts n)]
       {:type     :criterium/histogram-fixed-width
        :counts   counts
        :centers  centers
        :width    width
        :density  density
        :n        n
        :num-bins num-bins
        :min      min-val
        :max      max-val}))))
