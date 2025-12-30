(ns stats.histogram
  "Histogram computation utilities with multiple binning methods.

  Supports:
  - :freedman-diaconis (default) - Uses IQR-based bin width calculation
  - :knuth - Bayesian optimal bin count selection"
  (:require
   [clojure.math :as math]
   [stats.knuth :as knuth]))

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

(defn- generate-bins-for-count
  "Generate bin edges and centers for a specific number of bins."
  [^double min-val ^double max-val ^long num-bins]
  (let [data-range (- max-val min-val)
        width      (/ data-range (double num-bins))
        edges      (mapv #(+ min-val (* (double %) width))
                         (range (inc num-bins)))
        centers    (mapv #(+ min-val (* (+ (double %) 0.5) width))
                         (range num-bins))]
    {:edges    edges
     :centers  centers
     :width    width
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

(defn- histogram-freedman-diaconis
  "Compute histogram using Freedman-Diaconis binning rule."
  [values min-val max-val {:keys [iqr]}]
  (let [iqr       (or iqr (compute-iqr values))
        bin-width (compute-bin-width values iqr)
        {:keys [edges centers width num-bins]}
        (generate-bins min-val max-val bin-width)
        counts  (count-values-in-bins values edges)
        n       (count values)
        density (compute-density counts n)]
    {:type     :criterium/histogram-fixed-width
     :counts   counts
     :centers  centers
     :width    width
     :density  density
     :n        n
     :num-bins num-bins
     :min      min-val
     :max      max-val}))

(defn- histogram-knuth
  "Compute histogram using Knuth's Bayesian optimal binning."
  [values min-val max-val {:keys [max-bins] :or {max-bins 50}}]
  (let [{:keys [optimal-bins log-posterior]}
        (knuth/optimal-bins values {:max-bins max-bins :min min-val :max max-val})
        {:keys [edges centers width num-bins]}
        (generate-bins-for-count min-val max-val optimal-bins)
        counts  (count-values-in-bins values edges)
        n       (count values)
        density (compute-density counts n)]
    {:type          :criterium/histogram-knuth
     :counts        counts
     :centers       centers
     :width         width
     :density       density
     :n             n
     :num-bins      num-bins
     :min           min-val
     :max           max-val
     :optimal-bins  optimal-bins
     :log-posterior log-posterior}))

(defn histogram
  "Compute histogram from vector of numeric values.

  Supports multiple binning methods via the :method option:
  - :freedman-diaconis (default) - Uses IQR-based bin width calculation
  - :knuth - Bayesian optimal bin count selection

  Options:
    :method   - Binning method (:freedman-diaconis or :knuth)
    :iqr      - Pre-computed IQR (only for :freedman-diaconis)
    :max-bins - Maximum bins to search (only for :knuth, default 50)

  Returns map containing:
    :type     - :criterium/histogram-fixed-width or :criterium/histogram-knuth
    :counts   - vector of bin counts
    :centers  - vector of bin centers
    :width    - bin width
    :density  - vector of probability density values
    :n        - total number of samples
    :num-bins - number of bins
    :min      - minimum value
    :max      - maximum value

  Additional keys for :knuth method:
    :optimal-bins  - optimal bin count M
    :log-posterior - log-posterior value at optimal M

  For backward compatibility, second argument can be a number (pre-computed IQR).

  Throws:
    ex-info {:error :histogram/no-values} for empty input
    ex-info {:error :histogram/same-values} when all values are the same"
  ([values]
   (histogram values {}))
  ([values opts-or-iqr]
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
     ;; Handle backward compatibility: number means IQR
     (let [opts   (if (number? opts-or-iqr)
                    {:iqr opts-or-iqr}
                    (or opts-or-iqr {}))
           method (get opts :method :freedman-diaconis)]
       (case method
         :freedman-diaconis (histogram-freedman-diaconis values min-val max-val opts)
         :knuth             (histogram-knuth values min-val max-val opts)
         (throw (ex-info
                 (str "Unknown histogram method: " method)
                 {:error  :histogram/unknown-method
                  :method method})))))))
