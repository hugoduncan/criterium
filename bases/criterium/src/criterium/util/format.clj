(ns criterium.util.format
  "Metric formatters")

(defmulti scale
  "Scale value with given dimensions keyword.
  Return a [scale units] tuple.
  scale is a multiplicative factor. units is a string."
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [dimension value] dimension))

(defmethod scale :default
  [_ _value]
  [1 ""])

(defmethod scale :time                  ; seconds
  [_ value]
  (cond
    (> (double value) 60)   [(/ 60) "min"]
    (< (double value) 1e-6) [1e9 "ns"]
    (< (double value) 1e-3) [1e6 "µs"]
    (< (double value) 1)    [1e3 "ms"]
    :else                   [1 "s"]))

(def ^:const ONE-KB 1024)
(def ^:const ONE-MB (* 1024 1024))
(def ^:const ONE-GB (* 1024 1024 1024))

(defmethod scale :memory
  [_ value]
  (cond
    (< (long value) ONE-KB) [1 "bytes"]
    (< (long value) ONE-MB) [(/ ONE-KB) "Kb"]
    (< (long value) ONE-GB) [(/ ONE-MB) "Mb"]
    :else                   [(/ ONE-GB) "Gb"]))

(defn format-scaled
  ([value scale]
   (format "%3.3g" (double (* (double scale) (double value)))))
  ([value scale unit]
   (format "%3.3g %s" (double (* (double scale) (double value))) unit)))

(defmulti format-value*
  "Format value to 3 significant figures in an appropriate unit for the scale."
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [dimension value opts] dimension))

(defn format-value
  "Format in an appropriate unit and precision for the scale."
  ([dimension value]
   (format-value* dimension value {:sf 3}))
  ([dimension value opts]
   (format-value* dimension value (merge {:sf 3} opts))))

(defmethod format-value* :default
  [_ value opts]
  (format "%s" (str value)))

(defn- double-format-str [sf]
  (case (long (or sf 0))
    3 "%3.3g"
    4 "%4.4g"
    "%3.3g"))

(defmethod format-value* :time
  [dimension value opts]
  (let [[scale unit] (scale dimension value)]
    (format
     (str (double-format-str (:sf opts)) " %s")
     (double (* (double scale) (double value)))
     unit)))

(defmethod format-value* :memory
  [dimension value opts]
  (let [[scale unit] (scale dimension value)]
    (format
     (str (double-format-str (:sf opts)) " %s")
     (double (* (double scale) (double value)))
     unit)))

(defmulti format-metric
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [metric val] metric))

(defmethod format-metric :elapsed-time
  [_ val]
  (let [v (/ (double val) 1e9)]
    (format "%32s: %s\n" "Elapsed time" (format-value :time v))))

(defn- format-count-time [[k {c :count t :time}]]
  (format "%36s:  count %d  time %s\n" (name k) c (format-value :time t)))

(defmethod format-metric :garbage-collector
  [_ val]
  (format "%32s:\n%s" "Garbage collection"
          (apply str (map format-count-time val))))

(defmethod format-metric :finalization
  [_ val]
  (format "%32s: %d\n" "Pending finalisations" (:pending val)))

(defn- format-memory-metrics [[k vs]]
  (apply
   str
   (format "%36s:\n" (name k))
   (for [[mk v] vs]
     (format "%40s: %s\n" (name mk) (format-value :memory v)))))

(defmethod format-metric :memory
  [_ val]
  (format "%32s:\n%s" "Memory"
          (apply str (map format-memory-metrics val))))

(defn- format-runtime-memory-metrics [[k v]]
  (format "%36s: %s\n" (name k) (format-value :memory v)))

(defmethod format-metric :runtime-memory
  [_ val]
  (format "%32s:\n%s" "Runtime Memory"
          (apply str (map format-runtime-memory-metrics val))))

(defmethod format-metric :compilation
  [_ val]
  (let [v (:time-ms val)]
    (format "%32s: %s\n" "JIT Compilation time" (format-value :time-ms v))))

(defn format-count [[k v]]
  (format "%36s: %d\n" (name k) v))

(defmethod format-metric :class-loader
  [_ val]
  (apply
   str
   (format "%32s:\n" "Classloader")
   (map format-count val)))

(defmethod format-metric :state
  [_ _]
  "")

(defmethod format-metric :expr-value
  [_ _]
  "")

(defmethod format-metric :num-evals
  [_ _]
  "")
