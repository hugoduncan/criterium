(ns criterium.types
  "Type predicates and accessors for benchmarking data structures")

;;; Type predicates

(defn collection-map?
  "Check if x is a collection map with required keys.
  Uses direct key checks to avoid allocation."
  [x]
  (and (map? x)
       (contains? x :eval-count)
       (contains? x :elapsed-time)
       (contains? x :collections)
       (contains? x :num-samples)
       (contains? x :batch-size)
       (contains? x :collector)))

(defn data-entry-map?
  "Check if x is a data entry map with required keys."
  [x]
  (and (map? x)
       (contains? x :type)
       (contains? x :transform)))

(defn collected-metrics-map?
  "Check if x is a collected-metrics map with required keys."
  [x]
  (and (map? x)
       (contains? x :type)
       (contains? x :transform)
       (contains? x :metric->values)
       (contains? x :elapsed-time)
       (contains? x :num-samples)
       (contains? x :batch-size)
       (contains? x :eval-count)
       (contains? x :metrics-defs)
       (contains? x :expr-value)))

(def metrics-samples-keys
  "Required keys for :criterium/metrics-samples type."
  #{:type
    :transform
    :source-id
    :metric->values
    :num-samples
    :batch-size
    :metrics-defs
    :expr-value})

(defn metrics-samples-map?
  "Check if x is a metrics-samples map with :type :criterium/metrics-samples."
  [x]
  (and (map? x)
       (= :criterium/metrics-samples (:type x))
       (contains? x :transform)
       (contains? x :source-id)
       (contains? x :metric->values)
       (contains? x :num-samples)
       (contains? x :batch-size)
       (contains? x :metrics-defs)
       (contains? x :expr-value)))

(def digest-samples-keys
  "Required keys for :criterium/digest type."
  #{:type
    :transform
    :source-id
    :metric->digest
    :metrics-defs
    :expr-value})

(defn digest-samples-map?
  "Check if x is a digest-samples map with :type :criterium/digest."
  [x]
  (and (map? x)
       (= :criterium/digest (:type x))
       (contains? x :transform)
       (contains? x :source-id)
       (contains? x :metric->digest)
       (contains? x :metrics-defs)
       (contains? x :expr-value)))

(defn generic-metrics-samples-map?
  [x]
  (#{:criterium/metrics-samples
     :criterium/collected-metrics-samples}
   (:type x)))

(defn generic-data-map?
  [x]
  (#{:criterium/metrics-samples
     :criterium/collected-metrics-samples
     :criterium/digest}
   (:type x)))

(defn quantiles-map?
  "Check if x is a quantiles map with :type :criterium/quantiles."
  [x]
  (and (map? x)
       (= :criterium/quantiles (:type x))
       (contains? x :quantiles)
       (contains? x :metrics-defs)
       (contains? x :source-id)))

(defn outliers-map?
  "Check if x is an outliers map with :type :criterium/outliers."
  [x]
  (and (map? x)
       (= :criterium/outliers (:type x))
       (contains? x :outliers)
       (contains? x :metrics-defs)
       (contains? x :source-id)
       (contains? x :quantiles-id)
       (contains? x :num-samples)
       (contains? x :transform)))

(defn stats-map?
  "Check if x is a stats map with :type :criterium/stats."
  [x]
  (and (map? x)
       (= :criterium/stats (:type x))
       (contains? x :stats)
       (contains? x :metrics-defs)
       (contains? x :transform)
       (contains? x :batch-size)
       (contains? x :source-id)
       (contains? x :outliers-id)))

(defn event-stats-map?
  "Check if x is an event-stats map with :type :criterium/event-stats."
  [x]
  (and (map? x)
       (= :criterium/event-stats (:type x))
       (contains? x :event-stats)
       (contains? x :metrics-defs)
       (contains? x :transform)
       (contains? x :batch-size)
       (contains? x :source-id)))

(defn histogram-map?
  "Check if x is a histogram map with :type :criterium/histogram."
  [x]
  (and (map? x)
       (= :criterium/histogram (:type x))
       (contains? x :histograms)
       (contains? x :metrics-defs)
       (contains? x :transform)
       (contains? x :batch-size)
       (contains? x :source-id)
       (contains? x :outliers-id)))

(defn outlier-significance-map?
  "Check if x is an outlier-significance map with :type :criterium/outlier-significance."
  [x]
  (and (map? x)
       (= :criterium/outlier-significance (:type x))
       (contains? x :outlier-significance)
       (contains? x :metrics-defs)
       (contains? x :source-id)
       (contains? x :outliers-id)))

(defn bootstrap-map?
  "Check if x is a bootstrap map with :type :criterium/bootstrap."
  [x]
  (and (map? x)
       (= :criterium/bootstrap (:type x))
       (contains? x :bootstrap)
       (contains? x :metrics-defs)
       (contains? x :transform)
       (contains? x :batch-size)
       (contains? x :source-id)))

(defn result-map?
  [x]
  (and (map? x)
       (every? data-entry-map? (vals x))))

(defn benchmark-map?
  [x]
  (and (map? x)
       (result-map? (:data x))))

;;; Allocation trace types

(defn allocation-trace?
  "Check if x is an allocation trace map with :type :criterium/allocation-trace."
  [x]
  (and (map? x)
       (= :criterium/allocation-trace (:type x))
       (contains? x :records)
       (contains? x :thread-id)
       (contains? x :eval-count)
       (contains? x :elapsed-time)))
