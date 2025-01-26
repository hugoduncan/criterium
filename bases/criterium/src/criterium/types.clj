(ns criterium.types
  "Type predicates and accessors for benchmarking data structures"
  (:require
   [clojure.set :as set]
   [criterium.util.invariant :as invariant :refer [have?]]))

;;; Type predicates

(defn collection-map?
  "Check if x is a collection map with required keys."
  [x]
  (and (map? x)
       (set/subset?
        #{:eval-count
          :elapsed-time
          :collections
          :num-samples
          :batch-size
          :collector}
        (set (keys x)))))

(defn data-entry-map?
  [x]
  (and (map? x)
       (set/subset? #{:type :transform} (set (keys x)))))

(defn collected-metrics-map?
  [x]
  (and (map? x)
       (set/subset?
        #{:type
          :transform
          :metric->values
          :elapsed-time
          :num-samples
          :batch-size
          :eval-count
          :metrics-defs
          :expr-value}
        (set (keys x)))))

(def metrics-samples-keys
  #{:type
    :transform
    :source-id
    :metric->values
    :num-samples
    :batch-size
    :metrics-defs
    :expr-value })


(defn metrics-samples-map?
  [x]
  (and (map? x)
       (or
        (= :criterium/metrics-samples (:type x))
        (throw
         (invariant/assertion-error
          "Invalid tupe"
          {:error-tupe ::invalid-type
           :date       {:expected :criterium/metrics-samples
                        :actual   (:type x)}})))
       (or
        (set/subset? metrics-samples-keys (set (keys x)))
        (throw
         (invariant/assertion-error
          "Invalid keys"
          {:error-tupe ::invalid-map-keys
           :date       {:expected metrics-samples-keys
                        :actual   (keys x)
                        :missing  (set/difference
                                   metrics-samples-keys
                                   (set (keys x)))}})))))

(def digest-samples-keys
  #{:type
    :transform
    :source-id
    :metric->digest
    :num-samples
    :batch-size
    :metrics-defs
    :expr-value })

(defn digest-samples-map?
  [x]
  (and (map? x)
       (or
        (= :criterium/digest (:type x))
        (throw
         (invariant/assertion-error
          "Invalid tupe"
          {:error-tupe ::invalid-type
           :date       {:expected :criterium/digest
                        :actual   (:type x)}})))
       (or
        (set/subset? digest-samples-keys (set (keys x)))
        (throw
         (invariant/assertion-error
          "Invalid keys"
          {:error-tupe ::invalid-map-keys
           :date       {:expected digest-samples-keys
                        :actual   (keys x)
                        :missing  (set/difference
                                   digest-samples-keys
                                   (set (keys x)))}})))))

(defn generic-metrics-samples-map?
  [x]
  (#{:criterium/metrics-samples
     :criterium/collected-metrics-samples
     :criterium/digest}
   (:type x)))

(def quantiles-map-keys #{:type :quantiles :metrics-defs :source-id})

(defn quantiles-map?
  [x]
  (and (map? x)
       (= :criterium/quantiles (:type x))
       (set/subset? quantiles-map-keys (set (keys x)))))

(def outliers-map-keys
  #{:type :outliers :metrics-defs :source-id :quantiles-id :num-samples
    :transform})

(defn outliers-map?
  [x]
  (and (map? x)
       (= :criterium/outliers (:type x))
       (or
        (set/subset? outliers-map-keys (set (keys x)))
        (throw
         (invariant/assertion-error
          "Invalid keys"
          {:error-tupe ::invalid-map-keys
           :date       {:expected outliers-map-keys
                        :actual   (keys x)
                        :missing  (set/difference
                                   outliers-map-keys
                                   (set (keys x)))}})))))

(def stats-map-keys
  #{:type :stats :metrics-defs :transform :batch-size :source-id
    :outliers-id})

(defn stats-map?
  [x]
  (and (map? x)
       (= :criterium/stats (:type x))
       (set/subset? stats-map-keys (set (keys x)))))

(def event-stats-map-keys
  #{:type :event-stats :metrics-defs :transform :batch-size :source-id})

(defn event-stats-map?
  [x]
  (and (map? x)
       (= :criterium/event-stats (:type x))
       (set/subset? event-stats-map-keys (set (keys x)))))

(def outlier-significance-map-keys
  #{:type :outlier-significance :metrics-defs :source-id :outliers-id})

(defn outlier-significance-map?
  [x]
  (and (map? x)
       (= :criterium/outlier-significance (:type x))
       (set/subset? outlier-significance-map-keys (set (keys x)))))

(def bootstrap-map-keys
  #{:type :bootstrap :metrics-defs :transform :batch-size :source-id})

(defn bootstrap-map?
  [x]
  (and (map? x)
       (= :criterium/bootstrap (:type x))
       (set/subset? bootstrap-map-keys (set (keys x)))))

(defn result-map?
  [x]
  (and (map? x)
       (every? data-entry-map? (vals x))))

(defn benchmark-map?
  [x]
  (and (map? x)
       (result-map? (:data x))))
