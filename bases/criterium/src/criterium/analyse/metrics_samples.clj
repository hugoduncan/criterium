(ns criterium.analyse.metrics-samples
  (:require
   [criterium.analyse.methods :as methods]
   [criterium.collect-plan :as collect-plan]
   [criterium.types :as types]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]))

(defmethod methods/transform :criterium/metrics-samples
  [metrics-samples metric-configs f inv-f options]
  (let [metric->values  (util/metric->values metrics-samples)
        metric->values' (reduce
                         (fn x-path [result path]
                           (assoc
                            result
                            path
                            (mapv f (metric->values path))))
                         {}
                         (mapv :path metric-configs))]
    (->
     (select-keys
      metrics-samples
      types/metrics-samples-keys)
     (merge
      {:metric->values metric->values'
       :transform      {:sample-> inv-f :->sample f}}))))

(defmethod methods/quantiles :criterium/metrics-samples
  [metrics-samples metric-configs options]
  (let [quantiles (sampled-stats/quantiles
                   (util/metric->values metrics-samples)
                   metric-configs
                   options)]
    {:type      :criterium/quantiles
     :quantiles quantiles
     :transform collect-plan/identity-transforms}))

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  {:low-severe  low-severe
   :low-mild    low-mild
   :high-mild   high-mild
   :high-severe high-severe})

(defn classifier
  [[^double low-severe ^double low-mild ^double high-mild ^double high-severe]]
  (fn [^double x i]
    (when-not (<= low-mild x high-mild)
      [i (cond
           (<= x low-severe)           :low-severe
           (< low-severe x low-mild)   :low-mild
           (> high-severe x high-mild) :high-mild
           (>= x high-severe)          :high-severe)])))

(defn samples-outliers [metric-configs all-quantiles samples]
  (reduce
   (fn sample-m [result metric-config]
     (let [path           (:path metric-config)
           quantiles      (have map? (get-in all-quantiles path)
                                {:all-quantiles all-quantiles})
           thresholds     (stats/boxplot-outlier-thresholds
                           (get quantiles 0.25)
                           (get quantiles 0.75))
           classifier     (classifier thresholds)
           outliers       (when (apply not= thresholds)
                            (into {}
                                  (mapv classifier
                                        (get samples path)
                                        (range))))
           outlier-counts (reduce-kv
                           (fn [counts _i v]
                             (update counts v inc))
                           (outlier-count 0 0 0 0)
                           outliers)]
       (update-in result path
                  assoc
                  :thresholds thresholds
                  :outliers outliers
                  :outlier-counts outlier-counts)))
   {}
   metric-configs))

(defmethod methods/outliers :criterium/metrics-samples
  [metrics-samples all-quantiles metric-configs _options]
  {:have [(have? types/digest-samples-map? metrics-samples)]}
  (let [outliers (samples-outliers
                  metric-configs
                  (util/quantiles all-quantiles)
                  (util/metric->values metrics-samples))]
    {:type        :criterium/outliers
     :outliers    outliers
     :num-samples (:num-samples metrics-samples)
     :transform   collect-plan/identity-transforms}))

(defmethod methods/stats :criterium/metrics-samples
  [metrics-samples outliers metric-configs options]
  {:have [(have? types/digest-samples-map? metrics-samples)]}
  (let [metric->values (util/metric->values metrics-samples)
        stats          (sampled-stats/sample-stats
                        metric->values
                        (when outliers (util/outliers outliers))
                        metric-configs
                        options)]
    {:type      :criterium/stats
     :stats     stats
     :transform collect-plan/identity-transforms}))

(defmethod methods/event-stats :criterium/metrics-samples
  [metrics-samples metrics-defs _options]
  {:have [(have? types/digest-samples-map? metrics-samples)]}
  (let [metric->values (util/metric->values metrics-samples)
        event-stats    (sampled-stats/event-stats
                        metrics-defs
                        metric->values)]
    {:type        :criterium/event-stats
     :event-stats event-stats
     :batch-size  (:batch-size metrics-samples)
     :transform   collect-plan/identity-transforms}))
