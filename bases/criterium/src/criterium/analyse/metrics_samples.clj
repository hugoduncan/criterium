(ns criterium.analyse.metrics-samples
  (:require
   [criterium.analyse.methods :as methods]
   [criterium.types :as types]
   [criterium.util.helpers :as util]))

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
