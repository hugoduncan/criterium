(ns criterium.analyse.digest-samples
  (:require
   [criterium.analyse.methods :as methods]
   [criterium.types :as types]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have?]]
   [criterium.util.t-digest :as t-digest]))

(defmethod methods/transform :criterium/digest
  [digest-samples metric-configs f inv-f options]
  {:have [(have? types/digest-samples-map? digest-samples)]}
  (let [metric->digest  (util/metric->digest digest-samples)
        metric->digest' (reduce
                         (fn x-path [result path]
                           (assoc
                            result
                            path
                            (t-digest/transform (metric->digest path) f)))
                         {}
                         (mapv :path metric-configs))]
    (->
     (select-keys
      digest-samples
      types/digest-samples-keys)
     (merge
      {:metric->digest metric->digest'
       :transform      {:sample-> inv-f :->sample f}}))))
