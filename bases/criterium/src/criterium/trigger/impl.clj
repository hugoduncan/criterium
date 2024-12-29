(ns criterium.trigger.impl
  (:require
   [criterium.collect :as collect]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have?]]))

(defrecord TriggerData
  [^long last-triggered
   samples])

(defn update-data [^TriggerData trigger-data extra-data]
  (let [prev-time (.last-triggered trigger-data)
        this-time (jvm/timestamp)]
    (->TriggerData
     this-time
     (if (zero? prev-time)
       (:samples trigger-data)
       (conj
        (:samples trigger-data)
        (merge
         {:elapsed-time (unchecked-subtract this-time prev-time)}
         extra-data))))))

(defn trigger-data []
  (->TriggerData 0 []))

(defn samples->samples-map
  [samples]
  {:post [(have? util/metrics-samples-map? %)]}
  (let [collector (collector/collector
                   {:terminator :elapsed-time})]
    {:type           :criterium/metrics-samples
     :batch-size     1
     :eval-count     (count samples)
     :metrics-defs   (:metrics-configs collector)
     :metric->values (collect/sample-maps->map-of-samples
                      samples
                      (:metrics-configs collector))
     :transform      collect-plan/identity-transforms
     :num-samples    (count samples)
     :source-id      nil
     :expr-value     nil}))
