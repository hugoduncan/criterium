(ns criterium.trigger.impl
  (:require
   [criterium.collect :as collect]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.sampler :as sampler]))

(defn samples->samples-map
  [samples]
  (let [collector (collector/collector
                   {:terminator :elapsed-time})]
    {:type           :criterium/metrics-samples
     :batch-size     1
     :eval-count     (count samples)
     :metrics-defs   (:metrics-defs collector)
     :metric->values (collect/sample-maps->map-of-samples
                      samples
                      (:metrics-defs collector))
     :transform      collect-plan/identity-transforms
     :num-samples    (count samples)
     :source-id      nil
     :expr-value     nil}))

(defrecord TriggerData
  [^long last-triggered
   samples])

(defrecord Trigger
  [state]

  sampler/Sampler
  (samples-map [_]
               (samples->samples-map (:samples @state)))
  (reset-samples! [_]
                  (reset! state (->TriggerData 0 []))
                  nil))

(defn fire! [^Trigger trigger extra-data]
  (swap! (:state trigger)
         (fn [^TriggerData trigger-data]
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
                  extra-data))))))))

(defn trigger []
  (->Trigger (atom (->TriggerData 0 []))))
