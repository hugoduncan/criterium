(ns criterium.trigger.impl
  (:require
   [criterium.jvm :as jvm]))

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
