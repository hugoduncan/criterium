(ns criterium.trigger
  "Provide a trigger, that collects timings of the period between two triggers."
  (:require
   [criterium.collect :as collect]
   [criterium.collector :as collector]
   [criterium.trigger.impl :as impl]))

(defn trigger
  "Return a new trigger sampler."
  []
  (volatile! (impl/trigger-data)))

(defn fire!
  "Fire a trigger sampler, recording a sample."
  ([trigger]
   (fire! trigger nil))
  ([trigger extra-data]
   (vswap! trigger impl/update-data extra-data)))

(defn samples!
  "Return a sampled map, with the samples from the trigger
  Resets the trigger sampler to be empty."
  [trigger]
  (let [samples   (:samples @trigger)
        collector (collector/collector
                   {:terminator :elapsed-time})]
    (vreset! trigger (impl/trigger-data))
    {:batch-size      1
     :eval-count      (count samples)
     :metrics-configs (:metrics-configs collector)
     :samples         (with-meta
                        ((collect/sample-maps->map-of-samples
                          (:metrics-configs collector))
                         samples)
                        {:transform {:sample-> identity :->sample identity}})}))
