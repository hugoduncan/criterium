(ns criterium.trigger
  "Provide a trigger for collecting elapsed time samples between trigger events.

  The trigger maintains internal state about when it was last triggered
  and collects samples of elapsed time between trigger
  events.

  Typical usage:

  (let [t (trigger)]
    (fire! t)          ;; Start timing
    (do-something)
    (fire! t)          ;; Record elapsed time
    (do-something-else)
    (fire! t)          ;; Record another sample
    (let [samples (samples! t)] ;; Get samples and reset
      (analyze-samples samples)))"
  (:require
   [criterium.trigger.impl :as impl]))

(defn trigger
  "Creates and returns a new trigger sampler.

  The returned trigger maintains state about:
  - When it was last triggered (timestamp)
  - A collection of timing samples between trigger events

  The trigger starts with no samples and must be fired at least twice
  to collect timing data."
  []
  (volatile! (impl/trigger-data)))

(defn fire!
  "Records a timing event in the trigger sampler.

  When fired:
  - Records current timestamp
  - If this isn't the first firing, calculates elapsed time since last firing
  - Adds the elapsed time and any extra-data as a new sample

  The first firing only records the initial timestamp - no sample is collected
  until the second firing.

  Parameters:
    trigger    - The trigger instance to fire
    extra-data - Optional map of additional data to attach to the sample"
  ([trigger]
   (fire! trigger nil))
  ([trigger extra-data]
   (vswap! trigger impl/update-data extra-data)))

(defn samples!
  "Returns collected samples and resets the trigger.

  Returns a map containing:
  - :batch-size      - Always 1 for triggers
  - :eval-count      - Number of samples collected
  - :metrics-configs - Configuration of metrics collected
  - :samples         - Map of collected metrics, including :elapsed-time

  The trigger is reset to its initial empty state after calling this function.

  Parameters:
    trigger - The trigger instance to get samples from"
  [trigger]
  (let [samples (:samples @trigger)]
    (vreset! trigger (impl/trigger-data))
    (impl/samples->samples-map samples)))
