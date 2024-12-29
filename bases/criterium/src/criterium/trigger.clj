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
    (let [samples (sampler/samples-map t)] ;; Get samples and reset
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
  (impl/trigger))

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
   (impl/fire! trigger nil))
  ([trigger extra-data]
   (impl/fire! trigger extra-data)))
