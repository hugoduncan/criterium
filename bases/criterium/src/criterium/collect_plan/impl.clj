(ns criterium.collect-plan.impl
  (:require
   [criterium.util.units :as units]))

(defmulti required-stages*
  "Pipeline stages required for the given schema-type"
  (fn [scheme-type] scheme-type))

(defmulti collect*
  (fn [collect-plan _pipeline _measured]
    (:scheme-type collect-plan)))

(defn limit-samples
  "Determine sample counts, limiting if projected time exceeds remaining time.
  Returns a map with :num-warmup-samples, :num-measure-samples, and when
  time-limited, :time-limited? true and :projected-time-ns."
  [limit-time-ns
   num-warmup-samples
   num-measure-samples
   taken-time
   remaining-time
   projected-time]
  (if (> (long projected-time) (long remaining-time))
    (let [t (unchecked-add (long projected-time) (long taken-time))
          t-s (double (/ t (long units/SEC-NS)))
          frac (/ (double remaining-time) (double projected-time))]
      (println
       (format
        "Estimated time required for full JIT is %.3gs, but limited to %.3gs."
        t-s
        (double (/ (long limit-time-ns) (long units/SEC-NS)))))
      (println (format "  pass `:limit-time-s %.3g` to improve accuracy," t-s))
      (println "  or consider benchmarks at a lower level.")
      {:num-warmup-samples (max 10 (long (* (long num-warmup-samples) frac)))
       :num-measure-samples (max 10 (long (* (long num-measure-samples) frac)))
       :time-limited? true
       :projected-time-ns t})
    {:num-warmup-samples (max 1 (long num-warmup-samples))
     :num-measure-samples (max 1 (long num-measure-samples))}))
