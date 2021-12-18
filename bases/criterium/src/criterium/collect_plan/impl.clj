(ns criterium.collect-plan.impl
  (:require
   [criterium.util.units :as units]))

(defmulti required-stages*
  "Pipeline stages required for the given schema-type"
  (fn [{:keys [scheme-type] :as _collect-plan}]
    scheme-type))

(defmulti collect*
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [collect-plan metrics-configs pipeline measured]
    (:scheme-type collect-plan)))

(defn limit-samples
  [limit-time-ns
   num-warmup-samples
   num-measure-samples
   taken-time
   remaining-time
   projected-time]
  (if (> (long projected-time) (long remaining-time))
    (let [t    (unchecked-add (long projected-time) (long taken-time))
          t-s  (double (/ t (long units/SEC-NS)))
          frac (/ (double remaining-time) (double projected-time))]
      (println
       (format
        "Estimated time required for full JIT is %.3gs, but limited to %.3gs."
        t-s
        (double (/ (long limit-time-ns) (long units/SEC-NS)))))
      (println (format "  pass `:limit-time-s %.3g` to improve accuracy," t-s))
      (println "  or consider benchmarks at a lower level.")
      [(max 10 (long (* (long num-warmup-samples) frac)))
       (max 10 (long (* (long num-measure-samples) frac)))])
    [num-warmup-samples
     num-measure-samples]))
