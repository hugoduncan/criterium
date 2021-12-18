(ns criterium.bench.config
  (:require
   [clojure.set :as set]
   [criterium.benchmarks :as benchmarks]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.util.helpers :as util]
   [criterium.util.units :as units]
   [criterium.viewer.print]))

(def default-config
  "Default options for criterium.measure."
  {:benchmark        benchmarks/minimal-stats-summary
   :collector-config {:stages     []
                      :terminator (collector/maybe-var-get-stage
                                   :elapsed-time)}
   :return-value     [:expr-value]
   :collect-plan     (collect-plan-config/full-collect-plan {})
   :verbose          false
   :viewer           :print})

(defn expand-options
  "Convert option arguments into a criterium config map.
  The config map specifies how criterium will execute."
  [options-map]
  (-> (util/deep-merge
       (merge
        default-config
        (select-keys options-map [:collect-plan]))
       options-map)
      collect-plan-config/ensure-pipeline-stages))

(defn config-map
  "Convert option arguments into a criterium configuration map.
  The config map specifies how criterium will execute."
  [options-map]
  (let [stages       (mapv
                      collector/maybe-var-get-stage
                      (:metric-ids options-map))
        terminator   (filterv collector/terminal? stages)
        pipeline-fns (remove collector/terminal? stages)
        scheme-type  (:collect-plan options-map :with-jit-warmup)
        limit-time-s (:limit-time-s options-map)
        benchmark    (:benchmark options-map)
        unknown-keys (set/difference
                      (set (keys options-map))
                      #{:limit-time-s
                        :metric-ids
                        :return-value
                        :collect-plan
                        :benchmark
                        :verbose
                        :viewer})]

    (when (seq unknown-keys)
      (throw (ex-info "Unknown options" {:options unknown-keys})))
    (when (> (count terminator) 1)
      (throw (ex-info
              "More than one terminal function specified in pipeline"
              {:terminators terminator})))
    (cond->
      (expand-options
       (cond-> (select-keys
                options-map
                [:benchmark :return-value :collect-plan
                 :verbose :viewer])

         (or (= scheme-type :with-jit-warmup)
             (= (:scheme-type scheme-type) :with-jit-warmup)
             limit-time-s)
         (assoc :collect-plan
                (collect-plan-config/full-collect-plan
                 (cond-> (when (map? scheme-type) (merge scheme-type))
                   limit-time-s
                   (assoc :limit-time-ns
                          (* (long limit-time-s) (long units/SEC-NS)))))
                :benchmark (or
                            benchmark
                            benchmarks/minimal-stats-summary))

         (= scheme-type :one-shot)
         (assoc :collect-plan (collect-plan-config/one-shot-collect-plan
                               options-map)
                :benchmark (or benchmark benchmarks/one-shot))

         (seq pipeline-fns)
         (assoc-in [:collector-config :stages] (vec pipeline-fns))

         (seq terminator)
         (assoc-in [:collector-config :terminator] (first terminator)))))))
