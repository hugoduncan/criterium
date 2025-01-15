(ns criterium.bench.config
  (:require
   [clojure.set :as set]
   [criterium.bench-plans :as bench-plans]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.collector-configs :as collector-configs]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.util.units :as units]
   [criterium.viewer.portal]
   [criterium.viewer.pprint]
   [criterium.viewer.print]))

(defn metric-ids->collector-config
  [metric-ids]
  (let [metrics    (zipmap
                    metric-ids
                    (mapv collector/maybe-var-get-stage metric-ids))
        terminator (util/filter-map collector/terminal? metrics)
        stages     (util/filter-map (complement collector/terminal?) metrics)]
    (when (> (count terminator) 1)
      (throw (ex-info
              "More than one terminal function specified in metric-ids"
              {:terminators (keys terminator)})))
    (when-let [unknown (not-empty (util/filter-map nil? metrics))]
      (throw (ex-info
              "Unknown metric-ids"
              {:metric-ids (keys unknown)})))
    {:stages     (filterv stages metric-ids)
     :terminator (or (some-> terminator
                             first
                             key)
                     :elapsed-time)}))

(defn config-map
  "Convert option arguments into a criterium configuration map.
  The config map specifies how criterium will execute."
  [options-map]
  (let [limit-time-s     (:limit-time-s options-map)
        analyse          (:analyse options-map)
        view             (:view options-map)
        bench-plan       (:bench-plan options-map)
        options-map      (cond-> options-map
                           (:limit-time-s options-map)
                           (assoc
                            (assoc :limit-time-ns
                                   (* (long limit-time-s)
                                      (long units/SEC-NS)))))
        collect-plan     (or (:collect-plan options-map)
                             (:collect-plan bench-plan)
                             :with-jit-warmup)
        collect-plan     (if (keyword? collect-plan)
                           (collect-plan-config/collect-plan-config
                            collect-plan
                            options-map)
                           (collect-plan-config/collect-plan-config
                            (:scheme-type collect-plan)
                            options-map))
        scheme-type      (have (:scheme-type collect-plan))
        collector-config (->>
                          (or (when-let [metric-ids (:metric-ids options-map)]
                                (metric-ids->collector-config metric-ids))
                              (:collector-config bench-plan)
                              collector-configs/default-collector-config)
                          (collect-plan-config/ensure-pipeline-stages
                           scheme-type))

        unknown-keys (set/difference
                      (set (keys options-map))
                      #{:limit-time-s
                        :metric-ids
                        :return-value
                        :collect-plan
                        :analyse
                        :view
                        :bench-plan
                        :verbose
                        :viewer})]

    (when (seq unknown-keys)
      (throw (ex-info "Unknown options" {:options unknown-keys})))
    (cond-> (assoc (select-keys
                    options-map
                    [:return-value :verbose])
                   :collect-plan collect-plan
                   :collector-config collector-config
                   :viewer (:viewer options-map :print)
                   :return-value (:return-value
                                  options-map
                                  [:samples :expr-value]))

      (= scheme-type :with-jit-warmup)
      (assoc :analyse (or analyse
                          (:analyse bench-plan)
                          (:analyse bench-plans/default-with-warmup))
             :view (or view
                       (:view bench-plan)
                       (:view bench-plans/default-with-warmup)))

      (= scheme-type :one-shot)
      (assoc :analyse (or analyse
                          (:analyse bench-plan)
                          (:analyse bench-plans/default-one-shot))
             :view (or view
                       (:view bench-plan)
                       (:view bench-plans/default-one-shot))))))
