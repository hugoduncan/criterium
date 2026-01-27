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
   [criterium.viewer.kindly]
   [criterium.viewer.portal]
   [criterium.viewer.pprint]
   [criterium.viewer.print]))

(def ^:dynamic *default-viewer*
  "Dynamic var for the default viewer to use when no explicit :viewer option
  is provided. Initial value is :print.

  This can be bound dynamically or altered via set-default-viewer!.

  Precedence (highest to lowest):
  1. Explicit :viewer option on bench call
  2. :viewer from bench-plan (if provided)
  3. This dynamic default viewer
  4. Hard-coded fallback :print (if this var is nil)"
  :print)

(defn set-default-viewer!
  "Set the default viewer for all bench calls that don't specify an explicit
  :viewer option.

  viewer - Keyword identifying the viewer, e.g. :print, :pprint, :portal, :kindly

  Example:
    (set-default-viewer! :kindly)
    (bench (+ 1 1))  ; Now uses :kindly viewer by default"
  [viewer]
  (alter-var-root #'*default-viewer* (constantly viewer)))

(defn metric-ids->collector-config
  [metric-ids]
  (let [metrics (zipmap
                 metric-ids
                 (mapv collector/maybe-var-get-stage metric-ids))
        terminator (util/filter-map collector/terminal? metrics)
        stages (util/filter-map (complement collector/terminal?) metrics)]
    (when (> (count terminator) 1)
      (throw (ex-info
              "More than one terminal function specified in metric-ids"
              {:terminators (keys terminator)})))
    (when-let [unknown (not-empty (util/filter-map nil? metrics))]
      (throw (ex-info
              "Unknown metric-ids"
              {:metric-ids (keys unknown)})))
    {:stages (filterv stages metric-ids)
     :terminator (or (some-> terminator
                             first
                             key)
                     :elapsed-time)}))

(defn config-map
  "Convert option arguments into a criterium configuration map.
  The config map specifies how criterium will execute."
  [options-map]
  (let [unknown-keys (set/difference
                      (set (keys options-map))
                      #{:limit-time-s
                        :metric-ids
                        :return-value
                        :collect-plan
                        :analyse
                        :view
                        :bench-plan
                        :verbose
                        :viewer
                        :with-allocation-trace
                        :warmup-args-fn})
        limit-time-s (:limit-time-s options-map)
        analyse (:analyse options-map)
        view (:view options-map)
        bench-plan (:bench-plan options-map)
        viewer (:viewer options-map (or *default-viewer* :print))
        options-map (cond-> options-map
                      (:limit-time-s options-map)
                      (assoc :limit-time-ns
                             (* (long limit-time-s)
                                (long units/SEC-NS))))
        collect-plan (or (:collect-plan options-map)
                         (:collect-plan bench-plan)
                         :with-jit-warmup)
        collect-plan (if (keyword? collect-plan)
                       (collect-plan-config/collect-plan-config
                        collect-plan
                        options-map)
                       collect-plan)
        scheme-type (have (:scheme-type collect-plan))
        collector-config (->>
                          (or (when-let [metric-ids (:metric-ids options-map)]
                                (metric-ids->collector-config metric-ids))
                              (:collector-config bench-plan)
                              collector-configs/default-collector-config)
                          (collect-plan-config/ensure-pipeline-stages
                           scheme-type))
        default-return (if (= viewer :kindly)
                         [:viewer :output]
                         [:samples :expr-value])]

    (when (seq unknown-keys)
      (throw (ex-info "Unknown options" {:options unknown-keys})))
    (cond-> (assoc (select-keys
                    options-map
                    [:return-value :verbose :with-allocation-trace])
                   :collect-plan collect-plan
                   :collector-config collector-config
                   :viewer viewer
                   :return-value (:return-value options-map default-return))

      (= scheme-type :with-jit-warmup)
      (assoc :analyse (or analyse
                          (:analyse bench-plan)
                          (:analyse bench-plans/default))
             :view (or view
                       (:view bench-plan)
                       (:view bench-plans/default)))

      (= scheme-type :one-shot)
      (assoc :analyse (or analyse
                          (:analyse bench-plan)
                          (:analyse bench-plans/one-shot))
             :view (or view
                       (:view bench-plan)
                       (:view bench-plans/one-shot))))))
