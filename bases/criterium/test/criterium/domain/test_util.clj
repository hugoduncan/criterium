(ns criterium.domain.test-util
  "Shared test utilities for domain test namespaces.

  Provides mock data constructors and sample data for testing
  domain types, analysis, and builder functions."
  (:require
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]))

;;; Sample Data

(def sample-data {:some "bench-result"})
(def sample-data-2 {:other "result"})

;;; Mock Bench Result Constructors

(defn mock-bench-result
  "Create a mock bench result with proper structure for stats-value.
  stats-data is a map of {metric-id {value-key value}}."
  [stats-data]
  {:stats {:type :criterium/stats
           :transform collect-plan/identity-transforms
           :stats stats-data
           :metrics-defs {}
           :batch-size 1
           :source-id nil
           :outliers-id nil}})

(defn mock-bench-result-with-defs
  "Create a mock bench result with metrics-defs for multi-metric discovery.
  stats-data is a map of {metric-id {value-key value}}."
  [stats-data]
  {:stats {:type :criterium/stats
           :transform collect-plan/identity-transforms
           :stats stats-data
           :metrics-defs (into {}
                               (map (fn [k] [k {:type :quantitative}]))
                               (keys stats-data))
           :batch-size 1
           :source-id nil
           :outliers-id nil}})

(defn- make-bca-estimate
  "Create a BCa estimate structure for a given point value.
  Derives CI from +/- 5% of the point value."
  [point-val]
  (let [pv (double point-val)]
    {:point-estimate pv
     :estimate-quantiles [{:value (* pv 0.95) :alpha 0.025}
                          {:value (* pv 1.05) :alpha 0.975}]}))

(defn mock-bench-result-with-bootstrap
  "Create a mock bench result with bootstrap stats for box plot testing.

  stats-data is a map of {metric-id {value-key value}}.
  For each metric-id, creates bootstrap stats with quantiles 0.1, 0.5, 0.9
  derived from the mean value in stats-data."
  [stats-data]
  (let [metrics-defs (into {}
                           (map (fn [k] [k {:type :quantitative}]))
                           (keys stats-data))
        bootstrap-data
        (into {}
              (map (fn [[metric-id values]]
                     (let [mean-val (double (:mean values 100.0))
                           p10 (* mean-val 0.9)
                           p50 mean-val
                           p90 (* mean-val 1.1)]
                       [metric-id
                        {:mean (make-bca-estimate mean-val)
                         :variance (make-bca-estimate (* mean-val 0.01))
                         :quantiles {0.1 (make-bca-estimate p10)
                                     0.25 (make-bca-estimate (* mean-val 0.95))
                                     0.5 (make-bca-estimate p50)
                                     0.75 (make-bca-estimate (* mean-val 1.05))
                                     0.9 (make-bca-estimate p90)}}])))
              stats-data)]
    {:stats {:type :criterium/stats
             :transform collect-plan/identity-transforms
             :stats stats-data
             :metrics-defs metrics-defs
             :batch-size 1
             :source-id :samples
             :outliers-id nil}
     :bootstrap-stats {:type :criterium/bootstrap
                       :bootstrap bootstrap-data
                       :metrics-defs metrics-defs
                       :transform collect-plan/identity-transforms
                       :batch-size 1
                       :source-id :samples
                       :outliers-id nil}
     :samples {:type :criterium/metrics-samples
               :transform collect-plan/identity-transforms
               :batch-size 1}}))

(defn mock-one-shot-result
  "Create a mock :one-shot bench result with only samples (no stats/bootstrap).
  metrics-data is a map of {metric-id value}."
  [metrics-data]
  (let [metrics-defs (into {}
                           (map (fn [k] [k {:type :quantitative}]))
                           (keys metrics-data))
        metric->values (into {}
                             (map (fn [[k v]]
                                    [[k] (arr/->double-array (double-array [v]))]))
                             metrics-data)]
    {:samples {:type :criterium/metrics-samples
               :metrics-defs metrics-defs
               :metric->values metric->values
               :transform collect-plan/identity-transforms
               :batch-size 1
               :eval-count 1
               :num-samples 1
               :elapsed-time 1}}))
