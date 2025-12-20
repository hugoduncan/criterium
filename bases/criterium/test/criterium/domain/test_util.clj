(ns criterium.domain.test-util
  "Shared test utilities for domain test namespaces.

  Provides mock data constructors and sample data for testing
  domain types, analysis, and builder functions."
  (:require
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
