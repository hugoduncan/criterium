(ns criterium.benchmarks
  "Provide metric independent benchmark definitions.
  Metrics need to be added under the :config key."
  (:require
   [criterium.analyse]
   [criterium.benchmark :as benchmark]
   [criterium.view]))

(def one-shot
  (criterium.benchmark/->benchmark
   {:analyse [:event-stats]
    :view    [:metrics
              :event-stats]}))

(def minimal-stats-summary
  (criterium.benchmark/->benchmark
   {:analyse [:transform-log
              [:quantiles {:quantiles [0.9 0.99 0.99]}]
              :outliers
              [:stats {:samples-id :log-samples}]
              :event-stats]
    :view    [:stats
              :event-stats
              :collect-plan
              #_[:final-gc-warnings
                 {:warn-threshold 0.01}]]}))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def log-histogram
  (criterium.benchmark/->benchmark
   {:analyse [:transform-log
              [:quantiles {:quantiles [0.9 0.99 0.99]}]
              :outliers
              [:stats {:samples-id :log-samples}]
              :event-stats]
    :view    [:samples
              :sample-percentiles
              :histogram
              :stats
              :quantiles
              :event-stats
              :outlier-counts
              :collect-plan]}))
