(ns criterium.collector.metrics-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.collector.metrics :as metrics]
   [criterium.metric :as metric]))

(deftest metrics-test
  (is (every?
       metric/metric-config?
       (metric/all-metric-configs (metrics/metrics)))))
