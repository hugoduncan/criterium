(ns criterium.viewer.pprint-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.metric :as metric]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.pprint]
   [criterium.viewer.test-data :as test-data]))

(def expected-stats-1
  [""
   "|      :metric | :mean-minus-3sigma |  :mean | :mean-plus-3sigma | :min-val | :max-val |"
   "|--------------+--------------------+--------+-------------------+----------+----------|"
   "| Elapsed Time |            88.0 ns | 100 ns |            112 ns |  89.0 ns |   114 ns |"])

(def expected-stats-2
  [""
   "|      :metric | :mean-minus-3sigma |   :mean | :mean-plus-3sigma | :min-val | :max-val |"
   "|--------------+--------------------+---------+-------------------+----------+----------|"
   "| Elapsed Time |            1.00 ns | 1.00 ns |           1.00 ns |  1.00 ns |  1.00 ns |"])

(deftest pprint-stats-test
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= expected-stats-1
             (trimmed-lines
              (with-out-str
                (view/stats*
                 {}
                 (-> (test-data/bench-stats-map)
                     (assoc :viewer :pprint)))))))

      (is (= expected-stats-2
             (let [bench-map  (-> (test-data/samples-with-2-values-map)
                                  (assoc :viewer :pprint))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      stats
                      view-stats)))))))))

(def expected-counts
  [""
   "|      :metric | :low-severe | :low-mild | :high-mild | :high-severe |"
   "|--------------+-------------+-----------+------------+--------------|"
   "| Elapsed Time |           0 |         2 |          3 |            0 |"])

(deftest print-outlier-count-test
  (testing "print-outlier-count"
    (testing "prints via view"
      (is
       (= expected-counts
          (trimmed-lines
           (with-out-str
             ((view/outlier-counts)
              (-> (test-data/outlier-count-map)
                  (assoc :viewer :pprint))))))))))

(deftest print-outlier-significance-test
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [""
              "|   :effect | :significance |"
              "|-----------+---------------|"
              "| :moderate |          0.25 |"]
             (trimmed-lines
              (with-out-str
                ((view/outlier-significance)
                 (-> (test-data/outlier-significance-map)
                     (assoc :viewer :pprint))))))))))

(def ^:private expected-event-stats
  [""
   "|           :metric | :sample-count | :loaded-count | :unloaded-count | :time-ms | :total-sample-count | :total-count | :total-time-ms |"
   "|-------------------+---------------+---------------+-----------------+----------+---------------------+--------------+----------------|"
   "|       ClassLoader |             1 |             1 |               1 |          |                     |              |                |"
   "|   JIT compilation |             1 |               |                 |  3.00 ms |                     |              |                |"
   "| Garbage Collector |               |               |                 |          |                   1 |            2 |        1.00 ms |"])

(deftest pprint-event-stats-test
  (testing "print-event-stats"
    (testing "prints via report"
      (is (= expected-event-stats
             (let [bench-map   (-> (test-data/samples-for-event-stats-map)
                                   (assoc :viewer :pprint))
                   event-stats (analyse/event-stats)
                   view        (view/event-stats)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      event-stats
                      view)))))))))
