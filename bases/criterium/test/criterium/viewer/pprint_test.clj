(ns criterium.viewer.pprint-test
    (:require
     [clojure.test :refer [deftest is testing]]
     [criterium.analyse :as analyse]
     [criterium.test-data :as test-data]
     [criterium.test-utils :refer [trimmed-lines]]
     [criterium.view :as view]
     [criterium.viewer.pprint]))

(def expected-stats-1
     [""
      "|        :_metric | :mean-minus-3sigma | :mean | :mean-plus-3sigma | :min-val | :max-val |"
      "|-----------------+--------------------+-------+-------------------+----------+----------|"
      "| Elapsed Time ns |               88.0 | 100.0 |             112.0 |     89.0 |    114.0 |"])

(def expected-stats-2
     [""
      "|        :_metric | :mean-minus-3sigma | :mean | :mean-plus-3sigma | :min-val | :max-val |"
      "|-----------------+--------------------+-------+-------------------+----------+----------|"
      "| Elapsed Time ns |                1.0 |   1.0 |               1.0 |      1.0 |      1.0 |"])

(deftest pprint-stats-test
         (testing "print-stats"
                  (testing "prints via output-view"
                           (is (= expected-stats-1
                                  (trimmed-lines
                                   (with-out-str
                                    (view/stats*
                                     :pprint
                                     {}
                                     (:data (test-data/bench-stats-map)))))))

                           (is (= expected-stats-2
                                  (let [data-map   (:data (test-data/samples-with-2-values-map))
                                        stats      (analyse/stats)
                                        view-stats (view/stats)]
                                       (trimmed-lines
                                        (with-out-str
                                         (->> data-map
                                              stats
                                              (view-stats :pprint))))))))))

(def expected-counts
     [""
      "|     :_metric | :low-severe | :low-mild | :high-mild | :high-severe |"
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
                                  :pprint
                                  (:data (test-data/outlier-count-map))))))))))

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
                                     :pprint
                                     (:data (test-data/outlier-significance-map))))))))))

(def ^:private expected-event-stats
     [""
      "|           :metric | :sample-count | :loaded-count | :unloaded-count | :time-ms | :total-sample-count | :total-count | :total-time-ms |"
      "|-------------------+---------------+---------------+-----------------+----------+---------------------+--------------+----------------|"
      "|       ClassLoader |           1.0 |           1.0 |             1.0 |          |                     |              |                |"
      "|   JIT compilation |           1.0 |               |                 |  3.00 ms |                     |              |                |"
      "| Garbage Collector |               |               |                 |          |                 1.0 |          2.0 |        1.00 ms |"])

(deftest pprint-event-stats-test
         (testing "print-event-stats"
                  (testing "prints via report"
                           (is (= expected-event-stats
                                  (let [data-map    (:data (test-data/samples-for-event-stats-map))
                                        event-stats (analyse/event-stats)
                                        view        (view/event-stats)]
                                       (trimmed-lines
                                        (with-out-str
                                         (->> data-map
                                              event-stats
                                              (view :pprint))))))))))
