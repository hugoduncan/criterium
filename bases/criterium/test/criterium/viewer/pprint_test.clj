(ns criterium.viewer.pprint-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.pprint]))

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
              {:metrics-configs (select-keys (metrics/metrics) [:elapsed-time])
               :num-samples     1
               :outliers        {:elapsed-time
                                 {:outlier-counts
                                  (analyse/outlier-count 0 2 3 0)}}
               :viewer          :pprint}))))))))

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
                 {:metrics-configs      (select-keys
                                         (metrics/metrics)
                                         [:elapsed-time])
                  :num-samples          1
                  :outlier-significance {:elapsed-time
                                         {:effect       :moderate
                                          :significance 0.25}}
                  :viewer               :pprint}))))))))
