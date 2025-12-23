(ns criterium.viewer.pprint-test
  (:require
   [clojure.string :as str]
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
  ;; Verifies stats display with conditional output behavior.
  (testing "view/stats*"
    (testing "displays stats when metrics match"
      (is (= expected-stats-1
             (trimmed-lines
              (with-out-str
                (view/stats*
                 :pprint
                 {}
                 (:data (test-data/bench-stats-map)))))))

      (is (= expected-stats-2
             (let [data-map (:data (test-data/samples-with-2-values-map))
                   stats (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       stats
                       (view-stats :pprint))))))))
    (testing "outputs nothing when metric-ids filter yields no matching metrics"
      (is (= ""
             (with-out-str
               (view/stats*
                :pprint
                {:metric-ids [:nonexistent-metric]}
                (:data (test-data/bench-stats-map)))))))))

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
             (let [data-map (:data (test-data/samples-for-event-stats-map))
                   event-stats (analyse/event-stats)
                   view (view/event-stats)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       event-stats
                       (view :pprint))))))))))

(def ^:private expected-non-numeric-metrics
  [""
   "|      :metric |      :value |"
   "|--------------+-------------|"
   "| Elapsed Time | unavailable |"
   "|   Expr value |             |"])

;; Tests that the pprint viewer handles non-numeric metric values gracefully
;; instead of throwing an exception when coercing to double.
(deftest pprint-metrics-non-numeric-test
  (testing "pprint-metrics"
    (testing "handles non-numeric metric values"
      (is (= expected-non-numeric-metrics
             (trimmed-lines
              (with-out-str
                (view/metrics*
                 :pprint
                 {}
                 (:data (test-data/samples-with-non-numeric-value-map))))))))))

(deftest domain-extract-pprint-test
  ;; Tests the pprint viewer table output for domain-extract results.
  ;; Verifies table formatting with single-key coordinate simplification.
  (testing "domain-extract*"
    (testing "displays table with metric columns"
      (is (= ["Domain Extract"
              ""
              "|   n | elapsed-time (ns) |"
              "|-----+-------------------|"
              "| 100 |               100 |"
              "| 200 |               200 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :pprint
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[{:n 100} 100]
                                     [{:n 200} 200]]}}}}))))))
    (testing "handles multiple metrics"
      (is (= ["Domain Extract"
              ""
              "|   n | elapsed-time (ns) | thread-allocation (Kb) |"
              "|-----+-------------------+------------------------|"
              "| 100 |               100 |                   1.00 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :pprint
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[{:n 100} 100]]}
                             :thread-allocation
                             {:metric [:stats :thread-allocation :mean]
                              :data [[{:n 100} 1024]]}}}}))))))
    (testing "uses custom extract-id"
      (is (= ["Domain Extract"
              ""
              "|   n | elapsed-time (ns) |"
              "|-----+-------------------|"
              "| 100 |              1.00 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :pprint
                 {:extract-id :my-extract}
                 {:my-extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[{:n 100} 1]]}}}}))))))))

(deftest domain-grouped-pprint-test
  ;; Tests the pprint viewer table output for domain-grouped results.
  (testing "domain-grouped*"
    (testing "displays axis values and run counts"
      (is (= ["Domain Grouped by: impl"
              ""
              "| :axis-value | :run-count |"
              "|-------------+------------|"
              "|        :bar |          1 |"
              "|        :foo |          2 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-grouped*
                 :pprint
                 {}
                 {:grouped
                  {:type :criterium/domain-grouped
                   :axis :impl
                   :data {:foo {:type :criterium/domain
                                :runs [{} {}]}
                          :bar {:type :criterium/domain
                                :runs [{}]}}}}))))))
    (testing "handles nil axis values"
      (is (= ["Domain Grouped by: impl"
              ""
              "| :axis-value | :run-count |"
              "|-------------+------------|"
              "|       <nil> |          1 |"
              "|        :foo |          1 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-grouped*
                 :pprint
                 {}
                 {:grouped
                  {:type :criterium/domain-grouped
                   :axis :impl
                   :data {:foo {:type :criterium/domain
                                :runs [{}]}
                          nil {:type :criterium/domain
                               :runs [{}]}}}}))))))))

(deftest domain-comparison-pprint-test
  ;; Tests the pprint viewer table output for domain-comparison results.
  ;; Verifies factor display and SI units.
  (testing "domain-comparison*"
    (testing "with :implementations shows factors for non-baseline"
      (is (= ["Domain Comparison by impl: [:stats :elapsed-time :mean]"
              ""
              "|   n |    foo | bar × |"
              "|-----+--------+-------|"
              "| 100 | 100 ns |  2.00 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-comparison*
                 :pprint
                 {}
                 {:comparison
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :implementations [:foo :bar]
                   :data {:foo [{:coord {:impl :foo :n 100} :value 100}]
                          :bar [{:coord {:impl :bar :n 100} :value 200}]}}}))))))
    (testing "without :implementations shows absolute values"
      (is (= ["Domain Comparison by impl: [:stats :elapsed-time :mean] (ns)"
              ""
              "|   n | :bar | :foo |"
              "|-----+------+------|"
              "| 100 |  200 |  100 |"]
             (trimmed-lines
              (with-out-str
                (view/domain-comparison*
                 :pprint
                 {}
                 {:comparison
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :data {:foo [{:coord {:impl :foo :n 100} :value 100}]
                          :bar [{:coord {:impl :bar :n 100} :value 200}]}}}))))))
    (testing "with mismatched :implementations throws error"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"implementations do not match"
           (view/domain-comparison*
            :pprint
            {}
            {:comparison
             {:type :criterium/domain-comparison
              :axis :impl
              :metric [:stats :elapsed-time :mean]
              :implementations [:default]
              :data {:foo [{:coord {:impl :foo :n 100} :value 100}]
                     :bar [{:coord {:impl :bar :n 100} :value 200}]}}}))))))

(deftest domain-regression-pprint-test
  ;; Tests the pprint viewer table output for domain-regression results.
  ;; Verifies model table with R², equations, and best-fit indicators.
  (testing "domain-regression*"
    (testing "displays models sorted by R² with equations"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              ""
              "|     :model | :r-squared |                          :equation | :best-fit |"
              "|------------+------------+------------------------------------+-----------|"
              "|       O(n) |     0.9900 |        y = 1.200e-09*n + 5.000e-08 |   <- best |"
              "| O(n log n) |     0.8500 | y = 2.500e-10*n*log(n) + 1.000e-07 |           |"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :pprint
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.2e-9 :b 5e-8}
                                            :equation-str "y = 1.200e-09*n + 5.000e-08"
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :equation-str "y = 2.500e-10*n*log(n) + 1.000e-07"
                                            :r-squared 0.85}]
                                  :best-fit :linear}}}}))))))
    (testing "shows [plotted] for models within tolerance"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              ""
              "|     :model | :r-squared |                          :equation | :best-fit |"
              "|------------+------------+------------------------------------+-----------|"
              "|       O(n) |     0.9900 |        y = 1.200e-09*n + 5.000e-08 |   <- best |"
              "| O(n log n) |     0.9850 | y = 2.500e-10*n*log(n) + 1.000e-07 | [plotted] |"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :pprint
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.2e-9 :b 5e-8}
                                            :equation-str "y = 1.200e-09*n + 5.000e-08"
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :equation-str "y = 2.500e-10*n*log(n) + 1.000e-07"
                                            :r-squared 0.985}]
                                  :best-fit :linear}}}}))))))
    (testing "handles empty models"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "(insufficient data for regression)"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :pprint
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models []
                                  :best-fit nil}}}}))))))))

(deftest allocation-treemap-pprint-test
  ;; Tests the pprint viewer output for allocation-treemap results.
  ;; Verifies that ASCII treemap is rendered (same as print viewer).
  (testing "allocation-treemap*"
    (testing "prints ASCII tree structure with header"
      (let [treemap-data {:type :criterium/allocation-treemap
                          :group-by :class→line→type
                          :size-by :bytes
                          :root {:name "allocations"
                                 :value 1024
                                 :children [{:name "MyClass"
                                             :value 1024
                                             :children [{:name "L42"
                                                         :value 1024
                                                         :children [{:name "String"
                                                                     :value 1024}]}]}]}}
            output (with-out-str
                     (view/allocation-treemap*
                      :pprint
                      {}
                      {:allocation-treemap treemap-data}))
            lines (str/split-lines output)]
        (is (str/includes? (first (drop-while str/blank? lines))
                           "Allocation Treemap"))
        (is (str/includes? output "class→line→type"))
        (is (str/includes? output "bytes"))
        (is (str/includes? output "allocations/"))
        (is (str/includes? output "MyClass/"))
        (is (str/includes? output "L42/"))
        (is (str/includes? output "String"))))

    (testing "handles missing treemap data"
      (let [output (with-out-str
                     (view/allocation-treemap*
                      :pprint
                      {}
                      {}))]
        (is (str/blank? output))))

    (testing "handles nil root"
      (let [output (with-out-str
                     (view/allocation-treemap*
                      :pprint
                      {}
                      {:allocation-treemap {:type :criterium/allocation-treemap
                                            :root nil}}))]
        (is (str/blank? output))))

    (testing "uses custom treemap-id"
      (let [treemap-data {:type :criterium/allocation-treemap
                          :group-by :type→class→line
                          :size-by :count
                          :root {:name "allocations"
                                 :value 100
                                 :children [{:name "Object" :value 100}]}}
            output (with-out-str
                     (view/allocation-treemap*
                      :pprint
                      {:treemap-id :my-treemap}
                      {:my-treemap treemap-data}))]
        (is (str/includes? output "type→class→line"))
        (is (str/includes? output "count"))
        (is (str/includes? output "Object"))))))
