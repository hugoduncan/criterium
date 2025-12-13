(ns criterium.viewer.print-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.analyse.metrics-samples :as metrics-samples]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-data :as test-data]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.view :as view]
   [criterium.viewer.print :as print]))

(deftest print-stat-test
  (testing "print-stat"
    (is (= "Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"
           (str/trim
            (with-out-str
              (print/print-stat
               {:label "Elapsed Time"
                :scale 1e-9
                :dimension :time}
               {:mean 100.0
                :variance 16.0
                :mean-plus-3sigma 112.0
                :mean-minus-3sigma 88.0
                :min-val 89.0}
               [collect-plan/identity-transforms])))))))

(defn identity-transform [samples]
  (with-meta samples {:transform {:sample-> identity :->sample identity}}))

(deftest print-stats-test
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= ["Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"]
             (trimmed-lines
              (with-out-str
                (view/stats*
                 :print
                 {}
                 (:data (test-data/bench-stats-map)))))))

      (is (= ["Elapsed Time: 1.00 ns  3σ [1.00 1.00]  min 1.00"]
             (let [data-map (:data (test-data/samples-with-2-values-map))
                   stats (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       stats
                       (view-stats :print)))))))))
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= ["Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"]
             (trimmed-lines
              (with-out-str
                (view/stats*
                 :print
                 {}
                 (:data (test-data/bench-stats-map)))))))

      (is (= ["Elapsed Time: 1.00 ns  3σ [1.00 1.00]  min 1.00"]
             (let [data-map (:data (test-data/samples-with-2-values-map))
                   stats (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       stats
                       (view-stats :print))))))))
    (testing "prints via output-view"
      (is (= ["Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"]
             (trimmed-lines
              (with-out-str
                (view/stats*
                 :print
                 {}
                 (:data (test-data/bench-stats-map)))))))

      (is (= ["Elapsed Time: 5.00 ns  3σ [-5.39 15.4]  min 1.00"]
             (let [data-map (:data (test-data/samples-with-variance-12-map))
                   stats (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       stats
                       (view-stats :print)))))))
      (is (= ["Elapsed Time: 2.50 ns  3σ [-2.70 7.70]  min 0.500"]
             (let [data-map
                   (-> (update-in
                        (:data (test-data/samples-with-variance-12-map))
                        [:samples]
                        merge
                        {:batch-size 2
                         :transform (#'collect-plan/batch-transforms 2)}))
                   stats (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       stats
                       (view-stats :print))))))))))

(deftest print-booststrap-stat-test
  (testing "print-bootstrap-stat"
    (is (= ["Elapsed Time min: 16.0 ns CI [9.00 25.0] (0.050 0.950)"
            "Elapsed Time mean: 100 ns CI [95.0 105] (0.050 0.950)"
            "Elapsed Time 3σ: [76.0 124] ns"]
           (trimmed-lines
            (with-out-str
              (print/print-bootstrap-stat
               {:scale 1e-9 :dimension :time :path [:elapsed-time]
                :label "Elapsed Time"}
               {:mean {:point-estimate 100.0
                       :estimate-quantiles
                       [{:value 95.0 :alpha 0.05}
                        {:value 105.0 :alpha 0.95}]}
                :variance {:point-estimate 16.0
                           :estimate-quantiles
                           [{:value 9.0 :alpha 0.05}
                            {:value 25.0 :alpha 0.95}]}
                :min-val {:point-estimate 16.0
                          :estimate-quantiles
                          [{:value 9.0 :alpha 0.05}
                           {:value 25.0 :alpha 0.95}]}
                :mean-plus-3sigma {:point-estimate 124.0
                                   :estimate-quantiles
                                   [{:value 9.0 :alpha 0.05}
                                    {:value 25.0 :alpha 0.95}]}
                :mean-minus-3sigma {:point-estimate 76.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}})))))
    (is (= ["Elapsed Time min: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
            "Elapsed Time mean: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
            "Elapsed Time 3σ: [1.00 1.00] ns"]
           (let [data-map
                 {:samples
                  {:type :criterium/collected-metrics-samples
                   :metric->values {[:elapsed-time] [1 1 1]}
                   :metrics-defs (select-keys
                                  (metrics/metrics)
                                  [:elapsed-time])
                   :transform collect-plan/identity-transforms
                   :batch-size 1
                   :eval-count 1
                   :elapsed-time 1}}
                 bootstrap (bootstrap/bootstrap-stats
                            {:quantiles [0.025 0.975]
                             :estimate-quantiles [0.025 0.975]})
                 view (view/bootstrap-stats {})]
             (trimmed-lines
              (with-out-str
                (->> data-map
                     bootstrap
                     (view :print)))))))))

(deftest print-samples-test
  (testing "print-samples"
    (testing "prints via view"
      (is (= ["Samples: 7 samples with batch-size 1"
              "Elapsed Time"
              "[    6] 10.0 µs high-severe"]
             (let [bench-map
                   (:data (test-data/samples-with-outliers-values-map))
                   quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
                   outliers (analyse/outliers)
                   stats (analyse/stats)
                   view (view/samples)]
               (trimmed-lines
                (with-out-str
                  (->> bench-map
                       quantiles
                       outliers
                       stats
                       (view :print))))))))))

(deftest print-outlier-count-test
  (testing "print-outlier-count"
    (testing "prints all outliers when all present"
      (is (= ["M: Found 10 outliers in 100 samples (10.0 %)"
              "low-severe\t 1 (1.0000 %)"
              "low-mild\t 2 (2.0000 %)"
              "high-mild\t 3 (3.0000 %)"
              "high-severe\t 4 (4.0000 %)"]
             (trimmed-lines
              (with-out-str (print/print-outlier-count
                             {:label "M"}
                             100
                             {:outlier-counts
                              (metrics-samples/outlier-count 1 2 3 4)}))))))
    (testing "prints only present outliers"
      (is (= ["M: Found 5 outliers in 100 samples (5.00 %)"
              "low-mild\t 2 (2.0000 %)"
              "high-mild\t 3 (3.0000 %)"]
             (trimmed-lines
              (with-out-str
                (print/print-outlier-count
                 {:label "M"}
                 100
                 {:outlier-counts
                  (metrics-samples/outlier-count
                   0 2 3 0)}))))))
    (testing "prints via view"
      (is (= ["Elapsed Time: Found 5 outliers in 1 samples (500 %)"
              "low-mild\t 2 (200.0000 %)"
              "high-mild\t 3 (300.0000 %)"]
             (trimmed-lines
              (with-out-str
                (let [data-map
                      (:data (test-data/outlier-count-map))
                      view (view/outlier-counts)]
                  (view :print data-map)))))))))

(deftest print-outlier-significance-test
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [(str "Elapsed Time Variance contribution from outliers : 25.0 %"
                   "Elapsed Time Variance is moderately inflated by outliers")]
             (trimmed-lines
              (with-out-str
                ((view/outlier-significance)
                 :print
                 (:data (test-data/outlier-significance-map))))))))))

(deftest print-event-stats-test
  (testing "print-event-stats"
    (testing "prints via report"
      (is (=
           ["ClassLoader: loaded 1 and unloaded 1 classes in 1 samples"
            "JIT compilation: ran for 3.00 ms in 1 samples"
            (str "Garbage Collector: ran 2 times for a total of 1.00 ms "
                 "in 1 samples")]
           (let [data-map (:data (test-data/samples-for-event-stats-map))
                 event-stats (analyse/event-stats)
                 view (view/event-stats)]
             (trimmed-lines
              (with-out-str
                (->> data-map
                     event-stats
                     (view :print))))))))))

(deftest print-final-gc-warnings-test
  (testing "print-final-gc-warnings-test"
    (testing "prints via view"
      (is (= ["Final GC ran for 1.00 ms, 1.0% of total sampling time (100 ms)"]
             (let [metrics-defs (->
                                 (select-keys
                                  (metrics/metrics)
                                  [:elapsed-time :class-loader :compilation])
                                 (assoc-in
                                  [:garbage-collector :values]
                                  [{:path
                                    [:garbage-collector :total :count]
                                    :label "GC total count"
                                    :scale 1
                                    :type :event
                                    :dimension :count}
                                   {:path
                                    [:garbage-collector :total :time-ms]
                                    :label "GC total time"
                                    :scale 1e-3
                                    :type :event
                                    :dimension :time}]))
                   view1 (view/final-gc-warnings
                          {:warn-threshold 0.01
                           :sampled-path [:sampled]})
                   view2 (view/final-gc-warnings
                          {:view-type :final-gc-warnings
                           :warn-threshold 0.02
                           :sampled-path [:sampled]})
                   data-map
                   {:samples
                    {:type :criterium/collected-metrics-samples
                     :metric->values
                     {[:elapsed-time] [99999999]}
                     :metrics-deps metrics-defs
                     :batch-size 1
                     :eval-count 1
                     :elapsed-time 1}
                    :final-gc
                    {:type :criterium/collected-metrics-samples
                     :metric->values
                     {[:compilation :time-ms] [3]
                      [:garbage-collector :total :time-ms] [1]
                      [:elapsed-time] [1]}
                     :metrics-deps metrics-defs
                     :batch-size 1
                     :eval-count 1
                     :elapsed-time 1}}]
               (trimmed-lines
                (with-out-str
                  (view1 :print data-map)
                  (view2 :print data-map)))))))))

(deftest print-os-test
  (let [s (with-out-str ((view/os) :print {}))]
    (is (str/ends-with? s "cpu(s)\n"))))

(deftest print-runtime-test
  (let [s (with-out-str ((view/runtime) :print {}))]
    (is (not (str/blank? s)))))

;; Tests that the print viewer handles non-numeric metric values gracefully
;; instead of throwing an exception when trying to multiply or format.
(deftest print-metrics-non-numeric-test
  (testing "print-metrics"
    (testing "handles non-numeric metric values"
      (is (= ["Elapsed Time: unavailable"]
             (trimmed-lines
              (with-out-str
                (view/metrics*
                 :print
                 {}
                 (:data (test-data/samples-with-non-numeric-value-map))))))))))

;;; Domain View Tests

(deftest domain-extract-print-test
  ;; Tests the print viewer output for domain-extract results.
  ;; Now using the multi-metric :metrics map structure.
  ;; Verifies coordinate formatting and value display with unit scaling.
  (testing "domain-extract*"
    (testing "prints metric path and coordinate-value pairs for each metric"
      (is (= ["Domain Extract: [:stats :elapsed-time :mean]"
              "baseline: 100 ns"
              "n=100: 200 ns"
              "impl=:foo n=100: 300 ns"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :print
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[:baseline 100]
                                     [{:n 100} 200]
                                     [{:impl :foo :n 100} 300]]}}}}))))))
    (testing "handles nil values"
      (is (= ["Domain Extract: [:stats :elapsed-time :mean]"
              "test: nil"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :print
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[:test nil]]}}}}))))))
    (testing "uses custom extract-id"
      (is (= ["Domain Extract: [:stats :elapsed-time :mean]"
              "a: 1.00 ns"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :print
                 {:extract-id :my-extract}
                 {:my-extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[:a 1]]}}}}))))))
    (testing "prints multiple metrics with separate headers"
      (is (= ["Domain Extract: [:stats :elapsed-time :mean]"
              "n=100: 100 ns"
              ""
              "Domain Extract: [:stats :thread-allocation :mean]"
              "n=100: 1.00 Kb"]
             (trimmed-lines
              (with-out-str
                (view/domain-extract*
                 :print
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[{:n 100} 100]]}
                             :thread-allocation
                             {:metric [:stats :thread-allocation :mean]
                              :data [[{:n 100} 1024]]}}}}))))))))

(deftest domain-grouped-print-test
  ;; Tests the print viewer output for domain-grouped results.
  ;; Verifies axis display and run counts per group.
  (testing "domain-grouped*"
    (testing "prints axis and run counts per group"
      (is (= ["Domain Grouped by: impl"
              "<nil>: 1 run"
              ":bar: 1 run"
              ":foo: 2 runs"]
             (trimmed-lines
              (with-out-str
                (view/domain-grouped*
                 :print
                 {}
                 {:grouped
                  {:type :criterium/domain-grouped
                   :axis :impl
                   :data {:foo {:type :criterium/domain
                                :runs [{} {}]}
                          :bar {:type :criterium/domain
                                :runs [{}]}
                          nil {:type :criterium/domain
                               :runs [{}]}}}}))))))
    (testing "uses custom grouped-id"
      (is (= ["Domain Grouped by: n"
              "100: 1 run"]
             (trimmed-lines
              (with-out-str
                (view/domain-grouped*
                 :print
                 {:grouped-id :by-n}
                 {:by-n
                  {:type :criterium/domain-grouped
                   :axis :n
                   :data {100 {:type :criterium/domain
                               :runs [{}]}}}}))))))))

(deftest domain-comparison-print-test
  ;; Tests the print viewer output for domain-comparison results.
  ;; Verifies table format with axis values as columns for side-by-side comparison.
  (testing "domain-comparison*"
    (testing "prints comparison as table with axis values as columns"
      (is (= ["Domain Comparison by impl: [:stats :elapsed-time :mean]"
              "│   :bar │   :foo"
              "─────────┼────────┼───────"
              "n=100 │ 200 ns │ 100 ns"]
             (trimmed-lines
              (with-out-str
                (view/domain-comparison*
                 :print
                 {}
                 {:comparison
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :data {:foo [{:coord {:impl :foo :n 100} :value 100}]
                          :bar [{:coord {:impl :bar :n 100} :value 200}]}}}))))))
    (testing "handles nil axis values"
      (is (= ["Domain Comparison by impl: [:stats :elapsed-time :mean]"
              "│   <nil>"
              "─────────┼────────"
              "baseline │ 50.0 ns"]
             (trimmed-lines
              (with-out-str
                (view/domain-comparison*
                 :print
                 {}
                 {:comparison
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :data {nil [{:coord :baseline :value 50}]}}}))))))))

(deftest domain-regression-print-test
  ;; Tests the print viewer output for domain-regression results.
  ;; Now using the multi-metric :regressions map structure.
  ;; Verifies display of models sorted by R² with equations, best-fit indicator,
  ;; and [plotted] marker for models within tolerance.
  (testing "domain-regression*"
    (testing "prints models sorted by R² with equations and best-fit indicator"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "O(n)        R²=0.9900  y = 1.200e-09*n + 5.000e-08  <- best fit"
              "O(n log n)  R²=0.8500  y = 2.500e-10*n*log(n) + 1.000e-07"
              "O(n²)       R²=0.7000  y = 1.000e-12*n² + 2.000e-07"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.2e-9 :b 5e-8}
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :r-squared 0.85}
                                           {:id :quadratic
                                            :label "O(n²)"
                                            :coefficients {:a 1e-12 :b 2e-7}
                                            :r-squared 0.70}]
                                  :best-fit :linear}}}}))))))
    (testing "shows [plotted] for models within tolerance"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "O(n)        R²=0.9900  y = 1.200e-09*n + 5.000e-08  <- best fit"
              "O(n log n)  R²=0.9850  y = 2.500e-10*n*log(n) + 1.000e-07  [plotted]"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.2e-9 :b 5e-8}
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :r-squared 0.985}]
                                  :best-fit :linear}}}}))))))
    (testing "respects custom tolerance parameter"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "O(n)        R²=0.9900  y = 1.200e-09*n + 5.000e-08  <- best fit"
              "O(n log n)  R²=0.8500  y = 2.500e-10*n*log(n) + 1.000e-07  [plotted]"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {:tolerance 0.20}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.2e-9 :b 5e-8}
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :r-squared 0.85}]
                                  :best-fit :linear}}}}))))))
    (testing "handles negative intercepts"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "O(n)  R²=0.9500  y = 1.200e-09*n - 5.000e-09  <- best fit"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.2e-9 :b -5e-9}
                                            :r-squared 0.95}]
                                  :best-fit :linear}}}}))))))
    (testing "handles models without coefficients"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "O(n)  R²=0.9500  <- best fit"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear :label "O(n)" :r-squared 0.95}]
                                  :best-fit :linear}}}}))))))
    (testing "handles empty models"
      (is (= ["Domain Regression (axis: n, metric: [:stats :elapsed-time :mean])"
              "(insufficient data for regression)"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {}
                 {:regression
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models []
                                  :best-fit nil}}}}))))))
    (testing "uses custom regression-id"
      (is (= ["Domain Regression (axis: size, metric: [:stats :elapsed-time :mean])"
              "O(n)  R²=0.9500  y = 1.500e-09*n + 1.000e-08  <- best fit"]
             (trimmed-lines
              (with-out-str
                (view/domain-regression*
                 :print
                 {:regression-id :scaling}
                 {:scaling
                  {:type :criterium/domain-regression
                   :axis :size
                   :regressions {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :models [{:id :linear
                                            :label "O(n)"
                                            :coefficients {:a 1.5e-9 :b 1e-8}
                                            :r-squared 0.95}]
                                  :best-fit :linear}}}}))))))))



