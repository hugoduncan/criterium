(ns criterium.viewer.print-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.analyse.metrics-samples :as metrics-samples]
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.domain.types :as domain.types]
   [criterium.test-data :as test-data]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.print :as print]))

;;; Transforms wrapped in vector format expected by transform-sample->
(def vectorized-identity-transforms
  (util/update-vals collect-plan/identity-transforms vector))

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

(defn char-positions
  "Return indices where char c appears in string s."
  [c s]
  (keep-indexed (fn [i ch] (when (= ch c) i)) s))

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
  ;; Tests print-bootstrap-stat function for bootstrap statistics display.
  ;; Covers: median with CI (first), mean with CI, and spread (10th-90th percentile).
  (testing "print-bootstrap-stat"
    (testing "without quantiles only prints mean"
      (is (= ["Elapsed Time mean: 100 ns CI [95.0 105] (0.050 0.950)"]
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
                              {:value 25.0 :alpha 0.95}]}}
                 vectorized-identity-transforms))))))
    (testing "with quantiles prints median first, then mean, then spread"
      (is (= ["Elapsed Time median: 98.0 ns CI [93.0 103] (0.025 0.975)"
              "Elapsed Time mean: 100 ns CI [95.0 105] (0.025 0.975)"
              "Elapsed Time spread: [85.0 115] ns (10th-90th percentile)"]
             (trimmed-lines
              (with-out-str
                (print/print-bootstrap-stat
                 {:scale 1e-9 :dimension :time :path [:elapsed-time]
                  :label "Elapsed Time"}
                 {:mean {:point-estimate 100.0
                         :estimate-quantiles
                         [{:value 95.0 :alpha 0.025}
                          {:value 105.0 :alpha 0.975}]}
                  :variance {:point-estimate 16.0
                             :estimate-quantiles
                             [{:value 9.0 :alpha 0.025}
                              {:value 25.0 :alpha 0.975}]}
                  :quantiles
                  {0.1 {:point-estimate 85.0
                        :estimate-quantiles
                        [{:value 80.0 :alpha 0.025}
                         {:value 90.0 :alpha 0.975}]}
                   0.5 {:point-estimate 98.0
                        :estimate-quantiles
                        [{:value 93.0 :alpha 0.025}
                         {:value 103.0 :alpha 0.975}]}
                   0.9 {:point-estimate 115.0
                        :estimate-quantiles
                        [{:value 110.0 :alpha 0.025}
                         {:value 120.0 :alpha 0.975}]}}}
                 vectorized-identity-transforms))))))
    (testing "via bootstrap pipeline with degenerate data"
      (is (= ["Elapsed Time median: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
              "Elapsed Time mean: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
              "Elapsed Time spread: [1.00 1.00] ns (10th-90th percentile)"]
             (let [data-map
                   {:samples
                    {:type :criterium/collected-metrics-samples
                     :metric->values {[:elapsed-time]
                                      (arr/->double-array (double-array [1 1 1]))}
                     :metrics-defs (select-keys
                                    (metrics/metrics)
                                    [:elapsed-time])
                     :transform collect-plan/identity-transforms
                     :batch-size 1
                     :eval-count 1
                     :elapsed-time 1}}
                   ;; Use min-samples 3 to suppress warning for this degenerate test
                   bootstrap (bootstrap/bootstrap-stats
                              {:quantiles [0.025 0.975]
                               :estimate-quantiles [0.025 0.975]
                               :min-samples 3})
                   view (view/bootstrap-stats {})]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       bootstrap
                       (view :print))))))))
    (testing "applies batch-size transform to per-execution values"
      ;; Raw samples are 10000 ns (batch of 10000), expected per-execution is 1 ns
      (is (= ["Elapsed Time median: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
              "Elapsed Time mean: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
              "Elapsed Time spread: [1.00 1.00] ns (10th-90th percentile)"]
             (let [batch-size 10000
                   data-map
                   {:samples
                    {:type :criterium/collected-metrics-samples
                     :metric->values {[:elapsed-time]
                                      (arr/->double-array
                                       (double-array [10000 10000 10000]))}
                     :metrics-defs (select-keys
                                    (metrics/metrics)
                                    [:elapsed-time])
                     :transform (#'collect-plan/batch-transforms batch-size)
                     :batch-size batch-size
                     :eval-count batch-size
                     :elapsed-time 10000}}
                   bootstrap (bootstrap/bootstrap-stats
                              {:quantiles [0.025 0.975]
                               :estimate-quantiles [0.025 0.975]
                               :min-samples 3})
                   view (view/bootstrap-stats {})]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       bootstrap
                       (view :print))))))))))

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
  ;; Tests print-outlier-count function and view/outlier-counts* multimethod.
  ;; Covers: outlier counts display, medcouple display with skewness classification.
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
                              (metrics-samples/outlier-count 1 2 3 4)}
                             false))))))
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
                   0 2 3 0)}
                 false))))))
    (testing "prints via view"
      (is (= ["Elapsed Time: Found 5 outliers in 1 samples (500 %)"
              "low-mild\t 2 (200.0000 %)"
              "high-mild\t 3 (300.0000 %)"]
             (trimmed-lines
              (with-out-str
                (let [data-map
                      (:data (test-data/outlier-count-map))
                      view (view/outlier-counts)]
                  (view :print data-map)))))))
    (testing "displays medcouple when show-medcouple is true"
      (testing "with symmetric distribution (mc ≈ 0)"
        (is (= ["M: Found 1 outliers in 100 samples (1.00 %)"
                "high-severe\t 1 (1.0000 %)"
                "M: medcouple 0.0000 (symmetric)"]
               (trimmed-lines
                (with-out-str
                  (print/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 0 0 0 1)
                    :medcouple 0.0}
                   true))))))
      (testing "with right-skewed distribution (mc > 0)"
        (is (= ["M: Found 1 outliers in 100 samples (1.00 %)"
                "high-severe\t 1 (1.0000 %)"
                "M: medcouple 0.3500 (moderately right-skewed)"]
               (trimmed-lines
                (with-out-str
                  (print/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 0 0 0 1)
                    :medcouple 0.35}
                   true))))))
      (testing "with left-skewed distribution (mc < 0)"
        (is (= ["M: Found 1 outliers in 100 samples (1.00 %)"
                "low-severe\t 1 (1.0000 %)"
                "M: medcouple -0.7500 (strongly left-skewed)"]
               (trimmed-lines
                (with-out-str
                  (print/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 1 0 0 0)
                    :medcouple -0.75}
                   true))))))
      (testing "without medcouple (e.g., digest samples)"
        (is (= ["M: Found 1 outliers in 100 samples (1.00 %)"
                "high-severe\t 1 (1.0000 %)"]
               (trimmed-lines
                (with-out-str
                  (print/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 0 0 0 1)
                    :medcouple nil}
                   true))))))
      (testing "displays medcouple even without outliers"
        (is (= ["M: medcouple 0.1500 (slightly right-skewed)"]
               (trimmed-lines
                (with-out-str
                  (print/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 0 0 0 0)
                    :medcouple 0.15}
                   true)))))))))

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
                     {[:elapsed-time] (arr/->long-array (long-array [99999999]))}
                     :metrics-deps metrics-defs
                     :batch-size 1
                     :eval-count 1
                     :elapsed-time 1}
                    :final-gc
                    {:type :criterium/collected-metrics-samples
                     :metric->values
                     {[:compilation :time-ms] (arr/->long-array (long-array [3]))
                      [:garbage-collector :total :time-ms] (arr/->long-array (long-array [1]))
                      [:elapsed-time] (arr/->long-array (long-array [1]))}
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
                                            :equation-str "y = 1.200e-09*n + 5.000e-08"
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :equation-str "y = 2.500e-10*n*log(n) + 1.000e-07"
                                            :r-squared 0.85}
                                           {:id :quadratic
                                            :label "O(n²)"
                                            :coefficients {:a 1e-12 :b 2e-7}
                                            :equation-str "y = 1.000e-12*n² + 2.000e-07"
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
                                            :equation-str "y = 1.200e-09*n + 5.000e-08"
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :equation-str "y = 2.500e-10*n*log(n) + 1.000e-07"
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
                                            :equation-str "y = 1.200e-09*n + 5.000e-08"
                                            :r-squared 0.99}
                                           {:id :n-log-n
                                            :label "O(n log n)"
                                            :coefficients {:a 2.5e-10 :b 1e-7}
                                            :equation-str "y = 2.500e-10*n*log(n) + 1.000e-07"
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
                                            :equation-str "y = 1.200e-09*n - 5.000e-09"
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
                                            :equation-str "y = 1.500e-09*n + 1.000e-08"
                                            :r-squared 0.95}]
                                  :best-fit :linear}}}}))))))))

;;; Allocation View Tests

(deftest allocation-summary-print-test
  ;; Tests the print viewer output for allocation-summary results.
  ;; Verifies display of totals, counts, and freed ratio (based on bytes).
  (testing "allocation-summary*"
    (testing "prints summary statistics with right-justified numbers"
      (is (= [""
              "Allocation Summary:"
              "Total allocated:         1024 bytes"
              "Total freed:          512 bytes"
              "Retained:          512 bytes"
              "Allocation count:          100"
              "Freed count:           50"
              "Freed ratio:         50.0%"]
             (trimmed-lines
              (with-out-str
                (view/allocation-summary*
                 :print
                 {}
                 {:allocation-summary
                  {:type :criterium/allocation-summary
                   :total-allocated 1024
                   :total-freed 512
                   :num-allocations 100
                   :num-freed 50
                   :freed-ratio 0.5}}))))))
    (testing "handles zero allocations"
      (is (= [""
              "Allocation Summary:"
              "Total allocated:            0 bytes"
              "Total freed:            0 bytes"
              "Retained:            0 bytes"
              "Allocation count:            0"
              "Freed count:            0"
              "Freed ratio:          0.0%"]
             (trimmed-lines
              (with-out-str
                (view/allocation-summary*
                 :print
                 {}
                 {:allocation-summary
                  {:type :criterium/allocation-summary
                   :total-allocated 0
                   :total-freed 0
                   :num-allocations 0
                   :num-freed 0
                   :freed-ratio 0.0}}))))))
    (testing "uses custom summary-id"
      (is (= [""
              "Allocation Summary:"
              "Total allocated:          256 bytes"
              "Total freed:          128 bytes"
              "Retained:          128 bytes"
              "Allocation count:           10"
              "Freed count:            5"
              "Freed ratio:         50.0%"]
             (trimmed-lines
              (with-out-str
                (view/allocation-summary*
                 :print
                 {:summary-id :my-summary}
                 {:my-summary
                  {:type :criterium/allocation-summary
                   :total-allocated 256
                   :total-freed 128
                   :num-allocations 10
                   :num-freed 5
                   :freed-ratio 0.5}}))))))))

(deftest allocation-hotspots-print-test
  ;; Tests the print viewer output for allocation-hotspots results.
  ;; Verifies table format with call site, object type, counts, and byte amounts.
  ;; Object type is truncated from the start to fit the 30-char column.
  (testing "allocation-hotspots*"
    (testing "prints hotspots table with object type"
      (is (= [""
              "Allocation Hotspots:"
              "Count        Bytes    Freed  Freed Bytes  Object Type                     Call Site"
              (apply str (repeat 110 "-"))
              "100         1024       50          512  Ljava/lang/String;              my.ns$fn.invoke (my_ns.clj:42)"
              "50          256       25          128  Ljava/lang/Object;              other.ns$g.apply (other.clj:10)"]
             (trimmed-lines
              (with-out-str
                (view/allocation-hotspots*
                 :print
                 {}
                 {:allocation-hotspots
                  {:type :criterium/allocation-hotspots
                   :hotspots [{:call-site {:call-class "my.ns$fn"
                                           :call-method "invoke"
                                           :call-file "my_ns.clj"
                                           :call-line 42}
                               :object-type "Ljava/lang/String;"
                               :count 100
                               :bytes 1024
                               :freed-count 50
                               :freed-bytes 512}
                              {:call-site {:call-class "other.ns$g"
                                           :call-method "apply"
                                           :call-file "other.clj"
                                           :call-line 10}
                               :object-type "Ljava/lang/Object;"
                               :count 50
                               :bytes 256
                               :freed-count 25
                               :freed-bytes 128}]}}))))))
    (testing "truncates long object type from the start"
      ;; "Ljava/util/concurrent/ArrayBlockingQueue;" is 41 chars
      ;; With 30-char column, keeps last 29 chars + ellipsis
      (is (= [""
              "Allocation Hotspots:"
              "Count        Bytes    Freed  Freed Bytes  Object Type                     Call Site"
              (apply str (repeat 110 "-"))
              "10          100        5           50  …oncurrent/ArrayBlockingQueue;  x.y$z.run (z.clj:1)"]
             (trimmed-lines
              (with-out-str
                (view/allocation-hotspots*
                 :print
                 {:hotspots-id :my-hotspots}
                 {:my-hotspots
                  {:type :criterium/allocation-hotspots
                   :hotspots [{:call-site {:call-class "x.y$z"
                                           :call-method "run"
                                           :call-file "z.clj"
                                           :call-line 1}
                               :object-type "Ljava/util/concurrent/ArrayBlockingQueue;"
                               :count 10
                               :bytes 100
                               :freed-count 5
                               :freed-bytes 50}]}}))))))
    (testing "handles empty hotspots gracefully"
      (let [output (with-out-str
                     (view/allocation-hotspots*
                      :print
                      {}
                      {:allocation-hotspots
                       {:type :criterium/allocation-hotspots
                        :hotspots []}}))]
        (is (str/blank? output))))
    (testing "handles nil object-type"
      (is (= [""
              "Allocation Hotspots:"
              "Count        Bytes    Freed  Freed Bytes  Object Type                     Call Site"
              (apply str (repeat 110 "-"))
              "10          100        5           50                                  x.y$z.run (z.clj:1)"]
             (trimmed-lines
              (with-out-str
                (view/allocation-hotspots*
                 :print
                 {:hotspots-id :my-hotspots}
                 {:my-hotspots
                  {:type :criterium/allocation-hotspots
                   :hotspots [{:call-site {:call-class "x.y$z"
                                           :call-method "run"
                                           :call-file "z.clj"
                                           :call-line 1}
                               :count 10
                               :bytes 100
                               :freed-count 5
                               :freed-bytes 50}]}}))))))))

(deftest allocation-by-type-print-test
  ;; Tests the print viewer output for allocation-by-type results.
  ;; Verifies table format sorted by bytes descending.
  (testing "allocation-by-type*"
    (testing "prints by-type table sorted by bytes"
      (is (= [""
              "Allocations by Type:"
              "Count        Bytes    Freed  Freed Bytes  Type"
              (apply str (repeat 80 "-"))
              "100         1024       50          512  [B"
              "50          256       25          128  Ljava/lang/String;"]
             (trimmed-lines
              (with-out-str
                (view/allocation-by-type*
                 :print
                 {}
                 {:allocation-by-type
                  {:type :criterium/allocation-by-type
                   :by-type {"Ljava/lang/String;" {:count 50
                                                   :bytes 256
                                                   :freed-count 25
                                                   :freed-bytes 128}
                             "[B" {:count 100
                                   :bytes 1024
                                   :freed-count 50
                                   :freed-bytes 512}}}}))))))
    (testing "handles empty by-type gracefully"
      (let [output (with-out-str
                     (view/allocation-by-type*
                      :print
                      {}
                      {:allocation-by-type
                       {:type :criterium/allocation-by-type
                        :by-type {}}}))]
        (is (str/blank? output))))
    (testing "uses custom by-type-id"
      (is (= [""
              "Allocations by Type:"
              "Count        Bytes    Freed  Freed Bytes  Type"
              (apply str (repeat 80 "-"))
              "10          100        5           50  Ljava/lang/Object;"]
             (trimmed-lines
              (with-out-str
                (view/allocation-by-type*
                 :print
                 {:by-type-id :my-by-type}
                 {:my-by-type
                  {:type :criterium/allocation-by-type
                   :by-type {"Ljava/lang/Object;" {:count 10
                                                   :bytes 100
                                                   :freed-count 5
                                                   :freed-bytes 50}}}}))))))))

(deftest allocation-treemap-print-test
  ;; Tests the print viewer output for allocation-treemap results.
  ;; Verifies that ASCII treemap is rendered with proper structure.
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
                      :print
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
                      :print
                      {}
                      {}))]
        (is (str/blank? output))))

    (testing "handles nil root"
      (let [output (with-out-str
                     (view/allocation-treemap*
                      :print
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
                      :print
                      {:treemap-id :my-treemap}
                      {:my-treemap treemap-data}))]
        (is (str/includes? output "type→class→line"))
        (is (str/includes? output "count"))
        (is (str/includes? output "Object"))))))

(deftest kde-print-test
  ;; Tests the print viewer output for KDE analysis results.
  ;; Verifies bandwidth display, mode count, and mode table formatting.
  (testing "kde*"
    (testing "prints KDE summary with bandwidth and modes from separate modes data"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform collect-plan/identity-transforms
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.3 0.1]
                              :lower-band [0.08 0.25 0.08]
                              :upper-band [0.12 0.35 0.12]
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 2.0
                                          :density 0.3
                                          :ci-lower 1.8
                                          :ci-upper 2.2}]
                                 :n-modes 1}}}
            output (with-out-str
                     (view/kde* :print {} {:kde kde-data :modes modes-data}))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Elapsed Time") lines))
        (is (some #(str/includes? % "KDE") lines))
        (is (some #(str/includes? % "n=100") lines))
        (is (some #(str/includes? % "bandwidth") lines))
        (is (some #(str/includes? % "modes: 1") lines))
        (is (some #(str/includes? % "Location") lines))
        (is (some #(str/includes? % "Density") lines))
        (is (some #(str/includes? % "CI Lower") lines))
        (is (some #(str/includes? % "CI Upper") lines))))

    (testing "handles KDE without modes data"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform collect-plan/identity-transforms
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.2 0.1]
                              :lower-band [0.08 0.18 0.08]
                              :upper-band [0.12 0.22 0.12]
                              :n 50}}}
            output (with-out-str
                     (view/kde* :print {} {:kde kde-data}))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "n=50") lines))
        (is (not (some #(str/includes? % "modes:") lines)))))

    (testing "handles missing KDE data"
      (let [output (with-out-str
                     (view/kde* :print {} {}))]
        (is (str/blank? output))))

    (testing "uses custom kde-id and modes-id"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform collect-plan/identity-transforms
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.3
                              :grid [1.0]
                              :density [0.5]
                              :lower-band [0.4]
                              :upper-band [0.6]
                              :n 25}}}
            modes-data {:type :criterium/modes
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 1.0
                                          :density 0.5
                                          :ci-lower 0.9
                                          :ci-upper 1.1}]
                                 :n-modes 1}}}
            output (with-out-str
                     (view/kde* :print {:kde-id :my-kde :modes-id :my-modes}
                                {:my-kde kde-data :my-modes modes-data}))]
        (is (str/includes? output "n=25"))
        (is (str/includes? output "modes: 1"))))))

;;; Modal Analysis Views

(deftest shape-stats-print-test
  ;; Tests the print viewer output for shape-stats results.
  ;; Covers: skewness, kurtosis, and CV with their classifications.
  (testing "shape-stats*"
    (testing "prints shape statistics with classifications"
      (let [data-map (test-data/bootstrap-stats-with-shape-map)
            output (with-out-str (view/shape-stats* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Shape Statistics") lines))
        (is (some #(str/includes? % "skewness") lines))
        (is (some #(str/includes? % "kurtosis") lines))
        (is (some #(str/includes? % "CV") lines))
        (is (some #(str/includes? % "slightly right-skewed") lines))
        (is (some #(str/includes? % "normal tails") lines))
        (is (some #(str/includes? % "low variability") lines))))
    (testing "handles missing bootstrap data gracefully"
      (let [output (with-out-str (view/shape-stats* :print {} {}))]
        (is (str/blank? output))))
    (testing "uses custom bootstrap-stats-id"
      (let [data-map (test-data/bootstrap-stats-with-shape-map)
            custom-map {:my-bootstrap (:bootstrap-stats data-map)}
            output (with-out-str
                     (view/shape-stats* :print {:bootstrap-stats-id :my-bootstrap}
                                        custom-map))]
        (is (str/includes? output "Shape Statistics"))))))

(deftest multimodal-warning-print-test
  ;; Tests the print viewer output for multimodal-warning results.
  ;; Verifies warning display only when n-modes > 1, with aligned output format.
  (testing "multimodal-warning*"
    (testing "displays warning when n-modes > 1"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.3
                                          :significant? true}
                                         {:location 200.0
                                          :density 0.25
                                          :significant? true}]
                                 :n-modes 2}}}
            output (with-out-str
                     (view/multimodal-warning* :print {} {:modes modes-data}))
            lines (trimmed-lines output)]
        (is (= ["Elapsed Time: Multimodal distribution detected"
                "Mode locations: 100 ns, 200 ns"]
               lines))
        (is (str/includes? output "                                  Mode locations:")
            "Mode locations should be indented to align with label")))

    (testing "does not display when n-modes = 1"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.5}]
                                 :n-modes 1}}}
            output (with-out-str
                     (view/multimodal-warning* :print {} {:modes modes-data}))]
        (is (str/blank? output))))

    (testing "does not display when modes data is missing"
      (let [output (with-out-str
                     (view/multimodal-warning* :print {} {}))]
        (is (str/blank? output))))

    (testing "uses custom modes-id"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 50.0
                                          :density 0.4}
                                         {:location 150.0
                                          :density 0.35}]
                                 :n-modes 2}}}
            output (with-out-str
                     (view/multimodal-warning* :print {:modes-id :my-modes}
                                               {:my-modes modes-data}))
            lines (trimmed-lines output)]
        (is (= ["Elapsed Time: Multimodal distribution detected"
                "Mode locations: 50.0 ns, 150 ns"]
               lines))
        (is (str/includes? output "                                  Mode locations:"))))))

;;; Domain Apply View Tests

(defn- make-bench-data
  "Create minimal benchmark data with stats for testing domain-apply."
  [mean-ns]
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
        mean-ns (double mean-ns)]
    {:samples {:type :criterium/collected-metrics-samples
               :metrics-defs metrics-defs
               :metric->values {[:elapsed-time] [mean-ns]}
               :transform collect-plan/identity-transforms
               :batch-size 1
               :num-samples 1
               :eval-count 1
               :elapsed-time mean-ns}
     :stats {:type :criterium/stats
             :metrics-defs metrics-defs
             :transform collect-plan/identity-transforms
             :batch-size 1
             :source-id :samples
             :outliers-id nil
             :stats {:elapsed-time {:mean mean-ns
                                    :variance 1.0
                                    :mean-plus-3sigma (+ mean-ns 3.0)
                                    :mean-minus-3sigma (- mean-ns 3.0)
                                    :min-val mean-ns}}}}))

(deftest domain-apply-print-test
  ;; Tests the print viewer for domain-apply.
  ;; Verifies that each run's coord is printed and the view-spec is applied.
  (testing "domain-apply*"
    (testing "prints coord and applies view-spec to each run"
      (let [domain (-> (domain.types/domain)
                       (domain.types/add-run {:n 100} (make-bench-data 100))
                       (domain.types/add-run {:n 200} (make-bench-data 200)))
            output (with-out-str
                     (view/domain-apply*
                      :print
                      {:view-spec [:stats {}]}
                      {:domain domain}))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "{:n 100}") lines)
            "Should print first coord")
        (is (some #(str/includes? % "{:n 200}") lines)
            "Should print second coord")
        (is (some #(str/includes? % "100 ns") lines)
            "Should show stats for first run")
        (is (some #(str/includes? % "200 ns") lines)
            "Should show stats for second run")))

    (testing "formats coord with Run: prefix"
      (let [domain (domain.types/domain
                    {:coord :baseline :data (make-bench-data 50)})
            output (with-out-str
                     (view/domain-apply*
                      :print
                      {:view-spec [:stats {}]}
                      {:domain domain}))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Run: :baseline") lines)
            "Should format coord with Run: prefix")))

    (testing "uses custom domain-id"
      (let [domain (domain.types/domain
                    {:coord {:impl :foo} :data (make-bench-data 150)})
            output (with-out-str
                     (view/domain-apply*
                      :print
                      {:domain-id :my-domain
                       :view-spec [:stats {}]}
                      {:my-domain domain}))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "{:impl :foo}") lines)
            "Should use custom domain-id")))

    (testing "handles empty domain gracefully"
      (let [domain (domain.types/domain)
            output (with-out-str
                     (view/domain-apply*
                      :print
                      {:view-spec [:stats {}]}
                      {:domain domain}))]
        (is (str/blank? output)
            "Should produce no output for empty domain")))

    (testing "handles missing domain gracefully"
      (let [output (with-out-str
                     (view/domain-apply*
                      :print
                      {:view-spec [:stats {}]}
                      {}))]
        (is (str/blank? output)
            "Should produce no output when domain is missing")))

    (testing "warns when view-spec is missing"
      (let [domain (domain.types/domain
                    {:coord :test :data (make-bench-data 100)})
            stderr-output (java.io.StringWriter.)
            stdout-output (with-out-str
                            (binding [*err* stderr-output]
                              (view/domain-apply*
                               :print
                               {}
                               {:domain domain})))]
        (is (str/blank? stdout-output)
            "Should produce no stdout output")
        (is (str/includes? (str stderr-output)
                           "WARNING: domain-apply requires :view-spec option")
            "Should warn on stderr when view-spec is missing")))))

;;; Domain Extract Table/Chart Tests

(deftest domain-extract-table-print-test
  ;; Tests the print viewer for domain-extract-table.
  ;; Covers single-point, multi-point, and default-table strategies.
  (testing "domain-extract-table*"
    (testing "renders transposed table for single-point strategy"
      (let [output (with-out-str
                     (view/domain-extract-table*
                      :print
                      {}
                      {:extract
                       {:type :criterium/domain-extract
                        :impl-axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :impl :foo} 1e-7]
                                          [{:n 100 :impl :bar} 2e-7]]}}}}))]
        (is (str/includes? output "Domain Extract"))
        (is (str/includes? output "foo"))
        (is (str/includes? output "bar"))))

    (testing "renders regular table for multi-point strategy"
      (let [output (with-out-str
                     (view/domain-extract-table*
                      :print
                      {}
                      {:extract
                       {:type :criterium/domain-extract
                        :impl-axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :impl :foo} 1e-7]
                                          [{:n 200 :impl :foo} 2e-7]
                                          [{:n 100 :impl :bar} 2e-7]
                                          [{:n 200 :impl :bar} 4e-7]]}}}}))]
        (is (str/includes? output "Domain Extract"))
        (is (str/includes? output "100"))
        (is (str/includes? output "200"))))

    (testing "renders regular table for default-table strategy"
      (let [output (with-out-str
                     (view/domain-extract-table*
                      :print
                      {}
                      {:extract
                       {:type :criterium/domain-extract
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :m 1} 1e-7]
                                          [{:n 100 :m 2} 2e-7]]}}}}))]
        (is (str/includes? output "Domain Extract"))
        (is (or (str/includes? output "1:")
                (str/includes? output "2:")))))

    (testing "handles nil extract gracefully"
      (let [output (with-out-str
                     (view/domain-extract-table* :print {} {:extract nil}))]
        (is (str/blank? output))))

    (testing "uses custom extract-id"
      (let [output (with-out-str
                     (view/domain-extract-table*
                      :print
                      {:extract-id :my-extract}
                      {:my-extract
                       {:type :criterium/domain-extract
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100} 1e-7]]}}}}))]
        (is (str/includes? output "Domain Extract"))))))

(deftest domain-extract-chart-print-test
  ;; Tests that print viewer returns nil for charts (text viewers don't render charts).
  (testing "domain-extract-chart*"
    (testing "returns nil for single-point strategy"
      (is (nil? (view/domain-extract-chart*
                 :print
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :impl-axis :impl
                   :implementations [:foo :bar]
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[{:n 100 :impl :foo} 1e-7]
                                     [{:n 100 :impl :bar} 2e-7]]}}}}))))

    (testing "returns nil for multi-point strategy"
      (is (nil? (view/domain-extract-chart*
                 :print
                 {}
                 {:extract
                  {:type :criterium/domain-extract
                   :impl-axis :impl
                   :implementations [:foo :bar]
                   :metrics {:elapsed-time
                             {:metric [:stats :elapsed-time :mean]
                              :data [[{:n 100 :impl :foo} 1e-7]
                                     [{:n 200 :impl :foo} 2e-7]
                                     [{:n 100 :impl :bar} 2e-7]
                                     [{:n 200 :impl :bar} 4e-7]]}}}}))))

    (testing "returns nil for nil extract"
      (is (nil? (view/domain-extract-chart* :print {} {:extract nil}))))))

;;; Domain Comparison Table/Chart Tests

(deftest domain-comparison-table-print-test
  ;; Tests the print viewer for domain-comparison-table.
  ;; Covers single-point, multi-point, and default-table strategies.
  (testing "domain-comparison-table*"
    (testing "renders transposed table for single-point strategy"
      (let [output (with-out-str
                     (view/domain-comparison-table*
                      :print
                      {}
                      {:comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data
                        {:foo [{:coord {:impl :foo} :value 1e-7}]
                         :bar [{:coord {:impl :bar} :value 2e-7}]}}}))]
        (is (str/includes? output "Domain Comparison"))
        (is (str/includes? output "foo"))
        (is (str/includes? output "bar"))))

    (testing "renders comparison table for multi-point strategy"
      (let [output (with-out-str
                     (view/domain-comparison-table*
                      :print
                      {}
                      {:comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data
                        {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}
                               {:coord {:n 200 :impl :foo} :value 2e-7}]
                         :bar [{:coord {:n 100 :impl :bar} :value 2e-7}
                               {:coord {:n 200 :impl :bar} :value 4e-7}]}}}))]
        (is (str/includes? output "Domain Comparison"))
        (is (str/includes? output "100"))
        (is (str/includes? output "200"))))

    (testing "renders table for default-table strategy"
      (let [output (with-out-str
                     (view/domain-comparison-table*
                      :print
                      {}
                      {:comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :data
                        {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}]
                         :bar [{:coord {:n 100 :impl :bar} :value 2e-7}]}}}))]
        (is (str/includes? output "Domain Comparison"))))

    (testing "handles nil comparison gracefully"
      (let [output (with-out-str
                     (view/domain-comparison-table* :print {} {:comparison nil}))]
        (is (str/blank? output))))

    (testing "uses custom comparison-id"
      (let [output (with-out-str
                     (view/domain-comparison-table*
                      :print
                      {:comparison-id :my-comparison}
                      {:my-comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data
                        {:foo [{:coord {:impl :foo} :value 1e-7}]
                         :bar [{:coord {:impl :bar} :value 2e-7}]}}}))]
        (is (str/includes? output "Domain Comparison"))))))

(deftest domain-comparison-chart-print-test
  ;; Tests that print viewer returns nil for charts (text viewers don't render charts).
  (testing "domain-comparison-chart*"
    (testing "returns nil for single-point strategy"
      (is (nil? (view/domain-comparison-chart*
                 :print
                 {}
                 {:comparison
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :implementations [:foo :bar]
                   :data
                   {:foo [{:coord {:impl :foo} :value 1e-7}]
                    :bar [{:coord {:impl :bar} :value 2e-7}]}}}))))

    (testing "returns nil for multi-point strategy"
      (is (nil? (view/domain-comparison-chart*
                 :print
                 {}
                 {:comparison
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :implementations [:foo :bar]
                   :data
                   {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}
                          {:coord {:n 200 :impl :foo} :value 2e-7}]
                    :bar [{:coord {:n 100 :impl :bar} :value 2e-7}
                          {:coord {:n 200 :impl :bar} :value 4e-7}]}}}))))

    (testing "returns nil for nil comparison"
      (is (nil? (view/domain-comparison-chart* :print {} {:comparison nil}))))))

;;; Tail Analysis View Tests

(deftest tail-analysis-print-test
  ;; Tests the print viewer output for tail-analysis results.
  ;; Verifies display of tail ratios, Hill estimate, GPD fit, and high quantiles.
  (testing "tail-analysis*"
    (testing "prints tail ratios with percentile values"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-analysis* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Tail Analysis:") lines))
        (is (some #(str/includes? % "tail ratios") lines))
        (is (some #(str/includes? % "p99/p95") lines))
        (is (some #(str/includes? % "p999/p99") lines))))

    (testing "prints Hill estimate with k range"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-analysis* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "tail index") lines))
        (is (some #(str/includes? % "Hill estimate") lines))
        (is (some #(str/includes? % "0.82") lines)
            "Should show stable estimate value")))

    (testing "prints GPD fit parameters"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-analysis* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "GPD fit") lines))
        (is (some #(str/includes? % "ξ=") lines) "Should show xi parameter")
        (is (some #(str/includes? % "σ=") lines) "Should show sigma parameter")
        (is (some #(str/includes? % "threshold") lines))))

    (testing "prints high quantile estimates"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-analysis* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "High quantile estimates") lines))
        (is (some #(str/includes? % "p99") lines) "Should show p99 estimate")
        (is (some #(str/includes? % "p99.9") lines) "Should show p99.9 estimate")))

    (testing "handles missing tail analysis gracefully"
      (let [output (with-out-str (view/tail-analysis* :print {} {}))]
        (is (str/blank? output))))

    (testing "uses custom tail-analysis-id"
      (let [data-map (test-data/tail-analysis-data-map)
            custom-map {:my-tail (:tail-analysis data-map)}
            output (with-out-str
                     (view/tail-analysis* :print
                                          {:tail-analysis-id :my-tail}
                                          custom-map))]
        (is (str/includes? output "Tail Analysis:"))))))
