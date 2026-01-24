(ns criterium.viewer.print.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.analyse.metrics-samples :as metrics-samples]
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-data :as test-data]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.print.core :as print-core]))

;;; Transforms wrapped in vector format expected by transform-sample->
(def vectorized-identity-transforms
  (util/update-vals collect-plan/identity-transforms vector))

;;; Metrics Tests

(deftest print-metrics-non-numeric-test
  ;; Tests that the print viewer handles non-numeric metric values gracefully
  ;; instead of throwing an exception when trying to multiply or format.
  (testing "print-metrics"
    (testing "handles non-numeric metric values"
      (is (= ["Elapsed Time: unavailable"]
             (trimmed-lines
              (with-out-str
                (view/metrics*
                 :print
                 {}
                 (:data (test-data/samples-with-non-numeric-value-map))))))))))

;;; Stats Tests

(deftest print-stat-test
  ;; Tests print-stat function for basic statistics display.
  ;; Covers: mean, 3-sigma bounds, and min value with SI unit formatting.
  (testing "print-stat"
    (is (= "Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"
           (str/trim
            (with-out-str
              (print-core/print-stat
               {:label "Elapsed Time"
                :scale 1e-9
                :dimension :time}
               {:mean 100.0
                :variance 16.0
                :mean-plus-3sigma 112.0
                :mean-minus-3sigma 88.0
                :min-val 89.0}
               [collect-plan/identity-transforms])))))))

(deftest print-stats-test
  ;; Tests print-stats function and view/stats* multimethod.
  ;; Covers: stats display with and without batch-size transforms.
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

    (testing "applies batch-size transform to per-execution values"
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

;;; Extremes Tests

(deftest print-extreme-test
  ;; Tests print-extreme function for min/max display.
  ;; Covers: SI unit formatting and value display.
  (testing "print-extreme"
    (testing "prints min and max values with SI units"
      (is (= "Elapsed Time: 89.0 ns - 114 ns"
             (str/trim
              (with-out-str
                (print-core/print-extreme
                 {:label "Elapsed Time"
                  :scale 1e-9
                  :dimension :time}
                 {:min-val 89.0
                  :max-val 114.0}
                 [collect-plan/identity-transforms]))))))))

(deftest print-extremes-test
  ;; Tests print-extremes function and view/extremes* multimethod.
  ;; Covers: min/max display for quantitative metrics.
  (testing "print-extremes"
    (testing "prints via output-view"
      (is (= ["Extremes:"
              "Elapsed Time: 89.0 ns - 114 ns"]
             (trimmed-lines
              (with-out-str
                (view/extremes*
                 :print
                 {}
                 (:data (test-data/bench-stats-map))))))))

    (testing "applies batch-size transform to per-execution values"
      (is (= ["Extremes:"
              "Elapsed Time: 0.500 ns - 4.50 ns"]
             (let [data-map
                   (-> (update-in
                        (:data (test-data/samples-with-variance-12-map))
                        [:samples]
                        merge
                        {:batch-size 2
                         :transform (#'collect-plan/batch-transforms 2)}))
                   stats (analyse/stats)
                   view-extremes (view/extremes)]
               (trimmed-lines
                (with-out-str
                  (->> data-map
                       stats
                       (view-extremes :print))))))))))

;;; Bootstrap Tests

(deftest print-bootstrap-stat-test
  ;; Tests print-bootstrap-stat function for bootstrap statistics display.
  ;; Covers: median with CI (first), mean with CI, and spread (10th-90th percentile).
  (testing "print-bootstrap-stat"
    (testing "without quantiles only prints mean"
      (is (= ["Elapsed Time mean: 100 ns CI [95.0 105] (0.050 0.950)"]
             (trimmed-lines
              (with-out-str
                (print-core/print-bootstrap-stat
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
                (print-core/print-bootstrap-stat
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

;;; Samples Tests

(deftest print-samples-test
  ;; Tests view/samples* for printing samples with outliers.
  ;; Covers: sample count, batch-size, and outlier display.
  (testing "print-samples"
    (testing "prints via view"
      (is (= ["Samples: 7 samples with batch-size 1"
              "Elapsed Time"
              "[    6] 10.0 µs high-severe"]
             (let [bench-map
                   (:data (test-data/samples-with-outliers-values-map))
                   quantiles (analyse/quantiles {:quantiles [0.9 0.99]})
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

;;; Outlier Tests

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
              (with-out-str (print-core/print-outlier-count
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
                (print-core/print-outlier-count
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
      (testing "with symmetric distribution (mc = 0)"
        (is (= ["M: Found 1 outliers in 100 samples (1.00 %)"
                "high-severe\t 1 (1.0000 %)"
                "M: medcouple 0.0000 (symmetric)"]
               (trimmed-lines
                (with-out-str
                  (print-core/print-outlier-count
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
                  (print-core/print-outlier-count
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
                  (print-core/print-outlier-count
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
                  (print-core/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 0 0 0 1)
                    :medcouple nil}
                   true))))))
      (testing "displays medcouple even without outliers"
        (is (= ["M: medcouple 0.1500 (slightly right-skewed)"]
               (trimmed-lines
                (with-out-str
                  (print-core/print-outlier-count
                   {:label "M"}
                   100
                   {:outlier-counts (metrics-samples/outlier-count 0 0 0 0)
                    :medcouple 0.15}
                   true)))))))))

(deftest print-outlier-significance-test
  ;; Tests print-outlier-significance function and view/outlier-significance*.
  ;; Covers: variance contribution and effect classification.
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [(str "Elapsed Time Variance contribution from outliers : 25.0 %"
                   "Elapsed Time Variance is moderately inflated by outliers")]
             (trimmed-lines
              (with-out-str
                ((view/outlier-significance)
                 :print
                 (:data (test-data/outlier-significance-map))))))))))

;;; Event Stats Tests

(deftest print-event-stats-test
  ;; Tests print-event-stats function and view/event-stats* multimethod.
  ;; Covers: class loader, JIT compilation, and GC event stats.
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

;;; Final GC Warnings Tests

(deftest print-final-gc-warnings-test
  ;; Tests print-final-gc-warnings function and view/final-gc-warnings*.
  ;; Covers: GC time as percentage of total sampling time.
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

;;; OS and Runtime Tests

(deftest print-os-test
  ;; Tests view/os* for printing OS information.
  (testing "os*"
    (let [s (with-out-str ((view/os) :print {}))]
      (is (str/ends-with? s "cpu(s)\n")))))

(deftest print-runtime-test
  ;; Tests view/runtime* for printing JVM runtime information.
  (testing "runtime*"
    (let [s (with-out-str ((view/runtime) :print {}))]
      (is (not (str/blank? s))))))

;;; KDE Tests

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
