(ns criterium.viewer.portal.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.array :as arr]
   [criterium.collector.metrics :as metrics]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.portal.core :as portal-core])
  (:import
   [java.util Queue]))

(set! *unchecked-math* false)

;;; Test Infrastructure

(defmacro with-tap-out
  "Capture tapped values during body execution.
   Optional expected-count parameter waits for that many values."
  ([& body]
   `(with-tap-out* 1 (fn [] ~@body))))

(defn with-tap-out*
  "Implementation for with-tap-out macro.
   Waits for expected-count taps to be received."
  [expected-count body-fn]
  (let [v (volatile! [])
        f (fn [x]
            (when-not (= :criterium.viewer.portal/_ x)
              (vswap! v conj x)))]
    (try
      (add-tap f)
      (body-fn)
      ;; Wait for tap queue to drain
      (loop []
        (when-not (.isEmpty ^Queue @#'clojure.core/tapq)
          (recur)))
      ;; Wait for expected number of values
      (loop [attempts (long 0)]
        (when (and (< (count @v) (long expected-count))
                   (< attempts 10000))
          (recur (inc attempts))))
      (portal-core/flush)
      @v
      (finally
        (remove-tap f)))))

(defmacro with-tap-out-n
  "Capture tapped values during body execution, waiting for n values."
  [n & body]
  `(with-tap-out* ~n (fn [] ~@body)))

;;; Samples Tests

(deftest portal-samples-test
  ;; Tests the portal viewer output for samples chart.
  ;; Verifies Vega-Lite spec generation with sample data points.
  (testing "view/samples*"
    (testing "charts the sample data"
      (let [[title chart] (with-tap-out
                            (view/samples*
                             :portal
                             {}
                             (:data (test-data/samples-with-2-values-map))))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 1.0, :index 1, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Samples"] title))))
    (testing "charts with transformed sample data"
      (let [[title chart] (with-tap-out
                            (view/samples*
                             :portal
                             {}
                             (:data
                              (test-data/samples-with-transformed-values-map))))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 2.0, :index 1, :outlier ""}
                {:elapsed-time 4.0, :index 2, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Samples"] title))))
    (testing "charts the sample data"
      (let [bench-map (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99]})
            outliers (analyse/outliers)
            stats (analyse/stats)
            view (view/samples)
            [title chart] (with-tap-out
                            (->> bench-map
                                 quantiles
                                 outliers
                                 stats
                                 (view :portal)))]
        (is (= [{:elapsed-time 9.0, :index 0, :outlier ""}
                {:elapsed-time 10.0, :index 1, :outlier ""}
                {:elapsed-time 9.0, :index 2, :outlier ""}
                {:elapsed-time 10.0, :index 3, :outlier ""}
                {:elapsed-time 9.0, :index 4, :outlier ""}
                {:elapsed-time 10.0, :index 5, :outlier ""}
                {:elapsed-time 10000.0, :index 6, :outlier :high-severe}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Samples"] title))))))

;;; Sample Percentiles Tests

(deftest portal-sample-percentiles-test
  ;; Tests the portal viewer output for sample percentiles chart.
  (testing "view/sample-percentiles*"
    (testing "charts the sample data"
      (let [[title chart] (with-tap-out
                            (view/sample-percentiles*
                             :portal
                             {}
                             (:data (test-data/samples-with-2-values-map))))]
        (is (= [{:elapsed-time 1.0, :x 0.0, :p 0.0}
                {:elapsed-time 1.0, :x 1.0, :p 100.0}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Percentiles"] title))))))

;;; Histogram Tests

(deftest portal-histogram-test
  ;; Tests the portal viewer output for histogram chart.
  (testing "view/histogram*"
    (testing "charts the sample data"
      (let [data-map
            (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99]})
            outliers (analyse/outliers)
            stats (analyse/stats)
            histogram (analyse/histogram)
            view-histogrem (view/histogram)
            [title chart] (with-tap-out
                            (->> data-map
                                 quantiles
                                 outliers
                                 stats
                                 histogram
                                 (view-histogrem :portal)))]
        (let [histogram-data (-> chart :vconcat first :layer first :data :values)]
          (is (vector? histogram-data))
          (is (pos? (count histogram-data)))
          (is (every? #(and (contains? % "elapsed-time")
                            (contains? % "end")
                            (contains? % "density"))
                      histogram-data)))
        (is (= [:b "Histogram"] title))))))

;;; Stats Tests

(deftest portal-stats-test
  ;; Verifies stats display with conditional heading behavior.
  (testing "view/stats*"
    (testing "displays stats when metrics match"
      (is (= [[:b "Summary stats"]
              [{:_metric "Elapsed Time ns",
                :mean 100.0
                :min-val 89.0
                :mean-minus-3sigma 88.0
                :mean-plus-3sigma 112.0
                :max-val 114.0}]]
             (with-tap-out
               (view/stats*
                :portal
                {}
                (:data (test-data/bench-stats-map))))))

      (is (= [[:b "Summary stats"]
              [{:_metric "Elapsed Time ns",
                :mean 1.00,
                :min-val 1.00,
                :mean-minus-3sigma 1.00,
                :mean-plus-3sigma 1.00,
                :max-val 1.00}]]
             (let [data-map
                   (:data (test-data/samples-with-2-values-map))
                   stats (analyse/stats)
                   view-stats (view/stats)]
               (with-tap-out
                 (->> data-map
                      stats
                      (view-stats :portal)))))))
    (testing "outputs nothing when metric-ids filter yields no matching metrics"
      (let [v (volatile! [])
            f (fn [x] (when-not (= :criterium.viewer.portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/stats*
           :portal
           {:metric-ids [:nonexistent-metric]}
           (:data (test-data/bench-stats-map)))
          (portal-core/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

;;; Extremes Tests

(deftest portal-extremes-test
  ;; Verifies extremes display with conditional heading behavior.
  (testing "view/extremes*"
    (testing "displays extremes when metrics match"
      (is (= [[:b "Extremes"]
              [{:metric "Elapsed Time ns",
                :min 89.0
                :max 114.0}]]
             (with-tap-out
               (view/extremes*
                :portal
                {}
                (:data (test-data/bench-stats-map)))))))
    (testing "outputs nothing when metric-ids filter yields no matching metrics"
      (let [v (volatile! [])
            f (fn [x] (when-not (= :criterium.viewer.portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/extremes*
           :portal
           {:metric-ids [:nonexistent-metric]}
           (:data (test-data/bench-stats-map)))
          (portal-core/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

;;; Outlier Tests

(deftest portal-outlier-count-test
  ;; Tests the portal viewer output for outlier counts.
  (testing "view/outlier-counts*"
    (testing "prints via view"
      (is
       (= [[:b "Outliers"]
           [{:low-severe 0,
             :low-mild 2,
             :high-mild 3,
             :high-severe 0,
             :_metric "Elapsed Time"}]]
          (with-tap-out
            ((view/outlier-counts)
             :portal
             (:data (test-data/outlier-count-map)))))))))

(deftest portal-outlier-significance-test
  ;; Tests the portal viewer output for outlier significance.
  (testing "view/outlier-significance*"
    (testing "prints via view"
      (is (= [[:b "Outlier Significance"]
              [{:effect :moderate :significance 0.25}]]
             (with-tap-out
               ((view/outlier-significance)
                :portal
                (:data (test-data/outlier-significance-map)))))))))

;;; Event Stats Tests

(deftest portal-event-stats-test
  ;; Tests the portal viewer output for event stats.
  (testing "view/event-stats*"
    (testing "prints via report"
      (is (= [[:b "Event stats"]
              [{:metric "ClassLoader",
                :sample-count "1.0",
                :loaded-count "1.0",
                :unloaded-count "1.0"}
               {:metric "JIT compilation",
                :sample-count "1.0",
                :time-ms "3.00 ms"}
               {:metric "Garbage Collector",
                :total-sample-count "1.0",
                :total-count "2.0",
                :total-time-ms "1.00 ms"}]]
             (let [data-map (:data (test-data/samples-for-event-stats-map))
                   event-stats (analyse/event-stats)
                   view (view/event-stats)]
               (with-tap-out
                 (->> data-map
                      event-stats
                      (view :portal)))))))))

;;; Metrics Tests

(deftest portal-metrics-non-numeric-test
  ;; Tests that the portal viewer handles non-numeric metric values gracefully
  ;; instead of throwing an exception when coercing to double.
  (testing "view/metrics*"
    (testing "handles non-numeric metric values"
      (is (= [[{:metric "Elapsed Time" :value "unavailable"}
               {:metric "Expr value" :value nil}]]
             (with-tap-out
               (view/metrics*
                :portal
                {}
                (:data (test-data/samples-with-non-numeric-value-map)))))))))

;;; KDE Tests

(deftest portal-kde-test
  ;; Tests the portal viewer output for KDE analysis results.
  ;; Verifies Vega-Lite spec generation with density curve and optional histogram overlay.
  (testing "view/kde*"
    (testing "produces vega-lite output with kde spec"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.3 0.1]
                              :lower-band [0.08 0.25 0.08]
                              :upper-band [0.12 0.35 0.12]
                              :modes [{:location 2.0
                                       :density 0.3
                                       :ci-lower 1.8
                                       :ci-upper 2.2}]
                              :n 100}}}
            [title vega-spec] (with-tap-out
                                (view/kde* :portal {} {:kde kde-data}))
            viewer-meta (meta vega-spec)]
        (is (= [:b "Kernel Density Estimation"] title))
        (is (= :portal.viewer/vega-lite (:portal.viewer/default viewer-meta)))
        (is (str/includes? (:$schema vega-spec) "vega-lite"))
        (is (contains? vega-spec :vconcat))
        (is (vector? (:vconcat vega-spec)))
        (let [first-chart (first (:vconcat vega-spec))
              layers (:layer first-chart)
              ;; KDE layers are nested in a group for independent Y-scale
              kde-group (first layers)
              kde-layers (:layer kde-group)]
          (is (contains? first-chart :layer))
          (is (>= (count kde-layers) 2)
              "Expected at least confidence band and density layers in nested group"))))

    (testing "handles missing kde data gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= :criterium.viewer.portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/kde* :portal {} {})
          (portal-core/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "uses custom kde-id"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.3
                              :grid [1.0]
                              :density [0.5]
                              :lower-band [0.4]
                              :upper-band [0.6]
                              :modes []
                              :n 25}}}
            [title _spec] (with-tap-out
                            (view/kde* :portal {:kde-id :my-kde} {:my-kde kde-data}))]
        (is (= [:b "Kernel Density Estimation"] title))))))

;;; Bootstrap Stats Tests

(deftest portal-bootstrap-stats-test
  ;; Tests the portal viewer output for bootstrap-stats results.
  ;; Verifies table generation with median-first column ordering.
  (testing "view/bootstrap-stats*"
    (testing "produces table with median first, then mean, CI bounds, percentiles"
      (let [data-map {:samples
                      {:type :criterium/metrics-samples
                       :metric->values {[:elapsed-time]
                                        (arr/->double-array (double-array [1 1 1]))}
                       :metrics-defs (select-keys
                                      (metrics/metrics)
                                      [:elapsed-time])
                       :transform {:sample-> identity :->sample identity}
                       :batch-size 1
                       :eval-count 1
                       :elapsed-time 1}}
            bootstrap-fn (analyse/bootstrap-stats
                          {:quantiles [0.025 0.975]
                           :estimate-quantiles [0.025 0.975]})
            view-fn (view/bootstrap-stats {})
            [title table] (with-tap-out-n 2
                            (->> data-map
                                 bootstrap-fn
                                 (view-fn :portal)))]
        (is (= [:b "Bootstrap Statistics"] title))
        (is (= 1 (count table)) "Expected 1 row for elapsed-time metric")
        (let [row (first table)]
          (is (= "Elapsed Time" (:metric row)))
          (is (contains? row :median))
          (is (contains? row :median-ci-lower))
          (is (contains? row :median-ci-upper))
          (is (contains? row :mean))
          (is (contains? row :mean-ci-lower))
          (is (contains? row :mean-ci-upper))
          (is (contains? row :p10))
          (is (contains? row :p90)))))))

;;; OS Info Tests

(deftest portal-os-test
  ;; Tests the portal viewer output for OS information.
  ;; Verifies table with name, version, architecture, and processor count.
  (testing "view/os*"
    (testing "displays OS info table"
      (let [[title table] (with-tap-out
                            (view/os* :portal {} {}))]
        (is (= [:b "Operating System"] title))
        (is (= 4 (count table)) "Expected 4 rows")
        (is (= "Name" (:property (nth table 0))))
        (is (= "Version" (:property (nth table 1))))
        (is (= "Architecture" (:property (nth table 2))))
        (is (= "Processors" (:property (nth table 3))))
        (is (string? (:value (nth table 0))))
        (is (number? (:value (nth table 3))))))))

;;; Runtime Info Tests

(deftest portal-runtime-test
  ;; Tests the portal viewer output for runtime information.
  ;; Verifies table with VM name, version, vendor, and arguments.
  (testing "view/runtime*"
    (testing "displays runtime info table"
      (let [[title table] (with-tap-out
                            (view/runtime* :portal {} {}))]
        (is (= [:b "Runtime"] title))
        (is (= 4 (count table)) "Expected 4 rows")
        (is (= "VM Name" (:property (nth table 0))))
        (is (= "VM Version" (:property (nth table 1))))
        (is (= "VM Vendor" (:property (nth table 2))))
        (is (= "Arguments" (:property (nth table 3))))
        (is (string? (:value (nth table 0))))
        (is (string? (:value (nth table 3))))))))

;;; Final GC Warnings Tests

(deftest portal-final-gc-warnings-test
  ;; Tests the portal viewer output for final GC warnings.
  ;; Verifies warning displays when GC time exceeds threshold.
  (testing "view/final-gc-warnings*"
    (testing "displays warning when GC exceeds threshold"
      (let [data-map (:data (test-data/final-gc-warning-map))
            [title table] (with-tap-out
                            (view/final-gc-warnings*
                             :portal
                             {:warn-threshold 0.01}
                             data-map))]
        (is (= [:b "Final GC Warning"] title))
        (is (= 1 (count table)))
        (is (str/includes? (:warning (first table)) "Final GC ran for"))))

    (testing "outputs nothing when GC below threshold"
      (let [v (volatile! [])
            f (fn [x] (when-not (= :criterium.viewer.portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/final-gc-warnings*
           :portal
           {:warn-threshold 0.99}
           (:data (test-data/final-gc-warning-map)))
          (portal-core/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))
