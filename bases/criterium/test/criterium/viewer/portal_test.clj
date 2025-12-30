(ns criterium.viewer.portal-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collector.metrics]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.portal :as portal])
  (:import
   [java.util Queue]))

(set! *unchecked-math* false)

(defmacro with-tap-out [& body]
  `(let [v# (volatile! [])
         f# (fn [x#]
              (when-not (= ::portal/_ x#)
                (vswap! v# conj x#)))]
     (try
       (add-tap f#)
       ~@body
       (loop []
         (when-not (.isEmpty ^Queue @#'clojure.core/tapq)
           (recur)))
       (loop []
         (when (empty? @v#)
           (recur)))
       (portal/flush)
       @v#
       (finally
         (remove-tap f#)))))

(deftest portal-samples-test
  (testing "portal-samples"
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
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
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

(deftest portal-sample-percentiles-test
  (testing "portal-percentiles"
    (testing "charts the sample data"
      (let [[title chart] (with-tap-out
                            (view/sample-percentiles*
                             :portal
                             {}
                             (:data (test-data/samples-with-2-values-map))))]
        (is (= [{:elapsed-time 1.0, :x 0.0, :p 0}
                {:elapsed-time 1.0, :x 1.0, :p 100.0}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Percentiles"] title))))))

(deftest portal-histogram-test
  (testing "portal-histogram"
    (testing "charts the sample data"
      (let [data-map
            (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
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
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/stats*
           :portal
           {:metric-ids [:nonexistent-metric]}
           (:data (test-data/bench-stats-map)))
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-outlier-count-test
  (testing "print-outlier-count"
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
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [[:b "Outlier Significance"]
              [{:effect :moderate :significance 0.25}]]
             (with-tap-out
               ((view/outlier-significance)
                :portal
                (:data (test-data/outlier-significance-map)))))))))

(deftest portal-event-stats-test
  (testing "print-event-stats"
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

;; Tests that the portal viewer handles non-numeric metric values gracefully
;; instead of throwing an exception when coercing to double.
(deftest portal-metrics-non-numeric-test
  (testing "portal-metrics"
    (testing "handles non-numeric metric values"
      (is (= [[{:metric "Elapsed Time" :value "unavailable"}
               {:metric "Expr value" :value nil}]]
             (with-tap-out
               (view/metrics*
                :portal
                {}
                (:data (test-data/samples-with-non-numeric-value-map)))))))))

;;; Domain View Tests

(deftest portal-domain-extract-test
  ;; Tests the portal viewer output for domain-extract results.
  ;; Verifies table generation with coordinate columns and metric columns.
  ;; New implementation provides table output with SI scaling like kindly viewer.
  ;; Uniform axes (same value across all coords) are stripped for cleaner display.
  (testing "domain-extract*"
    (testing "produces table with single-key coords"
      (let [[title table] (with-tap-out
                            (view/domain-extract*
                             :portal
                             {}
                             {:extract
                              {:type :criterium/domain-extract
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data [[{:n 100} 1e-7]
                                                 [{:n 200} 2e-7]
                                                 [{:n 400} 4e-7]]}}}}))]
        (is (= [:b "Domain Extract"] title))
        (is (= 3 (count table)) "Expected 3 rows")
        (is (every? #(contains? % :n) table)
            "Expected :n column for single-key coords")
        (is (= [100 200 400] (mapv :n table))
            "Expected rows sorted by n")))

    (testing "strips uniform axes leaving single-key coords"
      ;; When one axis is uniform (same value in all coords), it's stripped
      ;; leaving the varying axis as a single-key display
      (let [[title table] (with-tap-out
                            (view/domain-extract*
                             :portal
                             {}
                             {:extract
                              {:type :criterium/domain-extract
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data [[{:n 100 :m 1} 1e-7]
                                                 [{:n 100 :m 2} 2e-7]]}}}}))]
        (is (= [:b "Domain Extract"] title))
        (is (= 2 (count table)))
        (is (every? #(contains? % :m) table)
            "Expected :m column after stripping uniform :n axis")
        (is (= [1 2] (mapv :m table))
            "Expected rows with stripped :m values")))

    (testing "produces table with multi-key coords when multiple axes vary"
      (let [[title table] (with-tap-out
                            (view/domain-extract*
                             :portal
                             {}
                             {:extract
                              {:type :criterium/domain-extract
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data [[{:n 100 :m 1} 1e-7]
                                                 [{:n 200 :m 2} 2e-7]]}}}}))]
        (is (= [:b "Domain Extract"] title))
        (is (= 2 (count table)))
        (is (every? #(contains? % :coordinate) table)
            "Expected :coordinate column when multiple axes vary")))

    (testing "handles nil values"
      (let [[_title table] (with-tap-out
                             (view/domain-extract*
                              :portal
                              {}
                              {:extract
                               {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} nil]
                                                  [{:n 200} 1e-7]]}}}}))]
        (is (= 2 (count table)))))

    (testing "uses custom extract-id"
      (let [[title _table] (with-tap-out
                             (view/domain-extract*
                              :portal
                              {:extract-id :my-extract}
                              {:my-extract
                               {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e-9]]}}}}))]
        (is (= [:b "Domain Extract"] title))))))

(deftest portal-domain-grouped-test
  ;; Tests the portal viewer output for domain-grouped results.
  ;; Verifies table generation with axis values and run counts.
  (testing "domain-grouped*"
    (testing "produces table with axis values and run counts"
      (let [[title table] (with-tap-out
                            (view/domain-grouped*
                             :portal
                             {}
                             {:grouped
                              {:type :criterium/domain-grouped
                               :axis :impl
                               :data {:foo {:type :criterium/domain
                                            :runs [{} {}]}
                                      :bar {:type :criterium/domain
                                            :runs [{}]}
                                      nil {:type :criterium/domain
                                           :runs [{}]}}}}))]
        (is (= [:b "Domain Grouped by: impl"] title))
        (is (= [{:axis-value "<nil>" :run-count 1}
                {:axis-value ":bar" :run-count 1}
                {:axis-value ":foo" :run-count 2}]
               table))))
    (testing "uses custom grouped-id"
      (let [[title _table] (with-tap-out
                             (view/domain-grouped*
                              :portal
                              {:grouped-id :by-n}
                              {:by-n
                               {:type :criterium/domain-grouped
                                :axis :n
                                :data {100 {:type :criterium/domain
                                            :runs [{}]}}}}))]
        (is (= [:b "Domain Grouped by: n"] title))))))

(deftest portal-domain-comparison-test
  ;; Tests the portal viewer output for domain-comparison results.
  ;; Verifies table generation with axis values as columns.
  ;; New implementation provides table output with SI scaling like kindly viewer.
  (testing "domain-comparison*"
    (testing "produces table with comparison data"
      (let [[title table] (with-tap-out
                            (view/domain-comparison*
                             :portal
                             {}
                             {:comparison
                              {:type :criterium/domain-comparison
                               :axis :impl
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:impl :foo :n 100}
                                                        :value 1e-7}]
                                                 :bar [{:coord {:impl :bar :n 100}
                                                        :value 2e-7}]}}}}}))]
        (is (string? (second title)))
        (is (str/includes? (second title) "Domain Comparison"))
        (is (= 1 (count table)) "Expected 1 row for single n value")
        (is (contains? (first table) "n")
            "Expected \"n\" column for single-key coord")))

    (testing "with implementations shows factors for non-baseline"
      (let [[title table] (with-tap-out
                            (view/domain-comparison*
                             :portal
                             {}
                             {:comparison
                              {:type :criterium/domain-comparison
                               :axis :impl
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:impl :foo :n 100}
                                                        :value 1e-7}
                                                       {:coord {:impl :foo :n 200}
                                                        :value 2e-7}]
                                                 :bar [{:coord {:impl :bar :n 100}
                                                        :value 2e-7}
                                                       {:coord {:impl :bar :n 200}
                                                        :value 4e-7}]}}}
                               :implementations [:foo :bar]}}))]
        (is (str/includes? (second title) "Domain Comparison"))
        (is (= 2 (count table)) "Expected 2 rows")
        ;; Check that baseline impl has absolute values and factor impl has ×
        (let [first-row (first table)
              col-keys (set (map str (keys first-row)))]
          (is (some #(str/includes? % "foo") col-keys)
              "Expected foo column")
          (is (some #(and (str/includes? % "bar")
                          (str/includes? % "×"))
                    col-keys)
              "Expected bar column with × for factors"))))

    (testing "handles nil comparison gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-comparison* :portal {} {:comparison nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-domain-regression-test
  ;; Tests the portal viewer output for domain-regression results.
  ;; Verifies table generation with model equations, and chart output with
  ;; scatter plots and fit curves.
  (testing "domain-regression*"
    (testing "produces table and chart with regression models"
      (let [outputs (with-tap-out
                      (view/domain-regression*
                       :portal
                       {}
                       {:extract {:type :criterium/domain-extract
                                  :metrics {:elapsed-time
                                            {:metric [:stats :elapsed-time :mean]
                                             :data [[{:n 100} 1e6]
                                                    [{:n 200} 2e6]
                                                    [{:n 400} 4e6]
                                                    [{:n 800} 8e6]]}}}
                        :regression {:type :criterium/domain-regression
                                     :axis :n
                                     :regressions {:elapsed-time
                                                   {:metric [:stats :elapsed-time :mean]
                                                    :models [{:id :linear
                                                              :label "O(n)"
                                                              :coefficients {:a 10000.0 :b 0.0}
                                                              :equation-str "y = 10000*n + 0"
                                                              :predict-fn (fn [x] (* 10000.0 x))
                                                              :r-squared 0.9999}
                                                             {:id :quadratic
                                                              :label "O(n²)"
                                                              :coefficients {:a 0.1 :b 100000.0}
                                                              :equation-str "y = 0.1*n² + 100000"
                                                              :predict-fn (fn [x] (+ (* 0.1 x x) 100000.0))
                                                              :r-squared 0.85}]
                                                    :best-fit :linear}}}}))]
        (is (>= (count outputs) 2) "Expected at least heading and table")
        (let [[heading table] outputs]
          (is (= :b (first heading)))
          (is (str/includes? (second heading) "Domain Regression"))
          (is (= 2 (count table)) "Expected 2 rows for 2 models")
          (is (every? #(contains? % :model) table))
          (is (every? #(contains? % :r-squared) table))
          (is (every? #(contains? % :equation) table)))))

    (testing "renders chart when extract data available"
      (let [outputs (with-tap-out
                      (view/domain-regression*
                       :portal
                       {}
                       {:extract {:type :criterium/domain-extract
                                  :metrics {:elapsed-time
                                            {:metric [:stats :elapsed-time :mean]
                                             :data [[{:n 100} 1e6]
                                                    [{:n 200} 2e6]
                                                    [{:n 400} 4e6]]}}}
                        :regression {:type :criterium/domain-regression
                                     :axis :n
                                     :regressions {:elapsed-time
                                                   {:metric [:stats :elapsed-time :mean]
                                                    :models [{:id :linear
                                                              :label "O(n)"
                                                              :coefficients {:a 10000.0 :b 0.0}
                                                              :equation-str "y = 10000*n + 0"
                                                              :predict-fn (fn [x] (* 10000.0 x))
                                                              :r-squared 0.9999}]
                                                    :best-fit :linear}}}}))]
        ;; Should have heading, table, chart, residual heading, residual chart
        (is (>= (count outputs) 4) "Expected heading, table, chart, residual outputs")
        ;; Find a vega-lite chart
        (let [charts (filter #(and (map? %) (contains? % :layer)) outputs)]
          (is (>= (count charts) 1) "Expected at least one Vega-Lite chart"))))

    (testing "handles nil regression gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-regression* :portal {} {:regression nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

;;; Allocation View Tests

(deftest portal-allocation-summary-test
  ;; Tests the portal viewer output for allocation-summary results.
  ;; Verifies table generation with allocation metrics.
  (testing "allocation-summary*"
    (testing "produces table with allocation metrics"
      (let [[title table] (with-tap-out
                            (view/allocation-summary*
                             :portal
                             {}
                             {:allocation-summary
                              {:type :criterium/allocation-summary
                               :total-allocated 1024000
                               :total-freed 512000
                               :num-allocations 100
                               :num-freed 50
                               :freed-ratio 0.5}}))]
        (is (= [:b "Allocation Summary"] title))
        (is (= 6 (count table)) "Expected 6 rows")
        (is (= "Total allocated" (:metric (first table))))
        (is (= 1024000 (:value (first table))))
        (is (= "Retained" (:metric (nth table 2))))
        (is (= 512000 (:value (nth table 2))))
        (is (= "Freed ratio" (:metric (last table))))
        (is (str/includes? (:value (last table)) "50.0%"))))

    (testing "uses custom summary-id"
      (let [[title _table] (with-tap-out
                             (view/allocation-summary*
                              :portal
                              {:summary-id :my-summary}
                              {:my-summary
                               {:type :criterium/allocation-summary
                                :total-allocated 1000
                                :total-freed 500
                                :num-allocations 10
                                :num-freed 5
                                :freed-ratio 0.5}}))]
        (is (= [:b "Allocation Summary"] title))))

    (testing "handles nil summary gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-summary* :portal {} {:allocation-summary nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-allocation-hotspots-test
  ;; Tests the portal viewer output for allocation-hotspots results.
  ;; Verifies table generation with call-site, object-type, and allocation stats.
  (testing "allocation-hotspots*"
    (testing "produces table with hotspot data including object-type"
      (let [[title table] (with-tap-out
                            (view/allocation-hotspots*
                             :portal
                             {}
                             {:allocation-hotspots
                              {:type :criterium/allocation-hotspots
                               :hotspots [{:call-site {:call-class "my.ns$fn"
                                                       :call-method "invoke"
                                                       :call-file "my_ns.clj"
                                                       :call-line 42}
                                           :object-type "Ljava/lang/String;"
                                           :count 50
                                           :bytes 4800
                                           :freed-count 30
                                           :freed-bytes 2880}
                                          {:call-site {:call-class "other.ns$g"
                                                       :call-method "invoke"
                                                       :call-file "other.clj"
                                                       :call-line 10}
                                           :object-type "[B"
                                           :count 25
                                           :bytes 2400
                                           :freed-count 10
                                           :freed-bytes 960}]}}))
            first-row (first table)]
        (is (= [:b "Allocation Hotspots"] title))
        (is (= 2 (count table)) "Expected 2 rows")
        (is (str/includes? (:call-site first-row) "my.ns$fn"))
        (is (str/includes? (:call-site first-row) "my_ns.clj:42"))
        (is (= "Ljava/lang/String;" (:object-type first-row)))
        (is (= 50 (:count first-row)))
        (is (= 4800 (:bytes first-row)))
        (is (= 30 (:freed-count first-row)))
        (is (= 2880 (:freed-bytes first-row)))))

    (testing "uses custom hotspots-id"
      (let [[title _table] (with-tap-out
                             (view/allocation-hotspots*
                              :portal
                              {:hotspots-id :my-hotspots}
                              {:my-hotspots
                               {:type :criterium/allocation-hotspots
                                :hotspots [{:call-site {:call-class "x"
                                                        :call-method "y"
                                                        :call-file "z"
                                                        :call-line 1}
                                            :object-type "Ljava/lang/Object;"
                                            :count 1
                                            :bytes 10
                                            :freed-count 0
                                            :freed-bytes 0}]}}))]
        (is (= [:b "Allocation Hotspots"] title))))

    (testing "handles empty hotspots gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-hotspots* :portal {} {:allocation-hotspots
                                                 {:type :criterium/allocation-hotspots
                                                  :hotspots []}})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "handles nil hotspots-map gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-hotspots* :portal {} {:allocation-hotspots nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-allocation-by-type-test
  ;; Tests the portal viewer output for allocation-by-type results.
  ;; Verifies table generation with type names and allocation stats,
  ;; sorted by bytes descending.
  (testing "allocation-by-type*"
    (testing "produces table sorted by bytes descending"
      (let [[title table] (with-tap-out
                            (view/allocation-by-type*
                             :portal
                             {}
                             {:allocation-by-type
                              {:type :criterium/allocation-by-type
                               :by-type {"[B" {:count 10 :bytes 1000
                                               :freed-count 5 :freed-bytes 500}
                                         "Ljava/lang/String;" {:count 50
                                                               :bytes 4800
                                                               :freed-count 30
                                                               :freed-bytes 2880}
                                         "[Ljava/lang/Object;" {:count 5
                                                                :bytes 200
                                                                :freed-count 2
                                                                :freed-bytes 80}}}}))
            types (mapv :type table)]
        (is (= [:b "Allocations by Type"] title))
        (is (= 3 (count table)) "Expected 3 rows")
        (is (= ["Ljava/lang/String;" "[B" "[Ljava/lang/Object;"] types)
            "Expected sorted by bytes descending")
        (let [first-row (first table)]
          (is (= "Ljava/lang/String;" (:type first-row)))
          (is (= 50 (:count first-row)))
          (is (= 4800 (:bytes first-row)))
          (is (= 30 (:freed-count first-row)))
          (is (= 2880 (:freed-bytes first-row))))))

    (testing "uses custom by-type-id"
      (let [[title _table] (with-tap-out
                             (view/allocation-by-type*
                              :portal
                              {:by-type-id :my-by-type}
                              {:my-by-type
                               {:type :criterium/allocation-by-type
                                :by-type {"[I" {:count 1 :bytes 10
                                                :freed-count 0 :freed-bytes 0}}}}))]
        (is (= [:b "Allocations by Type"] title))))

    (testing "handles empty by-type gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-by-type* :portal {} {:allocation-by-type
                                                {:type :criterium/allocation-by-type
                                                 :by-type {}}})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "handles nil by-type-map gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-by-type* :portal {} {:allocation-by-type nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-allocation-treemap-test
  ;; Tests the portal viewer output for allocation-treemap results.
  ;; Verifies Vega spec generation with treemap visualization.
  (testing "allocation-treemap*"
    (testing "produces vega output with treemap spec"
      (let [[title vega-spec] (with-tap-out
                                (view/allocation-treemap*
                                 :portal
                                 {}
                                 {:allocation-treemap
                                  {:type :criterium/allocation-treemap
                                   :size-by :bytes
                                   :root {:name "root"
                                          :value 1000
                                          :children [{:name "java.lang.String"
                                                      :value 600
                                                      :children [{:name "L42"
                                                                  :value 600}]}
                                                     {:name "[B"
                                                      :value 400
                                                      :children [{:name "L10"
                                                                  :value 400}]}]}}}))
            viewer-meta (meta vega-spec)]
        (is (= [:b "Allocation Treemap"] title))
        (is (= :portal.viewer/vega (:portal.viewer/default viewer-meta)))
        (is (str/includes? (:$schema vega-spec) "vega/v5.json"))
        (is (contains? vega-spec :data))
        (is (contains? vega-spec :marks))))

    (testing "uses custom treemap-id"
      (let [[title _spec] (with-tap-out
                            (view/allocation-treemap*
                             :portal
                             {:treemap-id :my-treemap}
                             {:my-treemap
                              {:type :criterium/allocation-treemap
                               :root {:name "root"
                                      :value 100
                                      :children [{:name "type1" :value 100}]}}}))]
        (is (= [:b "Allocation Treemap"] title))))

    (testing "handles nil treemap data gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-treemap* :portal {} {:allocation-treemap nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "handles missing root gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/allocation-treemap* :portal {}
                                    {:allocation-treemap
                                     {:type :criterium/allocation-treemap}})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-kde-test
  ;; Tests the portal viewer output for KDE analysis results.
  ;; Verifies Vega-Lite spec generation with density curve and optional histogram overlay.
  (testing "kde*"
    (testing "produces vega-lite output with kde spec"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics) [:elapsed-time])
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
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/kde* :portal {} {})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "uses custom kde-id"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics) [:elapsed-time])
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

(deftest portal-kde-modes-rendering-test
  ;; Tests mode markers, significance indicators, and CI lines in portal KDE visualization.
  ;; Verifies that when modes data is provided via separate :modes key,
  ;; the chart includes point markers and rule lines for confidence intervals.
  (testing "kde* modes rendering"
    (testing "includes mode markers and CI lines when modes data present"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
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
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 2.0
                                          :density 0.3
                                          :ci-lower 1.8
                                          :ci-upper 2.2
                                          :significant? true}]
                                 :n-modes 1}}}
            [_title vega-spec] (with-tap-out
                                 (view/kde* :portal {}
                                            {:kde kde-data :modes modes-data}))
            first-chart (first (:vconcat vega-spec))
            kde-group (first (:layer first-chart))
            kde-layers (:layer kde-group)
            point-layer (first (filter #(= "point" (get-in % [:mark :type]))
                                       kde-layers))
            rule-layer (first (filter #(= "rule" (get-in % [:mark :type]))
                                      kde-layers))]
        (is (some? point-layer) "Expected point layer for mode markers")
        (is (some? rule-layer) "Expected rule layer for CI lines")
        ;; Verify point layer has shape encoding for significance
        (is (= "significant" (get-in point-layer [:encoding :shape :field]))
            "Expected shape encoding by significance field")
        (is (= ["yes" "no"]
               (get-in point-layer [:encoding :shape :scale :domain]))
            "Expected significance domain")
        (is (= ["circle" "triangle-up"]
               (get-in point-layer [:encoding :shape :scale :range]))
            "Expected significance shape range")
        ;; Verify rule layer encodes CI bounds
        (is (= "ci-lower" (get-in rule-layer [:encoding :x :field]))
            "Expected x encoding from ci-lower")
        (is (= "ci-upper" (get-in rule-layer [:encoding :x2 :field]))
            "Expected x2 encoding from ci-upper")
        ;; Verify stroke dash encoding for significance
        (is (= "significant" (get-in rule-layer [:encoding :strokeDash :field]))
            "Expected strokeDash encoding by significance")))

    (testing "distinguishes significant vs non-significant modes via shape"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0 4.0]
                              :density [0.1 0.3 0.2 0.15]
                              :lower-band [0.08 0.25 0.15 0.10]
                              :upper-band [0.12 0.35 0.25 0.20]
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 2.0
                                          :density 0.3
                                          :ci-lower 1.8
                                          :ci-upper 2.2
                                          :significant? true}
                                         {:location 3.5
                                          :density 0.18
                                          :ci-lower 3.2
                                          :ci-upper 3.8
                                          :significant? false}]
                                 :n-modes 2}}}
            [_title vega-spec] (with-tap-out
                                 (view/kde* :portal {}
                                            {:kde kde-data :modes modes-data}))
            first-chart (first (:vconcat vega-spec))
            kde-group (first (:layer first-chart))
            kde-layers (:layer kde-group)
            point-layer (first (filter #(= "point" (get-in % [:mark :type]))
                                       kde-layers))
            point-data (get-in point-layer [:data :values])]
        (is (= 2 (count point-data)) "Expected 2 mode markers")
        (is (= #{"yes" "no"} (set (map #(get % "significant") point-data)))
            "Expected both significant and non-significant modes")))

    (testing "omits mode layers when modes data not present"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
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
                              :n 100}}}
            [_title vega-spec] (with-tap-out
                                 (view/kde* :portal {} {:kde kde-data}))
            first-chart (first (:vconcat vega-spec))
            kde-group (first (:layer first-chart))
            kde-layers (:layer kde-group)
            point-layers (filter #(= "point" (get-in % [:mark :type]))
                                 kde-layers)
            rule-layers (filter #(= "rule" (get-in % [:mark :type]))
                                kde-layers)]
        (is (empty? point-layers)
            "Expected no point layers without modes data")
        (is (empty? rule-layers)
            "Expected no rule layers without modes data")))

    (testing "omits mode layers when modes list is empty"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
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
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes []
                                 :n-modes 0}}}
            [_title vega-spec] (with-tap-out
                                 (view/kde* :portal {}
                                            {:kde kde-data :modes modes-data}))
            first-chart (first (:vconcat vega-spec))
            kde-group (first (:layer first-chart))
            kde-layers (:layer kde-group)
            point-layers (filter #(= "point" (get-in % [:mark :type]))
                                 kde-layers)]
        (is (empty? point-layers)
            "Expected no point layers when modes list empty")))))

;;; Modal Analysis Views

(deftest portal-multimodal-warning-test
  ;; Tests the portal viewer output for multimodal-warning when multiple
  ;; modes are detected. Verifies warning heading, mode count table,
  ;; and mode locations table are produced.
  (testing "multimodal-warning*"
    (testing "outputs warning with mode count and locations when n-modes > 1"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 1e-9
                                          :density 0.3}
                                         {:location 2e-9
                                          :density 0.25}]
                                 :n-modes 2}}}
            outputs (with-tap-out
                      (view/multimodal-warning* :portal {} {:modes modes-data}))]
        (is (= 4 (count outputs))
            "Expected heading, status table, locations heading, locations table")
        (let [[heading status-table loc-heading loc-table] outputs]
          (is (= :b (first heading)))
          (is (str/includes? (second heading) "WARNING"))
          (is (str/includes? (second heading) "Multimodal"))
          (is (= 2 (count status-table)) "Expected 2 rows in status table")
          (is (= "Mode count" (:metric (first status-table))))
          (is (= 2 (:value (first status-table))))
          (is (= :em (first loc-heading)))
          (is (= 2 (count loc-table)) "Expected 2 mode location rows")
          (is (every? #(contains? % :location) loc-table))
          (is (every? #(contains? % :density) loc-table)))))

    (testing "outputs nothing when n-modes <= 1"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 1e-9 :density 0.3}]
                                 :n-modes 1}}}
            v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/multimodal-warning* :portal {} {:modes modes-data})
          (portal/flush)
          (is (empty? @v) "Expected no output when only 1 mode")
          (finally
            (remove-tap f)))))

    (testing "outputs nothing when modes-map is nil"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/multimodal-warning* :portal {} {:modes nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "uses custom modes-id"
      (let [metrics-defs (select-keys (criterium.collector.metrics/metrics)
                                      [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 1e-9 :density 0.3}
                                         {:location 2e-9 :density 0.25}]
                                 :n-modes 2}}}
            outputs (with-tap-out
                      (view/multimodal-warning*
                       :portal
                       {:modes-id :my-modes}
                       {:my-modes modes-data}))]
        (is (= 4 (count outputs))
            "Expected output with custom modes-id")))))
