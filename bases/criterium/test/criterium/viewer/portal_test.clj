(ns criterium.viewer.portal-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
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
                               :num-freed 50}}))]
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
                                :num-freed 5}}))]
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
