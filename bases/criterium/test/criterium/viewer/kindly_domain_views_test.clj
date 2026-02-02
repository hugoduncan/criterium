(ns criterium.viewer.kindly-domain-views-test
  ;; Tests for the Kindly viewer domain-related views.
  ;; Covers domain-grouped, domain-extract (table/chart), domain-comparison
  ;; (table/chart), domain-regression, and domain-apply views.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.domain.types :as domain.types]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]))

(deftest domain-grouped-view-test
  ;; Tests the view/domain-grouped* multimethod for :kindly viewer.
  ;; Verifies that domain grouped data is rendered as heading and table with
  ;; axis-value and run-count columns.
  (testing "view/domain-grouped* :kindly"
    (testing "renders grouped as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:grouped
                      {:type :criterium/domain-grouped
                       :axis :impl
                       :data
                       {:foo {:type :criterium/domain
                              :runs [{:coord {:impl :foo} :data {}}]}
                        :bar {:type :criterium/domain
                              :runs [{:coord {:impl :bar} :data {}}
                                     {:coord {:impl :bar :n 10} :data {}}]}}}}]
        (view/domain-grouped* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (clojure.string/includes? (first heading) "Domain Grouped"))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table))
                "Expected 2 rows for 2 axis values")
            (is (every? #(contains? % :axis-value) table)
                "Expected :axis-value column")
            (is (every? #(contains? % :run-count) table)
                "Expected :run-count column")))))

    (testing "handles nil grouped gracefully"
      (reset! kindly/accumulated [])
      (view/domain-grouped* :kindly {} {:grouped nil})
      (is (nil? (kindly/flush))))))

(deftest domain-extract-table-view-test
  ;; Tests the view/domain-extract-table* multimethod for :kindly viewer.
  ;; Verifies that domain extract table data is rendered as heading and table
  ;; without any chart output.
  (testing "view/domain-extract-table* :kindly"
    (testing "renders table only for single-point extract"
      (reset! kindly/accumulated [])
      (let [data-map {:extract
                      {:type :criterium/domain-extract
                       :impl-axis :impl
                       :implementations [:foo :bar]
                       :metrics {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :data [[{:n 100 :impl :foo} 1e6]
                                         [{:n 100 :impl :bar} 2e6]]}}}}]
        (view/domain-extract-table* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table only")
          (let [[heading table-data] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (str/includes? (first heading) "Domain Extract"))
            (is (= :kind/table (:kindly/kind (meta table-data))))))))

    (testing "renders table only for multi-point extract"
      (reset! kindly/accumulated [])
      (let [data-map {:extract
                      {:type :criterium/domain-extract
                       :impl-axis :impl
                       :implementations [:foo :bar]
                       :metrics {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :data [[{:n 100 :impl :foo} 1e6]
                                         [{:n 1000 :impl :foo} 1e7]
                                         [{:n 100 :impl :bar} 2e6]
                                         [{:n 1000 :impl :bar} 2e7]]}}}}]
        (view/domain-extract-table* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table only")
          (let [[heading table-data] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table-data))))))))

    (testing "renders table for default-table strategy"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 1000} 1e7]]}}}}]
        (view/domain-extract-table* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table")
          (let [[heading table-data] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table-data))))))))

    (testing "handles nil extract gracefully"
      (reset! kindly/accumulated [])
      (view/domain-extract-table* :kindly {} {:extract nil})
      (is (nil? (kindly/flush))))))

(deftest domain-extract-chart-view-test
  ;; Tests the view/domain-extract-chart* multimethod for :kindly viewer.
  ;; Verifies that domain extract chart is rendered without table output.
  (testing "view/domain-extract-chart* :kindly"
    (testing "renders bar chart for single-point extract without bootstrap"
      (reset! kindly/accumulated [])
      (let [data-map {:extract
                      {:type :criterium/domain-extract
                       :impl-axis :impl
                       :implementations [:foo :bar]
                       :metrics {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :data [[{:n 100 :impl :foo} 1e6]
                                         [{:n 100 :impl :bar} 2e6]]}}}}]
        (view/domain-extract-chart* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 1 (count result)) "Expected chart only")
          (let [[chart] result]
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)))))))

    (testing "renders line chart for multi-point extract"
      (reset! kindly/accumulated [])
      (let [data-map {:extract
                      {:type :criterium/domain-extract
                       :impl-axis :impl
                       :implementations [:foo :bar]
                       :metrics {:elapsed-time
                                 {:metric [:stats :elapsed-time :mean]
                                  :data [[{:n 100 :impl :foo} 1e6]
                                         [{:n 1000 :impl :foo} 1e7]
                                         [{:n 100 :impl :bar} 2e6]
                                         [{:n 1000 :impl :bar} 2e7]]}}}}]
        (view/domain-extract-chart* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 1 (count result)) "Expected chart only")
          (let [[chart] result]
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)))))))

    (testing "outputs line chart for single-impl multi-point extract"
      ;; Single-impl with multiple axis values now renders line chart
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :implementations [:default]
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 1000} 1e7]]}}}}]
        (view/domain-extract-chart* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 1 (count result)) "Expected chart only")
          (let [[chart] result]
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)))))))

    (testing "outputs nothing for default-table strategy (multi-axis)"
      ;; Multi-axis scenarios fall into default-table with no chart
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :impl-axis :impl
                                :implementations [:foo :bar]
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100 :m 10 :impl :foo}
                                                   1e6]
                                                  [{:n 1000 :m 20 :impl :bar}
                                                   1e7]]}}}}]
        (view/domain-extract-chart* :kindly {} data-map)
        (is (nil? (kindly/flush)))))

    (testing "handles nil extract gracefully"
      (reset! kindly/accumulated [])
      (view/domain-extract-chart* :kindly {} {:extract nil})
      (is (nil? (kindly/flush))))))

(deftest domain-comparison-table-view-test
  ;; Tests the view/domain-comparison-table* multimethod for :kindly viewer.
  ;; Verifies that domain comparison table data is rendered as heading and table
  ;; without any chart output.
  (testing "view/domain-comparison-table* :kindly"
    (testing "renders table only for single-point comparison"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :implementations [:foo :bar]
                       :data
                       {:foo [{:coord {:impl :foo} :value 1e6}]
                        :bar [{:coord {:impl :bar} :value 2e6}]}}}]
        (view/domain-comparison-table* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table only")
          (let [[heading table-data] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (str/includes? (first heading) "Domain Comparison"))
            (is (= :kind/table (:kindly/kind (meta table-data))))))))

    (testing "renders tables only for multi-point comparison"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :implementations [:foo :bar]
                       :data
                       {:foo [{:coord {:n 100 :impl :foo} :value 1e6}
                              {:coord {:n 1000 :impl :foo} :value 1e7}]
                        :bar [{:coord {:n 100 :impl :bar} :value 2e6}
                              {:coord {:n 1000 :impl :bar} :value 2e7}]}}}]
        (view/domain-comparison-table* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table only")
          (let [[heading table-data] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table-data))))))))

    (testing "handles nil comparison gracefully"
      (reset! kindly/accumulated [])
      (view/domain-comparison-table* :kindly {} {:comparison nil})
      (is (nil? (kindly/flush))))))

(deftest domain-comparison-chart-view-test
  ;; Tests the view/domain-comparison-chart* multimethod for :kindly viewer.
  ;; Verifies that domain comparison chart is rendered without table output.
  (testing "view/domain-comparison-chart* :kindly"
    (testing "renders bar chart for single-point comparison without bootstrap"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :implementations [:foo :bar]
                       :data
                       {:foo [{:coord {:impl :foo} :value 1e6}]
                        :bar [{:coord {:impl :bar} :value 2e6}]}}}]
        (view/domain-comparison-chart* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 1 (count result)) "Expected chart only")
          (let [[chart] result]
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)))))))

    (testing "renders line chart for multi-point comparison"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :implementations [:foo :bar]
                       :data
                       {:foo [{:coord {:n 100 :impl :foo} :value 1e6}
                              {:coord {:n 1000 :impl :foo} :value 1e7}]
                        :bar [{:coord {:n 100 :impl :bar} :value 2e6}
                              {:coord {:n 1000 :impl :bar} :value 2e7}]}}}]
        (view/domain-comparison-chart* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 1 (count result)) "Expected chart only")
          (let [[chart] result]
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)))))))

    (testing "outputs nothing for default-table strategy"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :data
                       {:foo [{:coord {:n 100 :impl :foo} :value 1e6}]
                        :bar [{:coord {:n 100 :impl :bar} :value 2e6}]}}}]
        (view/domain-comparison-chart* :kindly {} data-map)
        (is (nil? (kindly/flush)))))

    (testing "handles nil comparison gracefully"
      (reset! kindly/accumulated [])
      (view/domain-comparison-chart* :kindly {} {:comparison nil})
      (is (nil? (kindly/flush))))))

(deftest domain-regression-view-test
  ;; Tests the view/domain-regression* multimethod for :kindly viewer.
  ;; Verifies that domain regression data is rendered as heading, model table,
  ;; scatter/line chart, and a combined residual plot for models within tolerance.
  (testing "view/domain-regression* :kindly"
    (testing "renders regression with single model within default tolerance"
      (reset! kindly/accumulated [])
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x]
                                        (* 10000.0 x))
                          :r-squared 0.9999}
                         {:id :quadratic
                          :label "O(n²)"
                          :coefficients {:a 0.1 :b 100000.0}
                          :equation-str "y = 0.1*n² + 100000"
                          :predict-fn (fn [^double x]
                                        (+ (* 0.1 x x) 100000.0))
                          :r-squared 0.85}]
                :best-fit :linear}}}}]
        ;; With default 1% tolerance, only linear (0.9999) is plotted
        ;; quadratic (0.85) is well below threshold (0.9999 * 0.99 = 0.9899)
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is
           (= 5 (count result))
           "Expected heading, table, chart, residual heading, residual chart")
          (let [[heading table chart residual-heading residual-chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (clojure.string/includes? (first heading) "Domain Regression"))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table))
                "Expected 2 rows for 2 models")
            (is (every? #(contains? % :model) table)
                "Expected :model column")
            (is (every? #(contains? % :r-squared) table)
                "Expected :r-squared column")

            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (= 2 (count (:layer chart)))
                "Expected 2 layers: scatter and line")
            ;; Combined residual plot
            (is (= :kind/md (:kindly/kind (meta residual-heading))))
            (is (clojure.string/includes? (first residual-heading) "Residual"))
            (is (= :kind/vega-lite (:kindly/kind (meta residual-chart))))
            (is (= 3 (count (:layer residual-chart)))
                "Expected 3 layers: scatter, loess line, zero line")))))

    (testing
     "renders combined residual plot when multiple models within tolerance"
      (reset! kindly/accumulated [])
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x]
                                        (* 10000.0 x))
                          :r-squared 0.9999}
                         {:id :n-log-n
                          :label "O(n log n)"
                          :coefficients {:a 1000.0 :b 0.0}
                          :equation-str "y = 1000*n*log(n) + 0"
                          :predict-fn (fn [^double x]
                                        (* 1000.0 x (Math/log x)))
                          :r-squared 0.9995}]
                :best-fit :linear}}}}]
        ;; Both models within 1% tolerance (0.9999 * 0.99 = 0.9899)
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is
           (= 5 (count result))
           "Expected heading, table, chart, residual heading, residual chart")
          (let [[_ _table chart _ residual-chart] result]
            ;; Chart should use color legend for multiple models
            (is (contains? (get-in chart [:layer 1 :encoding :color]) :field))
            ;; Residual chart should also use color legend
            (is (contains?
                 (get-in residual-chart [:layer 0 :encoding :color])
                 :field)
                "Residual chart should have color encoding by model")))))

    (testing "respects custom tolerance parameter"
      (reset! kindly/accumulated [])
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x]
                                        (* 10000.0 x))
                          :r-squared 0.9999}
                         {:id :quadratic
                          :label "O(n²)"
                          :coefficients {:a 0.1 :b 100000.0}
                          :equation-str "y = 0.1*n² + 100000"
                          :predict-fn (fn [^double x]
                                        (+ (* 0.1 x x) 100000.0))
                          :r-squared 0.85}]
                :best-fit :linear}}}}]
        ;; With 20% tolerance, quadratic (0.85) is within threshold
        ;; (0.9999 * 0.80 = 0.7999)
        (view/domain-regression* :kindly {:tolerance 0.20} data-map)
        (let [result (kindly/flush)]
          (is
           (= 5 (count result))
           "Expected heading, table, chart, residual heading, residual chart")
          ;; Verify residual chart has color encoding for multiple models
          (let [residual-chart (nth result 4)]
            (is (contains?
                 (get-in residual-chart [:layer 0 :encoding :color])
                 :field))))))

    (testing "renders table only when no extract data"
      (reset! kindly/accumulated [])
      (let [data-map {:regression
                      {:type :criterium/domain-regression
                       :axis :n
                       :regressions
                       {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :models [{:id :linear
                                   :label "O(n)"
                                   :coefficients {:a 10000.0 :b 0.0}
                                   :equation-str "y = 10000*n + 0"
                                   :predict-fn (fn [^double x]
                                                 (* 10000.0 x))
                                   :r-squared 0.9999}]
                         :best-fit :linear}}}}]
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= 2 (count result))
              "Expected heading and table only when no extract")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table))))))))

    (testing "handles empty models gracefully"
      (reset! kindly/accumulated [])
      (let [data-map {:regression
                      {:type :criterium/domain-regression
                       :axis :n
                       :regressions {:elapsed-time
                                     {:metric [:stats :elapsed-time :mean]
                                      :models []
                                      :best-fit nil}}}}]
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= 1 (count result))
              "Expected heading only when no models"))))

    (testing "handles nil regression gracefully"
      (reset! kindly/accumulated [])
      (view/domain-regression* :kindly {} {:regression nil})
      (is (nil? (kindly/flush))))))

(deftest domain-regression-log-log-callback-integration-test
  ;; Tests that the render-log-log-charts callback in domain-regression* extracts
  ;; the metric-name correctly from the metric path vector. This catches type
  ;; errors like (name metric) on a vector vs (name (second metric)) on a keyword.
  (testing "view/domain-regression* :kindly log-log callback"
    (testing "generates y-axis label using metric-name from path"
      (reset! kindly/accumulated [])
      (let [;; Create realistic log-log data structure with metric as path vector
            data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :log-log
             {:type :criterium/domain-log-log-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :xs [100 200 400 800]
                :ys [1e6 2e6 4e6 8e6]
                :log-xs [(Math/log 100) (Math/log 200) (Math/log 400) (Math/log 800)]
                :log-ys [(Math/log 1e6) (Math/log 2e6) (Math/log 4e6) (Math/log 8e6)]
                :slope 1.0
                :intercept 6.9
                :r-squared 0.9999
                :residuals [0.001 -0.001 0.001 -0.001]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x] (* 10000.0 x))
                          :r-squared 0.9999}]
                :best-fit :linear}}}}]
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          ;; Find the log-log chart (first vega-lite chart with log(n) x-axis)
          (let [log-log-chart (->> result
                                   (filter #(and (= :kind/vega-lite
                                                    (:kindly/kind (meta %)))
                                                 (some-> % :layer first :encoding :x :title
                                                         (str/includes? "log(n)"))))
                                   first)]
            (is (some? log-log-chart)
                "Should have a log-log chart")
            ;; Verify the y-axis uses the metric name (elapsed-time)
            ;; not the full path vector and not the hardcoded "time"
            (let [y-axis-title (-> log-log-chart :layer first :encoding :y :title)]
              (is (= "log(elapsed-time)" y-axis-title)
                  (str "Y-axis should use metric name from path. "
                       "Got: " (pr-str y-axis-title))))))))

    (testing "handles allocation metric correctly"
      (reset! kindly/accumulated [])
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:thread-allocation
                        {:metric [:stats :thread-allocation :mean]
                         :data [[{:n 100} 1e4]
                                [{:n 200} 2e4]]}}}
             :log-log
             {:type :criterium/domain-log-log-regression
              :axis :n
              :regressions
              {:thread-allocation
               {:metric [:stats :thread-allocation :mean]
                :xs [100 200]
                :ys [1e4 2e4]
                :log-xs [(Math/log 100) (Math/log 200)]
                :log-ys [(Math/log 1e4) (Math/log 2e4)]
                :slope 1.0
                :intercept 6.9
                :r-squared 0.99
                :residuals [0.01 -0.01]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:thread-allocation
               {:metric [:stats :thread-allocation :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 100.0 :b 0.0}
                          :equation-str "y = 100*n"
                          :predict-fn (fn [^double x] (* 100.0 x))
                          :r-squared 0.99}]
                :best-fit :linear}}}}]
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)
              log-log-chart (->> result
                                 (filter #(and (= :kind/vega-lite
                                                  (:kindly/kind (meta %)))
                                               (some-> % :layer first :encoding :x :title
                                                       (str/includes? "log(n)"))))
                                 first)
              y-axis-title (-> log-log-chart :layer first :encoding :y :title)]
          (is (= "log(thread-allocation)" y-axis-title)
              "Y-axis should use thread-allocation metric name"))))))

;;; Domain Apply View Tests

(defn- make-bench-data
  "Create minimal benchmark data with stats for testing domain-apply.
  Unlike print viewer, kindly stats view also requires :max-val in stats."
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
                                    :min-val mean-ns
                                    :max-val (+ mean-ns 10.0)}}}}))

(deftest domain-apply-kindly-test
  ;; Tests the view/domain-apply* multimethod for :kindly viewer.
  ;; Verifies that each run is rendered with a heading showing the coord
  ;; and the view-spec is applied to produce output in the accumulator.
  (testing "view/domain-apply* :kindly"
    (testing "renders heading and applies view-spec to each run"
      (reset! kindly/accumulated [])
      (let [domain (-> (domain.types/domain)
                       (domain.types/add-run {:n 100} (make-bench-data 100))
                       (domain.types/add-run {:n 200} (make-bench-data 200)))]
        (view/domain-apply*
         :kindly
         {:view-spec [:stats {}]}
         {:domain domain})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          ;; Each run produces: heading + stats heading + stats table = 3 items
          ;; 2 runs = 6 items total
          (is (= 6 (count result))
              "Expected 6 items: 2 runs × (coord heading + stats heading + stats table)")
          ;; First item should be the coord heading for first run
          (let [first-heading (first result)]
            (is (= :kind/md (:kindly/kind (meta first-heading))))
            (is (str/includes? (first first-heading) "Run:")
                "First item should be run heading")
            (is (str/includes? (first first-heading) "{:n 100}")
                "Run heading should contain coord"))
          ;; Fourth item should be the coord heading for second run
          (let [second-heading (nth result 3)]
            (is (= :kind/md (:kindly/kind (meta second-heading))))
            (is (str/includes? (first second-heading) "{:n 200}")
                "Second run heading should contain coord")))))

    (testing "handles keyword coord"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain
                    {:coord :baseline :data (make-bench-data 50)})]
        (view/domain-apply*
         :kindly
         {:view-spec [:stats {}]}
         {:domain domain})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (let [first-heading (first result)]
            (is (str/includes? (first first-heading) ":baseline")
                "Coord heading should contain keyword coord")))))

    (testing "uses custom domain-id"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain
                    {:coord {:impl :foo} :data (make-bench-data 150)})]
        (view/domain-apply*
         :kindly
         {:domain-id :my-domain
          :view-spec [:stats {}]}
         {:my-domain domain})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (let [first-heading (first result)]
            (is (str/includes? (first first-heading) "{:impl :foo}")
                "Should use custom domain-id")))))

    (testing "handles empty domain gracefully"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain)]
        (view/domain-apply*
         :kindly
         {:view-spec [:stats {}]}
         {:domain domain})
        (is (nil? (kindly/flush))
            "Should produce no output for empty domain")))

    (testing "handles missing domain gracefully"
      (reset! kindly/accumulated [])
      (view/domain-apply*
       :kindly
       {:view-spec [:stats {}]}
       {})
      (is (nil? (kindly/flush))
          "Should produce no output when domain is missing"))

    (testing "warns when view-spec is missing"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain
                    {:coord :test :data (make-bench-data 100)})
            stderr-output (java.io.StringWriter.)]
        (binding [*err* stderr-output]
          (view/domain-apply*
           :kindly
           {}
           {:domain domain}))
        (is (nil? (kindly/flush))
            "Should produce no kindly output")
        (is (str/includes? (str stderr-output)
                           "WARNING: domain-apply requires :view-spec option")
            "Should warn on stderr when view-spec is missing")))))
