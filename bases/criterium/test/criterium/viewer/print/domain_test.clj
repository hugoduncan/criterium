(ns criterium.viewer.print.domain-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.domain.types :as domain.types]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.domain]))

;;; Domain Grouped Tests

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

;;; Domain Regression Tests

(deftest domain-regression-print-test
  ;; Tests the print viewer output for domain-regression results.
  ;; Uses multi-metric :regressions map structure.
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

;;; Domain Apply Tests

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
  ;; Tests the print viewer ASCII chart rendering for domain-extract.
  ;; Charts are rendered for multi-point strategy; single-point returns nil.
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

    (testing "renders ASCII chart for multi-point strategy with single impl"
      (let [output (with-out-str
                     (view/domain-extract-chart*
                      :print
                      {}
                      {:extract
                       {:type :criterium/domain-extract
                        :impl-axis :impl
                        :implementations [:foo]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :impl :foo} 1e-7]
                                          [{:n 200 :impl :foo} 2e-7]
                                          [{:n 300 :impl :foo} 3e-7]]}}}}))]
        (is (str/includes? output "elapsed-time"))
        (is (str/includes? output "[foo]"))
        ;; Chart should contain point markers
        (is (str/includes? output "*"))))

    (testing "renders separate ASCII charts for multi-point multi-impl"
      (let [output (with-out-str
                     (view/domain-extract-chart*
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
        ;; Should have charts for both implementations
        (is (str/includes? output "[foo]"))
        (is (str/includes? output "[bar]"))
        ;; Chart should contain point markers
        (is (str/includes? output "*"))))

    (testing "respects chart-width and chart-height options"
      (let [output (with-out-str
                     (view/domain-extract-chart*
                      :print
                      {:chart-width 40 :chart-height 10}
                      {:extract
                       {:type :criterium/domain-extract
                        :impl-axis :impl
                        :implementations [:foo]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :impl :foo} 1e-7]
                                          [{:n 200 :impl :foo} 2e-7]
                                          [{:n 300 :impl :foo} 3e-7]]}}}}))
            lines (str/split-lines output)]
        ;; With smaller dimensions, lines should be shorter
        (is (every? #(<= (count %) 45) lines))))

    (testing "uses custom extract-id"
      (let [output (with-out-str
                     (view/domain-extract-chart*
                      :print
                      {:extract-id :my-extract}
                      {:my-extract
                       {:type :criterium/domain-extract
                        :impl-axis :impl
                        :implementations [:foo]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :impl :foo} 1e-7]
                                          [{:n 200 :impl :foo} 2e-7]
                                          [{:n 300 :impl :foo} 3e-7]]}}}}))]
        (is (str/includes? output "elapsed-time"))))

    (testing "returns nil for nil extract"
      (is (nil? (view/domain-extract-chart* :print {} {:extract nil}))))

    (testing "renders chart with multiple metrics"
      (let [output (with-out-str
                     (view/domain-extract-chart*
                      :print
                      {}
                      {:extract
                       {:type :criterium/domain-extract
                        :impl-axis :impl
                        :implementations [:foo]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data [[{:n 100 :impl :foo} 1e-7]
                                          [{:n 200 :impl :foo} 2e-7]]}
                                  :allocation
                                  {:metric [:stats :thread-allocation :mean]
                                   :data [[{:n 100 :impl :foo} 1000]
                                          [{:n 200 :impl :foo} 2000]]}}}}))]
        ;; Should have charts for both metrics
        (is (str/includes? output "elapsed-time"))
        (is (str/includes? output "allocation"))))))

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
  ;; Tests the print viewer ASCII chart rendering for domain-comparison.
  ;; Charts are rendered for multi-point strategy; single-point returns nil.
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

    (testing "renders ASCII chart for multi-point strategy"
      (let [output (with-out-str
                     (view/domain-comparison-chart*
                      :print
                      {}
                      {:comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data
                        {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}
                               {:coord {:n 200 :impl :foo} :value 2e-7}
                               {:coord {:n 300 :impl :foo} :value 3e-7}]
                         :bar [{:coord {:n 100 :impl :bar} :value 2e-7}
                               {:coord {:n 200 :impl :bar} :value 4e-7}
                               {:coord {:n 300 :impl :bar} :value 6e-7}]}}}))]
        ;; Should have charts for both implementations
        (is (str/includes? output "[foo]"))
        (is (str/includes? output "[bar]"))
        ;; Chart should contain point markers
        (is (str/includes? output "*"))))

    (testing "respects chart-width and chart-height options"
      (let [output (with-out-str
                     (view/domain-comparison-chart*
                      :print
                      {:chart-width 40 :chart-height 10}
                      {:comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data
                        {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}
                               {:coord {:n 200 :impl :foo} :value 2e-7}
                               {:coord {:n 300 :impl :foo} :value 3e-7}]}}}))
            lines (str/split-lines output)]
        ;; With smaller dimensions, lines should be shorter
        (is (every? #(<= (count %) 45) lines))))

    (testing "uses custom comparison-id"
      (let [output (with-out-str
                     (view/domain-comparison-chart*
                      :print
                      {:comparison-id :my-comparison}
                      {:my-comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data
                        {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}
                               {:coord {:n 200 :impl :foo} :value 2e-7}
                               {:coord {:n 300 :impl :foo} :value 3e-7}]
                         :bar [{:coord {:n 100 :impl :bar} :value 2e-7}
                               {:coord {:n 200 :impl :bar} :value 4e-7}
                               {:coord {:n 300 :impl :bar} :value 6e-7}]}}}))]
        (is (str/includes? output "[foo]"))))

    (testing "returns nil for nil comparison"
      (is (nil? (view/domain-comparison-chart* :print {} {:comparison nil}))))

    (testing "renders chart with multi-metric comparison"
      (let [output (with-out-str
                     (view/domain-comparison-chart*
                      :print
                      {}
                      {:comparison
                       {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}
                                                {:coord {:n 200 :impl :foo} :value 2e-7}]
                                          :bar [{:coord {:n 100 :impl :bar} :value 2e-7}
                                                {:coord {:n 200 :impl :bar} :value 4e-7}]}}
                                  :allocation
                                  {:metric [:stats :thread-allocation :mean]
                                   :data {:foo [{:coord {:n 100 :impl :foo} :value 1000}
                                                {:coord {:n 200 :impl :foo} :value 2000}]
                                          :bar [{:coord {:n 100 :impl :bar} :value 2000}
                                                {:coord {:n 200 :impl :bar} :value 4000}]}}}}}))]
        ;; Should have charts for both metrics
        (is (str/includes? output "elapsed-time"))
        (is (str/includes? output "allocation"))))))
