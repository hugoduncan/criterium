(ns criterium.viewer.portal.domain-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.domain.types :as domain.types]
   [criterium.view :as view]
   [criterium.viewer.portal :as portal]
   [criterium.viewer.portal.domain])
  (:import
   [java.util Queue]))

(set! *unchecked-math* false)

;;; Test Helpers

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
            (when-not (= ::portal/_ x)
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
      (portal/flush)
      @v
      (finally
        (remove-tap f)))))

(defmacro with-tap-out-n
  "Capture tapped values during body execution, waiting for n values."
  [n & body]
  `(with-tap-out* ~n (fn [] ~@body)))

;;; Domain Extract Tests

(deftest portal-domain-extract-table-test
  ;; Tests the portal viewer output for domain-extract-table results.
  ;; Verifies table generation without chart output.
  (testing "domain-extract-table*"
    (testing "renders table only for single-point extract"
      (let [outputs (with-tap-out
                      (view/domain-extract-table*
                       :portal
                       {}
                       {:extract
                        {:type :criterium/domain-extract
                         :metrics {:elapsed-time
                                   {:metric [:stats :elapsed-time :mean]
                                    :data [[{:n 100} 1e-7]]}}}}))]
        (is (= 2 (count outputs)) "Expected heading and table only")
        (let [[title table] outputs]
          (is (= [:b "Domain Extract"] title))
          (is (vector? table) "Expected table data"))))

    (testing "renders table only for multi-point extract"
      (let [outputs (with-tap-out
                      (view/domain-extract-table*
                       :portal
                       {}
                       {:extract
                        {:type :criterium/domain-extract
                         :metrics {:elapsed-time
                                   {:metric [:stats :elapsed-time :mean]
                                    :data [[{:n 100} 1e-7]
                                           [{:n 200} 2e-7]
                                           [{:n 400} 4e-7]]}}}}))]
        (is (= 2 (count outputs)) "Expected heading and table only")
        (let [[title table] outputs]
          (is (= [:b "Domain Extract"] title))
          (is (= 3 (count table)) "Expected 3 rows"))))

    (testing "handles nil extract gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-extract-table* :portal {} {:extract nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-domain-extract-chart-test
  ;; Tests the portal viewer output for domain-extract-chart results.
  ;; Verifies chart generation without table output.
  (testing "domain-extract-chart*"
    (testing "renders bar chart for single-point extract without bootstrap"
      (let [[chart] (with-tap-out
                      (view/domain-extract-chart*
                       :portal
                       {}
                       {:extract
                        {:type :criterium/domain-extract
                         :impl-axis :impl
                         :implementations [:foo :bar]
                         :metrics {:elapsed-time
                                   {:metric [:stats :elapsed-time :mean]
                                    :data [[{:n 100 :impl :foo} 1e6]
                                           [{:n 100 :impl :bar} 2e6]]}}}}))]
        (is (map? chart) "Expected chart output")
        (is (str/includes? (:$schema chart) "vega-lite"))))

    (testing "renders line chart for multi-point extract"
      (let [[chart] (with-tap-out
                      (view/domain-extract-chart*
                       :portal
                       {}
                       {:extract
                        {:type :criterium/domain-extract
                         :impl-axis :impl
                         :implementations [:foo :bar]
                         :metrics {:elapsed-time
                                   {:metric [:stats :elapsed-time :mean]
                                    :data [[{:n 100 :impl :foo} 1e6]
                                           [{:n 1000 :impl :foo} 1e7]
                                           [{:n 100 :impl :bar} 2e6]
                                           [{:n 1000 :impl :bar} 2e7]]}}}}))]
        (is (map? chart) "Expected chart output")
        (is (str/includes? (:$schema chart) "vega-lite"))))

    (testing "outputs nothing for default-table strategy"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-extract-chart*
           :portal
           {}
           {:extract
            {:type :criterium/domain-extract
             :metrics {:elapsed-time
                       {:metric [:stats :elapsed-time :mean]
                        :data [[{:n 100 :m 1} 1e-7]
                               [{:n 100 :m 2} 2e-7]]}}}})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "handles nil extract gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-extract-chart* :portal {} {:extract nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

;;; Domain Grouped Tests

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

;;; Domain Comparison Tests

(deftest portal-domain-comparison-table-test
  ;; Tests the portal viewer output for domain-comparison-table results.
  ;; Verifies table generation without chart output.
  (testing "domain-comparison-table*"
    (testing "renders table only for single-point comparison"
      (let [outputs (with-tap-out
                      (view/domain-comparison-table*
                       :portal
                       {}
                       {:comparison
                        {:type :criterium/domain-comparison
                         :axis :impl
                         :metric [:stats :elapsed-time :mean]
                         :implementations [:foo :bar]
                         :data
                         {:foo [{:coord {:impl :foo} :value 1e-7}]
                          :bar [{:coord {:impl :bar} :value 2e-7}]}}}))]
        (is (= 2 (count outputs)) "Expected heading and table only")
        (let [[title table] outputs]
          (is (string? (second title)))
          (is (str/includes? (second title) "Domain Comparison"))
          (is (vector? table) "Expected table data"))))

    (testing "renders tables only for multi-point comparison"
      (let [outputs (with-tap-out
                      (view/domain-comparison-table*
                       :portal
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
        (is (= 2 (count outputs)) "Expected heading and table only")
        (let [[title table] outputs]
          (is (string? (second title)))
          (is (str/includes? (second title) "Domain Comparison"))
          (is (= 2 (count table)) "Expected 2 rows"))))

    (testing "handles nil comparison gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-comparison-table* :portal {} {:comparison nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest portal-domain-comparison-chart-test
  ;; Tests the portal viewer output for domain-comparison-chart results.
  ;; Verifies chart generation without table output.
  (testing "domain-comparison-chart*"
    (testing "renders bar chart for single-point comparison without bootstrap"
      (let [[chart] (with-tap-out
                      (view/domain-comparison-chart*
                       :portal
                       {}
                       {:comparison
                        {:type :criterium/domain-comparison
                         :axis :impl
                         :metric [:stats :elapsed-time :mean]
                         :implementations [:foo :bar]
                         :data
                         {:foo [{:coord {:impl :foo} :value 1e-7}]
                          :bar [{:coord {:impl :bar} :value 2e-7}]}}}))]
        (is (map? chart) "Expected chart output")
        (is (str/includes? (:$schema chart) "vega-lite"))))

    (testing "renders line chart for multi-point comparison"
      (let [[chart] (with-tap-out
                      (view/domain-comparison-chart*
                       :portal
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
        (is (map? chart) "Expected chart output")
        (is (str/includes? (:$schema chart) "vega-lite"))))

    (testing "outputs nothing for default-table strategy"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-comparison-chart*
           :portal
           {}
           {:comparison
            {:type :criterium/domain-comparison
             :axis :impl
             :metric [:stats :elapsed-time :mean]
             :data
             {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}]
              :bar [{:coord {:n 100 :impl :bar} :value 2e-7}]}}})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "handles nil comparison gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-comparison-chart* :portal {} {:comparison nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

;;; Domain Regression Tests

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
                                            {:metric [:stats
                                                      :elapsed-time
                                                      :mean]
                                             :data [[{:n 100} 1e6]
                                                    [{:n 200} 2e6]
                                                    [{:n 400} 4e6]
                                                    [{:n 800} 8e6]]}}}
                        :regression {:type :criterium/domain-regression
                                     :axis :n
                                     :regressions {:elapsed-time
                                                   {:metric [:stats
                                                             :elapsed-time
                                                             :mean]
                                                    :models [{:id :linear
                                                              :label "O(n)"
                                                              :coefficients {:a 10000.0
                                                                             :b 0.0}
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

;;; Domain Apply Tests

(defn- make-bench-data
  "Create minimal benchmark data with stats for testing domain-apply."
  [mean-ns]
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
        mean-ns      (double mean-ns)]
    {:samples {:type :criterium/collected-metrics-samples
               :metrics-defs metrics-defs
               :metric->values {[:elapsed-time]
                                (arr/->double-array (double-array [mean-ns]))}
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
                                    :mean-plus-3sigma (+ mean-ns 3)
                                    :mean-minus-3sigma (- mean-ns 3)
                                    :min-val mean-ns
                                    :max-val (+ mean-ns 3)}}}}))

(deftest domain-apply-portal-test
  ;; Tests the portal viewer for domain-apply.
  ;; Verifies that each run's coord is tapped with heading and view-spec applied.
  (testing "domain-apply*"
    (testing "taps coord heading and applies view-spec to each run"
      (let [domain (-> (domain.types/domain)
                       (domain.types/add-run {:n 100} (make-bench-data 100))
                       (domain.types/add-run {:n 200} (make-bench-data 200)))
            outputs (with-tap-out
                      (view/domain-apply*
                       :portal
                       {:view-spec [:stats {}]}
                       {:domain domain}))]
        (is (>= (count outputs) 4)
            "Expected at least 2 headings and 2 tables (one per run)")
        (let [headings (filter #(and (vector? %) (= :b (first %))) outputs)]
          (is (some #(str/includes? (second %) "{:n 100}") headings)
              "Should tap heading with first coord")
          (is (some #(str/includes? (second %) "{:n 200}") headings)
              "Should tap heading with second coord"))))

    (testing "formats keyword coord in heading"
      (let [domain (domain.types/domain
                    {:coord :baseline :data (make-bench-data 50)})
            outputs (with-tap-out
                      (view/domain-apply*
                       :portal
                       {:view-spec [:stats {}]}
                       {:domain domain}))
            headings (filter #(and (vector? %) (= :b (first %))) outputs)]
        (is (some #(str/includes? (second %) ":baseline") headings)
            "Should format keyword coord in heading")))

    (testing "uses custom domain-id"
      (let [domain (domain.types/domain
                    {:coord {:impl :foo} :data (make-bench-data 150)})
            outputs (with-tap-out
                      (view/domain-apply*
                       :portal
                       {:domain-id :my-domain
                        :view-spec [:stats {}]}
                       {:my-domain domain}))
            headings (filter #(and (vector? %) (= :b (first %))) outputs)]
        (is (some #(str/includes? (second %) "{:impl :foo}") headings)
            "Should use custom domain-id")))

    (testing "handles empty domain gracefully"
      (let [domain (domain.types/domain)
            v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-apply*
           :portal
           {:view-spec [:stats {}]}
           {:domain domain})
          (portal/flush)
          (is (empty? @v)
              "Should produce no output for empty domain")
          (finally
            (remove-tap f)))))

    (testing "handles missing domain gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/domain-apply*
           :portal
           {:view-spec [:stats {}]}
           {})
          (portal/flush)
          (is (empty? @v)
              "Should produce no output when domain is missing")
          (finally
            (remove-tap f)))))

    (testing "warns when view-spec is missing"
      (let [domain (domain.types/domain
                    {:coord :test :data (make-bench-data 100)})
            v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))
            stderr-output (java.io.StringWriter.)]
        (try
          (add-tap f)
          (binding [*err* stderr-output]
            (view/domain-apply*
             :portal
             {}
             {:domain domain}))
          (portal/flush)
          (is (empty? @v)
              "Should produce no tap output")
          (is (str/includes? (str stderr-output)
                             "WARNING: domain-apply requires :view-spec option")
              "Should warn on stderr when view-spec is missing")
          (finally
            (remove-tap f)))))))
