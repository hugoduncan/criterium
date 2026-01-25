(ns criterium.viewer.kindly.domain-test
  ;; Tests for kindly domain views: domain-extract-table/chart,
  ;; domain-grouped, domain-comparison-table/chart, domain-regression.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.view :as view]
   [criterium.viewer.kindly.core :as core]
   [criterium.viewer.kindly.domain]))

;;; Domain Extract Tests

(deftest kindly-domain-extract-table-test
  ;; Tests the kindly viewer output for domain-extract-table results.
  ;; Verifies table generation without chart output.
  (testing "domain-extract-table*"
    (testing "renders table for single-point extract"
      (reset! core/accumulated [])
      (view/domain-extract-table*
       :kindly
       {}
       {:extract
        {:type :criterium/domain-extract
         :metrics {:elapsed-time
                   {:metric [:stats :elapsed-time :mean]
                    :data [[{:n 100} 1e-7]]}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (str/includes? (first heading) "Domain Extract"))
          (is (= :kind/table (:kindly/kind (meta table)))))))

    (testing "renders table for multi-point extract"
      (reset! core/accumulated [])
      (view/domain-extract-table*
       :kindly
       {}
       {:extract
        {:type :criterium/domain-extract
         :metrics {:elapsed-time
                   {:metric [:stats :elapsed-time :mean]
                    :data [[{:n 100} 1e-7]
                           [{:n 200} 2e-7]
                           [{:n 400} 4e-7]]}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[_heading table] result]
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 3 (count (:row-maps table))) "Expected 3 rows"))))

    (testing "handles nil extract gracefully"
      (reset! core/accumulated [])
      (view/domain-extract-table* :kindly {} {:extract nil})
      (is (empty? @core/accumulated)))))

(deftest kindly-domain-extract-chart-test
  ;; Tests the kindly viewer output for domain-extract-chart results.
  ;; Verifies chart generation without table output.
  (testing "domain-extract-chart*"
    (testing "renders bar chart for single-point extract"
      (reset! core/accumulated [])
      (view/domain-extract-chart*
       :kindly
       {}
       {:extract
        {:type :criterium/domain-extract
         :impl-axis :impl
         :implementations [:foo :bar]
         :metrics {:elapsed-time
                   {:metric [:stats :elapsed-time :mean]
                    :data [[{:n 100 :impl :foo} 1e6]
                           [{:n 100 :impl :bar} 2e6]]}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (>= (count result) 1) "Expected at least chart")
        (let [chart (first result)]
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (str/includes? (:$schema chart) "vega-lite")))))

    (testing "renders line chart for multi-point extract"
      (reset! core/accumulated [])
      (view/domain-extract-chart*
       :kindly
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
                           [{:n 1000 :impl :bar} 2e7]]}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (>= (count result) 1) "Expected at least chart")
        (let [chart (first result)]
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (str/includes? (:$schema chart) "vega-lite")))))

    (testing "outputs nothing for default-table strategy"
      (reset! core/accumulated [])
      (view/domain-extract-chart*
       :kindly
       {}
       {:extract
        {:type :criterium/domain-extract
         :metrics {:elapsed-time
                   {:metric [:stats :elapsed-time :mean]
                    :data [[{:n 100 :m 1} 1e-7]
                           [{:n 100 :m 2} 2e-7]]}}}})
      (is (empty? @core/accumulated)))

    (testing "handles nil extract gracefully"
      (reset! core/accumulated [])
      (view/domain-extract-chart* :kindly {} {:extract nil})
      (is (empty? @core/accumulated)))))

;;; Domain Grouped Tests

(deftest kindly-domain-grouped-test
  ;; Tests the kindly viewer output for domain-grouped results.
  ;; Verifies table generation with axis values and run counts.
  (testing "domain-grouped*"
    (testing "produces table with axis values and run counts"
      (reset! core/accumulated [])
      (view/domain-grouped*
       :kindly
       {}
       {:grouped
        {:type :criterium/domain-grouped
         :axis :impl
         :data {:foo {:type :criterium/domain
                      :runs [{} {}]}
                :bar {:type :criterium/domain
                      :runs [{}]}
                nil {:type :criterium/domain
                     :runs [{}]}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (str/includes? (first heading) "Grouped by: impl"))
          (is (= :kind/table (:kindly/kind (meta table)))))))

    (testing "uses custom grouped-id"
      (reset! core/accumulated [])
      (view/domain-grouped*
       :kindly
       {:grouped-id :by-n}
       {:by-n
        {:type :criterium/domain-grouped
         :axis :n
         :data {100 {:type :criterium/domain
                     :runs [{}]}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)))
        (let [[heading _table] result]
          (is (str/includes? (first heading) "Grouped by: n")))))))

;;; Domain Comparison Tests

(deftest kindly-domain-comparison-table-test
  ;; Tests the kindly viewer output for domain-comparison-table results.
  ;; Verifies table generation without chart output.
  (testing "domain-comparison-table*"
    (testing "renders table for single-point comparison"
      (reset! core/accumulated [])
      (view/domain-comparison-table*
       :kindly
       {}
       {:comparison
        {:type :criterium/domain-comparison
         :axis :impl
         :metric [:stats :elapsed-time :mean]
         :implementations [:foo :bar]
         :data
         {:foo [{:coord {:impl :foo} :value 1e-7}]
          :bar [{:coord {:impl :bar} :value 2e-7}]}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (str/includes? (first heading) "Comparison"))
          (is (= :kind/table (:kindly/kind (meta table)))))))

    (testing "renders tables for multi-point comparison"
      (reset! core/accumulated [])
      (view/domain-comparison-table*
       :kindly
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
                {:coord {:n 200 :impl :bar} :value 4e-7}]}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[_heading table] result]
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 2 (count (:row-maps table))) "Expected 2 rows"))))

    (testing "handles nil comparison gracefully"
      (reset! core/accumulated [])
      (view/domain-comparison-table* :kindly {} {:comparison nil})
      (is (empty? @core/accumulated)))))

(deftest kindly-domain-comparison-chart-test
  ;; Tests the kindly viewer output for domain-comparison-chart results.
  ;; Verifies chart generation without table output.
  (testing "domain-comparison-chart*"
    (testing "renders bar chart for single-point comparison"
      (reset! core/accumulated [])
      (view/domain-comparison-chart*
       :kindly
       {}
       {:comparison
        {:type :criterium/domain-comparison
         :axis :impl
         :metric [:stats :elapsed-time :mean]
         :implementations [:foo :bar]
         :data
         {:foo [{:coord {:impl :foo} :value 1e-7}]
          :bar [{:coord {:impl :bar} :value 2e-7}]}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (>= (count result) 1) "Expected at least chart")
        (let [chart (first result)]
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (str/includes? (:$schema chart) "vega-lite")))))

    (testing "renders line chart for multi-point comparison"
      (reset! core/accumulated [])
      (view/domain-comparison-chart*
       :kindly
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
                {:coord {:n 200 :impl :bar} :value 4e-7}]}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (>= (count result) 1) "Expected at least chart")
        (let [chart (first result)]
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (str/includes? (:$schema chart) "vega-lite")))))

    (testing "outputs nothing for default-table strategy"
      (reset! core/accumulated [])
      (view/domain-comparison-chart*
       :kindly
       {}
       {:comparison
        {:type :criterium/domain-comparison
         :axis :impl
         :metric [:stats :elapsed-time :mean]
         :data
         {:foo [{:coord {:n 100 :impl :foo} :value 1e-7}]
          :bar [{:coord {:n 100 :impl :bar} :value 2e-7}]}}})
      (is (empty? @core/accumulated)))

    (testing "handles nil comparison gracefully"
      (reset! core/accumulated [])
      (view/domain-comparison-chart* :kindly {} {:comparison nil})
      (is (empty? @core/accumulated)))))

;;; Domain Regression Tests

(deftest kindly-domain-regression-test
  ;; Tests the kindly viewer output for domain-regression results.
  ;; Verifies table generation with model equations, and chart output.
  (testing "domain-regression*"
    (testing "produces table and chart with regression models"
      (reset! core/accumulated [])
      (view/domain-regression*
       :kindly
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
                                              :predict-fn (fn [^double x] (* 10000.0 x))
                                              :r-squared 0.9999}
                                             {:id :quadratic
                                              :label "O(n²)"
                                              :coefficients {:a 0.1 :b 100000.0}
                                              :equation-str "y = 0.1*n² + 100000"
                                              :predict-fn (fn [^double x] (+ (* 0.1 x x) 100000.0))
                                              :r-squared 0.85}]
                                    :best-fit :linear}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (>= (count result) 2) "Expected at least heading and table")
        (let [[heading table & _rest] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (str/includes? (first heading) "Regression"))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 2 (count table)) "Expected 2 rows for 2 models"))))

    (testing "renders chart when extract data available"
      (reset! core/accumulated [])
      (view/domain-regression*
       :kindly
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
                                              :predict-fn (fn [^double x] (* 10000.0 x))
                                              :r-squared 0.9999}]
                                    :best-fit :linear}}}})
      (let [result (core/flush)]
        (is (>= (count result) 4) "Expected heading, table, chart, residual")
        (let [charts (filter #(= :kind/vega-lite (:kindly/kind (meta %))) result)]
          (is (>= (count charts) 1) "Expected at least one Vega-Lite chart"))))

    (testing "handles nil regression gracefully"
      (reset! core/accumulated [])
      (view/domain-regression* :kindly {} {:regression nil})
      (is (empty? @core/accumulated)))))
