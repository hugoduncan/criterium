(ns criterium.viewer.common.domain.comparison-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common.domain.comparison :as comparison]))

;;; Tests for prepare-comparison-bar-data helper.
;;; Verifies bar chart data preparation from domain-comparison data.

(deftest prepare-comparison-bar-data-test
  (testing "prepare-comparison-bar-data"
    (testing "prepares data for single-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar :baz]
                               :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}]
                                      :baz [{:coord {:n 100} :value 1.5e-6}]}}
            result (comparison/prepare-comparison-bar-data domain-comparison)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (nil? (:metric-id (first result))))
        (is (= 3 (count (:data (first result)))))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") (:data (first result))))))))

    (testing "prepares data for multi-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :implementations [:foo :bar]
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                                                 :bar [{:coord {:n 100} :value 2.0e-6}]}}
                                         :thread-allocation
                                         {:metric [:stats :thread-allocation :mean]
                                          :data {:foo [{:coord {:n 100} :value 1000}]
                                                 :bar [{:coord {:n 100} :value 2000}]}}}}
            result (comparison/prepare-comparison-bar-data domain-comparison)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "returns has-error-bounds? false for plain values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}]}}
            result (comparison/prepare-comparison-bar-data domain-comparison)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        (is (every? #(not (contains? % "valueLower")) (:data first-metric)))
        (is (every? #(not (contains? % "valueUpper")) (:data first-metric)))))

    (testing "extracts error bounds for single-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:value 1.0e-6
                                                     :lower 0.9e-6
                                                     :upper 1.1e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:value 2.0e-6
                                                     :lower 1.8e-6
                                                     :upper 2.2e-6}}]}}
            result (comparison/prepare-comparison-bar-data domain-comparison)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "valueLower") data))
          (is (every? #(contains? % "valueUpper") data))
          (doseq [d data]
            (is (< (get d "valueLower") (get d "value")))
            (is (< (get d "value") (get d "valueUpper")))))))

    (testing "extracts error bounds for multi-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :implementations [:foo :bar]
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:n 100}
                                                        :value {:value 1.0e-6
                                                                :lower 0.9e-6
                                                                :upper 1.1e-6}}]
                                                 :bar [{:coord {:n 100}
                                                        :value {:value 2.0e-6
                                                                :lower 1.8e-6
                                                                :upper 2.2e-6}}]}}}}
            result (comparison/prepare-comparison-bar-data domain-comparison)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "valueLower") data))
          (is (every? #(contains? % "valueUpper") data)))))

    (testing "graceful degradation for mixed values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:value 1.0e-6
                                                     :lower 0.9e-6
                                                     :upper 1.1e-6}}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}]}}
            result (comparison/prepare-comparison-bar-data domain-comparison)
            first-metric (first result)
            data (:data first-metric)]
        (is (true? (:has-error-bounds? first-metric)))
        (let [foo-data (first (filter #(= "foo" (get % "impl")) data))
              bar-data (first (filter #(= "bar" (get % "impl")) data))]
          (is (contains? foo-data "valueLower"))
          (is (contains? foo-data "valueUpper"))
          (is (not (contains? bar-data "valueLower")))
          (is (not (contains? bar-data "valueUpper"))))))))

;;; Tests for prepare-comparison-box-data helper.
;;; Verifies box plot data preparation from domain-comparison data with bootstrap stats.

(deftest prepare-comparison-box-data-test
  ;; Tests box plot data preparation for domain-comparison with bootstrap stats.
  ;; Verifies correct extraction of median, CI bounds, and percentiles.
  (testing "prepare-comparison-box-data"
    (testing "prepares data for single-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar :baz]
                               :data {:foo [{:coord {:n 100}
                                             :value {:median 1.0e-6 :ci-lower 0.9e-6 :ci-upper 1.1e-6
                                                     :p10 0.8e-6 :p90 1.2e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:median 2.0e-6 :ci-lower 1.8e-6 :ci-upper 2.2e-6
                                                     :p10 1.6e-6 :p90 2.4e-6}}]
                                      :baz [{:coord {:n 100}
                                             :value {:median 1.5e-6 :ci-lower 1.3e-6 :ci-upper 1.7e-6
                                                     :p10 1.2e-6 :p90 1.8e-6}}]}}
            result (comparison/prepare-comparison-box-data domain-comparison)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (nil? (:metric-id (first result))))
        (is (= 3 (count (:data (first result)))))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") (:data (first result))))))))

    (testing "prepares data for multi-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :implementations [:foo :bar]
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:n 100}
                                                        :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                                                 :bar [{:coord {:n 100}
                                                        :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
                                         :thread-allocation
                                         {:metric [:stats :thread-allocation :mean]
                                          :data {:foo [{:coord {:n 100}
                                                        :value {:median 1000 :p10 800 :p90 1200}}]
                                                 :bar [{:coord {:n 100}
                                                        :value {:median 2000 :p10 1600 :p90 2400}}]}}}}
            result (comparison/prepare-comparison-box-data domain-comparison)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "extracts median, p10, p90 values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (comparison/prepare-comparison-box-data domain-comparison)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))))

    (testing "extracts CI bounds when present"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:median 1.0e-6 :ci-lower 0.9e-6 :ci-upper 1.1e-6
                                                     :p10 0.8e-6 :p90 1.2e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:median 2.0e-6 :ci-lower 1.8e-6 :ci-upper 2.2e-6
                                                     :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (comparison/prepare-comparison-box-data domain-comparison)
            data (:data (first result))]
        (is (every? #(contains? % "ciLower") data))
        (is (every? #(contains? % "ciUpper") data))
        ;; Verify order: ciLower < median < ciUpper
        (doseq [d data]
          (is (< (get d "ciLower") (get d "median")))
          (is (< (get d "median") (get d "ciUpper"))))))

    (testing "omits CI bounds when not present"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (comparison/prepare-comparison-box-data domain-comparison)
            data (:data (first result))]
        (is (every? #(not (contains? % "ciLower")) data))
        (is (every? #(not (contains? % "ciUpper")) data))))

    (testing "y-title contains 'median'"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (comparison/prepare-comparison-box-data domain-comparison)
            y-title (:y-title (first result))]
        (is (re-find #"median" y-title))))

    (testing "warns and returns empty for missing bootstrap stats"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}]}}
            output (with-out-str
                     (let [result (comparison/prepare-comparison-box-data domain-comparison)]
                       (is (empty? result))))]
        ;; Should have printed a warning
        (is (re-find #"WARNING.*bootstrap" output))))))

;;; Tests for prepare-line-chart-data helper.
;;; Verifies line chart data preparation from domain-extract data.

(deftest prepare-line-chart-data-test
  ;; Tests line chart data preparation for domain-extract
  (testing "prepare-line-chart-data"
    (testing "prepares data with x, y, impl fields"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]
                                              [{:n 200 :impl :foo} 1.5e-6]
                                              [{:n 200 :impl :bar} 2.5e-6]]}}}
            result (comparison/prepare-line-chart-data domain-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))
        (is (= "n" (:x-title (first result))))
        (is (string? (:y-title (first result))))
        (let [data (:data (first result))]
          (is (= 4 (count data)))
          (is (every? #(contains? % "x") data))
          (is (every? #(contains? % "y") data))
          (is (every? #(contains? % "impl") data))
          (is (= #{100 200} (set (map #(get % "x") data))))
          (is (= #{"foo" "bar"} (set (map #(get % "impl") data)))))))

    (testing "handles multiple metrics"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]]}
                                      :thread-allocation
                                      {:metric [:stats :thread-allocation :mean]
                                       :data [[{:n 100 :impl :foo} 1000]
                                              [{:n 100 :impl :bar} 2000]]}}}
            result (comparison/prepare-line-chart-data domain-extract)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "applies SI scaling to y values"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            y-title (:y-title (first result))]
        ;; Should have SI unit in y-title
        (is (or (str/includes? y-title "(")
                (str/includes? y-title "μ")
                (str/includes? y-title "m")))))

    (testing "handles error-bound values"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} {:value 1.0e-6 :error 0.1e-6}]
                                              [{:n 100 :impl :bar} {:value 2.0e-6 :error 0.2e-6}]
                                              [{:n 200 :impl :foo} {:value 1.5e-6 :error 0.1e-6}]
                                              [{:n 200 :impl :bar} {:value 2.5e-6 :error 0.2e-6}]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        (testing "includes 'mean' in y-title for error-bound values"
          (is (str/starts-with? y-title "mean ")))))

    (testing "does not prefix y-title with 'mean' for plain values"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]
                                              [{:n 200 :impl :foo} 1.5e-6]
                                              [{:n 200 :impl :bar} 2.5e-6]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            {:keys [y-title]} (first result)]
        (is (not (str/starts-with? y-title "mean ")))))

    (testing "returns has-error-bounds? false for plain values"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]
                                              [{:n 200 :impl :foo} 1.5e-6]
                                              [{:n 200 :impl :bar} 2.5e-6]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        (is (every? #(not (contains? % "yLower")) (:data first-metric)))
        (is (every? #(not (contains? % "yUpper")) (:data first-metric)))))

    (testing "extracts error bounds when :lower/:upper present"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo}
                                               {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                                              [{:n 100 :impl :bar}
                                               {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]
                                              [{:n 200 :impl :foo}
                                               {:value 1.5e-6 :lower 1.4e-6 :upper 1.6e-6}]
                                              [{:n 200 :impl :bar}
                                               {:value 2.5e-6 :lower 2.3e-6 :upper 2.7e-6}]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "yLower") data))
          (is (every? #(contains? % "yUpper") data))
          (doseq [d data]
            (is (< (get d "yLower") (get d "y")))
            (is (< (get d "y") (get d "yUpper")))))))

    (testing "graceful degradation for mixed values with bounds"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo}
                                               {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                                              [{:n 100 :impl :bar} 2.0e-6]
                                              [{:n 200 :impl :foo} 1.5e-6]
                                              [{:n 200 :impl :bar}
                                               {:value 2.5e-6 :lower 2.3e-6 :upper 2.7e-6}]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            first-metric (first result)
            data (:data first-metric)]
        (is (true? (:has-error-bounds? first-metric)))
        (let [foo-100 (first (filter #(and (= "foo" (get % "impl"))
                                           (= 100 (get % "x")))
                                     data))
              bar-100 (first (filter #(and (= "bar" (get % "impl"))
                                           (= 100 (get % "x")))
                                     data))]
          (is (contains? foo-100 "yLower"))
          (is (contains? foo-100 "yUpper"))
          (is (not (contains? bar-100 "yLower")))
          (is (not (contains? bar-100 "yUpper"))))))))

;;; Edge case tests

(deftest edge-cases-nil-values-test
  ;; Tests handling of nil values in metric data vectors
  (testing "nil values in metric data"
    (testing "prepare-line-chart-data handles nil values in data"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} nil]
                                              [{:n 100 :impl :bar} 2.0e-6]
                                              [{:n 200 :impl :foo} 1.5e-6]
                                              [{:n 200 :impl :bar} nil]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            data (:data (first result))]
        ;; Should include points with non-nil y values
        (is (= 4 (count data)))
        ;; nil values should result in nil y values
        (is (some #(nil? (get % "y")) data))))))

(deftest edge-cases-mixed-value-formats-test
  ;; Tests handling of mixed error-bound and plain values in the same extract
  (testing "mixed error-bound and plain values"
    (testing "prepare-line-chart-data handles mixed value formats"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} {:value 1.0e-6 :error 0.1e-6}]
                                              [{:n 100 :impl :bar} 2.0e-6] ; plain value
                                              [{:n 200 :impl :foo} 1.5e-6] ; plain value
                                              [{:n 200 :impl :bar} {:value 2.5e-6 :error 0.2e-6}]]}}}
            result (comparison/prepare-line-chart-data domain-extract)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        ;; When any value has error bounds, y-title should have "mean " prefix
        (is (str/starts-with? y-title "mean "))))

    (testing "prepare-comparison-line-data handles mixed value formats"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value {:value 1.0e-6 :error 0.1e-6}}
                                            {:coord {:n 200} :value 1.5e-6}] ; plain value
                                      :bar [{:coord {:n 100} :value 2.0e-6} ; plain value
                                            {:coord {:n 200} :value {:value 2.5e-6 :error 0.2e-6}}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        ;; When any value has error bounds, y-title should have "mean " prefix
        (is (str/starts-with? y-title "mean "))))))

;;; Tests for prepare-comparison-line-data helper.
;;; Verifies line chart data preparation from domain-comparison data.

(deftest prepare-comparison-line-data-test
  ;; Tests line chart data preparation for domain-comparison
  (testing "prepare-comparison-line-data"
    (testing "prepares data for single-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                            {:coord {:n 200} :value 1.5e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}
                                            {:coord {:n 200} :value 2.5e-6}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (nil? (:metric-id (first result))))
        (is (= "n" (:x-title (first result))))
        (let [data (:data (first result))]
          (is (= 4 (count data)))
          (is (every? #(contains? % "x") data))
          (is (every? #(contains? % "y") data))
          (is (every? #(contains? % "impl") data))
          (is (= #{100 200} (set (map #(get % "x") data))))
          (is (= #{"foo" "bar"} (set (map #(get % "impl") data)))))))

    (testing "prepares data for multi-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :implementations [:foo :bar]
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                                       {:coord {:n 200} :value 1.5e-6}]
                                                 :bar [{:coord {:n 100} :value 2.0e-6}
                                                       {:coord {:n 200} :value 2.5e-6}]}}
                                         :thread-allocation
                                         {:metric [:stats :thread-allocation :mean]
                                          :data {:foo [{:coord {:n 100} :value 1000}
                                                       {:coord {:n 200} :value 1500}]
                                                 :bar [{:coord {:n 100} :value 2000}
                                                       {:coord {:n 200} :value 2500}]}}}}
            result (comparison/prepare-comparison-line-data domain-comparison)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))
        (doseq [metric-result result]
          (is (= 4 (count (:data metric-result))))
          (is (= "n" (:x-title metric-result))))))

    (testing "handles error-bound values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value {:value 1.0e-6 :error 0.1e-6}}
                                            {:coord {:n 200} :value {:value 1.5e-6 :error 0.1e-6}}]
                                      :bar [{:coord {:n 100} :value {:value 2.0e-6 :error 0.2e-6}}
                                            {:coord {:n 200} :value {:value 2.5e-6 :error 0.2e-6}}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        (testing "includes 'mean' in y-title for error-bound values"
          (is (str/starts-with? y-title "mean ")))))

    (testing "does not prefix y-title with 'mean' for plain values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                            {:coord {:n 200} :value 1.5e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}
                                            {:coord {:n 200} :value 2.5e-6}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            {:keys [y-title]} (first result)]
        (is (not (str/starts-with? y-title "mean ")))))

    (testing "returns has-error-bounds? false for plain values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                            {:coord {:n 200} :value 1.5e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}
                                            {:coord {:n 200} :value 2.5e-6}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        (is (every? #(not (contains? % "yLower")) (:data first-metric)))
        (is (every? #(not (contains? % "yUpper")) (:data first-metric)))))

    (testing "extracts error bounds for single-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:value 1.0e-6
                                                     :lower 0.9e-6
                                                     :upper 1.1e-6}}
                                            {:coord {:n 200}
                                             :value {:value 1.5e-6
                                                     :lower 1.4e-6
                                                     :upper 1.6e-6}}]
                                      :bar [{:coord {:n 100}
                                             :value {:value 2.0e-6
                                                     :lower 1.8e-6
                                                     :upper 2.2e-6}}
                                            {:coord {:n 200}
                                             :value {:value 2.5e-6
                                                     :lower 2.3e-6
                                                     :upper 2.7e-6}}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "yLower") data))
          (is (every? #(contains? % "yUpper") data))
          (doseq [d data]
            (is (< (get d "yLower") (get d "y")))
            (is (< (get d "y") (get d "yUpper")))))))

    (testing "extracts error bounds for multi-metric comparison"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :implementations [:foo :bar]
                               :metrics {:elapsed-time
                                         {:metric [:stats :elapsed-time :mean]
                                          :data {:foo [{:coord {:n 100}
                                                        :value {:value 1.0e-6
                                                                :lower 0.9e-6
                                                                :upper 1.1e-6}}
                                                       {:coord {:n 200}
                                                        :value {:value 1.5e-6
                                                                :lower 1.4e-6
                                                                :upper 1.6e-6}}]
                                                 :bar [{:coord {:n 100}
                                                        :value {:value 2.0e-6
                                                                :lower 1.8e-6
                                                                :upper 2.2e-6}}
                                                       {:coord {:n 200}
                                                        :value {:value 2.5e-6
                                                                :lower 2.3e-6
                                                                :upper 2.7e-6}}]}}}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "yLower") data))
          (is (every? #(contains? % "yUpper") data)))))

    (testing "graceful degradation for mixed values"
      (let [domain-comparison {:type :criterium/domain-comparison
                               :axis :n
                               :metric [:stats :elapsed-time :mean]
                               :implementations [:foo :bar]
                               :data {:foo [{:coord {:n 100}
                                             :value {:value 1.0e-6
                                                     :lower 0.9e-6
                                                     :upper 1.1e-6}}
                                            {:coord {:n 200} :value 1.5e-6}]
                                      :bar [{:coord {:n 100} :value 2.0e-6}
                                            {:coord {:n 200}
                                             :value {:value 2.5e-6
                                                     :lower 2.3e-6
                                                     :upper 2.7e-6}}]}}
            result (comparison/prepare-comparison-line-data domain-comparison)
            first-metric (first result)
            data (:data first-metric)]
        (is (true? (:has-error-bounds? first-metric)))
        (let [foo-100 (first (filter #(and (= "foo" (get % "impl"))
                                           (= 100 (get % "x")))
                                     data))
              foo-200 (first (filter #(and (= "foo" (get % "impl"))
                                           (= 200 (get % "x")))
                                     data))]
          (is (contains? foo-100 "yLower"))
          (is (contains? foo-100 "yUpper"))
          (is (not (contains? foo-200 "yLower")))
          (is (not (contains? foo-200 "yUpper"))))))))

;;; Integration test: compare-by → prepare-comparison-box-data flow

(deftest compare-by-to-box-data-integration-test
  ;; Tests the full flow from domain with bootstrap stats through compare-by
  ;; to prepare-comparison-box-data. Validates that bootstrap box plot data
  ;; is correctly extracted and formatted for chart rendering.
  (testing "full flow from compare-by to box data preparation"
    (let [;; Create a comparison result with bootstrap stats merged into value
          ;; This simulates the output of compare-by when bootstrap stats exist
          domain-comparison
          {:type :criterium/domain-comparison
           :axis :impl
           :metrics
           {:elapsed-time
            {:metric [:stats :elapsed-time :mean]
             :with-error-bounds false
             :data {:foo [{:coord {:n 100 :impl :foo}
                           :value {:value 100.0
                                   :median 100.0
                                   :p10 90.0
                                   :p90 110.0
                                   :ci-lower 95.0
                                   :ci-upper 105.0}}]
                    :bar [{:coord {:n 100 :impl :bar}
                           :value {:value 200.0
                                   :median 200.0
                                   :p10 180.0
                                   :p90 220.0
                                   :ci-lower 190.0
                                   :ci-upper 210.0}}]}}}
           :implementations [:foo :bar]}
          result (comparison/prepare-comparison-box-data domain-comparison)]
      (is (= 1 (count result)) "should have one metric result")
      (let [metric-result (first result)
            data (:data metric-result)]
        (is (= :elapsed-time (:metric-id metric-result)))
        (is (= 2 (count data)) "should have two impl data points")
        ;; Verify foo data
        (let [foo-data (first (filter #(= "foo" (get % "impl")) data))]
          (is (some? foo-data) "should have foo data")
          (is (number? (get foo-data "median")) "median should be numeric")
          (is (number? (get foo-data "p10")) "p10 should be numeric")
          (is (number? (get foo-data "p90")) "p90 should be numeric")
          (is (number? (get foo-data "ciLower")) "ciLower should be numeric")
          (is (number? (get foo-data "ciUpper")) "ciUpper should be numeric")
          ;; Verify relationships
          (is (< (get foo-data "p10")
                 (get foo-data "ciLower")
                 (get foo-data "median")
                 (get foo-data "ciUpper")
                 (get foo-data "p90"))
              "values should be in correct order"))))))

;;; Tests for prepare-domain-comparison-table-transposed helper.
;;; Verifies transposed table preparation with bootstrapped median values.

(deftest prepare-domain-comparison-table-transposed-test
  ;; Tests transposed table data preparation for single-point domain comparison.
  ;; Verifies correct extraction of median values, CI bounds display, and factor calculation.
  (testing "prepare-domain-comparison-table-transposed"
    (testing "extracts median values from bootstrap stats"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value {:median 1.0e-6
                                                         :value 1.1e-6}}]
                                          :bar [{:coord {:impl :bar}
                                                 :value {:median 2.0e-6
                                                         :value 2.2e-6}}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            rows (:rows result)]
        (is (= 2 (count rows)))
        ;; The median column header should contain "median"
        (is (some #(str/includes? % "median") (:col-headers result)))))

    (testing "includes CI columns when CI bounds present"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value {:median 1.0e-6
                                                         :ci-lower 0.9e-6
                                                         :ci-upper 1.1e-6}}]
                                          :bar [{:coord {:impl :bar}
                                                 :value {:median 2.0e-6
                                                         :ci-lower 1.8e-6
                                                         :ci-upper 2.2e-6}}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            col-headers (:col-headers result)]
        ;; Should have CI column
        (is (some #(str/includes? % "CI") col-headers))
        ;; Implementation + median + CI + factor = 4 columns
        (is (= 4 (count col-headers)))))

    (testing "omits CI columns when CI bounds absent"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value {:median 1.0e-6}}]
                                          :bar [{:coord {:impl :bar}
                                                 :value {:median 2.0e-6}}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            col-headers (:col-headers result)]
        ;; Should not have CI column
        (is (not (some #(str/includes? % "CI") col-headers)))
        ;; Implementation + median + factor = 3 columns
        (is (= 3 (count col-headers)))))

    (testing "calculates factor using median values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value {:median 1.0e-6
                                                         :value 1.5e-6}}]
                                          :bar [{:coord {:impl :bar}
                                                 :value {:median 2.0e-6
                                                         :value 3.0e-6}}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            rows (:rows result)
            foo-row (first (filter #(= "foo" (get % "Implementation")) rows))
            bar-row (first (filter #(= "bar" (get % "Implementation")) rows))
            factor-header (first (filter #(str/includes? % "×") (:col-headers result)))]
        ;; Baseline (foo) factor should be 1.00
        (is (= "1.00" (get foo-row factor-header)))
        ;; Bar factor should be 2.0 (2.0e-6 / 1.0e-6) based on median, not 2.0 (3.0e-6 / 1.5e-6)
        (is (= "2.00" (get bar-row factor-header)))))

    (testing "formats CI bounds as range"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value {:median 1.0e-6
                                                         :ci-lower 0.9e-6
                                                         :ci-upper 1.1e-6}}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            rows (:rows result)
            foo-row (first rows)
            ci-header (first (filter #(str/includes? % "CI") (:col-headers result)))
            ci-value (get foo-row ci-header)]
        ;; CI should be formatted as "lower - upper"
        (is (string? ci-value))
        (is (str/includes? ci-value " - "))))

    (testing "falls back to :value when :median absent"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value {:value 1.0e-6}}]
                                          :bar [{:coord {:impl :bar}
                                                 :value {:value 2.0e-6}}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            rows (:rows result)
            bar-row (first (filter #(= "bar" (get % "Implementation")) rows))
            factor-header (first (filter #(str/includes? % "×") (:col-headers result)))]
        ;; Factor should still work using :value fallback
        (is (= "2.00" (get bar-row factor-header)))))

    (testing "handles plain numeric values (backward compatibility)"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:impl :foo}
                                                 :value 1.0e-6}]
                                          :bar [{:coord {:impl :bar}
                                                 :value 2.0e-6}]}}}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)
            rows (:rows result)
            bar-row (first (filter #(= "bar" (get % "Implementation")) rows))
            factor-header (first (filter #(str/includes? % "×") (:col-headers result)))]
        ;; Factor should work with plain numeric values
        (is (= "2.00" (get bar-row factor-header)))))

    (testing "handles single-metric mode"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:impl :foo}
                                      :value {:median 1.0e-6}}]
                               :bar [{:coord {:impl :bar}
                                      :value {:median 2.0e-6}}]}}
            result (comparison/prepare-domain-comparison-table-transposed comparison)]
        (is (map? result))
        (is (= 2 (count (:rows result))))
        (is (some #(str/includes? % "median") (:col-headers result)))))))
