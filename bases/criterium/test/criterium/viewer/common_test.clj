(ns criterium.viewer.common-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common :as common]))

;;; Domain shape detection tests.
;;; Verifies single-point-multi-impl? correctly identifies the scenario where
;;; we have multiple implementations at a single parameter point.

(deftest single-point-multi-impl?-test
  ;; Tests the detection of single-point comparison scenarios
  ;; (one axis value, multiple implementations)
  (testing "single-point-multi-impl?"
    (testing "returns true when one axis with one value and multiple impls"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}]
        (is (true? (common/single-point-multi-impl? extract)))))

    (testing "returns false when one axis with multiple values"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}]
        (is (not (common/single-point-multi-impl? extract)))))

    (testing "returns false when single implementation"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]]}}}]
        (is (not (common/single-point-multi-impl? extract)))))

    (testing "returns false when no implementations key"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]]}}}]
        (is (not (common/single-point-multi-impl? extract)))))

    (testing "returns false when multiple non-impl axes"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :m 10 :impl :foo} 1.0e-6]
                                       [{:n 100 :m 10 :impl :bar} 2.0e-6]]}}}]
        (is (not (common/single-point-multi-impl? extract)))))

    (testing "returns nil for nil extract"
      (is (nil? (common/single-point-multi-impl? nil))))))

;;; Tests for single-axis-multi-point? helper.
;;; Verifies detection of the line chart scenario where we have multiple
;;; implementations across a range of values on a single axis.

(deftest single-axis-multi-point?-test
  (testing "single-axis-multi-point?"
    (testing "returns true when one axis with multiple values and multiple impls"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}]
        (is (true? (common/single-axis-multi-point? extract)))))

    (testing "returns false when one axis with single value"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}]
        (is (not (common/single-axis-multi-point? extract)))))

    (testing "returns false when single implementation"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (not (common/single-axis-multi-point? extract)))))

    (testing "returns false when no implementations key"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (not (common/single-axis-multi-point? extract)))))

    (testing "returns false when multiple non-impl axes"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :m 10 :impl :foo} 1.0e-6]
                                       [{:n 100 :m 10 :impl :bar} 2.0e-6]
                                       [{:n 200 :m 20 :impl :foo} 1.5e-6]
                                       [{:n 200 :m 20 :impl :bar} 2.5e-6]]}}}]
        (is (not (common/single-axis-multi-point? extract)))))

    (testing "returns nil for nil extract"
      (is (nil? (common/single-axis-multi-point? nil))))))

;;; Tests for visualization-strategy helper.
;;; Verifies the helper returns correct strategy keywords based on extract shape.

(deftest visualization-strategy-test
  ;; Tests visualization strategy selection for domain-extract
  (testing "visualization-strategy"
    (testing "returns :single-point for single-point multi-impl"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}]
        (is (= :single-point (common/visualization-strategy extract)))))

    (testing "returns :multi-point for multi-point single-axis"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}]
        (is (= :multi-point (common/visualization-strategy extract)))))

    (testing "returns :default-table for single implementation"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (= :default-table (common/visualization-strategy extract)))))

    (testing "returns :default-table for multiple non-impl axes"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :m 10 :impl :foo} 1.0e-6]
                                       [{:n 100 :m 10 :impl :bar} 2.0e-6]]}}}]
        (is (= :default-table (common/visualization-strategy extract)))))

    (testing "returns :default-table for nil extract"
      (is (= :default-table (common/visualization-strategy nil))))))

;;; Tests for prepare-domain-extract-table helper.
;;; Verifies table generation with correct column key/header matching.

(deftest prepare-domain-extract-table-test
  ;; Tests prepare-domain-extract-table for various scenarios
  (testing "prepare-domain-extract-table"
    (testing "single-impl multi-point uses string key matching column-name"
      ;; This test verifies the fix for issue where axis values were blank
      ;; because coord-header was used as keyword key but column-names are strings
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 10 :impl :default} 1.0e-6]
                                       [{:n 50 :impl :default} 2.0e-6]
                                       [{:n 100 :impl :default} 3.0e-6]]}}}
            result (common/prepare-domain-extract-table extract {})]
        (is (= "Domain Extract" (:heading result)))
        (is (= "n" (:coord-header result)))
        ;; Key point: row map keys must match column-names (strings, not keywords)
        (is (= 3 (count (:rows result))))
        (let [first-row (first (:rows result))]
          ;; The coord column key should be the string "n", not :n
          (is (contains? first-row "n"))
          (is (= 10 (get first-row "n"))))))

    (testing "multi-impl uses string key for coord column"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 10 :impl :foo} 1.0e-6]
                                       [{:n 10 :impl :bar} 2.0e-6]
                                       [{:n 50 :impl :foo} 1.5e-6]
                                       [{:n 50 :impl :bar} 2.5e-6]]}}}
            result (common/prepare-domain-extract-table extract {})]
        (is (= "n" (:coord-header result)))
        (is (= 2 (count (:rows result))))
        (let [first-row (first (:rows result))]
          (is (contains? first-row "n"))
          (is (= 10 (get first-row "n"))))))

    (testing "returns nil for nil extract"
      (is (nil? (common/prepare-domain-extract-table nil {}))))))

;;; Tests for prepare-domain-extract-table-transposed helper.
;;; Verifies transposed table generation for single-point multi-impl scenarios
;;; where each row is an implementation with value and factor columns.

(deftest prepare-domain-extract-table-transposed-test
  (testing "prepare-domain-extract-table-transposed"
    (testing "returns transposed table with implementation rows"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar :baz]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 100 :impl :baz} 1.5e-6]]}}}
            result (common/prepare-domain-extract-table-transposed extract)]
        (is (= "Domain Extract" (:heading result)))
        (is (vector? (:col-headers result)))
        (is (= "Implementation" (first (:col-headers result))))
        (is (= 3 (count (:rows result))))
        (is (= "foo" (get (first (:rows result)) "Implementation")))
        (is (= "bar" (get (second (:rows result)) "Implementation")))
        (is (= "baz" (get (nth (:rows result) 2) "Implementation")))))

    (testing "includes factor columns relative to baseline"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}
            result (common/prepare-domain-extract-table-transposed extract)
            foo-row (first (:rows result))
            bar-row (second (:rows result))]
        ;; Baseline (foo) should have factor 1.00
        (is (= "1.00" (get foo-row "elapsed-time ×")))
        ;; Bar is 2x baseline
        (is (= "2.00" (get bar-row "elapsed-time ×")))))

    (testing "handles multiple metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}
                               :thread-allocation
                               {:metric [:stats :thread-allocation :mean]
                                :data [[{:n 100 :impl :foo} 1000]
                                       [{:n 100 :impl :bar} 500]]}}}
            result (common/prepare-domain-extract-table-transposed extract)
            col-headers (:col-headers result)]
        ;; Should have Implementation + 2 metrics * 2 columns each = 5 headers
        (is (= 5 (count col-headers)))
        (is (= "Implementation" (first col-headers)))
        ;; Should have value and factor columns for each metric
        (is (some #(str/includes? % "elapsed-time") col-headers))
        (is (some #(str/includes? % "thread-allocation") col-headers))
        (is (some #(str/ends-with? % "×") col-headers))))

    (testing "applies SI scaling to values"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]]}}}
            result (common/prepare-domain-extract-table-transposed extract)
            col-headers (:col-headers result)]
        ;; Should have SI unit in header (μs or similar for microseconds)
        (is (some #(or (str/includes? % "(")
                       (str/includes? % "μ")
                       (str/includes? % "m"))
                  col-headers))))

    (testing "returns nil for nil extract"
      (is (nil? (common/prepare-domain-extract-table-transposed nil))))))

;;; Tests for single-point-multi-impl-comparison? helper.
;;; Verifies detection of single-point scenarios in domain-comparison data.

(deftest single-point-multi-impl-comparison?-test
  (testing "single-point-multi-impl-comparison?"
    (testing "returns true for single-metric single-point with multiple impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}]
        (is (true? (common/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false for multi-point comparison"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}]
        (is (not (common/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false for single implementation"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (not (common/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false when no implementations key"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (not (common/single-point-multi-impl-comparison? comparison)))))

    (testing "returns true for multi-metric single-point with multiple impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                                          :bar [{:coord {:n 100} :value 2.0e-6}]}}}}]
        (is (true? (common/single-point-multi-impl-comparison? comparison)))))

    (testing "returns true when axis is impl-axis with single n value"
      ;; When :axis :impl with same n value across all impls, it's single-point
      ;; (bar chart scenario) because there's only one parameter point.
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:impl :foo :n 100} :value 1.0e-6}]
                               :bar [{:coord {:impl :bar :n 100} :value 2.0e-6}]}}]
        (is (true? (common/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false when axis is impl-axis with multiple n values"
      ;; When :axis :impl with varying n values, it's multi-point (line chart).
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:impl :foo :n 100} :value 1.0e-6}
                                     {:coord {:impl :foo :n 200} :value 1.5e-6}]
                               :bar [{:coord {:impl :bar :n 100} :value 2.0e-6}
                                     {:coord {:impl :bar :n 200} :value 2.5e-6}]}}]
        (is (not (common/single-point-multi-impl-comparison? comparison)))))))

;;; Tests for prepare-comparison-bar-data helper.
;;; Verifies bar chart data preparation from domain-comparison data.

(deftest prepare-comparison-bar-data-test
  (testing "prepare-comparison-bar-data"
    (testing "prepares data for single-metric comparison"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar :baz]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]
                               :baz [{:coord {:n 100} :value 1.5e-6}]}}
            result (common/prepare-comparison-bar-data comparison)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (nil? (:metric-id (first result))))
        (is (= 3 (count (:data (first result)))))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") (:data (first result))))))))

    (testing "prepares data for multi-metric comparison"
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-bar-data comparison)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "returns has-error-bounds? false for plain values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}
            result (common/prepare-comparison-bar-data comparison)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        (is (every? #(not (contains? % "valueLower")) (:data first-metric)))
        (is (every? #(not (contains? % "valueUpper")) (:data first-metric)))))

    (testing "extracts error bounds for single-metric comparison"
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-bar-data comparison)
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
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-bar-data comparison)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "valueLower") data))
          (is (every? #(contains? % "valueUpper") data)))))

    (testing "graceful degradation for mixed values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100}
                                      :value {:value 1.0e-6
                                              :lower 0.9e-6
                                              :upper 1.1e-6}}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}
            result (common/prepare-comparison-bar-data comparison)
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
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-box-data comparison)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (nil? (:metric-id (first result))))
        (is (= 3 (count (:data (first result)))))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") (:data (first result))))))))

    (testing "prepares data for multi-metric comparison"
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-box-data comparison)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "extracts median, p10, p90 values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100}
                                      :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                               :bar [{:coord {:n 100}
                                      :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (common/prepare-comparison-box-data comparison)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))))

    (testing "extracts CI bounds when present"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100}
                                      :value {:median 1.0e-6 :ci-lower 0.9e-6 :ci-upper 1.1e-6
                                              :p10 0.8e-6 :p90 1.2e-6}}]
                               :bar [{:coord {:n 100}
                                      :value {:median 2.0e-6 :ci-lower 1.8e-6 :ci-upper 2.2e-6
                                              :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (common/prepare-comparison-box-data comparison)
            data (:data (first result))]
        (is (every? #(contains? % "ciLower") data))
        (is (every? #(contains? % "ciUpper") data))
        ;; Verify order: ciLower < median < ciUpper
        (doseq [d data]
          (is (< (get d "ciLower") (get d "median")))
          (is (< (get d "median") (get d "ciUpper"))))))

    (testing "omits CI bounds when not present"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100}
                                      :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                               :bar [{:coord {:n 100}
                                      :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (common/prepare-comparison-box-data comparison)
            data (:data (first result))]
        (is (every? #(not (contains? % "ciLower")) data))
        (is (every? #(not (contains? % "ciUpper")) data))))

    (testing "y-title contains 'median'"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100}
                                      :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
                               :bar [{:coord {:n 100}
                                      :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}}
            result (common/prepare-comparison-box-data comparison)
            y-title (:y-title (first result))]
        (is (re-find #"median" y-title))))

    (testing "warns and returns empty for missing bootstrap stats"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}
            output (with-out-str
                     (let [result (common/prepare-comparison-box-data comparison)]
                       (is (empty? result))))]
        ;; Should have printed a warning
        (is (re-find #"WARNING.*bootstrap" output))))))

;;; Tests for single-axis-multi-point-comparison? helper.
;;; Verifies detection of multi-point line chart scenarios in domain-comparison data.

(deftest single-axis-multi-point-comparison?-test
  ;; Tests detection of line chart scenarios in domain-comparison
  ;; (multiple axis values, multiple implementations)
  (testing "single-axis-multi-point-comparison?"
    (testing "returns true for single-metric multi-point with multiple impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}]
        (is (true? (common/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false for single-point comparison"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}]
        (is (not (common/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false for single implementation"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]}}]
        (is (not (common/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false when no implementations key"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]}}]
        (is (not (common/single-axis-multi-point-comparison? comparison)))))

    (testing "returns true for multi-metric multi-point with multiple impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                                {:coord {:n 200} :value 1.5e-6}]
                                          :bar [{:coord {:n 100} :value 2.0e-6}
                                                {:coord {:n 200} :value 2.5e-6}]}}}}]
        (is (true? (common/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false when axis is impl-axis with single n value"
      ;; Single n value means single-point (bar chart), not multi-point (line chart).
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:impl :foo :n 100} :value 1.0e-6}]
                               :bar [{:coord {:impl :bar :n 100} :value 2.0e-6}]}}]
        (is (not (common/single-axis-multi-point-comparison? comparison)))))

    (testing "returns true when axis is impl-axis with multiple n values"
      ;; Multiple n values means line chart with n on x-axis, impl as color.
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:impl :foo :n 100} :value 1.0e-6}
                                     {:coord {:impl :foo :n 200} :value 1.5e-6}]
                               :bar [{:coord {:impl :bar :n 100} :value 2.0e-6}
                                     {:coord {:impl :bar :n 200} :value 2.5e-6}]}}]
        (is (true? (common/single-axis-multi-point-comparison? comparison)))))))

;;; Tests for comparison-visualization-strategy helper.
;;; Verifies the helper returns correct strategy keywords based on comparison shape.

(deftest comparison-visualization-strategy-test
  ;; Tests visualization strategy selection for domain-comparison
  (testing "comparison-visualization-strategy"
    (testing "returns :single-point for single-point multi-impl"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}]
        (is (= :single-point
               (common/comparison-visualization-strategy comparison)))))

    (testing "returns :multi-point for multi-point single-axis"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}]
        (is (= :multi-point
               (common/comparison-visualization-strategy comparison)))))

    (testing "returns :default-table for single implementation"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]}}]
        (is (= :default-table
               (common/comparison-visualization-strategy comparison)))))

    (testing "returns :default-table when no implementations key"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (= :default-table
               (common/comparison-visualization-strategy comparison)))))))

;;; Tests for prepare-line-chart-data helper.
;;; Verifies line chart data preparation from domain-extract data.

(deftest prepare-line-chart-data-test
  ;; Tests line chart data preparation for domain-extract
  (testing "prepare-line-chart-data"
    (testing "prepares data with x, y, impl fields"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}
            result (common/prepare-line-chart-data extract)]
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
      (let [extract {:type :criterium/domain-extract
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
            result (common/prepare-line-chart-data extract)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "applies SI scaling to y values"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]]}}}
            result (common/prepare-line-chart-data extract)
            y-title (:y-title (first result))]
        ;; Should have SI unit in y-title
        (is (or (str/includes? y-title "(")
                (str/includes? y-title "μ")
                (str/includes? y-title "m")))))

    (testing "handles error-bound values"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} {:value 1.0e-6 :error 0.1e-6}]
                                       [{:n 100 :impl :bar} {:value 2.0e-6 :error 0.2e-6}]
                                       [{:n 200 :impl :foo} {:value 1.5e-6 :error 0.1e-6}]
                                       [{:n 200 :impl :bar} {:value 2.5e-6 :error 0.2e-6}]]}}}
            result (common/prepare-line-chart-data extract)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        (testing "includes 'mean' in y-title for error-bound values"
          (is (str/starts-with? y-title "mean ")))))

    (testing "does not prefix y-title with 'mean' for plain values"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}
            result (common/prepare-line-chart-data extract)
            {:keys [y-title]} (first result)]
        (is (not (str/starts-with? y-title "mean ")))))

    (testing "returns has-error-bounds? false for plain values"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}
            result (common/prepare-line-chart-data extract)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        (is (every? #(not (contains? % "yLower")) (:data first-metric)))
        (is (every? #(not (contains? % "yUpper")) (:data first-metric)))))

    (testing "extracts error bounds when :lower/:upper present"
      (let [extract {:type :criterium/domain-extract
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
            result (common/prepare-line-chart-data extract)
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
      (let [extract {:type :criterium/domain-extract
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
            result (common/prepare-line-chart-data extract)
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

;;; Edge case tests for empty data and degenerate inputs.
;;; Verifies that detection helpers and preparation functions handle edge cases
;;; gracefully without throwing exceptions.

(deftest edge-cases-empty-metrics-test
  ;; Tests handling of empty or missing metrics data structures
  (testing "empty metrics map"
    (testing "single-point-multi-impl? returns nil for empty metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {}}]
        (is (nil? (common/single-point-multi-impl? extract)))))

    (testing "single-axis-multi-point? returns nil for empty metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {}}]
        (is (nil? (common/single-axis-multi-point? extract)))))

    (testing "visualization-strategy returns :default-table for empty metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {}}]
        (is (= :default-table (common/visualization-strategy extract))))))

  (testing "metric with empty data"
    (testing "single-point-multi-impl? returns nil for metric with empty data"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data []}}}]
        (is (nil? (common/single-point-multi-impl? extract)))))

    (testing "visualization-strategy returns :default-table for empty data"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data []}}}]
        (is (= :default-table (common/visualization-strategy extract)))))))

(deftest edge-cases-nil-values-test
  ;; Tests handling of nil values in metric data vectors
  (testing "nil values in metric data"
    (testing "single-point-multi-impl? handles nil values in data"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} nil]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}]
        ;; Should still detect single-point structure despite nil value
        (is (true? (common/single-point-multi-impl? extract)))))

    (testing "prepare-line-chart-data handles nil values in data"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} nil]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} nil]]}}}
            result (common/prepare-line-chart-data extract)
            data (:data (first result))]
        ;; Should include points with non-nil y values
        (is (= 4 (count data)))
        ;; nil values should result in nil y values
        (is (some #(nil? (get % "y")) data))))))

(deftest edge-cases-mixed-value-formats-test
  ;; Tests handling of mixed error-bound and plain values in the same extract
  (testing "mixed error-bound and plain values"
    (testing "prepare-line-chart-data handles mixed value formats"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} {:value 1.0e-6 :error 0.1e-6}]
                                       [{:n 100 :impl :bar} 2.0e-6] ; plain value
                                       [{:n 200 :impl :foo} 1.5e-6] ; plain value
                                       [{:n 200 :impl :bar} {:value 2.5e-6 :error 0.2e-6}]]}}}
            result (common/prepare-line-chart-data extract)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        ;; When any value has error bounds, y-title should have "mean " prefix
        (is (str/starts-with? y-title "mean "))))

    (testing "prepare-comparison-line-data handles mixed value formats"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value {:value 1.0e-6 :error 0.1e-6}}
                                     {:coord {:n 200} :value 1.5e-6}] ; plain value
                               :bar [{:coord {:n 100} :value 2.0e-6} ; plain value
                                     {:coord {:n 200} :value {:value 2.5e-6 :error 0.2e-6}}]}}
            result (common/prepare-comparison-line-data comparison)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        ;; When any value has error bounds, y-title should have "mean " prefix
        (is (str/starts-with? y-title "mean "))))))

(deftest edge-cases-empty-implementations-test
  ;; Tests handling of empty implementations vector
  (testing "empty implementations vector"
    (testing "single-point-multi-impl? returns nil for empty implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations []
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]]}}}]
        (is (not (common/single-point-multi-impl? extract)))))

    (testing "single-axis-multi-point? returns nil for empty implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations []
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 200 :impl :foo} 2.0e-6]]}}}]
        (is (not (common/single-axis-multi-point? extract)))))

    (testing "visualization-strategy returns :default-table for empty implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations []
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]]}}}]
        (is (= :default-table (common/visualization-strategy extract))))))

  (testing "comparison with empty implementations"
    (testing "single-point-multi-impl-comparison? returns nil for empty impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations []
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (not (common/single-point-multi-impl-comparison? comparison)))))

    (testing "comparison-visualization-strategy returns :default-table for empty impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations []
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (= :default-table (common/comparison-visualization-strategy comparison)))))))

;;; Tests for prepare-comparison-line-data helper.
;;; Verifies line chart data preparation from domain-comparison data.

(deftest prepare-comparison-line-data-test
  ;; Tests line chart data preparation for domain-comparison
  (testing "prepare-comparison-line-data"
    (testing "prepares data for single-metric comparison"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}
            result (common/prepare-comparison-line-data comparison)]
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
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-line-data comparison)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))
        (doseq [metric-result result]
          (is (= 4 (count (:data metric-result))))
          (is (= "n" (:x-title metric-result))))))

    (testing "handles error-bound values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value {:value 1.0e-6 :error 0.1e-6}}
                                     {:coord {:n 200} :value {:value 1.5e-6 :error 0.1e-6}}]
                               :bar [{:coord {:n 100} :value {:value 2.0e-6 :error 0.2e-6}}
                                     {:coord {:n 200} :value {:value 2.5e-6 :error 0.2e-6}}]}}
            result (common/prepare-comparison-line-data comparison)
            {:keys [y-title data]} (first result)]
        (is (= 4 (count data)))
        (is (every? #(number? (get % "y")) data))
        (testing "includes 'mean' in y-title for error-bound values"
          (is (str/starts-with? y-title "mean ")))))

    (testing "does not prefix y-title with 'mean' for plain values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}
            result (common/prepare-comparison-line-data comparison)
            {:keys [y-title]} (first result)]
        (is (not (str/starts-with? y-title "mean ")))))

    (testing "returns has-error-bounds? false for plain values"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}
            result (common/prepare-comparison-line-data comparison)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        (is (every? #(not (contains? % "yLower")) (:data first-metric)))
        (is (every? #(not (contains? % "yUpper")) (:data first-metric)))))

    (testing "extracts error bounds for single-metric comparison"
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-line-data comparison)
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
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-line-data comparison)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "yLower") data))
          (is (every? #(contains? % "yUpper") data)))))

    (testing "graceful degradation for mixed values"
      (let [comparison {:type :criterium/domain-comparison
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
            result (common/prepare-comparison-line-data comparison)
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

;; Tests for ASCII treemap rendering functions.
;; Verifies ascii-bar generates proportional bars and render-ascii-treemap
;; produces correct tree structure with proper formatting, filtering, and depth limits.

(def sample-treemap
  "Sample allocation treemap for testing."
  {:type :criterium/allocation-treemap
   :group-by :class→line→type
   :size-by :bytes
   :root {:name "allocations"
          :value 1000000
          :children [{:name "MyClass"
                      :value 800000
                      :children [{:name "L42"
                                  :value 600000
                                  :children [{:name "java.lang.String"
                                              :value 400000}
                                             {:name "clojure.lang.Keyword"
                                              :value 200000}]}
                                 {:name "L58"
                                  :value 200000
                                  :children [{:name "java.lang.Long"
                                              :value 200000}]}]}
                     {:name "OtherClass"
                      :value 200000
                      :children [{:name "L10"
                                  :value 200000
                                  :children [{:name "java.util.HashMap"
                                              :value 200000}]}]}]}})

(deftest ascii-bar-test
  (testing "ascii-bar"
    (testing "produces correct proportional bars"
      (is (= "████████████████████" (common/ascii-bar 100.0 100.0 20)))
      (is (= "██████████" (common/ascii-bar 50.0 100.0 20)))
      (is (= "█████" (common/ascii-bar 25.0 100.0 20)))
      (is (= "██" (common/ascii-bar 10.0 100.0 20))))

    (testing "handles zero and negative values"
      (is (= "" (common/ascii-bar 0.0 100.0 20)))
      (is (= "" (common/ascii-bar -10.0 100.0 20)))
      (is (= "" (common/ascii-bar 100.0 0.0 20)))
      (is (= "" (common/ascii-bar 100.0 -10.0 20))))

    (testing "respects width parameter"
      (is (= "██████████" (common/ascii-bar 100.0 100.0 10)))
      (is (= "█████" (common/ascii-bar 100.0 100.0 5)))
      (is (= "██████████████████████████████" (common/ascii-bar 100.0 100.0 30))))

    (testing "caps at max width when value exceeds max"
      (is (= "████████████████████" (common/ascii-bar 150.0 100.0 20))))))

(deftest render-ascii-treemap-test
  (testing "render-ascii-treemap"
    (testing "produces correct tree structure"
      (let [result (common/render-ascii-treemap sample-treemap)
            lines (str/split-lines result)]
        (is (string? result))
        (is (str/starts-with? (first lines) "Allocation Treemap"))
        (is (str/includes? (first lines) "bytes"))
        (is (str/includes? (first lines) "class→line→type"))
        (is (str/includes? (second lines) "allocations/"))
        (is (some #(str/includes? % "├──") lines))
        (is (some #(str/includes? % "└──") lines))
        (is (some #(str/includes? % "│") lines))))

    (testing "shows non-leaf nodes with trailing slash"
      (let [result (common/render-ascii-treemap sample-treemap)]
        (is (str/includes? result "allocations/"))
        (is (str/includes? result "MyClass/"))
        (is (str/includes? result "L42/"))))

    (testing "shows leaf nodes without trailing slash"
      (let [result (common/render-ascii-treemap sample-treemap)]
        (is (str/includes? result "java.lang.String "))
        (is (not (str/includes? result "java.lang.String/")))))

    (testing "shows bars only on leaf nodes"
      (let [result (common/render-ascii-treemap sample-treemap)
            lines (str/split-lines result)]
        (doseq [line lines]
          (when (str/includes? line "█")
            (is (not (str/ends-with? (first (str/split line #"\[")) "/"))
                (str "Bar found on non-leaf: " line))))))

    (testing "respects depth-limit option"
      (let [result (common/render-ascii-treemap
                    sample-treemap
                    {:depth-limit 1})]
        (is (not (str/includes? result "L42")))
        (is (not (str/includes? result "java.lang.String")))
        (is (str/includes? result "MyClass/"))))

    (testing "respects depth-limit 2"
      (let [result (common/render-ascii-treemap
                    sample-treemap
                    {:depth-limit 2})]
        (is (str/includes? result "L42/"))
        (is (not (str/includes? result "java.lang.String")))))

    (testing "filters by min-percent"
      (let [result (common/render-ascii-treemap sample-treemap {:min-percent 25})]
        (is (str/includes? result "MyClass/"))
        (is (str/includes? result "L42/"))
        (is (not (str/includes? result "OtherClass")))
        (is (not (str/includes? result "L58")))))

    (testing "handles empty children"
      (let [empty-treemap {:type :criterium/allocation-treemap
                           :group-by :class→line→type
                           :size-by :bytes
                           :root {:name "allocations" :value 0}}
            result (common/render-ascii-treemap empty-treemap)]
        (is (string? result))
        (is (str/includes? result "Allocation Treemap"))
        (is (str/includes? result "allocations/"))))

    (testing "handles nil root"
      (let [nil-treemap {:type :criterium/allocation-treemap :root nil}
            result (common/render-ascii-treemap nil-treemap)]
        (is (= "" result))))

    (testing "formats sizes correctly"
      (let [result (common/render-ascii-treemap sample-treemap)]
        (is (or (str/includes? result "Kb")
                (str/includes? result "Mb")
                (str/includes? result "bytes")))))

    (testing "respects name-width option"
      (let [result-wide (common/render-ascii-treemap sample-treemap {:name-width 60})
            result-narrow (common/render-ascii-treemap sample-treemap {:name-width 30})
            lines-wide (str/split-lines result-wide)
            lines-narrow (str/split-lines result-narrow)]
        (is (> (count (second lines-wide)) (count (second lines-narrow))))))

    (testing "respects bar-width option"
      (let [result-wide (common/render-ascii-treemap sample-treemap {:bar-width 30})
            result-narrow (common/render-ascii-treemap sample-treemap {:bar-width 10})]
        (is (> (count (filter #(= % \█) result-wide))
               (count (filter #(= % \█) result-narrow))))))

    (testing "shows correct header for different size-by options"
      (is (str/includes?
           (common/render-ascii-treemap (assoc sample-treemap :size-by :count))
           "by count"))
      (is (str/includes?
           (common/render-ascii-treemap (assoc sample-treemap :size-by :bytes-per-allocation))
           "by bytes/alloc")))

    (testing "shows correct header for different group-by options"
      (is (str/includes?
           (common/render-ascii-treemap (assoc sample-treemap :group-by :type→class→line))
           "type→class→line")))))

(def deep-long-names-treemap
  "Treemap with deep nesting and long class names for alignment testing."
  {:type :criterium/allocation-treemap
   :group-by :class→line→type
   :size-by :bytes
   :root {:name "allocations"
          :value 248
          :children
          [{:name "clojure.lang.PersistentVector$TransientVector"
            :value 112
            :children [{:name "L767"
                        :value 72
                        :children [{:name "clojure.lang.PersistentVector$Node"
                                    :value 72}]}
                       {:name "L720"
                        :value 40
                        :children [{:name "clojure.lang.PersistentVector"
                                    :value 40}]}]}
           {:name "clojure.lang.PersistentVector"
            :value 72
            :children [{:name "L69"
                        :value 72
                        :children
                        [{:name "clojure.lang.PersistentVector$TransientVector"
                          :value 32}
                         {:name "clojure.lang.PersistentVector$Node"
                          :value 24}
                         {:name "java.util.concurrent.atomic.AtomicReference"
                          :value 16}]}]}
           {:name "clojure.lang.Compiler"
            :value 64
            :children [{:name "L7757"
                        :value 64
                        :children [{:name "clojure.lang.PersistentVector"
                                    :value 40}
                                   {:name "java.lang.Long"
                                    :value 24}]}]}]}})

(deftest treemap-alignment-test
  ;; Tests that treemap leaf bars start in the same column and tree structure
  ;; is preserved even with deep nesting and long class names that require truncation.
  (testing "treemap-alignment"
    (testing "aligns leaf bars in same column"
      (let [result (common/render-ascii-treemap deep-long-names-treemap)
            lines (str/split-lines result)
            leaf-lines (filter #(str/includes? % "█") lines)
            ;; Extract the position where the bar starts (first █ character)
            bar-positions (map #(.indexOf ^String % "█") leaf-lines)]
        ;; All bars should start at the same position
        (is (apply = bar-positions)
            (str "Bar positions differ: " (vec bar-positions)
                 "\nLines:\n" (str/join "\n" leaf-lines)))))

    (testing "preserves tree connectors without corruption"
      (let [result (common/render-ascii-treemap deep-long-names-treemap)]
        ;; Should not contain replacement characters
        (is (not (str/includes? result "�"))
            "Found replacement character in output")
        ;; All tree connectors should be intact
        (is (or (str/includes? result "├── ")
                (str/includes? result "├──"))
            "Missing branch connector")
        (is (or (str/includes? result "└── ")
                (str/includes? result "└──"))
            "Missing last-child connector")))

    (testing "truncates long names from left with ellipsis"
      (let [result (common/render-ascii-treemap deep-long-names-treemap
                                                {:name-width 40})]
        ;; Long names should be truncated with ellipsis prefix
        (is (str/includes? result "…")
            "Expected ellipsis for truncated names")))

    (testing "maintains fixed column width for all lines"
      (let [result (common/render-ascii-treemap deep-long-names-treemap
                                                {:name-width 40})
            lines (str/split-lines result)
            ;; Skip header line, check data lines
            data-lines (rest lines)
            ;; Find position of first [ in each line (start of size)
            bracket-positions (map #(.indexOf ^String % "[") data-lines)]
        ;; All size brackets should start at same position (column 41, 0-indexed 40)
        (is (apply = bracket-positions)
            (str "Size column positions differ: " (vec bracket-positions)
                 "\nLines:\n" (str/join "\n" data-lines)))))

    (testing "handles last-child at multiple nesting levels"
      ;; This specifically tests the case where continuation prefixes
      ;; are "    " (spaces) from multiple last-child ancestors
      (let [result (common/render-ascii-treemap deep-long-names-treemap)
            lines (str/split-lines result)]
        ;; The Compiler branch is last at level 1, L7757 is last at level 2
        ;; Their children should have proper tree structure
        (is (some #(and (str/includes? % "java.lang.Long")
                        (str/includes? % "└──"))
                  lines)
            "Expected java.lang.Long as last child with └── connector")))))

;;; Log-Log regression view helper tests.
;;; Verifies data preparation functions for log-log charts.

(deftest prepare-log-log-points-test
  ;; Tests prepare-log-log-points which formats log-log regression data for charts.
  ;; Contracts: returns correct structure, handles error bounds, supports multi-impl.
  (testing "prepare-log-log-points"
    (testing "returns points in log space"
      (let [log-log-data {:log-xs [(Math/log 10) (Math/log 20)]
                          :log-ys [(Math/log 100) (Math/log 200)]}
            result (common/prepare-log-log-points
                    log-log-data
                    {:axis :n})]
        (is (map? result))
        (is (= 2 (count (:points result))))
        (is (= "n" (:axis-name result)))
        (is (= (Math/log 10) (get (first (:points result)) "x")))
        (is (= (Math/log 100) (get (first (:points result)) "y")))))
    (testing "includes error bounds when present"
      (let [log-log-data {:log-xs [(Math/log 10)]
                          :log-ys [(Math/log 100)]
                          :log-lowers [(Math/log 90)]
                          :log-uppers [(Math/log 110)]}
            result (common/prepare-log-log-points
                    log-log-data
                    {:axis :n})]
        (is (true? (:has-error-bounds? result)))
        (is (= (Math/log 90) (get (first (:points result)) "yLower")))
        (is (= (Math/log 110) (get (first (:points result)) "yUpper")))))
    (testing "returns nil for missing data"
      (is (nil? (common/prepare-log-log-points nil {:axis :n})))
      (is (nil? (common/prepare-log-log-points {} {:axis :n}))))
    (testing "handles multi-implementation data"
      (let [log-log-data {:by-impl {:vec {:log-xs [(Math/log 10)]
                                          :log-ys [(Math/log 100)]}
                                    :list {:log-xs [(Math/log 10)]
                                           :log-ys [(Math/log 200)]}}}
            result (common/prepare-log-log-points
                    log-log-data
                    {:axis :n :impl-axis :impl})]
        (is (= 2 (count (:points result))))
        (is (some #(= "vec" (get % "impl")) (:points result)))
        (is (some #(= "list" (get % "impl")) (:points result)))))))

(deftest prepare-log-log-fit-line-test
  ;; Tests prepare-log-log-fit-line which generates fit line points.
  ;; Uses slope and intercept to compute line: y = slope * x + intercept
  (testing "prepare-log-log-fit-line"
    (testing "generates fit line points"
      (let [log-log-data {:slope 1.0
                          :intercept 0.0
                          :log-xs [(Math/log 10) (Math/log 100)]}
            result (common/prepare-log-log-fit-line
                    log-log-data
                    {:axis :n})]
        (is (vector? result))
        (is (> (count result) 2))
        ;; Check that y = x (slope 1, intercept 0)
        (is (every? #(< (Math/abs (- (double (get % "y")) (double (get % "x")))) 0.01) result))))
    (testing "returns nil for missing data"
      (is (nil? (common/prepare-log-log-fit-line nil {:axis :n})))
      (is (nil? (common/prepare-log-log-fit-line {} {:axis :n}))))
    (testing "handles multi-implementation data"
      (let [log-log-data {:by-impl {:vec {:slope 1.0
                                          :intercept 0.0
                                          :log-xs [(Math/log 10) (Math/log 20)]}
                                    :list {:slope 2.0
                                           :intercept 0.0
                                           :log-xs [(Math/log 10) (Math/log 20)]}}}
            result (common/prepare-log-log-fit-line
                    log-log-data
                    {:axis :n :impl-axis :impl})]
        (is (vector? result))
        (is (some #(= "vec" (get % "impl")) result))
        (is (some #(= "list" (get % "impl")) result))))))

(deftest prepare-log-log-residuals-test
  ;; Tests prepare-log-log-residuals which formats residual points.
  ;; Residuals are pre-computed in analysis layer.
  (testing "prepare-log-log-residuals"
    (testing "returns residual points"
      (let [log-log-data {:log-xs [(Math/log 10) (Math/log 20)]
                          :residuals [0.01 -0.02]}
            result (common/prepare-log-log-residuals
                    log-log-data
                    {:axis :n})]
        (is (vector? result))
        (is (= 2 (count result)))
        (is (= 0.01 (get (first result) "residual")))
        (is (= (Math/log 10) (get (first result) "x")))))
    (testing "returns nil for missing data"
      (is (nil? (common/prepare-log-log-residuals nil {:axis :n})))
      (is (nil? (common/prepare-log-log-residuals {} {:axis :n}))))
    (testing "handles multi-implementation data"
      (let [log-log-data {:by-impl {:vec {:log-xs [(Math/log 10)]
                                          :residuals [0.01]}
                                    :list {:log-xs [(Math/log 10)]
                                           :residuals [-0.01]}}}
            result (common/prepare-log-log-residuals
                    log-log-data
                    {:axis :n :impl-axis :impl})]
        (is (= 2 (count result)))
        (is (some #(= "vec" (get % "impl")) result))
        (is (some #(= "list" (get % "impl")) result))))))

(deftest format-log-log-slope-test
  ;; Tests formatting of log-log slope as complexity class estimate.
  ;; The function uses a 0.05 (5%) tolerance for integer rounding.
  (testing "format-log-log-slope"
    (testing "exact integer slopes use simplified form"
      (is (= "O(1)" (common/format-log-log-slope 0.0)))
      (is (= "O(n)" (common/format-log-log-slope 1.0)))
      (is (= "O(n²)" (common/format-log-log-slope 2.0)))
      (is (= "O(n³)" (common/format-log-log-slope 3.0))))

    (testing "slopes greater than 3 use O(n^k) form"
      (is (= "O(n^4)" (common/format-log-log-slope 4.0)))
      (is (= "O(n^5)" (common/format-log-log-slope 5.0))))

    (testing "slopes within 0.05 of integer round to integer form"
      ;; Near 0: tolerance = 0.05
      (is (= "O(1)" (common/format-log-log-slope 0.04)))
      (is (= "O(1)" (common/format-log-log-slope -0.04)))
      ;; Near 1
      (is (= "O(n)" (common/format-log-log-slope 0.96)))
      (is (= "O(n)" (common/format-log-log-slope 1.04)))
      ;; Near 2
      (is (= "O(n²)" (common/format-log-log-slope 1.96)))
      (is (= "O(n²)" (common/format-log-log-slope 2.04)))
      ;; Near 3
      (is (= "O(n³)" (common/format-log-log-slope 2.96)))
      (is (= "O(n³)" (common/format-log-log-slope 3.04))))

    (testing "slopes outside 0.05 tolerance show decimal form"
      ;; Just outside the 0.05 threshold (testing boundary)
      (is (= "O(n^0.06)" (common/format-log-log-slope 0.06)))
      (is (= "O(n^0.94)" (common/format-log-log-slope 0.94)))
      (is (= "O(n^1.06)" (common/format-log-log-slope 1.06)))
      (is (= "O(n^1.94)" (common/format-log-log-slope 1.94)))
      (is (= "O(n^2.06)" (common/format-log-log-slope 2.06)))
      (is (= "O(n^2.94)" (common/format-log-log-slope 2.94))))

    (testing "non-integer slopes show two decimal places"
      (is (= "O(n^0.50)" (common/format-log-log-slope 0.5)))
      (is (= "O(n^1.50)" (common/format-log-log-slope 1.5)))
      (is (= "O(n^2.50)" (common/format-log-log-slope 2.5)))
      (is (= "O(n^1.23)" (common/format-log-log-slope 1.23))))

    (testing "edge cases at exact tolerance boundary"
      ;; The 0.05 threshold uses strict < comparison. Due to floating point
      ;; representation, values like 2.05, 2.95, 3.05 round to integer form
      ;; (their diff from nearest int is 0.04999... < 0.05), while 0.05, 0.95,
      ;; 1.05, 1.95 show decimal (their diff is exactly 0.05 or slightly more).
      (is (= "O(n^0.05)" (common/format-log-log-slope 0.05)))
      (is (= "O(n^0.95)" (common/format-log-log-slope 0.95)))
      (is (= "O(n^1.05)" (common/format-log-log-slope 1.05)))
      (is (= "O(n^1.95)" (common/format-log-log-slope 1.95)))
      ;; Larger values round to integer due to floating-point representation
      (is (= "O(n²)" (common/format-log-log-slope 2.05)))
      (is (= "O(n³)" (common/format-log-log-slope 2.95)))
      (is (= "O(n³)" (common/format-log-log-slope 3.05)))
      ;; Just inside boundary: 0.049 is within tolerance (< 0.05)
      (is (= "O(1)" (common/format-log-log-slope 0.049)))
      (is (= "O(n)" (common/format-log-log-slope 0.951)))
      (is (= "O(n)" (common/format-log-log-slope 1.049)))
      (is (= "O(n²)" (common/format-log-log-slope 1.951)))
      (is (= "O(n²)" (common/format-log-log-slope 2.049)))
      (is (= "O(n³)" (common/format-log-log-slope 2.951)))
      (is (= "O(n³)" (common/format-log-log-slope 3.049))))))

;;; Regression model table tests.
;;; Verifies prepare-regression-model-table and prepare-regression-model-table-multi-impl
;;; correctly format model data for table rendering, including AIC and BIC columns.

(deftest prepare-regression-model-table-test
  ;; Tests prepare-regression-model-table which formats model data for table rendering.
  ;; Contracts: returns vector of row maps with :model :r-squared :aic :bic :equation :best-fit keys.
  (testing "prepare-regression-model-table"
    (testing "includes AIC and BIC columns"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95
                     :aic 10.5
                     :bic 12.3}
                    {:id :quadratic
                     :label "O(n²)"
                     :equation-str "y = 0.1x² + 0.5"
                     :r-squared 0.98
                     :aic 8.2
                     :bic 10.1}]
            result (common/prepare-regression-model-table
                    {:models models :best-fit :quadratic}
                    {})]
        (is (vector? result))
        (is (= 2 (count result)))
        (let [best-row (first (filter #(= "✓" (:best-fit %)) result))
              other-row (first (filter #(= "" (:best-fit %)) result))]
          (is (= "O(n²)" (:model best-row)))
          (is (= "0.9800" (:r-squared best-row)))
          (is (= "8.2" (:aic best-row)))
          (is (= "10.1" (:bic best-row)))
          (is (= "10.5" (:aic other-row)))
          (is (= "12.3" (:bic other-row))))))

    (testing "handles nil AIC/BIC values"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95
                     :aic nil
                     :bic nil}]
            result (common/prepare-regression-model-table
                    {:models models :best-fit :linear}
                    {})]
        (is (= 1 (count result)))
        (is (nil? (:aic (first result))))
        (is (nil? (:bic (first result))))))

    (testing "handles missing AIC/BIC keys"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95}]
            result (common/prepare-regression-model-table
                    {:models models :best-fit :linear}
                    {})]
        (is (= 1 (count result)))
        (is (nil? (:aic (first result))))
        (is (nil? (:bic (first result))))))

    (testing "formats negative AIC/BIC values"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95
                     :aic -15.7
                     :bic -12.3}]
            result (common/prepare-regression-model-table
                    {:models models :best-fit :linear}
                    {})]
        (is (= "-15.7" (:aic (first result))))
        (is (= "-12.3" (:bic (first result))))))

    (testing "returns nil for empty models"
      (is (nil? (common/prepare-regression-model-table {:models [] :best-fit nil} {}))))

    (testing "sorts models by r-squared descending"
      (let [models [{:id :linear :label "O(n)" :r-squared 0.8 :aic 10.0 :bic 12.0}
                    {:id :quadratic :label "O(n²)" :r-squared 0.95 :aic 8.0 :bic 10.0}
                    {:id :log :label "O(log n)" :r-squared 0.7 :aic 15.0 :bic 17.0}]
            result (common/prepare-regression-model-table
                    {:models models :best-fit :quadratic}
                    {})]
        (is (= ["O(n²)" "O(n)" "O(log n)"]
               (mapv :model result)))))))

(deftest prepare-regression-model-table-multi-impl-test
  ;; Tests prepare-regression-model-table-multi-impl which formats multi-impl model data.
  ;; Contracts: returns vector with :implementation :model :r-squared :aic :bic :equation :best-fit.
  (testing "prepare-regression-model-table-multi-impl"
    (testing "includes implementation and AIC/BIC columns"
      (let [by-impl {:vec {:models [{:id :linear :label "O(n)" :r-squared 0.95 :aic 10.5 :bic 12.3}]
                           :best-fit :linear}
                     :list {:models [{:id :linear :label "O(n)" :r-squared 0.85 :aic 15.2 :bic 17.0}]
                            :best-fit :linear}}
            impl-keys [:vec :list]
            result (common/prepare-regression-model-table-multi-impl by-impl impl-keys {})]
        (is (vector? result))
        (is (= 2 (count result)))
        (let [vec-row (first (filter #(= "vec" (:implementation %)) result))
              list-row (first (filter #(= "list" (:implementation %)) result))]
          (is (= "10.5" (:aic vec-row)))
          (is (= "12.3" (:bic vec-row)))
          (is (= "15.2" (:aic list-row)))
          (is (= "17.0" (:bic list-row))))))

    (testing "handles nil AIC/BIC in multi-impl"
      (let [by-impl {:vec {:models [{:id :linear :label "O(n)" :r-squared 0.95}]
                           :best-fit :linear}}
            result (common/prepare-regression-model-table-multi-impl by-impl [:vec] {})]
        (is (nil? (:aic (first result))))
        (is (nil? (:bic (first result))))))))
