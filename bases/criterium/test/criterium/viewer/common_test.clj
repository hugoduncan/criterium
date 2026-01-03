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
    (testing "returns :single-point-bar for single-point multi-impl"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}]
        (is (= :single-point-bar (common/visualization-strategy extract)))))

    (testing "returns :multi-point-line for multi-point single-axis"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]
                                       [{:n 200 :impl :foo} 1.5e-6]
                                       [{:n 200 :impl :bar} 2.5e-6]]}}}]
        (is (= :multi-point-line (common/visualization-strategy extract)))))

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
    (testing "returns :single-point-bar for single-point multi-impl"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}]
        (is (= :single-point-bar
               (common/comparison-visualization-strategy comparison)))))

    (testing "returns :multi-point-line for multi-point single-axis"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}]
        (is (= :multi-point-line
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
        (is (not (str/starts-with? y-title "mean ")))))))

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
        (is (not (str/starts-with? y-title "mean ")))))))

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
