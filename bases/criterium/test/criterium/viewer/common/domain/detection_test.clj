(ns criterium.viewer.common.domain.detection-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common.domain.detection :as detection]))

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
        (is (true? (detection/single-point-multi-impl? extract)))))

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
        (is (not (detection/single-point-multi-impl? extract)))))

    (testing "returns false when single implementation"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]]}}}]
        (is (not (detection/single-point-multi-impl? extract)))))

    (testing "returns false when no implementations key"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]]}}}]
        (is (not (detection/single-point-multi-impl? extract)))))

    (testing "returns false when multiple non-impl axes"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :m 10 :impl :foo} 1.0e-6]
                                       [{:n 100 :m 10 :impl :bar} 2.0e-6]]}}}]
        (is (not (detection/single-point-multi-impl? extract)))))

    (testing "returns nil for nil extract"
      (is (nil? (detection/single-point-multi-impl? nil))))))

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
        (is (true? (detection/single-axis-multi-point? extract)))))

    (testing "returns false when one axis with single value"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 100 :impl :bar} 2.0e-6]]}}}]
        (is (not (detection/single-axis-multi-point? extract)))))

    (testing "returns false when single implementation"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (not (detection/single-axis-multi-point? extract)))))

    (testing "returns false when no implementations key"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (not (detection/single-axis-multi-point? extract)))))

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
        (is (not (detection/single-axis-multi-point? extract)))))

    (testing "returns nil for nil extract"
      (is (nil? (detection/single-axis-multi-point? nil))))))

;;; Tests for single-axis-multi-point-any-impl? helper.
;;; Verifies detection of the line chart scenario for both single and
;;; multiple implementations across a range of values on a single axis.

(deftest single-axis-multi-point-any-impl?-test
  (testing "single-axis-multi-point-any-impl?"
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
        (is (true? (detection/single-axis-multi-point-any-impl? extract)))))

    (testing "returns true when one axis with multiple values and single impl"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (true? (detection/single-axis-multi-point-any-impl? extract)))))

    (testing "returns true when no implementations key but multi-point axis"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (true? (detection/single-axis-multi-point-any-impl? extract)))))

    (testing "returns true when coords have :impl but extract has no :impl-axis"
      ;; This is the actual output from domain-builder for single-impl domains:
      ;; coordinates include {:n X :impl :default} but extract has no :impl-axis
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :default} 1.0e-6]
                                       [{:n 200 :impl :default} 2.0e-6]]}}}]
        (is (true? (detection/single-axis-multi-point-any-impl? extract)))))

    (testing "returns false when one axis with single value"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]]}}}]
        (is (not (detection/single-axis-multi-point-any-impl? extract)))))

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
        (is (not (detection/single-axis-multi-point-any-impl? extract)))))

    (testing "returns nil for nil extract"
      (is (nil? (detection/single-axis-multi-point-any-impl? nil))))))

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
        (is (= :single-point (detection/visualization-strategy extract)))))

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
        (is (= :multi-point (detection/visualization-strategy extract)))))

    (testing "returns :multi-point for single implementation with multiple axis values"
      ;; Single-impl multi-point now uses line chart visualization
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100} 1.0e-6]
                                       [{:n 200} 2.0e-6]]}}}]
        (is (= :multi-point (detection/visualization-strategy extract)))))

    (testing "returns :multi-point for single impl with :impl in coords but no :impl-axis"
      ;; Real domain-builder output: coords have :impl but extract has no :impl-axis
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :default} 1.0e-6]
                                       [{:n 200 :impl :default} 2.0e-6]]}}}]
        (is (= :multi-point (detection/visualization-strategy extract)))))

    (testing "returns :default-table for multiple non-impl axes"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :m 10 :impl :foo} 1.0e-6]
                                       [{:n 100 :m 10 :impl :bar} 2.0e-6]]}}}]
        (is (= :default-table (detection/visualization-strategy extract)))))

    (testing "returns :default-table for nil extract"
      (is (= :default-table (detection/visualization-strategy nil))))))

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
        (is (true? (detection/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false for multi-point comparison"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}
                                     {:coord {:n 200} :value 2.5e-6}]}}]
        (is (not (detection/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false for single implementation"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (not (detection/single-point-multi-impl-comparison? comparison)))))

    (testing "returns false when no implementations key"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (not (detection/single-point-multi-impl-comparison? comparison)))))

    (testing "returns true for multi-metric single-point with multiple impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :implementations [:foo :bar]
                        :metrics {:elapsed-time
                                  {:metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                                          :bar [{:coord {:n 100} :value 2.0e-6}]}}}}]
        (is (true? (detection/single-point-multi-impl-comparison? comparison)))))

    (testing "returns true when axis is impl-axis with single n value"
      ;; When :axis :impl with same n value across all impls, it's single-point
      ;; (bar chart scenario) because there's only one parameter point.
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:impl :foo :n 100} :value 1.0e-6}]
                               :bar [{:coord {:impl :bar :n 100} :value 2.0e-6}]}}]
        (is (true? (detection/single-point-multi-impl-comparison? comparison)))))

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
        (is (not (detection/single-point-multi-impl-comparison? comparison)))))))

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
        (is (true? (detection/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false for single-point comparison"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo :bar]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]
                               :bar [{:coord {:n 100} :value 2.0e-6}]}}]
        (is (not (detection/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false for single implementation"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]}}]
        (is (not (detection/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false when no implementations key"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]}}]
        (is (not (detection/single-axis-multi-point-comparison? comparison)))))

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
        (is (true? (detection/single-axis-multi-point-comparison? comparison)))))

    (testing "returns false when axis is impl-axis with single n value"
      ;; Single n value means single-point (bar chart), not multi-point (line chart).
      (let [comparison {:type :criterium/domain-comparison
                        :axis :impl
                        :implementations [:foo :bar]
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:impl :foo :n 100} :value 1.0e-6}]
                               :bar [{:coord {:impl :bar :n 100} :value 2.0e-6}]}}]
        (is (not (detection/single-axis-multi-point-comparison? comparison)))))

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
        (is (true? (detection/single-axis-multi-point-comparison? comparison)))))))

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
               (detection/comparison-visualization-strategy comparison)))))

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
               (detection/comparison-visualization-strategy comparison)))))

    (testing "returns :default-table for single implementation"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations [:foo]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}
                                     {:coord {:n 200} :value 1.5e-6}]}}]
        (is (= :default-table
               (detection/comparison-visualization-strategy comparison)))))

    (testing "returns :default-table when no implementations key"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (= :default-table
               (detection/comparison-visualization-strategy comparison)))))))

;;; Edge case tests for empty data and degenerate inputs.

(deftest edge-cases-empty-metrics-test
  ;; Tests handling of empty or missing metrics data structures
  (testing "empty metrics map"
    (testing "single-point-multi-impl? returns nil for empty metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {}}]
        (is (nil? (detection/single-point-multi-impl? extract)))))

    (testing "single-axis-multi-point? returns nil for empty metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {}}]
        (is (nil? (detection/single-axis-multi-point? extract)))))

    (testing "visualization-strategy returns :default-table for empty metrics"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {}}]
        (is (= :default-table (detection/visualization-strategy extract))))))

  (testing "metric with empty data"
    (testing "single-point-multi-impl? returns nil for metric with empty data"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data []}}}]
        (is (nil? (detection/single-point-multi-impl? extract)))))

    (testing "visualization-strategy returns :default-table for empty data"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:foo :bar]
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data []}}}]
        (is (= :default-table (detection/visualization-strategy extract)))))))

(deftest edge-cases-nil-values-in-detection-test
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
        (is (true? (detection/single-point-multi-impl? extract)))))))

(deftest edge-cases-empty-implementations-detection-test
  ;; Tests handling of empty implementations vector
  (testing "empty implementations vector"
    (testing "single-point-multi-impl? returns false for empty implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations []
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]]}}}]
        (is (not (detection/single-point-multi-impl? extract)))))

    (testing "single-axis-multi-point? returns false for empty implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations []
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]
                                       [{:n 200 :impl :foo} 2.0e-6]]}}}]
        (is (not (detection/single-axis-multi-point? extract)))))

    (testing "visualization-strategy returns :default-table for empty implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations []
                     :metrics {:elapsed-time
                               {:metric [:stats :elapsed-time :mean]
                                :data [[{:n 100 :impl :foo} 1.0e-6]]}}}]
        (is (= :default-table (detection/visualization-strategy extract))))))

  (testing "comparison with empty implementations"
    (testing "single-point-multi-impl-comparison? returns false for empty impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations []
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (not (detection/single-point-multi-impl-comparison? comparison)))))

    (testing "comparison-visualization-strategy returns :default-table for empty impls"
      (let [comparison {:type :criterium/domain-comparison
                        :axis :n
                        :metric [:stats :elapsed-time :mean]
                        :implementations []
                        :data {:foo [{:coord {:n 100} :value 1.0e-6}]}}]
        (is (= :default-table (detection/comparison-visualization-strategy comparison)))))))
