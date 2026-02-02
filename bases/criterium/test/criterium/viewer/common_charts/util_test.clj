(ns criterium.viewer.common-charts.util-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.util :as util]))

;;; Tests for common-charts utility functions.
;;; Verifies metric-type-prefix extraction and chart-layer construction.

(deftest metric-type-prefix-test
  ;; Tests extraction of mean/median prefix from metric paths for y-axis titles.
  ;; Contracts: returns "mean" or "median" when present, nil otherwise.
  (testing "metric-type-prefix"
    (testing "returns \"mean\" for paths containing :mean at index 2"
      (is (= "mean" (util/metric-type-prefix [:elapsed-time :stats :mean])))
      (is (= "mean" (util/metric-type-prefix [:memory :stats :mean :extra]))))
    (testing "returns \"median\" for paths containing :median at index 2"
      (is (= "median" (util/metric-type-prefix [:elapsed-time :stats :median])))
      (is
       (= "median" (util/metric-type-prefix [:memory :stats :median :extra]))))
    (testing "returns nil for paths without :mean or :median at index 2"
      (is (nil? (util/metric-type-prefix [:elapsed-time :stats :variance])))
      (is (nil? (util/metric-type-prefix [:elapsed-time :stats :std-dev]))))
    (testing "returns nil for paths shorter than 3 elements"
      (is (nil? (util/metric-type-prefix [:elapsed-time])))
      (is (nil? (util/metric-type-prefix [:elapsed-time :stats]))))
    (testing "returns nil for non-vector inputs"
      (is (nil? (util/metric-type-prefix nil)))
      (is (nil? (util/metric-type-prefix '(:elapsed-time :stats :mean)))))))

(deftest chart-layer-test
  ;; Tests chart layer construction by merging options with mark and encoding.
  ;; Contracts: returns merged map with :data, :mark, and :encoding keys.
  (testing "chart-layer"
    (testing "merges chart-options with data, mark, and encoding"
      (let [data [{:x 1 :y 2} {:x 2 :y 4}]
            chart-options {:width 400 :height 300}
            mark {:type "line"}
            encoding {:x {:field "x"} :y {:field "y"}}
            result (util/chart-layer data chart-options mark encoding)]
        (is (= {:width 400
                :height 300
                :data {:values data}
                :mark mark
                :encoding encoding}
               result))))
    (testing "handles empty chart-options"
      (let [data [{:x 1}]
            chart-options {}
            mark {:type "point"}
            encoding {:x {:field "x"}}
            result (util/chart-layer data chart-options mark encoding)]
        (is (= {:data {:values data}
                :mark mark
                :encoding encoding}
               result))))
    (testing "handles empty data"
      (let [result (util/chart-layer [] {:title "Empty"} {:type "bar"} {})]
        (is (= {:title "Empty"
                :data {:values []}
                :mark {:type "bar"}
                :encoding {}}
               result))))
    (testing "chart-options keys override if conflicting with result structure"
      ;; chart-layer uses merge with chart-options first, so explicit :data
      ;; in chart-options would be overwritten
      (let [result (util/chart-layer
                    [{:v 1}]
                    {:data "ignored"}
                    {:type "area"}
                    {:x {}})]
        (is (= {:values [{:v 1}]} (:data result))
            "explicit :data in chart-options is overwritten by constructed data")))))
