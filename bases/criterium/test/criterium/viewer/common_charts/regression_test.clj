(ns criterium.viewer.common-charts.regression-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.regression :as charts.regression]
   [criterium.viewer.schema-validation :as schema]))

;;; Standard regression chart schema validation tests

(deftest regression-chart-spec-schema-validation-test
  ;; Validates regression-chart-spec output against Vega-Lite v6 schema.
  ;; Tests regression scatter plot with fit lines.
  (testing "regression-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [points [{:x 100 :y 1e6}
                    {:x 200 :y 2e6}
                    {:x 400 :y 4e6}]
            line-pts (mapv (fn [{:keys [x]}]
                             {:x x :y (* 10000.0 (double x)) :model "O(n)"})
                           points)
            opts {:width 600
                  :height 400
                  :axis-name "n"
                  :y-title "Time (ns)"
                  :color-field "model"}
            spec (charts.regression/regression-chart-spec points line-pts opts)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "regression-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

(deftest regression-chart-spec-tooltip-test
  ;; Verifies regression chart scatter points include tooltips showing x and y values.
  (testing "regression-chart-spec"
    (testing "includes tooltips with x and y values"
      (let [points [{"x" 100 "y" 1e6}
                    {"x" 200 "y" 2e6}]
            line-pts [{"x" 100 "y" 1e6 "model" "O(n)"}
                      {"x" 200 "y" 2e6 "model" "O(n)"}]
            opts {:axis-name "n"
                  :y-title "Time (ns)"
                  :color-field "model"}
            spec (charts.regression/regression-chart-spec points line-pts opts)
            scatter-layer (first (:layer spec))
            tooltip (get-in scatter-layer [:encoding :tooltip])]
        (is (vector? tooltip))
        (is (some #(= "n" (:title %)) tooltip))
        (is (some #(= "Time (ns)" (:title %)) tooltip))))
    (testing "includes color field in tooltip when multi-impl"
      (let [points [{"x" 100 "y" 1e6 "impl" "foo"}
                    {"x" 200 "y" 2e6 "impl" "bar"}]
            line-pts [{"x" 100 "y" 1e6 "impl" "foo"}
                      {"x" 200 "y" 2e6 "impl" "bar"}]
            opts {:axis-name "n"
                  :y-title "Time (ns)"
                  :color-field "impl"}
            spec (charts.regression/regression-chart-spec points line-pts opts)
            scatter-layer (first (:layer spec))
            tooltip (get-in scatter-layer [:encoding :tooltip])]
        (is (vector? tooltip))
        (is (some #(= "Implementation" (:title %)) tooltip))
        (is (some #(= "n" (:title %)) tooltip))))))

(deftest regression-residual-spec-schema-validation-test
  ;; Validates regression-residual-spec output against Vega-Lite v6 schema.
  ;; Tests residual plot with loess smoothing.
  (testing "regression-residual-spec"
    (testing "produces valid Vega-Lite spec"
      (let [residual-pts [{:x 100 :residual 0.05 :model "O(n)"}
                          {:x 200 :residual -0.02 :model "O(n)"}
                          {:x 400 :residual 0.01 :model "O(n)"}]
            opts {:width 600
                  :height 200
                  :axis-name "n"
                  :residual-title "Residual"
                  :color-field "model"}
            spec (charts.regression/regression-residual-spec residual-pts opts)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "regression-residual-spec validation failed: "
                 (pr-str (:errors result))))))))

;;; Log-Log Chart Tests
;;
;; Tests for log-log regression chart functions.
;; These charts display log-transformed data where slope indicates complexity class.

(def sample-log-log-points
  "Sample log-log scatter plot points."
  [{"x" 2.3 "y" 4.6}
   {"x" 3.0 "y" 6.0}
   {"x" 3.7 "y" 7.4}
   {"x" 4.4 "y" 8.8}])

(def sample-log-log-line-points
  "Sample log-log fit line points."
  [{"x" 2.0 "y" 4.0}
   {"x" 3.0 "y" 6.0}
   {"x" 4.0 "y" 8.0}
   {"x" 5.0 "y" 10.0}])

(def sample-log-log-residuals
  "Sample log-log residual points."
  [{"x" 2.3 "residual" 0.01}
   {"x" 3.0 "residual" -0.02}
   {"x" 3.7 "residual" 0.01}
   {"x" 4.4 "residual" -0.01}])

(deftest log-log-chart-spec-test
  ;; Tests the log-log-chart-spec function for structure correctness.
  ;; Contracts: returns valid Vega-Lite spec with correct layers.
  (testing "log-log-chart-spec"
    (testing "produces valid Vega-Lite spec structure"
      (let [spec (charts.regression/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:width 600 :height 400 :axis-name "n" :metric-name "time"})]
        (is (map? spec))
        (is (contains? spec :width))
        (is (contains? spec :height))
        (is (contains? spec :layer))
        (is (= 600 (:width spec)))
        (is (= 400 (:height spec)))
        ;; Should have scatter layer and line layer
        (is (>= (count (:layer spec)) 2))))
    (testing "includes error bar layer when error bounds present"
      (let [points-with-error [{"x" 2.3 "y" 4.6 "yLower" 4.4 "yUpper" 4.8}]
            spec (charts.regression/log-log-chart-spec
                  points-with-error
                  sample-log-log-line-points
                  {:has-error-bounds? true :metric-name "time"})]
        ;; Should have scatter, line, and error bar layers
        (is (= 3 (count (:layer spec))))))
    (testing "includes title with slope and r-squared when provided"
      (let [spec (charts.regression/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:slope 1.02 :r-squared 0.998 :metric-name "time"})]
        (is (some? (:title spec)))
        (is (string? (:title spec)))))
    (testing "handles multi-impl with color field"
      (let [points [{"x" 2.3 "y" 4.6 "impl" "vec"}
                    {"x" 2.3 "y" 5.0 "impl" "list"}]
            line-pts [{"x" 2.0 "y" 4.0 "impl" "vec"}
                      {"x" 2.0 "y" 4.5 "impl" "list"}]
            spec (charts.regression/log-log-chart-spec
                  points line-pts
                  {:color-field "impl" :metric-name "time"})]
        (is (map? spec))
        ;; Check that color encoding exists in scatter layer
        (let [scatter-layer (first (:layer spec))]
          (is (contains? (get-in scatter-layer [:encoding :color]) :field)))))
    (testing "uses provided metric-name in y-axis title"
      (let [spec (charts.regression/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:axis-name "n" :metric-name "allocation"})
            scatter-layer (first (:layer spec))
            y-title (get-in scatter-layer [:encoding :y :title])]
        (is (= "log(allocation)" y-title))))))

(deftest log-log-residual-spec-test
  ;; Tests the log-log-residual-spec function for structure correctness.
  (testing "log-log-residual-spec"
    (testing "produces valid Vega-Lite spec structure"
      (let [spec (charts.regression/log-log-residual-spec
                  sample-log-log-residuals
                  {:width 600 :height 200 :axis-name "n"})]
        (is (map? spec))
        (is (contains? spec :width))
        (is (contains? spec :height))
        (is (contains? spec :layer))
        (is (= 200 (:height spec)))
        ;; Should have scatter layer, loess layer, and zero line
        (is (= 3 (count (:layer spec))))))
    (testing "uses log axis title"
      (let [spec (charts.regression/log-log-residual-spec
                  sample-log-log-residuals
                  {:axis-name "n"})
            scatter-layer (first (:layer spec))
            x-title (get-in scatter-layer [:encoding :x :title])]
        (is (= "log(n)" x-title))))))

(deftest log-log-chart-spec-schema-validation-test
  ;; Validates log-log-chart-spec output against Vega-Lite v5 schema.
  (testing "log-log-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.regression/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:width 600 :height 400 :axis-name "n" :metric-name "time"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-chart-spec validation failed: "
                 (pr-str (:errors result))))))
    (testing "with error bounds produces valid spec"
      (let [points-with-error [{"x" 2.3 "y" 4.6 "yLower" 4.4 "yUpper" 4.8}
                               {"x" 3.0 "y" 6.0 "yLower" 5.8 "yUpper" 6.2}]
            spec (charts.regression/log-log-chart-spec
                  points-with-error
                  sample-log-log-line-points
                  {:has-error-bounds? true :metric-name "time"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-chart-spec with error bounds failed: "
                 (pr-str (:errors result))))))
    (testing "with color field produces valid spec"
      (let [points [{"x" 2.3 "y" 4.6 "impl" "vec"}
                    {"x" 3.0 "y" 6.0 "impl" "list"}]
            line-pts [{"x" 2.0 "y" 4.0 "impl" "vec"}
                      {"x" 3.0 "y" 6.0 "impl" "list"}]
            spec (charts.regression/log-log-chart-spec
                  points line-pts
                  {:color-field "impl" :metric-name "time"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-chart-spec with color field failed: "
                 (pr-str (:errors result))))))))

(deftest log-log-residual-spec-schema-validation-test
  ;; Validates log-log-residual-spec output against Vega-Lite v5 schema.
  (testing "log-log-residual-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.regression/log-log-residual-spec
                  sample-log-log-residuals
                  {:width 600 :height 200 :axis-name "n"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            "log-log-residual-spec validation failed")))))
