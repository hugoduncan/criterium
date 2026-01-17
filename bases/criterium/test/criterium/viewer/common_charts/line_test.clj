(ns criterium.viewer.common-charts.line-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.schema-validation :as schema]))

;;; Test fixtures for line chart tests.

(def multi-point-extract
  "Sample multi-point multi-impl extract for line chart testing."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo} 1.0e-6]
                     [{:n 100 :impl :bar} 2.0e-6]
                     [{:n 200 :impl :foo} 1.5e-6]
                     [{:n 200 :impl :bar} 2.5e-6]
                     [{:n 400 :impl :foo} 2.0e-6]
                     [{:n 400 :impl :bar} 3.5e-6]]}}})

(def multi-point-extract-with-bounds
  "Sample multi-point extract with error bounds for line chart testing."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo}
                      {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                     [{:n 100 :impl :bar}
                      {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]
                     [{:n 200 :impl :foo}
                      {:value 1.5e-6 :lower 1.3e-6 :upper 1.7e-6}]
                     [{:n 200 :impl :bar}
                      {:value 2.5e-6 :lower 2.3e-6 :upper 2.7e-6}]
                     [{:n 400 :impl :foo}
                      {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]
                     [{:n 400 :impl :bar}
                      {:value 3.5e-6 :lower 3.2e-6 :upper 3.8e-6}]]}}})

(def single-impl-extract
  "Sample single-impl multi-point extract for line chart testing."
  {:type :criterium/domain-extract
   :implementations [:default]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100} 1.0e-6]
                     [{:n 200} 1.5e-6]
                     [{:n 400} 2.0e-6]]}}})

(def single-impl-extract-with-bounds
  "Sample single-impl extract with error bounds for line chart testing."
  {:type :criterium/domain-extract
   :implementations [:default]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100} {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                     [{:n 200} {:value 1.5e-6 :lower 1.3e-6 :upper 1.7e-6}]
                     [{:n 400} {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]]}}})

(def multi-point-comparison
  "Sample multi-point multi-impl comparison for line chart testing."
  {:type :criterium/domain-comparison
   :axis :n
   :metric [:stats :elapsed-time :mean]
   :implementations [:foo :bar]
   :data {:foo [{:coord {:n 100} :value 1.0e-6}
                {:coord {:n 200} :value 1.5e-6}
                {:coord {:n 400} :value 2.0e-6}]
          :bar [{:coord {:n 100} :value 2.0e-6}
                {:coord {:n 200} :value 2.5e-6}
                {:coord {:n 400} :value 3.5e-6}]}})

(def multi-point-comparison-with-bounds
  "Sample comparison with error bounds for line chart testing."
  {:type :criterium/domain-comparison
   :axis :n
   :metric [:stats :elapsed-time :mean]
   :implementations [:foo :bar]
   :data {:foo [{:coord {:n 100}
                 :value {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}}
                {:coord {:n 200}
                 :value {:value 1.5e-6 :lower 1.3e-6 :upper 1.7e-6}}
                {:coord {:n 400}
                 :value {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}}]
          :bar [{:coord {:n 100}
                 :value {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}}
                {:coord {:n 200}
                 :value {:value 2.5e-6 :lower 2.3e-6 :upper 2.7e-6}}
                {:coord {:n 400}
                 :value {:value 3.5e-6 :lower 3.2e-6 :upper 3.8e-6}}]}})

;;; Domain line chart tests.
;;; Verifies line chart generation for single-axis multi-point comparison scenarios.

(deftest domain-line-chart-spec-test
  ;; Tests line chart spec generation for single-axis multi-point comparisons.
  ;; Contracts: returns Vega-Lite spec with :vconcat, line mark with points,
  ;; x/y/color encodings, shared legend.
  (testing "domain-line-chart-spec"
    (testing "produces valid structure"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes line mark with points"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "encodes axis value on x-axis"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "x" (:field x-encoding)))
        (is (= "quantitative" (:type x-encoding)))
        (is (= "n" (:title x-encoding)))))

    (testing "encodes value on y-axis"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (= "y" (:field y-encoding)))
        (is (= "quantitative" (:type y-encoding)))))

    (testing "encodes implementation as color"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            color-encoding (get-in chart [:encoding :color])]
        (is (= "impl" (:field color-encoding)))
        (is (= "nominal" (:type color-encoding)))
        (is (= {:title "Implementation"} (:legend color-encoding)))))

    (testing "respects chart dimensions"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))

    (testing "includes tooltip"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            tooltip (get-in chart [:encoding :tooltip])]
        (is (vector? tooltip))
        (is (= 3 (count tooltip)))))

    (testing "shares legend across vconcated charts"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})]
        (is (= {:legend {:color "shared"}} (:resolve spec)))))))

;;; Single-impl line chart tests.
;;; Verifies line chart generation for single-implementation extracts.

(deftest domain-line-chart-spec-single-impl-test
  ;; Tests line chart spec for single-implementation extracts.
  ;; Contracts: same structure, data uses single impl name.
  (testing "domain-line-chart-spec with single implementation"
    (testing "produces valid structure"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes line mark with points"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "chart data uses single impl name"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            data (get-in chart [:data :values])]
        (is (= 3 (count data)))
        (is (every? #(= "default" (get % "impl")) data))))))

(deftest domain-line-chart-spec-single-impl-with-bounds-test
  ;; Tests line chart spec for single-impl with error bounds.
  ;; Contracts: layered structure with confidence band + line.
  (testing "domain-line-chart-spec single-impl with error bounds"
    (testing "produces layered structure"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes confidence band layer with area mark"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))]
        (is (= "area" (get-in band-layer [:mark :type])))
        (is (= 0.2 (get-in band-layer [:mark :opacity])))))

    (testing "data includes bounds"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))
            data (get-in band-layer [:data :values])]
        (is (every? #(contains? % "yLower") data))
        (is (every? #(contains? % "yUpper") data))))))

;;; Line chart confidence band tests.
;;; Verifies confidence band generation for line charts with error bounds.

(deftest domain-line-chart-with-confidence-bands-test
  ;; Tests line chart spec generation when error bounds are present.
  ;; Contracts: layered spec with confidence band layer (area) + line layer.
  (testing "domain-line-chart-spec with error bounds"
    (testing "produces layered structure"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes confidence band layer with area mark"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))]
        (is (= "area" (get-in band-layer [:mark :type])))
        (is (= 0.2 (get-in band-layer [:mark :opacity])))))

    (testing "confidence band layer encodes y/y2 for bounds"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))
            encoding (:encoding band-layer)]
        (is (= "yLower" (get-in encoding [:y :field])))
        (is (= "yUpper" (get-in encoding [:y2 :field])))))

    (testing "includes line layer with line mark"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            line-layer (second (:layer chart))]
        (is (= "line" (get-in line-layer [:mark :type])))
        (is (true? (get-in line-layer [:mark :point])))))

    (testing "confidence band data includes bounds"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))
            data (get-in band-layer [:data :values])]
        (is (every? #(contains? % "yLower") data))
        (is (every? #(contains? % "yUpper") data))))

    (testing "respects chart dimensions"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))))

(deftest domain-line-chart-graceful-degradation-test
  ;; Tests that line charts without error bounds render normally.
  ;; Contracts: no layered structure when no bounds, simple line mark.
  (testing "domain-line-chart-spec without error bounds"
    (testing "produces simple structure without layer"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "still includes line mark and encodings"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= "x" (get-in chart [:encoding :x :field])))
        (is (= "y" (get-in chart [:encoding :y :field])))
        (is (= "impl" (get-in chart [:encoding :color :field])))))))

;;; Comparison line chart tests.
;;; Verifies line chart generation from domain-comparison data.

(deftest comparison-line-chart-spec-test
  ;; Tests line chart spec generation from domain-comparison data.
  ;; Contracts: same structure as domain-line-chart-spec.
  (testing "comparison-line-chart-spec"
    (testing "produces valid structure"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes line mark with points"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "encodes axis value on x-axis"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "x" (:field x-encoding)))
        (is (= "quantitative" (:type x-encoding)))
        (is (= "n" (:title x-encoding)))))

    (testing "encodes implementation as color"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            color-encoding (get-in chart [:encoding :color])]
        (is (= "impl" (:field color-encoding)))
        (is (= "nominal" (:type color-encoding)))
        (is (= {:title "Implementation"} (:legend color-encoding)))))

    (testing "shares legend across vconcated charts"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})]
        (is (= {:legend {:color "shared"}} (:resolve spec)))))))

(deftest comparison-line-chart-with-confidence-bands-test
  ;; Tests comparison line chart spec when error bounds are present.
  ;; Contracts: layered spec with confidence band + line.
  (testing "comparison-line-chart-spec with error bounds"
    (testing "produces layered structure"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes confidence band layer with area mark"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))]
        (is (= "area" (get-in band-layer [:mark :type])))
        (is (= 0.2 (get-in band-layer [:mark :opacity])))))))

(deftest comparison-line-chart-graceful-degradation-test
  ;; Tests that comparison line charts without error bounds render normally.
  ;; Contracts: no layered structure when no bounds.
  (testing "comparison-line-chart-spec without error bounds"
    (testing "produces simple structure without layer"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "line" :point true} (:mark chart)))))))

;;; Schema validation tests.
;;; Validates line chart specs against Vega-Lite v6 schema.

(deftest line-chart-schema-validation-test
  ;; Validates all line chart variants produce valid Vega-Lite specs.
  ;; Contracts: all specs pass Vega-Lite v6 schema validation.
  (testing "line chart schema validation"
    (testing "domain-line-chart-spec produces valid spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "domain-line-chart-spec with confidence bands produces valid spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "single-impl line chart produces valid spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "single-impl line chart with bounds produces valid spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "comparison-line-chart-spec produces valid spec"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "comparison-line-chart-spec with confidence bands produces valid spec"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))))
