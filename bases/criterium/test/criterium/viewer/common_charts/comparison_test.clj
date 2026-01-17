(ns criterium.viewer.common-charts.comparison-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.schema-validation :as schema]))

;;; Single-point bar chart tests.
;;; Verifies bar chart generation for single-point multi-impl comparison scenarios.

(def single-point-extract
  "Sample single-point multi-impl extract for bar chart testing."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar :baz]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo} 1.0e-6]
                     [{:n 100 :impl :bar} 2.0e-6]
                     [{:n 100 :impl :baz} 1.5e-6]]}}})

(deftest prepare-single-point-bar-data-test
  ;; Tests data preparation for single-point bar charts.
  ;; Verifies correct extraction and SI scaling of implementation values.
  (testing "prepare-single-point-bar-data"
    (testing "extracts data for each metric"
      (let [result (charts.comparison/prepare-single-point-bar-data single-point-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))))

    (testing "includes all implementations in data"
      (let [result (charts.comparison/prepare-single-point-bar-data single-point-extract)
            data (:data (first result))]
        (is (= 3 (count data)))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") data))))))

    (testing "applies SI scaling to values"
      (let [result (charts.comparison/prepare-single-point-bar-data single-point-extract)
            first-metric (first result)]
        ;; y-title should contain SI unit
        (is (string? (:y-title first-metric)))
        ;; values should be scaled (not raw nanoseconds)
        (let [data (:data first-metric)
              values (keep #(get % "value") data)]
          (is (seq values))
          ;; All values should be positive numbers
          (is (every? pos? values)))))

    (testing "handles multiple metrics"
      (let [multi-metric-extract
            (assoc-in single-point-extract
                      [:metrics :thread-allocation]
                      {:metric [:stats :thread-allocation :mean]
                       :data [[{:n 100 :impl :foo} 1000]
                              [{:n 100 :impl :bar} 2000]
                              [{:n 100 :impl :baz} 1500]]})
            result (charts.comparison/prepare-single-point-bar-data multi-metric-extract)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "returns has-error-bounds? false for plain values"
      (let [result (charts.comparison/prepare-single-point-bar-data single-point-extract)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
        ;; Data should not have valueLower/valueUpper
        (is (every? #(not (contains? % "valueLower")) (:data first-metric)))
        (is (every? #(not (contains? % "valueUpper")) (:data first-metric)))))

    (testing "extracts error bounds when present"
      (let [extract-with-bounds
            {:type :criterium/domain-extract
             :impl-axis :impl
             :implementations [:foo :bar]
             :metrics {:elapsed-time
                       {:metric [:stats :elapsed-time :mean]
                        :data [[{:n 100 :impl :foo}
                                {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                               [{:n 100 :impl :bar}
                                {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]]}}}
            result (charts.comparison/prepare-single-point-bar-data extract-with-bounds)
            first-metric (first result)]
        ;; Check has-error-bounds? flag
        (is (true? (:has-error-bounds? first-metric)))
        ;; Check that y-title includes "mean" prefix
        (is (re-find #"mean" (:y-title first-metric)))
        ;; Check that data includes error bound fields
        (let [data (:data first-metric)]
          (is (every? #(contains? % "valueLower") data))
          (is (every? #(contains? % "valueUpper") data))
          ;; Verify order: lower < value < upper
          (doseq [d data]
            (is (< (get d "valueLower") (get d "value")))
            (is (< (get d "value") (get d "valueUpper")))))))

    (testing "uses 'median' in y-title when metric path contains :median"
      (let [extract-with-median
            {:type :criterium/domain-extract
             :impl-axis :impl
             :implementations [:foo :bar]
             :metrics {:elapsed-time
                       {:metric [:stats :elapsed-time :median]
                        :data [[{:n 100 :impl :foo}
                                {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                               [{:n 100 :impl :bar}
                                {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]]}}}
            result (charts.comparison/prepare-single-point-bar-data extract-with-median)
            first-metric (first result)]
        (is (re-find #"median" (:y-title first-metric)))))

    (testing "graceful degradation for mixed values"
      ;; When some values have bounds and some don't
      (let [extract-mixed
            {:type :criterium/domain-extract
             :impl-axis :impl
             :implementations [:foo :bar]
             :metrics {:elapsed-time
                       {:metric [:stats :elapsed-time :mean]
                        :data [[{:n 100 :impl :foo}
                                {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                               [{:n 100 :impl :bar} 2.0e-6]]}}}
            result (charts.comparison/prepare-single-point-bar-data extract-mixed)
            first-metric (first result)
            data (:data first-metric)]
        ;; has-error-bounds? true because some have bounds
        (is (true? (:has-error-bounds? first-metric)))
        ;; foo has bounds, bar does not
        (let [foo-data (first (filter #(= "foo" (get % "impl")) data))
              bar-data (first (filter #(= "bar" (get % "impl")) data))]
          (is (contains? foo-data "valueLower"))
          (is (contains? foo-data "valueUpper"))
          (is (not (contains? bar-data "valueLower")))
          (is (not (contains? bar-data "valueUpper"))))))))

(deftest single-point-bar-chart-spec-test
  ;; Tests bar chart spec generation for single-point multi-impl comparisons.
  ;; Verifies correct Vega-Lite structure with implementation bars.
  (testing "single-point-bar-chart-spec"
    (testing "produces valid structure"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes bar mark"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "bar"} (:mark chart)))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "preserves implementation order from data"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding))
            "x-axis sort should be nil to preserve data order")))

    (testing "encodes value on y-axis"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (= "value" (:field y-encoding)))
        (is (= "quantitative" (:type y-encoding)))))

    (testing "respects chart dimensions"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))

    (testing "includes tooltip"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            tooltip (get-in chart [:encoding :tooltip])]
        (is (vector? tooltip))
        (is (= 2 (count tooltip)))))))

(deftest single-point-bar-chart-spec-schema-validation-test
  ;; Validates single-point-bar-chart-spec output against Vega-Lite v6 schema.
  ;; Tests bar chart visualization for implementation comparison.
  (testing "single-point-bar-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "single-point-bar-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

;;; Bar chart error bars tests.
;;; Verifies error bar generation for bar charts with error bounds.

(def single-point-extract-with-bounds
  "Sample single-point extract with error bounds for testing."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar :baz]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo}
                      {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                     [{:n 100 :impl :bar}
                      {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}]
                     [{:n 100 :impl :baz}
                      {:value 1.5e-6 :lower 1.3e-6 :upper 1.7e-6}]]}}})

(deftest single-point-bar-chart-with-error-bars-test
  ;; Tests bar chart spec generation when error bounds are present.
  ;; Verifies layered spec structure with bar layer + error layer.
  (testing "single-point-bar-chart-spec with error bounds"
    (testing "produces layered structure"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes bar layer with bar mark"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            bar-layer (first (:layer chart))]
        (is (= {:type "bar"} (:mark bar-layer)))))

    (testing "includes error layer with rule and tick marks"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            error-composite (second (:layer chart))
            rule-layer (first (:layer error-composite))]
        (is (contains? error-composite :layer) "error layer is a composite")
        (is (= 3 (count (:layer error-composite))) "rule + 2 tick caps")
        (is (= "rule" (get-in rule-layer [:mark :type])))
        (is (= 1.5 (get-in rule-layer [:mark :strokeWidth])))))

    (testing "error rule layer encodes y/y2 for bounds"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            error-composite (second (:layer chart))
            rule-layer (first (:layer error-composite))
            encoding (:encoding rule-layer)]
        (is (= "valueLower" (get-in encoding [:y :field])))
        (is (= "valueUpper" (get-in encoding [:y2 :field])))))

    (testing "error layer data includes bounds"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            error-composite (second (:layer chart))
            rule-layer (first (:layer error-composite))
            data (get-in rule-layer [:data :values])]
        (is (every? #(contains? % "valueLower") data))
        (is (every? #(contains? % "valueUpper") data))))

    (testing "respects chart dimensions"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))))

(deftest single-point-bar-chart-graceful-degradation-test
  ;; Tests that bar charts without error bounds render normally.
  ;; Verifies graceful degradation - no layered structure when no bounds.
  (testing "single-point-bar-chart-spec without error bounds"
    (testing "produces simple structure without layer"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "bar"} (:mark chart)))))

    (testing "still includes bar mark and encodings"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= "impl" (get-in chart [:encoding :x :field])))
        (is (= "value" (get-in chart [:encoding :y :field])))))))

(deftest single-point-bar-chart-with-error-bars-schema-validation-test
  ;; Validates bar chart with error bars against Vega-Lite v6 schema.
  (testing "single-point-bar-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "bar chart with error bars validation failed: "
                 (pr-str (:errors result))))))))

;;; Comparison bar chart tests.
;;; Verifies bar chart generation from domain-comparison data.

(def single-point-comparison
  "Sample single-point multi-impl comparison for bar chart testing."
  {:type :criterium/domain-comparison
   :axis :n
   :metric [:stats :elapsed-time :mean]
   :implementations [:foo :bar :baz]
   :data {:foo [{:coord {:n 100} :value 1.0e-6}]
          :bar [{:coord {:n 100} :value 2.0e-6}]
          :baz [{:coord {:n 100} :value 1.5e-6}]}})

(deftest comparison-bar-chart-spec-test
  ;; Tests bar chart spec generation from domain-comparison data.
  ;; Verifies correct Vega-Lite structure with implementation bars.
  (testing "comparison-bar-chart-spec"
    (testing "produces valid structure"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes bar mark"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "bar"} (:mark chart)))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "preserves implementation order from data"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding))
            "x-axis sort should be nil to preserve data order")))))

(deftest comparison-bar-chart-spec-schema-validation-test
  ;; Validates comparison-bar-chart-spec output against Vega-Lite v6 schema.
  ;; Tests bar chart visualization from domain-comparison data.
  (testing "comparison-bar-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison-bar-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

;;; Comparison bar chart error bars tests.

(def single-point-comparison-with-bounds
  "Sample comparison with error bounds for testing."
  {:type :criterium/domain-comparison
   :axis :n
   :metric [:stats :elapsed-time :mean]
   :implementations [:foo :bar :baz]
   :data {:foo [{:coord {:n 100}
                 :value {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}}]
          :bar [{:coord {:n 100}
                 :value {:value 2.0e-6 :lower 1.8e-6 :upper 2.2e-6}}]
          :baz [{:coord {:n 100}
                 :value {:value 1.5e-6 :lower 1.3e-6 :upper 1.7e-6}}]}})

(deftest comparison-bar-chart-with-error-bars-test
  ;; Tests comparison bar chart spec when error bounds are present.
  ;; Verifies layered spec structure with bar layer + error layer.
  (testing "comparison-bar-chart-spec with error bounds"
    (testing "produces layered structure"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes error layer with rule and tick marks"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            error-composite (second (:layer chart))
            rule-layer (first (:layer error-composite))]
        (is (contains? error-composite :layer) "error layer is a composite")
        (is (= 3 (count (:layer error-composite))) "rule + 2 tick caps")
        (is (= "rule" (get-in rule-layer [:mark :type])))))))

(deftest comparison-bar-chart-graceful-degradation-test
  ;; Tests that comparison bar charts without error bounds render normally.
  (testing "comparison-bar-chart-spec without error bounds"
    (testing "produces simple structure without layer"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "bar"} (:mark chart)))))))

(deftest comparison-bar-chart-with-error-bars-schema-validation-test
  ;; Validates comparison bar chart with error bars against Vega-Lite schema.
  (testing "comparison-bar-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison bar chart with error bars failed: "
                 (pr-str (:errors result))))))))

;;; Multi-point line chart tests.
;;; Verifies line chart generation for single-axis multi-point comparison scenarios.

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

(deftest domain-line-chart-spec-test
  ;; Tests line chart spec generation for single-axis multi-point comparisons.
  ;; Verifies correct Vega-Lite structure with lines per implementation.
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

(deftest domain-line-chart-spec-schema-validation-test
  ;; Validates domain-line-chart-spec output against Vega-Lite v6 schema.
  ;; Tests line chart visualization for implementation comparison.
  (testing "domain-line-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "domain-line-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

;;; Single-impl line chart tests.
;;; Verifies line chart generation for single-implementation extracts.

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

(deftest domain-line-chart-spec-single-impl-test
  ;; Tests line chart spec for single-implementation extracts.
  ;; Verifies correct Vega-Lite structure and data handling.
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

(deftest domain-line-chart-spec-single-impl-schema-validation-test
  ;; Validates single-impl line chart against Vega-Lite v6 schema.
  (testing "domain-line-chart-spec single-impl"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "single-impl line chart validation failed: "
                 (pr-str (:errors result))))))

    (testing "with error bounds produces valid Vega-Lite spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  single-impl-extract-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "single-impl line chart with bounds failed: "
                 (pr-str (:errors result))))))))

;;; Comparison line chart tests.
;;; Verifies line chart generation from domain-comparison data.

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

(deftest comparison-line-chart-spec-test
  ;; Tests line chart spec generation from domain-comparison data.
  ;; Verifies correct Vega-Lite structure with lines per implementation.
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

(deftest comparison-line-chart-spec-schema-validation-test
  ;; Validates comparison-line-chart-spec output against Vega-Lite v6 schema.
  ;; Tests line chart visualization from domain-comparison data.
  (testing "comparison-line-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison-line-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

;;; Line chart confidence band tests.
;;; Verifies confidence band generation for line charts with error bounds.

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

(deftest domain-line-chart-with-confidence-bands-test
  ;; Tests line chart spec generation when error bounds are present.
  ;; Verifies layered spec structure with confidence band layer + line layer.
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
  ;; Verifies graceful degradation - no layered structure when no bounds.
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

(deftest domain-line-chart-with-confidence-bands-schema-validation-test
  ;; Validates line chart with confidence bands against Vega-Lite v6 schema.
  (testing "domain-line-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "line chart with confidence bands failed: "
                 (pr-str (:errors result))))))))

;;; Comparison line chart confidence band tests.

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

(deftest comparison-line-chart-with-confidence-bands-test
  ;; Tests comparison line chart spec when error bounds are present.
  ;; Verifies layered spec structure with confidence band layer + line layer.
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
  (testing "comparison-line-chart-spec without error bounds"
    (testing "produces simple structure without layer"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "line" :point true} (:mark chart)))))))

(deftest comparison-line-chart-with-confidence-bands-schema-validation-test
  ;; Validates comparison line chart with confidence bands against Vega-Lite schema.
  (testing "comparison-line-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/comparison-line-chart-spec
                  multi-point-comparison-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison line chart with confidence bands failed: "
                 (pr-str (:errors result))))))))

;;; Single-point box plot data preparation tests.
;;; Verifies box plot data preparation from domain extract with bootstrap stats.

(def single-point-box-extract
  "Sample single-point multi-impl extract with box plot data for testing."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar :baz]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo}
                      {:median 1.0e-6 :ci-lower 0.9e-6 :ci-upper 1.1e-6
                       :p10 0.8e-6 :p90 1.2e-6}]
                     [{:n 100 :impl :bar}
                      {:median 2.0e-6 :ci-lower 1.8e-6 :ci-upper 2.2e-6
                       :p10 1.6e-6 :p90 2.4e-6}]
                     [{:n 100 :impl :baz}
                      {:median 1.5e-6 :ci-lower 1.3e-6 :ci-upper 1.7e-6
                       :p10 1.2e-6 :p90 1.8e-6}]]}}})

(def single-point-box-extract-no-ci
  "Sample extract with box data but without CI bounds."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo}
                      {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}]
                     [{:n 100 :impl :bar}
                      {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}]]}}})

(def single-point-missing-bootstrap
  "Sample extract missing bootstrap stats (plain values)."
  {:type :criterium/domain-extract
   :impl-axis :impl
   :implementations [:foo :bar]
   :metrics {:elapsed-time
             {:metric [:stats :elapsed-time :mean]
              :data [[{:n 100 :impl :foo} 1.0e-6]
                     [{:n 100 :impl :bar} 2.0e-6]]}}})

(deftest prepare-single-point-box-data-test
  ;; Tests data preparation for single-point box plots.
  ;; Verifies correct extraction of median, CI bounds, and percentiles.
  (testing "prepare-single-point-box-data"
    (testing "extracts data for each metric"
      (let [result (charts.comparison/prepare-single-point-box-data single-point-box-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))))

    (testing "includes all implementations in data"
      (let [result (charts.comparison/prepare-single-point-box-data single-point-box-extract)
            data (:data (first result))]
        (is (= 3 (count data)))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") data))))))

    (testing "extracts median, p10, p90 values"
      (let [result (charts.comparison/prepare-single-point-box-data single-point-box-extract)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))))

    (testing "extracts CI bounds when present"
      (let [result (charts.comparison/prepare-single-point-box-data single-point-box-extract)
            data (:data (first result))]
        (is (every? #(contains? % "ciLower") data))
        (is (every? #(contains? % "ciUpper") data))
        ;; Verify order: ciLower < median < ciUpper
        (doseq [d data]
          (is (< (get d "ciLower") (get d "median")))
          (is (< (get d "median") (get d "ciUpper"))))))

    (testing "omits CI bounds when not present"
      (let [result (charts.comparison/prepare-single-point-box-data single-point-box-extract-no-ci)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))
        (is (every? #(not (contains? % "ciLower")) data))
        (is (every? #(not (contains? % "ciUpper")) data))))

    (testing "applies SI scaling to values"
      (let [result (charts.comparison/prepare-single-point-box-data single-point-box-extract)
            first-metric (first result)]
        ;; y-title should contain SI unit
        (is (string? (:y-title first-metric)))
        ;; y-title should contain "median"
        (is (re-find #"median" (:y-title first-metric)))
        ;; values should be scaled (not raw nanoseconds)
        (let [data (:data first-metric)
              medians (keep #(get % "median") data)]
          (is (seq medians))
          ;; All values should be positive numbers
          (is (every? pos? medians)))))

    (testing "warns and returns nil for missing bootstrap stats"
      (let [output (with-out-str
                     (let [result (charts.comparison/prepare-single-point-box-data
                                   single-point-missing-bootstrap)]
                       (is (empty? result))))]
        ;; Should have printed a warning
        (is (re-find #"WARNING.*bootstrap" output))))

    (testing "handles multiple metrics"
      (let [multi-metric-extract
            (assoc-in single-point-box-extract
                      [:metrics :thread-allocation]
                      {:metric [:stats :thread-allocation :mean]
                       :data [[{:n 100 :impl :foo}
                               {:median 1000 :p10 800 :p90 1200}]
                              [{:n 100 :impl :bar}
                               {:median 2000 :p10 1600 :p90 2400}]
                              [{:n 100 :impl :baz}
                               {:median 1500 :p10 1200 :p90 1800}]]})
            result (charts.comparison/prepare-single-point-box-data multi-metric-extract)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))))

;;; Single-point box chart tests.
;;; Verifies box plot chart generation for single-point multi-impl comparison scenarios.

(deftest single-point-box-chart-spec-test
  ;; Tests box plot spec generation for single-point multi-impl comparisons.
  ;; Verifies correct Vega-Lite structure with implementation box plots.
  (testing "single-point-box-chart-spec"
    (testing "produces valid structure"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes layered structure"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 4 layers: whisker, CI box, median, tooltip
        (is (= 4 (count (:layer chart))))))

    (testing "includes whisker layer with rule mark and end caps"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            whisker-layer (first (:layer chart))
            whisker-rule (first (:layer whisker-layer))
            whisker-cap-p10 (second (:layer whisker-layer))
            whisker-cap-p90 (nth (:layer whisker-layer) 2)]
        (is (= "rule" (get-in whisker-rule [:mark :type])))
        (is (= "p10" (get-in whisker-rule [:encoding :y :field])))
        (is (= "p90" (get-in whisker-rule [:encoding :y2 :field])))
        (is (= "tick" (get-in whisker-cap-p10 [:mark :type]))
            "p10 cap should be a tick mark")
        (is (= "tick" (get-in whisker-cap-p90 [:mark :type]))
            "p90 cap should be a tick mark")))

    (testing "includes CI box layer with bar mark"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            ci-layer (second (:layer chart))]
        (is (= "bar" (get-in ci-layer [:mark :type])))
        (is (= "ciLower" (get-in ci-layer [:encoding :y :field])))
        (is (= "ciUpper" (get-in ci-layer [:encoding :y2 :field])))
        (is (= "#333" (get-in ci-layer [:mark :stroke]))
            "CI box should have stroke for visibility when CI is tight")
        (is (= 1 (get-in ci-layer [:mark :strokeWidth]))
            "CI box should have 1px stroke width")))

    (testing "includes median layer with tick mark"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            median-layer (nth (:layer chart) 2)]
        (is (= "tick" (get-in median-layer [:mark :type])))
        (is (= "median" (get-in median-layer [:encoding :y :field])))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "sets y-axis scale to exclude zero"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (false? (get-in y-encoding [:scale :zero]))
            "y-axis scale :zero should be false to fit data range")))

    (testing "preserves implementation order from data"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding))
            "x-axis sort should be nil to preserve data order")))

    (testing "y-axis title includes median prefix"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (re-find #"median" (:title y-encoding)))))

    (testing "respects chart dimensions"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))

    (testing "includes tooltip layer"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            tooltip-layer (last (:layer chart))
            tooltip (get-in tooltip-layer [:encoding :tooltip])]
        (is (vector? tooltip))
        ;; Should include impl, median, p10, p90, ciLower, ciUpper
        (is (= 6 (count tooltip)))))))

(deftest single-point-box-chart-without-ci-test
  ;; Tests box plot spec when CI bounds are not present.
  ;; Verifies graceful degradation - only whiskers and median shown.
  (testing "single-point-box-chart-spec without CI bounds"
    (testing "produces structure without CI layer"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 3 layers: whisker, median, tooltip (no CI box)
        (is (= 3 (count (:layer chart))))))

    (testing "still includes whisker and median layers"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            whisker-layer (first (:layer chart))
            whisker-rule (first (:layer whisker-layer))
            median-layer (second (:layer chart))]
        (is (= "rule" (get-in whisker-rule [:mark :type])))
        (is (= "tick" (get-in median-layer [:mark :type])))))

    (testing "tooltip excludes CI fields"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            tooltip-layer (last (:layer chart))
            tooltip (get-in tooltip-layer [:encoding :tooltip])]
        ;; Should include impl, median, p10, p90 (no CI fields)
        (is (= 4 (count tooltip)))))))

(deftest single-point-box-chart-missing-bootstrap-test
  ;; Tests box plot spec when bootstrap stats are missing entirely.
  ;; Verifies empty chart is produced (data prep filters out).
  (testing "single-point-box-chart-spec with missing bootstrap stats"
    (testing "produces empty vconcat"
      (let [output (with-out-str
                     (let [spec (charts.comparison/single-point-box-chart-spec
                                 single-point-missing-bootstrap
                                 {:width 400 :height 300})]
                       (is (empty? (:vconcat spec)))))]
        ;; Should have printed a warning
        (is (re-find #"WARNING.*bootstrap" output))))))

(deftest single-point-box-chart-spec-schema-validation-test
  ;; Validates single-point-box-chart-spec output against Vega-Lite v6 schema.
  (testing "single-point-box-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "single-point-box-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

(deftest single-point-box-chart-no-ci-schema-validation-test
  ;; Validates box chart without CI bounds against Vega-Lite v6 schema.
  (testing "single-point-box-chart-spec without CI"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "box chart without CI validation failed: "
                 (pr-str (:errors result))))))))

;;; Comparison box chart tests.
;;; Verifies box plot chart generation from domain-comparison data.

(def single-point-box-comparison
  "Sample single-point multi-impl comparison with box plot data for testing."
  {:type :criterium/domain-comparison
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
                         :p10 1.2e-6 :p90 1.8e-6}}]}})

(def single-point-box-comparison-no-ci
  "Sample comparison with box data but without CI bounds."
  {:type :criterium/domain-comparison
   :axis :n
   :metric [:stats :elapsed-time :mean]
   :implementations [:foo :bar]
   :data {:foo [{:coord {:n 100}
                 :value {:median 1.0e-6 :p10 0.8e-6 :p90 1.2e-6}}]
          :bar [{:coord {:n 100}
                 :value {:median 2.0e-6 :p10 1.6e-6 :p90 2.4e-6}}]}})

(deftest comparison-box-chart-spec-test
  ;; Tests box plot spec generation from domain-comparison data.
  (testing "comparison-box-chart-spec"
    (testing "produces valid structure"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes layered structure"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 4 layers: whisker, CI box, median, tooltip
        (is (= 4 (count (:layer chart))))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "sets y-axis scale to exclude zero"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (false? (get-in y-encoding [:scale :zero]))
            "y-axis scale :zero should be false to fit data range")))

    (testing "preserves implementation order from data"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding))
            "x-axis sort should be nil to preserve data order")))))

(deftest comparison-box-chart-without-ci-test
  ;; Tests comparison box plot spec when CI bounds are not present.
  (testing "comparison-box-chart-spec without CI bounds"
    (testing "produces structure without CI layer"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 3 layers: whisker, median, tooltip (no CI box)
        (is (= 3 (count (:layer chart))))))))

(deftest comparison-box-chart-spec-schema-validation-test
  ;; Validates comparison-box-chart-spec output against Vega-Lite v6 schema.
  (testing "comparison-box-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison-box-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

(deftest comparison-box-chart-no-ci-schema-validation-test
  ;; Validates comparison box chart without CI bounds against Vega-Lite schema.
  (testing "comparison-box-chart-spec without CI"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison-no-ci
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison box chart without CI failed: "
                 (pr-str (:errors result))))))))
