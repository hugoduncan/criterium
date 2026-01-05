(ns criterium.viewer.common-charts-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.viewer.common-charts :as charts]
   [criterium.viewer.schema-validation :as schema]))

;; Tests for treemap-vega-spec function.
;; Verifies Vega spec generation for treemap visualizations of allocation data.

(def sample-treemap
  "Sample allocation treemap for testing."
  {:type :criterium/allocation-treemap
   :group-by :class→line→type
   :size-by :bytes
   :root {:name "allocations"
          :value 1000
          :children [{:name "MyClass"
                      :value 800
                      :children [{:name "L42"
                                  :value 500}
                                 {:name "L50"
                                  :value 300}]}
                     {:name "OtherClass"
                      :value 200}]}})

(deftest treemap-vega-spec-test
  (testing "treemap-vega-spec"
    (testing "produces valid Vega spec structure"
      (let [spec (charts/treemap-vega-spec sample-treemap {})]
        (is (map? spec))
        (is (contains? spec :$schema))
        (is (contains? spec :width))
        (is (contains? spec :height))
        (is (contains? spec :data))
        (is (contains? spec :scales))
        (is (contains? spec :marks))
        (is (vector? (:data spec)))
        (is (vector? (:scales spec)))
        (is (vector? (:marks spec)))))

    (testing "includes correct $schema"
      (let [spec (charts/treemap-vega-spec sample-treemap {})]
        (is (= "https://vega.github.io/schema/vega/v5.json" (:$schema spec)))))

    (testing "data values match input hierarchy"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            tree-data (first (:data spec))
            values (:values tree-data)]
        (is (= "tree" (:name tree-data)))
        (is (= 5 (count values)) "Expected 5 nodes in flattened tree")
        ;; Root node (parent) - no value, Vega computes from children
        (is (= "root" (:id (first values))))
        (is (nil? (:parent (first values))))
        (is (= "allocations" (:name (first values))))
        (is (nil? (:value (first values))) "Parent nodes should not have values")
        ;; Check an intermediate node (parent)
        (let [myclass-node (second values)]
          (is (= "allocations/MyClass" (:id myclass-node)))
          (is (= "root" (:parent myclass-node)))
          (is (= "MyClass" (:name myclass-node)))
          (is (nil? (:value myclass-node)) "Parent nodes should not have values"))
        ;; Check leaf nodes have values
        (let [leaf-nodes (filter :value values)]
          (is (= 3 (count leaf-nodes)) "Expected 3 leaf nodes with values")
          (is (= #{500 300 200} (set (map :value leaf-nodes)))))))

    (testing "respects width/height options"
      (let [spec (charts/treemap-vega-spec sample-treemap {:width 500 :height 300})]
        (is (= 500 (:width spec)))
        (is (= 300 (:height spec)))))

    (testing "uses default width/height when not specified"
      (let [spec (charts/treemap-vega-spec sample-treemap {})]
        (is (= 700 (:width spec)))
        (is (= 400 (:height spec)))))

    (testing "respects color-scheme option"
      (let [spec (charts/treemap-vega-spec sample-treemap {:color-scheme "category20"})
            color-scale (first (:scales spec))]
        (is (= {:scheme "category20"} (:range color-scale)))))

    (testing "with empty children produces minimal spec"
      (let [empty-treemap {:type :criterium/allocation-treemap
                           :root {:name "allocations"
                                  :value 0}}
            spec (charts/treemap-vega-spec empty-treemap {})]
        (is (map? spec))
        (is (= "https://vega.github.io/schema/vega/v5.json" (:$schema spec)))
        (let [values (-> spec :data first :values)]
          (is (= 1 (count values)))
          (is (= "root" (:id (first values))))
          (is (= 0 (:value (first values))) "Single node (leaf) has value"))))

    (testing "with nil root produces empty data"
      (let [nil-treemap {:type :criterium/allocation-treemap
                         :root nil}
            spec (charts/treemap-vega-spec nil-treemap {})]
        (is (map? spec))
        (is (empty? (-> spec :data first :values)))))

    (testing "includes stratify and treemap transforms"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            transforms (-> spec :data first :transform)]
        (is (= 2 (count transforms)))
        (is (= "stratify" (:type (first transforms))))
        (is (= "treemap" (:type (second transforms))))
        (is (= "squarify" (:method (second transforms))))))

    (testing "includes two rect marks for nodes and leaves"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            marks (:marks spec)]
        (is (= 2 (count marks)))
        (is (= "rect" (:type (first marks))))
        (is (= "rect" (:type (second marks))))))))

;;; Schema validation tests for all chart spec functions.
;;; Validates that generated specs conform to official Vega/Vega-Lite JSON schemas.

(deftest samples-vega-spec-schema-validation-test
  ;; Validates samples-vega-spec output against Vega-Lite v6 schema.
  ;; Tests the scatter plot visualization of benchmark samples.
  (testing "samples-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/samples-data-map)
            view {}
            chart-options {:width 400 :height 300}
            spec (charts/samples-vega-spec data-map view chart-options)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "samples-vega-spec validation failed: "
                 (pr-str (:errors result))))))))

(deftest histogram-vega-spec-schema-validation-test
  ;; Validates histogram-vega-spec output against Vega-Lite v6 schema.
  ;; Tests histogram visualization with density bars.
  (testing "histogram-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/histogram-data-map)
            view {}
            chart-options {:width 400 :height 300}
            spec (charts/histogram-vega-spec data-map view chart-options)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "histogram-vega-spec validation failed: "
                 (pr-str (:errors result))))))))

(deftest histogram-with-boxplot-schema-validation-test
  ;; Validates histogram-vega-spec with boxplot overlay against Vega-Lite v6 schema.
  ;; Tests histogram with bootstrap-stats for median CI and spread percentiles.
  (testing "histogram-vega-spec"
    (testing "produces valid Vega-Lite spec with boxplot overlay"
      (let [data-map (test-data/histogram-with-bootstrap-data-map)
            view {}
            chart-options {:width 400 :height 300}
            spec (charts/histogram-vega-spec data-map view chart-options)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "histogram-vega-spec with boxplot validation failed: "
                 (pr-str (:errors result))))))))

(deftest kde-vega-spec-schema-validation-test
  ;; Validates kde-vega-spec output against Vega-Lite v6 schema.
  ;; Tests KDE density curve visualization.
  (testing "kde-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/kde-data-map)
            view {}
            chart-options {:width 400 :height 300}
            spec (charts/kde-vega-spec data-map view chart-options)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "kde-vega-spec validation failed: "
                 (pr-str (:errors result))))))))

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
            spec (charts/regression-chart-spec points line-pts opts)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "regression-chart-spec validation failed: "
                 (pr-str (:errors result))))))))

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
            spec (charts/regression-residual-spec residual-pts opts)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "regression-residual-spec validation failed: "
                 (pr-str (:errors result))))))))

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
      (let [result (charts/prepare-single-point-bar-data single-point-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))))

    (testing "includes all implementations in data"
      (let [result (charts/prepare-single-point-bar-data single-point-extract)
            data (:data (first result))]
        (is (= 3 (count data)))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") data))))))

    (testing "applies SI scaling to values"
      (let [result (charts/prepare-single-point-bar-data single-point-extract)
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
            result (charts/prepare-single-point-bar-data multi-metric-extract)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "returns has-error-bounds? false for plain values"
      (let [result (charts/prepare-single-point-bar-data single-point-extract)
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
            result (charts/prepare-single-point-bar-data extract-with-bounds)
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
            result (charts/prepare-single-point-bar-data extract-mixed)
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
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes bar mark"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "bar"} (:mark chart)))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "preserves implementation order from data"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding))
            "x-axis sort should be nil to preserve data order")))

    (testing "encodes value on y-axis"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (= "value" (:field y-encoding)))
        (is (= "quantitative" (:type y-encoding)))))

    (testing "respects chart dimensions"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))

    (testing "includes tooltip"
      (let [spec (charts/single-point-bar-chart-spec
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
      (let [spec (charts/single-point-bar-chart-spec
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
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes bar layer with bar mark"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            bar-layer (first (:layer chart))]
        (is (= {:type "bar"} (:mark bar-layer)))))

    (testing "includes error layer with rule and tick marks"
      (let [spec (charts/single-point-bar-chart-spec
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
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            error-composite (second (:layer chart))
            rule-layer (first (:layer error-composite))
            encoding (:encoding rule-layer)]
        (is (= "valueLower" (get-in encoding [:y :field])))
        (is (= "valueUpper" (get-in encoding [:y2 :field])))))

    (testing "error layer data includes bounds"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            error-composite (second (:layer chart))
            rule-layer (first (:layer error-composite))
            data (get-in rule-layer [:data :values])]
        (is (every? #(contains? % "valueLower") data))
        (is (every? #(contains? % "valueUpper") data))))

    (testing "respects chart dimensions"
      (let [spec (charts/single-point-bar-chart-spec
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
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "bar"} (:mark chart)))))

    (testing "still includes bar mark and encodings"
      (let [spec (charts/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= "impl" (get-in chart [:encoding :x :field])))
        (is (= "value" (get-in chart [:encoding :y :field])))))))

(deftest single-point-bar-chart-with-error-bars-schema-validation-test
  ;; Validates bar chart with error bars against Vega-Lite v6 schema.
  (testing "single-point-bar-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/single-point-bar-chart-spec
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
      (let [spec (charts/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes bar mark"
      (let [spec (charts/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "bar"} (:mark chart)))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "preserves implementation order from data"
      (let [spec (charts/comparison-bar-chart-spec
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
      (let [spec (charts/comparison-bar-chart-spec
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
      (let [spec (charts/comparison-bar-chart-spec
                  single-point-comparison-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes error layer with rule and tick marks"
      (let [spec (charts/comparison-bar-chart-spec
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
      (let [spec (charts/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "bar"} (:mark chart)))))))

(deftest comparison-bar-chart-with-error-bars-schema-validation-test
  ;; Validates comparison bar chart with error bars against Vega-Lite schema.
  (testing "comparison-bar-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/comparison-bar-chart-spec
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
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes line mark with points"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "encodes axis value on x-axis"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "x" (:field x-encoding)))
        (is (= "quantitative" (:type x-encoding)))
        (is (= "n" (:title x-encoding)))))

    (testing "encodes value on y-axis"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (= "y" (:field y-encoding)))
        (is (= "quantitative" (:type y-encoding)))))

    (testing "encodes implementation as color"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            color-encoding (get-in chart [:encoding :color])]
        (is (= "impl" (:field color-encoding)))
        (is (= "nominal" (:type color-encoding)))
        (is (= "Implementation" (:title color-encoding)))))

    (testing "respects chart dimensions"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))

    (testing "includes tooltip"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            tooltip (get-in chart [:encoding :tooltip])]
        (is (vector? tooltip))
        (is (= 3 (count tooltip)))))))

(deftest domain-line-chart-spec-schema-validation-test
  ;; Validates domain-line-chart-spec output against Vega-Lite v6 schema.
  ;; Tests line chart visualization for implementation comparison.
  (testing "domain-line-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "domain-line-chart-spec validation failed: "
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
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes line mark with points"
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "encodes axis value on x-axis"
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "x" (:field x-encoding)))
        (is (= "quantitative" (:type x-encoding)))
        (is (= "n" (:title x-encoding)))))

    (testing "encodes implementation as color"
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            color-encoding (get-in chart [:encoding :color])]
        (is (= "impl" (:field color-encoding)))
        (is (= "nominal" (:type color-encoding)))
        (is (= "Implementation" (:title color-encoding)))))))

(deftest comparison-line-chart-spec-schema-validation-test
  ;; Validates comparison-line-chart-spec output against Vega-Lite v6 schema.
  ;; Tests line chart visualization from domain-comparison data.
  (testing "comparison-line-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/comparison-line-chart-spec
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
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes confidence band layer with area mark"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))]
        (is (= "area" (get-in band-layer [:mark :type])))
        (is (= 0.2 (get-in band-layer [:mark :opacity])))))

    (testing "confidence band layer encodes y/y2 for bounds"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))
            encoding (:encoding band-layer)]
        (is (= "yLower" (get-in encoding [:y :field])))
        (is (= "yUpper" (get-in encoding [:y2 :field])))))

    (testing "includes line layer with line mark"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            line-layer (second (:layer chart))]
        (is (= "line" (get-in line-layer [:mark :type])))
        (is (true? (get-in line-layer [:mark :point])))))

    (testing "confidence band data includes bounds"
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            band-layer (first (:layer chart))
            data (get-in band-layer [:data :values])]
        (is (every? #(contains? % "yLower") data))
        (is (every? #(contains? % "yUpper") data))))

    (testing "respects chart dimensions"
      (let [spec (charts/domain-line-chart-spec
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
      (let [spec (charts/domain-line-chart-spec
                  multi-point-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "line" :point true} (:mark chart)))))

    (testing "still includes line mark and encodings"
      (let [spec (charts/domain-line-chart-spec
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
      (let [spec (charts/domain-line-chart-spec
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
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison-with-bounds
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 2 (count (:layer chart))))))

    (testing "includes confidence band layer with area mark"
      (let [spec (charts/comparison-line-chart-spec
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
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "line" :point true} (:mark chart)))))))

(deftest comparison-line-chart-with-confidence-bands-schema-validation-test
  ;; Validates comparison line chart with confidence bands against Vega-Lite schema.
  (testing "comparison-line-chart-spec with error bounds"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/comparison-line-chart-spec
                  multi-point-comparison-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison line chart with confidence bands failed: "
                 (pr-str (:errors result))))))))

;;; Boxplot layer tests.
;;; Verifies boxplot layer generation for histogram median/spread overlays.

(def identity-transforms
  "Identity transforms for testing - `:sample->` must be a list of functions."
  {:sample-> (list identity)
   :->sample [identity]})

(def sample-bootstrap-stats
  "Sample bootstrap stats with median (0.5) and spread (0.1, 0.9) quantiles."
  {:quantiles
   {0.1 {:point-estimate 90.0
         :estimate-quantiles []}
    0.5 {:point-estimate 100.0
         :estimate-quantiles [{:quantile 0.025 :value 95.0}
                              {:quantile 0.975 :value 105.0}]}
    0.9 {:point-estimate 110.0
         :estimate-quantiles []}}})

(def sample-metric-config
  "Sample metric config for elapsed-time."
  {:path [:elapsed-time]
   :label "Elapsed Time"
   :type :quantitative})

(deftest metric-bootstrap-boxplot-layer-test
  ;; Tests boxplot layer generation for histogram overlays showing
  ;; median CI and 10th/90th percentile spread.
  (testing "metric-bootstrap-boxplot-layer"
    (testing "returns vector of layers when quantiles present"
      (let [result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (map? (first result)))
        (is (contains? (first result) :layer))))

    (testing "contains whisker, CI box, and median line layers"
      (let [result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            inner-layers (get-in result [0 :layer])]
        (is (= 3 (count inner-layers)))
        ;; Whisker layer (p10 to p90)
        (let [whisker (first inner-layers)]
          (is (= "rule" (get-in whisker [:mark :type])))
          (is (= 1 (get-in whisker [:mark :strokeWidth]))))
        ;; CI box layer
        (let [ci-box (second inner-layers)]
          (is (= "rect" (get-in ci-box [:mark :type])))
          (is (= 0.6 (get-in ci-box [:mark :opacity]))))
        ;; Median line layer
        (let [median-line (nth inner-layers 2)]
          (is (= "rule" (get-in median-line [:mark :type])))
          (is (= 2 (get-in median-line [:mark :strokeWidth]))))))

    (testing "whisker spans from p10 to p90"
      (let [result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            whisker (get-in result [0 :layer 0])
            data (get-in whisker [:data :values 0])]
        ;; Vega-Lite data uses string keys
        (is (= 90.0 (get data "elapsed-time")))
        (is (= 110.0 (get data :end)))))

    (testing "CI box spans median confidence interval"
      (let [result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            ci-box (get-in result [0 :layer 1])
            data (get-in ci-box [:data :values 0])]
        ;; Vega-Lite data uses string keys for field names
        (is (= 95.0 (get data "elapsed-time")))
        (is (= 105.0 (get data :end)))))

    (testing "median line at point estimate"
      (let [result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            median-line (get-in result [0 :layer 2])
            data (get-in median-line [:data :values 0])]
        ;; Vega-Lite data uses string keys for field names
        (is (= 100.0 (get data "elapsed-time")))))

    (testing "applies transforms to raw bootstrap values"
      (let [scale-transforms {:sample-> (list (fn [^double v] (/ v 1e9)))
                              :->sample [identity]}
            result (charts/metric-bootstrap-boxplot-layer
                    scale-transforms sample-bootstrap-stats sample-metric-config)
            whisker (get-in result [0 :layer 0])
            whisker-data (get-in whisker [:data :values 0])]
        ;; Values are transformed (divided by 1e9)
        (is (= 90.0e-9 (double (get whisker-data "elapsed-time"))))))

    (testing "returns nil when quantiles missing"
      (let [missing-quantiles {:quantiles {}}
            result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms missing-quantiles sample-metric-config)]
        (is (nil? result))))

    (testing "returns nil when p50 missing"
      (let [missing-p50 {:quantiles {0.1 {:point-estimate 90.0}
                                     0.9 {:point-estimate 110.0}}}
            result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms missing-p50 sample-metric-config)]
        (is (nil? result))))

    (testing "omits CI box when median CI empty"
      (let [no-ci {:quantiles
                   {0.1 {:point-estimate 90.0}
                    0.5 {:point-estimate 100.0
                         :estimate-quantiles []}
                    0.9 {:point-estimate 110.0}}}
            result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms no-ci sample-metric-config)
            inner-layers (get-in result [0 :layer])]
        ;; Only whisker and median line (no CI box)
        (is (= 2 (count inner-layers)))
        (is (= "rule" (get-in (first inner-layers) [:mark :type])))
        (is (= "rule" (get-in (second inner-layers) [:mark :type])))))

    (testing "includes layer transforms for legend"
      (let [result (charts/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            whisker (get-in result [0 :layer 0])]
        (is (some? (get-in whisker [:transform])))
        (is (some #(contains? % :calculate) (get-in whisker [:transform])))))))

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
      (let [result (charts/prepare-single-point-box-data single-point-box-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))))

    (testing "includes all implementations in data"
      (let [result (charts/prepare-single-point-box-data single-point-box-extract)
            data (:data (first result))]
        (is (= 3 (count data)))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") data))))))

    (testing "extracts median, p10, p90 values"
      (let [result (charts/prepare-single-point-box-data single-point-box-extract)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))))

    (testing "extracts CI bounds when present"
      (let [result (charts/prepare-single-point-box-data single-point-box-extract)
            data (:data (first result))]
        (is (every? #(contains? % "ciLower") data))
        (is (every? #(contains? % "ciUpper") data))
        ;; Verify order: ciLower < median < ciUpper
        (doseq [d data]
          (is (< (get d "ciLower") (get d "median")))
          (is (< (get d "median") (get d "ciUpper"))))))

    (testing "omits CI bounds when not present"
      (let [result (charts/prepare-single-point-box-data single-point-box-extract-no-ci)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))
        (is (every? #(not (contains? % "ciLower")) data))
        (is (every? #(not (contains? % "ciUpper")) data))))

    (testing "applies SI scaling to values"
      (let [result (charts/prepare-single-point-box-data single-point-box-extract)
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
                     (let [result (charts/prepare-single-point-box-data
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
            result (charts/prepare-single-point-box-data multi-metric-extract)]
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
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes layered structure"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 4 layers: whisker, CI box, median, tooltip
        (is (= 4 (count (:layer chart))))))

    (testing "includes whisker layer with rule mark and end caps"
      (let [spec (charts/single-point-box-chart-spec
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
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            ci-layer (second (:layer chart))]
        (is (= "bar" (get-in ci-layer [:mark :type])))
        (is (= "ciLower" (get-in ci-layer [:encoding :y :field])))
        (is (= "ciUpper" (get-in ci-layer [:encoding :y2 :field])))))

    (testing "includes median layer with tick mark"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            median-layer (nth (:layer chart) 2)]
        (is (= "tick" (get-in median-layer [:mark :type])))
        (is (= "median" (get-in median-layer [:encoding :y :field])))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "sets y-axis scale to exclude zero"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (false? (get-in y-encoding [:scale :zero]))
            "y-axis scale :zero should be false to fit data range")))

    (testing "preserves implementation order from data"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding))
            "x-axis sort should be nil to preserve data order")))

    (testing "y-axis title includes median prefix"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (re-find #"median" (:title y-encoding)))))

    (testing "respects chart dimensions"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 500 :height 250})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 250 (:height chart)))))

    (testing "includes tooltip layer"
      (let [spec (charts/single-point-box-chart-spec
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
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 3 layers: whisker, median, tooltip (no CI box)
        (is (= 3 (count (:layer chart))))))

    (testing "still includes whisker and median layers"
      (let [spec (charts/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            whisker-layer (first (:layer chart))
            whisker-rule (first (:layer whisker-layer))
            median-layer (second (:layer chart))]
        (is (= "rule" (get-in whisker-rule [:mark :type])))
        (is (= "tick" (get-in median-layer [:mark :type])))))

    (testing "tooltip excludes CI fields"
      (let [spec (charts/single-point-box-chart-spec
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
                     (let [spec (charts/single-point-box-chart-spec
                                 single-point-missing-bootstrap
                                 {:width 400 :height 300})]
                       (is (empty? (:vconcat spec)))))]
        ;; Should have printed a warning
        (is (re-find #"WARNING.*bootstrap" output))))))

(deftest single-point-box-chart-spec-schema-validation-test
  ;; Validates single-point-box-chart-spec output against Vega-Lite v6 schema.
  (testing "single-point-box-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/single-point-box-chart-spec
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
      (let [spec (charts/single-point-box-chart-spec
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
      (let [spec (charts/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes layered structure"
      (let [spec (charts/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        ;; 4 layers: whisker, CI box, median, tooltip
        (is (= 4 (count (:layer chart))))))

    (testing "encodes implementation on x-axis"
      (let [spec (charts/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (= "impl" (:field x-encoding)))
        (is (= "nominal" (:type x-encoding)))
        (is (= "Implementation" (:title x-encoding)))))

    (testing "sets y-axis scale to exclude zero"
      (let [spec (charts/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            y-encoding (get-in chart [:encoding :y])]
        (is (false? (get-in y-encoding [:scale :zero]))
            "y-axis scale :zero should be false to fit data range")))

    (testing "preserves implementation order from data"
      (let [spec (charts/comparison-box-chart-spec
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
      (let [spec (charts/comparison-box-chart-spec
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
      (let [spec (charts/comparison-box-chart-spec
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
      (let [spec (charts/comparison-box-chart-spec
                  single-point-box-comparison-no-ci
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "comparison box chart without CI failed: "
                 (pr-str (:errors result))))))))

(deftest treemap-vega-spec-schema-validation-test
  ;; Validates treemap-vega-spec output against Vega v5 schema.
  ;; Tests treemap visualization for allocation data.
  (testing "treemap-vega-spec"
    (testing "produces valid Vega spec"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            result (schema/validate-vega-spec spec)]
        (is (:valid? result)
            (str "treemap-vega-spec validation failed: "
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
      (let [spec (charts/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:width 600 :height 400 :axis-name "n"})]
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
            spec (charts/log-log-chart-spec
                  points-with-error
                  sample-log-log-line-points
                  {:has-error-bounds? true})]
        ;; Should have scatter, line, and error bar layers
        (is (= 3 (count (:layer spec))))))
    (testing "includes title with slope and r-squared when provided"
      (let [spec (charts/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:slope 1.02 :r-squared 0.998})]
        (is (some? (:title spec)))
        (is (string? (:title spec)))))
    (testing "handles multi-impl with color field"
      (let [points [{"x" 2.3 "y" 4.6 "impl" "vec"}
                    {"x" 2.3 "y" 5.0 "impl" "list"}]
            line-pts [{"x" 2.0 "y" 4.0 "impl" "vec"}
                      {"x" 2.0 "y" 4.5 "impl" "list"}]
            spec (charts/log-log-chart-spec
                  points line-pts
                  {:color-field "impl"})]
        (is (map? spec))
        ;; Check that color encoding exists in scatter layer
        (let [scatter-layer (first (:layer spec))]
          (is (contains? (get-in scatter-layer [:encoding :color]) :field)))))))

(deftest log-log-residual-spec-test
  ;; Tests the log-log-residual-spec function for structure correctness.
  (testing "log-log-residual-spec"
    (testing "produces valid Vega-Lite spec structure"
      (let [spec (charts/log-log-residual-spec
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
      (let [spec (charts/log-log-residual-spec
                  sample-log-log-residuals
                  {:axis-name "n"})
            scatter-layer (first (:layer spec))
            x-title (get-in scatter-layer [:encoding :x :title])]
        (is (= "log(n)" x-title))))))

(deftest log-log-chart-spec-schema-validation-test
  ;; Validates log-log-chart-spec output against Vega-Lite v5 schema.
  (testing "log-log-chart-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/log-log-chart-spec
                  sample-log-log-points
                  sample-log-log-line-points
                  {:width 600 :height 400 :axis-name "n"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-chart-spec validation failed: "
                 (pr-str (:errors result))))))
    (testing "with error bounds produces valid spec"
      (let [points-with-error [{"x" 2.3 "y" 4.6 "yLower" 4.4 "yUpper" 4.8}
                               {"x" 3.0 "y" 6.0 "yLower" 5.8 "yUpper" 6.2}]
            spec (charts/log-log-chart-spec
                  points-with-error
                  sample-log-log-line-points
                  {:has-error-bounds? true})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-chart-spec with error bounds failed: "
                 (pr-str (:errors result))))))
    (testing "with color field produces valid spec"
      (let [points [{"x" 2.3 "y" 4.6 "impl" "vec"}
                    {"x" 3.0 "y" 6.0 "impl" "list"}]
            line-pts [{"x" 2.0 "y" 4.0 "impl" "vec"}
                      {"x" 3.0 "y" 6.0 "impl" "list"}]
            spec (charts/log-log-chart-spec
                  points line-pts
                  {:color-field "impl"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-chart-spec with color field failed: "
                 (pr-str (:errors result))))))))

(deftest log-log-residual-spec-schema-validation-test
  ;; Validates log-log-residual-spec output against Vega-Lite v5 schema.
  (testing "log-log-residual-spec"
    (testing "produces valid Vega-Lite spec"
      (let [spec (charts/log-log-residual-spec
                  sample-log-log-residuals
                  {:width 600 :height 200 :axis-name "n"})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "log-log-residual-spec validation failed: ";;; Distribution PDF overlay tests.
;;; Verifies PDF overlay layer generation for fitted distributions.

(def sample-fit-result
  "Sample fit result for a single distribution."
  {:params {:shape 2.0 :scale 1.5}
   :log-likelihood -150.0
   :aic 304.0
   :delta-aic 0.0})

(def sample-grid
  "Sample KDE grid for testing."
  [1.0 2.0 3.0 4.0 5.0])

(deftest distribution-pdf-layer-test
  ;; Tests PDF layer generation for a single fitted distribution.
  ;; Verifies layer structure, data points, and styling.
  (testing "distribution-pdf-layer"
    (testing "produces valid layer for fitted distribution"
      (let [layer (charts/distribution-pdf-layer
                   :gamma sample-fit-result sample-grid identity-transforms)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))))

    (testing "includes PDF density values in data"
      (let [layer (charts/distribution-pdf-layer
                   :gamma sample-fit-result sample-grid identity-transforms)
            data (get-in layer [:data :values])]
        (is (= 5 (count data)))
        (is (every? #(contains? % "x") data))
        (is (every? #(contains? % "pdf-density") data))
        ;; PDF values should be positive
        (is (every? #(pos? (get % "pdf-density")) data))))

    (testing "uses line mark"
      (let [layer (charts/distribution-pdf-layer
                   :gamma sample-fit-result sample-grid identity-transforms)]
        (is (= "line" (get-in layer [:mark :type])))))

    (testing "best model has solid line"
      (let [best-result (assoc sample-fit-result :best-model :gamma)
            layer (charts/distribution-pdf-layer
                   :gamma best-result sample-grid identity-transforms)]
        (is (= [1 0] (get-in layer [:mark :strokeDash])))
        (is (= 2.5 (get-in layer [:mark :strokeWidth])))))

    (testing "non-best model has dashed line"
      (let [non-best-result (assoc sample-fit-result :best-model :lognormal)
            layer (charts/distribution-pdf-layer
                   :gamma non-best-result sample-grid identity-transforms)]
        (is (= [4 4] (get-in layer [:mark :strokeDash])))
        (is (= 1.5 (get-in layer [:mark :strokeWidth])))))

    (testing "returns nil for failed fit"
      (let [failed-result {:error "Fitting failed"}
            layer (charts/distribution-pdf-layer
                   :gamma failed-result sample-grid identity-transforms)]
        (is (nil? layer))))

    (testing "returns nil for skipped distribution"
      (let [skipped-result {:skipped :moment-match-failed}
            layer (charts/distribution-pdf-layer
                   :gamma skipped-result sample-grid identity-transforms)]
        (is (nil? layer))))

    (testing "works for all distribution types"
      (doseq [[dist params] [[:gamma {:shape 2.0 :scale 1.5}]
                             [:lognormal {:mu 0.5 :sigma 0.8}]
                             [:weibull {:shape 1.8 :scale 3.2}]
                             [:inverse-gaussian {:mu 3.0 :lambda 2.0}]]]
        (let [result {:params params}
              layer (charts/distribution-pdf-layer
                     dist result sample-grid identity-transforms)]
          (is (map? layer)
              (str "Failed for distribution: " dist))
          (is (seq (get-in layer [:data :values]))
              (str "No data for distribution: " dist)))))))

(deftest distribution-pdf-overlay-layers-test
  ;; Tests overlay layer generation for multiple distributions.
  ;; Verifies filtering of failed/skipped distributions.
  (testing "distribution-pdf-overlay-layers"
    (testing "returns layers for all successfully fitted distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:params {:mu 0.5 :sigma 0.8}}
                       :weibull {:params {:shape 1.8 :scale 3.2}}}
                      :best-model :gamma}
            layers (charts/distribution-pdf-overlay-layers
                    fit-data sample-grid identity-transforms)]
        (is (= 3 (count layers)))
        (is (every? map? layers))))

    (testing "filters out failed distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:error "Fitting failed"}}
                      :best-model :gamma}
            layers (charts/distribution-pdf-overlay-layers
                    fit-data sample-grid identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "filters out skipped distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :inverse-gaussian {:skipped :moment-match-failed}}
                      :best-model :gamma}
            layers (charts/distribution-pdf-overlay-layers
                    fit-data sample-grid identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "returns empty vector when all fail"
      (let [fit-data {:distributions
                      {:gamma {:error "Fitting failed"}
                       :lognormal {:skipped :moment-match-failed}}
                      :best-model nil}
            layers (charts/distribution-pdf-overlay-layers
                    fit-data sample-grid identity-transforms)]
        (is (empty? layers))))))

(deftest distribution-pdf-vega-spec-test
  ;; Tests complete Vega-Lite spec generation for KDE with PDF overlays.
  ;; Verifies layer composition and structure.
  (testing "distribution-pdf-vega-spec"
    (testing "produces valid structure"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes chart dimensions"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts/distribution-pdf-vega-spec
                  data-map {} {:width 500 :height 350})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 350 (:height chart)))))

    (testing "includes KDE and distribution layers"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})
            chart (first (:vconcat spec))
            inner-group (first (:layer chart))
            inner-layers (:layer inner-group)]
        ;; Should have KDE confidence band + KDE density + distribution PDFs
        ;; (gamma, lognormal, weibull - inverse-gaussian is skipped)
        (is (>= (count inner-layers) 3))))

    (testing "works without distribution-fit data"
      (let [data-map (test-data/kde-data-map)
            spec (charts/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))))))

(deftest distribution-pdf-vega-spec-schema-validation-test
  ;; Validates distribution-pdf-vega-spec output against Vega-Lite v6 schema.
  ;; Tests KDE + distribution PDF overlay visualization.
  (testing "distribution-pdf-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "distribution-pdf-vega-spec validation failed: "

                 (pr-str (:errors result))))))))

;;; Distribution CDF overlay tests

(def sample-cdf-grid
  "Sample x-values grid for CDF testing."
  [1.0 2.0 3.0 4.0 5.0])

(deftest ecdf-layer-test
  ;; Tests ECDF (empirical cumulative distribution function) layer generation.
  ;; Verifies step function structure and cumulative probability values.
  (testing "ecdf-layer"
    (testing "produces valid layer structure"
      (let [samples [1.0 2.0 3.0 4.0 5.0]
            layer (charts/ecdf-layer samples identity-transforms)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))))

    (testing "includes correct number of data points"
      (let [samples [1.0 2.0 3.0 4.0 5.0]
            layer (charts/ecdf-layer samples identity-transforms)
            data (get-in layer [:data :values])]
        (is (= 5 (count data)))))

    (testing "computes correct ECDF values"
      (let [samples [1.0 2.0 3.0 4.0 5.0]
            layer (charts/ecdf-layer samples identity-transforms)
            data (get-in layer [:data :values])
            ecdf-values (mapv #(get % "ecdf") data)]
        ;; ECDF at each point should be i/n
        (is (= [0.2 0.4 0.6 0.8 1.0] ecdf-values))))

    (testing "uses step-after interpolation"
      (let [samples [1.0 2.0 3.0]
            layer (charts/ecdf-layer samples identity-transforms)]
        (is (= "step-after" (get-in layer [:mark :interpolate])))))

    (testing "handles unsorted samples"
      (let [samples [5.0 1.0 3.0 2.0 4.0]
            layer (charts/ecdf-layer samples identity-transforms)
            data (get-in layer [:data :values])
            x-values (mapv #(get % "x") data)]
        ;; Should be sorted
        (is (= [1.0 2.0 3.0 4.0 5.0] x-values))))))

(deftest distribution-cdf-layer-test
  ;; Tests CDF layer generation for a single fitted distribution.
  ;; Verifies layer structure, data points, and styling.
  (testing "distribution-cdf-layer"
    (testing "produces valid layer for fitted distribution"
      (let [layer (charts/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))))

    (testing "includes CDF values in data"
      (let [layer (charts/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)
            data (get-in layer [:data :values])]
        (is (= 5 (count data)))
        (is (every? #(contains? % "x") data))
        (is (every? #(contains? % "cdf") data))
        ;; CDF values should be between 0 and 1
        (is (every? #(<= 0.0 (get % "cdf") 1.0) data))))

    (testing "CDF values are monotonically increasing"
      (let [layer (charts/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)
            data (get-in layer [:data :values])
            cdf-values (mapv #(get % "cdf") data)]
        (is (apply <= cdf-values))))

    (testing "uses line mark"
      (let [layer (charts/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)]
        (is (= "line" (get-in layer [:mark :type])))))

    (testing "best model has solid line"
      (let [best-result (assoc sample-fit-result :best-model :gamma)
            layer (charts/distribution-cdf-layer
                   :gamma best-result sample-cdf-grid identity-transforms)]
        (is (= [1 0] (get-in layer [:mark :strokeDash])))
        (is (= 2.5 (get-in layer [:mark :strokeWidth])))))

    (testing "non-best model has dashed line"
      (let [non-best-result (assoc sample-fit-result :best-model :lognormal)
            layer (charts/distribution-cdf-layer
                   :gamma non-best-result sample-cdf-grid identity-transforms)]
        (is (= [4 4] (get-in layer [:mark :strokeDash])))
        (is (= 1.5 (get-in layer [:mark :strokeWidth])))))

    (testing "returns nil for failed fit"
      (let [failed-result {:error "Fitting failed"}
            layer (charts/distribution-cdf-layer
                   :gamma failed-result sample-cdf-grid identity-transforms)]
        (is (nil? layer))))

    (testing "returns nil for skipped distribution"
      (let [skipped-result {:skipped :moment-match-failed}
            layer (charts/distribution-cdf-layer
                   :gamma skipped-result sample-cdf-grid identity-transforms)]
        (is (nil? layer))))

    (testing "works for all distribution types"
      (doseq [[dist params] [[:gamma {:shape 2.0 :scale 1.5}]
                             [:lognormal {:mu 0.5 :sigma 0.8}]
                             [:weibull {:shape 1.8 :scale 3.2}]
                             [:inverse-gaussian {:mu 3.0 :lambda 2.0}]]]
        (let [result {:params params}
              layer (charts/distribution-cdf-layer
                     dist result sample-cdf-grid identity-transforms)]
          (is (map? layer)
              (str "Failed for distribution: " dist))
          (is (seq (get-in layer [:data :values]))
              (str "No data for distribution: " dist)))))))

(deftest distribution-cdf-overlay-layers-test
  ;; Tests overlay layer generation for multiple distribution CDFs.
  ;; Verifies filtering of failed/skipped distributions.
  (testing "distribution-cdf-overlay-layers"
    (testing "returns layers for all successfully fitted distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:params {:mu 0.5 :sigma 0.8}}
                       :weibull {:params {:shape 1.8 :scale 3.2}}}
                      :best-model :gamma}
            layers (charts/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (= 3 (count layers)))
        (is (every? map? layers))))

    (testing "filters out failed distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:error "Fitting failed"}}
                      :best-model :gamma}
            layers (charts/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "filters out skipped distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :inverse-gaussian {:skipped :moment-match-failed}}
                      :best-model :gamma}
            layers (charts/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "returns empty vector when all fail"
      (let [fit-data {:distributions
                      {:gamma {:error "Fitting failed"}
                       :lognormal {:skipped :moment-match-failed}}
                      :best-model nil}
            layers (charts/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (empty? layers))))))

(deftest distribution-cdf-vega-spec-test
  ;; Tests complete Vega-Lite spec generation for ECDF with CDF overlays.
  ;; Verifies layer composition and structure.
  (testing "distribution-cdf-vega-spec"
    (testing "produces valid structure"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes chart dimensions"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts/distribution-cdf-vega-spec
                  data-map {} {:width 500 :height 350})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 350 (:height chart)))))

    (testing "includes ECDF and distribution CDF layers"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})
            chart (first (:vconcat spec))
            layers (:layer chart)]
        ;; Should have ECDF + distribution CDFs (gamma, lognormal, weibull)
        (is (= 4 (count layers)))))

    (testing "works without distribution-fit data"
      (let [data-map {:samples (:samples (test-data/distribution-cdf-data-map))}
            spec (charts/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        ;; Should still have ECDF layer
        (let [chart (first (:vconcat spec))
              layers (:layer chart)]
          (is (= 1 (count layers))))))))

(deftest distribution-cdf-vega-spec-schema-validation-test
  ;; Validates distribution-cdf-vega-spec output against Vega-Lite v6 schema.
  ;; Tests ECDF + distribution CDF overlay visualization.
  (testing "distribution-cdf-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "distribution-cdf-vega-spec validation failed: "
                 (pr-str (:errors result))))))))
