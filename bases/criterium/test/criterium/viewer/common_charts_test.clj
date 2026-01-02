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
               (set (map :metric-id result))))))))

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
        (is (= "Implementation" (:title x-encoding)))))))

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
