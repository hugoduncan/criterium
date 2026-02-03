(ns criterium.viewer.common-charts.bar-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.schema-validation :as schema]))

;;; Test fixtures for bar chart tests.

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

(def single-point-comparison
  "Sample single-point multi-impl comparison for bar chart testing."
  {:type :criterium/domain-comparison
   :axis :n
   :metric [:stats :elapsed-time :mean]
   :implementations [:foo :bar :baz]
   :data {:foo [{:coord {:n 100} :value 1.0e-6}]
          :bar [{:coord {:n 100} :value 2.0e-6}]
          :baz [{:coord {:n 100} :value 1.5e-6}]}})

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

;;; Data preparation tests.
;;; Verifies correct extraction and SI scaling of implementation values.

(deftest prepare-single-point-bar-data-test
  ;; Tests data preparation for single-point bar charts.
  ;; Contracts: returns vector of metric maps with :metric-id, :data, :y-title,
  ;; :has-error-bounds?. Data values are SI-scaled.
  (testing "prepare-single-point-bar-data"
    (testing "extracts data for each metric"
      (let [result (charts.comparison/prepare-single-point-bar-data
                    single-point-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))))

    (testing "includes all implementations in data"
      (let [result (charts.comparison/prepare-single-point-bar-data
                    single-point-extract)
            data (:data (first result))]
        (is (= 3 (count data)))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") data))))))

    (testing "applies SI scaling to values"
      (let [result (charts.comparison/prepare-single-point-bar-data
                    single-point-extract)
            first-metric (first result)]
        (is (string? (:y-title first-metric)))
        (let [data (:data first-metric)
              values (keep #(get % "value") data)]
          (is (seq values))
          (is (every? pos? values)))))

    (testing "handles multiple metrics"
      (let [multi-metric-extract
            (assoc-in single-point-extract
                      [:metrics :thread-allocation]
                      {:metric [:stats :thread-allocation :mean]
                       :data [[{:n 100 :impl :foo} 1000]
                              [{:n 100 :impl :bar} 2000]
                              [{:n 100 :impl :baz} 1500]]})
            result (charts.comparison/prepare-single-point-bar-data
                    multi-metric-extract)]
        (is (= 2 (count result)))
        (is (= #{:elapsed-time :thread-allocation}
               (set (map :metric-id result))))))

    (testing "returns has-error-bounds? false for plain values"
      (let [result (charts.comparison/prepare-single-point-bar-data
                    single-point-extract)
            first-metric (first result)]
        (is (false? (:has-error-bounds? first-metric)))
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
            result (charts.comparison/prepare-single-point-bar-data
                    extract-with-bounds)
            first-metric (first result)]
        (is (true? (:has-error-bounds? first-metric)))
        (is (re-find #"mean" (:y-title first-metric)))
        (let [data (:data first-metric)]
          (is (every? #(contains? % "valueLower") data))
          (is (every? #(contains? % "valueUpper") data))
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
            result (charts.comparison/prepare-single-point-bar-data
                    extract-with-median)
            first-metric (first result)]
        (is (re-find #"median" (:y-title first-metric)))))

    (testing "graceful degradation for mixed values"
      (let [extract-mixed
            {:type :criterium/domain-extract
             :impl-axis :impl
             :implementations [:foo :bar]
             :metrics {:elapsed-time
                       {:metric [:stats :elapsed-time :mean]
                        :data [[{:n 100 :impl :foo}
                                {:value 1.0e-6 :lower 0.9e-6 :upper 1.1e-6}]
                               [{:n 100 :impl :bar} 2.0e-6]]}}}
            result (charts.comparison/prepare-single-point-bar-data
                    extract-mixed)
            first-metric (first result)
            data (:data first-metric)]
        (is (true? (:has-error-bounds? first-metric)))
        (let [foo-data (first (filter #(= "foo" (get % "impl")) data))
              bar-data (first (filter #(= "bar" (get % "impl")) data))]
          (is (contains? foo-data "valueLower"))
          (is (contains? foo-data "valueUpper"))
          (is (not (contains? bar-data "valueLower")))
          (is (not (contains? bar-data "valueUpper"))))))))

;;; Single-point bar chart spec tests.
;;; Verifies bar chart generation for single-point multi-impl comparison scenarios.

(deftest single-point-bar-chart-spec-test
  ;; Tests bar chart spec generation for single-point multi-impl comparisons.
  ;; Contracts: returns Vega-Lite spec with :vconcat, bar mark, x/y encodings.
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
        (is (nil? (:sort x-encoding)))))

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

;;; Bar chart error bars tests.
;;; Verifies error bar generation for bar charts with error bounds.

(deftest single-point-bar-chart-with-error-bars-test
  ;; Tests bar chart spec generation when error bounds are present.
  ;; Contracts: produces layered spec with bar + error layers, rule encodes y/y2.
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
        (is (contains? error-composite :layer))
        (is (= 3 (count (:layer error-composite))))
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
  ;; Contracts: no layered structure when no bounds, simple bar mark.
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

;;; Comparison bar chart tests.
;;; Verifies bar chart generation from domain-comparison data.

(deftest comparison-bar-chart-spec-test
  ;; Tests bar chart spec generation from domain-comparison data.
  ;; Contracts: same structure as single-point-bar-chart-spec.
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
        (is (nil? (:sort x-encoding)))))))

(deftest comparison-bar-chart-with-error-bars-test
  ;; Tests comparison bar chart spec when error bounds are present.
  ;; Contracts: produces layered spec with bar + error layers.
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
        (is (contains? error-composite :layer))
        (is (= 3 (count (:layer error-composite))))
        (is (= "rule" (get-in rule-layer [:mark :type])))))))

(deftest comparison-bar-chart-graceful-degradation-test
  ;; Tests that comparison bar charts without error bounds render normally.
  ;; Contracts: no layered structure when no bounds.
  (testing "comparison-bar-chart-spec without error bounds"
    (testing "produces simple structure without layer"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (not (contains? chart :layer)))
        (is (= {:type "bar"} (:mark chart)))))))

;;; Schema validation tests.
;;; Validates bar chart specs against Vega-Lite v6 schema.

(deftest bar-chart-schema-validation-test
  ;; Validates all bar chart variants produce valid Vega-Lite specs.
  ;; Contracts: all specs pass Vega-Lite v6 schema validation.
  (testing "bar chart schema validation"
    (testing "single-point-bar-chart-spec produces valid spec"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "single-point-bar-chart-spec with error bounds produces valid spec"
      (let [spec (charts.comparison/single-point-bar-chart-spec
                  single-point-extract-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "comparison-bar-chart-spec produces valid spec"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "comparison-bar-chart-spec with error bounds produces valid spec"
      (let [spec (charts.comparison/comparison-bar-chart-spec
                  single-point-comparison-with-bounds
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))))
