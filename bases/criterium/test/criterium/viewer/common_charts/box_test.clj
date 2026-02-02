(ns criterium.viewer.common-charts.box-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.schema-validation :as schema]))

;;; Test fixtures for box plot tests.

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

;;; Data preparation tests.
;;; Verifies correct extraction of median, CI bounds, and percentiles.

(deftest prepare-single-point-box-data-test
  ;; Tests data preparation for single-point box plots.
  ;; Contracts: returns vector of metric maps with :metric-id, :data, :y-title.
  ;; Data includes median, p10, p90, and optionally ciLower/ciUpper.
  (testing "prepare-single-point-box-data"
    (testing "extracts data for each metric"
      (let [result (charts.comparison/prepare-single-point-box-data
                    single-point-box-extract)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= :elapsed-time (:metric-id (first result))))))

    (testing "includes all implementations in data"
      (let [result (charts.comparison/prepare-single-point-box-data
                    single-point-box-extract)
            data (:data (first result))]
        (is (= 3 (count data)))
        (is (= #{"foo" "bar" "baz"}
               (set (map #(get % "impl") data))))))

    (testing "extracts median, p10, p90 values"
      (let [result (charts.comparison/prepare-single-point-box-data
                    single-point-box-extract)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))))

    (testing "extracts CI bounds when present"
      (let [result (charts.comparison/prepare-single-point-box-data
                    single-point-box-extract)
            data (:data (first result))]
        (is (every? #(contains? % "ciLower") data))
        (is (every? #(contains? % "ciUpper") data))
        (doseq [d data]
          (is (< (get d "ciLower") (get d "median")))
          (is (< (get d "median") (get d "ciUpper"))))))

    (testing "omits CI bounds when not present"
      (let [result (charts.comparison/prepare-single-point-box-data
                    single-point-box-extract-no-ci)
            data (:data (first result))]
        (is (every? #(contains? % "median") data))
        (is (every? #(contains? % "p10") data))
        (is (every? #(contains? % "p90") data))
        (is (every? #(not (contains? % "ciLower")) data))
        (is (every? #(not (contains? % "ciUpper")) data))))

    (testing "applies SI scaling to values"
      (let [result (charts.comparison/prepare-single-point-box-data
                    single-point-box-extract)
            first-metric (first result)]
        (is (string? (:y-title first-metric)))
        (is (re-find #"median" (:y-title first-metric)))
        (let [data (:data first-metric)
              medians (keep #(get % "median") data)]
          (is (seq medians))
          (is (every? pos? medians)))))

    (testing "warns and returns empty for missing bootstrap stats"
      (let [output (with-out-str
                     (let [result (charts.comparison/prepare-single-point-box-data
                                   single-point-missing-bootstrap)]
                       (is (empty? result))))]
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
  ;; Contracts: returns Vega-Lite spec with :vconcat, layered structure
  ;; (whisker, CI, median, tooltip), x encodes impl, y excludes zero.
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
        (is (= "tick" (get-in whisker-cap-p10 [:mark :type])))
        (is (= "tick" (get-in whisker-cap-p90 [:mark :type])))))

    (testing "includes CI box layer with bar mark"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            ci-layer (second (:layer chart))]
        (is (= "bar" (get-in ci-layer [:mark :type])))
        (is (= "ciLower" (get-in ci-layer [:encoding :y :field])))
        (is (= "ciUpper" (get-in ci-layer [:encoding :y2 :field])))
        (is (= "#333" (get-in ci-layer [:mark :stroke])))
        (is (= 1 (get-in ci-layer [:mark :strokeWidth])))))

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
        (is (false? (get-in y-encoding [:scale :zero])))))

    (testing "preserves implementation order from data"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding)))))

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
        (is (= 6 (count tooltip)))))))

(deftest single-point-box-chart-without-ci-test
  ;; Tests box plot spec when CI bounds are not present.
  ;; Contracts: only whiskers and median shown, tooltip excludes CI fields.
  (testing "single-point-box-chart-spec without CI bounds"
    (testing "produces structure without CI layer"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
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
        (is (= 4 (count tooltip)))))))

(deftest single-point-box-chart-missing-bootstrap-test
  ;; Tests box plot spec when bootstrap stats are missing entirely.
  ;; Contracts: empty vconcat, warning printed.
  (testing "single-point-box-chart-spec with missing bootstrap stats"
    (testing "produces empty vconcat"
      (let [output (with-out-str
                     (let [spec (charts.comparison/single-point-box-chart-spec
                                 single-point-missing-bootstrap
                                 {:width 400 :height 300})]
                       (is (empty? (:vconcat spec)))))]
        (is (re-find #"WARNING.*bootstrap" output))))))

;;; Comparison box chart tests.
;;; Verifies box plot chart generation from domain-comparison data.

(deftest comparison-box-chart-spec-test
  ;; Tests box plot spec generation from domain-comparison data.
  ;; Contracts: same structure as single-point-box-chart-spec.
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
        (is (false? (get-in y-encoding [:scale :zero])))))

    (testing "preserves implementation order from data"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            chart (first (:vconcat spec))
            x-encoding (get-in chart [:encoding :x])]
        (is (nil? (:sort x-encoding)))))))

(deftest comparison-box-chart-without-ci-test
  ;; Tests comparison box plot spec when CI bounds are not present.
  ;; Contracts: 3 layers (no CI box).
  (testing "comparison-box-chart-spec without CI bounds"
    (testing "produces structure without CI layer"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison-no-ci
                  {:width 400 :height 300})
            chart (first (:vconcat spec))]
        (is (contains? chart :layer))
        (is (= 3 (count (:layer chart))))))))

;;; Schema validation tests.
;;; Validates box chart specs against Vega-Lite v6 schema.

(deftest box-chart-schema-validation-test
  ;; Validates all box chart variants produce valid Vega-Lite specs.
  ;; Contracts: all specs pass Vega-Lite v6 schema validation.
  (testing "box chart schema validation"
    (testing "single-point-box-chart-spec produces valid spec"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "single-point-box-chart-spec without CI produces valid spec"
      (let [spec (charts.comparison/single-point-box-chart-spec
                  single-point-box-extract-no-ci
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "comparison-box-chart-spec produces valid spec"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))

    (testing "comparison-box-chart-spec without CI produces valid spec"
      (let [spec (charts.comparison/comparison-box-chart-spec
                  single-point-box-comparison-no-ci
                  {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "validation failed: " (pr-str (:errors result))))))))
