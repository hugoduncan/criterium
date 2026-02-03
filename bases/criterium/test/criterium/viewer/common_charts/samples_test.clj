(ns criterium.viewer.common-charts.samples-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.viewer.common-charts.samples :as charts.samples]
   [criterium.viewer.schema-validation :as schema]))

;;; Schema validation tests for samples/histogram chart spec functions.
;;; Validates that generated specs conform to official Vega-Lite JSON schemas.

(deftest samples-vega-spec-schema-validation-test
  ;; Validates samples-vega-spec output against Vega-Lite v6 schema.
  ;; Tests the scatter plot visualization of benchmark samples.
  (testing "samples-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/samples-data-map)
            view {}
            chart-options {:width 400 :height 300}
            spec (charts.samples/samples-vega-spec data-map view chart-options)
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
            spec (charts.samples/histogram-vega-spec
                  data-map
                  view
                  chart-options)
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
            spec (charts.samples/histogram-vega-spec data-map view chart-options)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "histogram-vega-spec with boxplot validation failed: "
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
      (let [result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (map? (first result)))
        (is (contains? (first result) :layer))))

    (testing "contains whisker, CI box, and median line layers"
      (let [result (charts.samples/metric-bootstrap-boxplot-layer
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
      (let [result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            whisker (get-in result [0 :layer 0])
            data (get-in whisker [:data :values 0])]
        ;; Vega-Lite data uses string keys
        (is (= 90.0 (get data "elapsed-time")))
        (is (= 110.0 (get data :end)))))

    (testing "CI box spans median confidence interval"
      (let [result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            ci-box (get-in result [0 :layer 1])
            data (get-in ci-box [:data :values 0])]
        ;; Vega-Lite data uses string keys for field names
        (is (= 95.0 (get data "elapsed-time")))
        (is (= 105.0 (get data :end)))))

    (testing "median line at point estimate"
      (let [result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            median-line (get-in result [0 :layer 2])
            data (get-in median-line [:data :values 0])]
        ;; Vega-Lite data uses string keys for field names
        (is (= 100.0 (get data "elapsed-time")))))

    (testing "applies transforms to raw bootstrap values"
      (let [scale-transforms {:sample-> (list (fn [^double v] (/ v 1e9)))
                              :->sample [identity]}
            result (charts.samples/metric-bootstrap-boxplot-layer
                    scale-transforms sample-bootstrap-stats sample-metric-config)
            whisker (get-in result [0 :layer 0])
            whisker-data (get-in whisker [:data :values 0])]
        ;; Values are transformed (divided by 1e9)
        (is (= 90.0e-9 (double (get whisker-data "elapsed-time"))))))

    (testing "returns nil when quantiles missing"
      (let [missing-quantiles {:quantiles {}}
            result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms missing-quantiles sample-metric-config)]
        (is (nil? result))))

    (testing "returns nil when p50 missing"
      (let [missing-p50 {:quantiles {0.1 {:point-estimate 90.0}
                                     0.9 {:point-estimate 110.0}}}
            result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms missing-p50 sample-metric-config)]
        (is (nil? result))))

    (testing "omits CI box when median CI empty"
      (let [no-ci {:quantiles
                   {0.1 {:point-estimate 90.0}
                    0.5 {:point-estimate 100.0
                         :estimate-quantiles []}
                    0.9 {:point-estimate 110.0}}}
            result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms no-ci sample-metric-config)
            inner-layers (get-in result [0 :layer])]
        ;; Only whisker and median line (no CI box)
        (is (= 2 (count inner-layers)))
        (is (= "rule" (get-in (first inner-layers) [:mark :type])))
        (is (= "rule" (get-in (second inner-layers) [:mark :type])))))

    (testing "includes layer transforms for legend"
      (let [result (charts.samples/metric-bootstrap-boxplot-layer
                    identity-transforms sample-bootstrap-stats sample-metric-config)
            whisker (get-in result [0 :layer 0])]
        (is (some? (get-in whisker [:transform])))
        (is (some #(contains? % :calculate) (get-in whisker [:transform])))))))
