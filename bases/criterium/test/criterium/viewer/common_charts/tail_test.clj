(ns criterium.viewer.common-charts.tail-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.test-data :as test-data]
   [criterium.viewer.common-charts.tail :as charts.tail]
   [criterium.viewer.schema-validation :as schema]))

;;; Shared test data

(def identity-transforms
  "Identity transforms for testing - `:sample->` must be a list of functions."
  {:sample-> (list identity)
   :->sample [identity]})

;;; Tail Ratios Table Tests

(deftest tail-ratios-table-test
  ;; Tests tail ratios table generation showing percentile ratios.
  ;; Verifies correct output structure and data values.
  (testing "tail-ratios-table"
    (testing "generates valid structure with all ratios"
      (let [tail-data {:tail-ratios {:p99-p95 1.5 :p999-p99 1.8 :p999-p95 2.7}
                       :empirical-quantiles {:p95 10.0 :p99 15.0 :p999 27.0}}
            spec (charts.tail/tail-ratios-table tail-data)]
        (is (map? spec))
        (is (contains? spec :data))
        (is (contains? spec :mark))
        (is (contains? spec :encoding))
        (is (= "bar" (get-in spec [:mark :type])))
        (is (= 3 (count (get-in spec [:data :values]))))))

    (testing "includes tooltip with percentile values"
      (let [tail-data {:tail-ratios {:p99-p95 1.5}
                       :empirical-quantiles {:p95 10.0 :p99 15.0}}
            spec (charts.tail/tail-ratios-table tail-data)
            tooltips (get-in spec [:encoding :tooltip])]
        (is (some #(= "Numerator" (:field %)) tooltips))
        (is (some #(= "Denominator" (:field %)) tooltips))))

    (testing "returns nil when no ratios present"
      (let [tail-data {:tail-ratios {}
                       :empirical-quantiles {:p95 10.0}}
            spec (charts.tail/tail-ratios-table tail-data)]
        (is (nil? spec))))

    (testing "handles partial ratios"
      (let [tail-data {:tail-ratios {:p99-p95 1.5}
                       :empirical-quantiles {:p95 10.0 :p99 15.0}}
            spec (charts.tail/tail-ratios-table tail-data)]
        (is (= 1 (count (get-in spec [:data :values]))))))))

;;; Hill Plot Tests

(deftest hill-plot-test
  ;; Tests Hill plot generation showing tail index estimates.
  ;; Verifies layer structure and stable estimate reference line.
  (testing "hill-plot"
    (testing "generates layered spec with Hill curve"
      (let [tail-data {:hill {:k-range [3 4 5 6 7]
                              :estimates [0.8 0.85 0.82 0.81 0.83]
                              :stable-estimate 0.82}}
            spec (charts.tail/hill-plot tail-data)]
        (is (map? spec))
        (is (contains? spec :layer))
        (is (vector? (:layer spec)))
        (is (>= (count (:layer spec)) 1))))

    (testing "includes stable estimate reference line when present"
      (let [tail-data {:hill {:k-range [3 4 5]
                              :estimates [0.8 0.85 0.82]
                              :stable-estimate 0.82}}
            spec (charts.tail/hill-plot tail-data)
            layers (:layer spec)]
        (is (= 2 (count layers)))
        ;; Second layer is the rule for stable estimate
        (is (= "rule" (get-in (second layers) [:mark :type])))))

    (testing "works without stable estimate"
      (let [tail-data {:hill {:k-range [3 4 5]
                              :estimates [0.8 0.85 0.82]}}
            spec (charts.tail/hill-plot tail-data)
            layers (:layer spec)]
        (is (= 1 (count layers)))))

    (testing "returns nil with empty data"
      (let [tail-data {:hill {:k-range [] :estimates []}}
            spec (charts.tail/hill-plot tail-data)]
        (is (nil? spec))))

    (testing "returns nil with nil hill data"
      (let [tail-data {:hill nil}
            spec (charts.tail/hill-plot tail-data)]
        (is (nil? spec))))))

;;; MRL Plot Tests

(deftest mrl-plot-test
  ;; Tests mean residual life plot generation.
  ;; Verifies layer structure and threshold indicator.
  (testing "mrl-plot"
    (testing "generates layered spec with MRL curve and threshold line"
      (let [tail-data {:threshold 5.0
                       :mrl {:thresholds [2.0 3.0 4.0 5.0 6.0]
                             :values [4.5 5.2 6.0 7.0 8.5]}}
            spec (charts.tail/mrl-plot tail-data identity-transforms)]
        (is (map? spec))
        (is (contains? spec :layer))
        (is (= 2 (count (:layer spec))))
        ;; First layer is MRL line
        (is (= "line" (get-in (first (:layer spec)) [:mark :type])))
        ;; Second layer is threshold rule
        (is (= "rule" (get-in (second (:layer spec)) [:mark :type])))))

    (testing "applies transforms to values"
      (let [tail-data {:threshold 5.0
                       :mrl {:thresholds [2.0 3.0]
                             :values [4.5 5.2]}}
            transforms {:sample-> (list #(* 1000.0 (double %)))
                        :->sample [#(/ (double %) 1000.0)]}
            spec (charts.tail/mrl-plot tail-data transforms)
            data (get-in (first (:layer spec)) [:data :values])]
        ;; Values should be transformed (multiplied by 1000)
        (is (= 2000.0 (get (first data) "threshold")))))

    (testing "returns nil with empty thresholds"
      (let [tail-data {:threshold 5.0
                       :mrl {:thresholds [] :values []}}
            spec (charts.tail/mrl-plot tail-data identity-transforms)]
        (is (nil? spec))))))

;;; Zipf Plot Tests

(deftest zipf-plot-test
  ;; Tests Zipf plot (complementary CDF on log-log scale) generation.
  ;; Verifies scatter plot structure and log transformation.
  (testing "zipf-plot"
    (testing "generates scatter plot with log-transformed axes"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            spec (charts.tail/zipf-plot samples identity-transforms)]
        (is (map? spec))
        (is (= "point" (get-in spec [:mark :type])))
        (is (contains? spec :encoding))
        (is (= "log_x" (get-in spec [:encoding :x :field])))
        (is (= "log_ccdf" (get-in spec [:encoding :y :field])))))

    (testing "includes original values in tooltip"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            spec (charts.tail/zipf-plot samples identity-transforms)
            tooltips (get-in spec [:encoding :tooltip])]
        (is (some #(= "x" (:field %)) tooltips))
        (is (some #(= "ccdf" (:field %)) tooltips))))

    (testing "applies transforms"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            transforms {:sample-> (list #(* 1000.0 (double %)))
                        :->sample [#(/ (double %) 1000.0)]}
            spec (charts.tail/zipf-plot samples transforms)
            data (get-in spec [:data :values])]
        ;; x values should be transformed
        (is (= 1000.0 (get (first data) "x")))))

    (testing "returns nil for empty samples"
      (let [samples (arr/->double-array (double-array []))
            spec (charts.tail/zipf-plot samples identity-transforms)]
        (is (nil? spec))))

    (testing "returns nil for nil samples"
      (let [spec (charts.tail/zipf-plot nil identity-transforms)]
        (is (nil? spec))))))

;;; Exponential Q-Q Plot Tests

(deftest exponential-qq-plot-test
  ;; Tests exponential Q-Q plot generation for tail analysis.
  ;; Verifies layer structure and Q-Q point generation.
  (testing "exponential-qq-plot"
    (testing "generates layered spec with reference line and scatter"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            spec (charts.tail/exponential-qq-plot
                  samples
                  threshold
                  identity-transforms)]
        (is (map? spec))
        (is (contains? spec :layer))
        (is (= 2 (count (:layer spec))))
        ;; First layer is reference line
        (is (= "line" (get-in (first (:layer spec)) [:mark :type])))
        ;; Second layer is scatter
        (is (= "point" (get-in (second (:layer spec)) [:mark :type])))))

    (testing "reference line has matching domain for x and y"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            spec (charts.tail/exponential-qq-plot
                  samples
                  threshold
                  identity-transforms)
            ref-line (first (:layer spec))
            x-domain (get-in ref-line [:encoding :x :scale :domain])
            y-domain (get-in ref-line [:encoding :y :scale :domain])]
        (is (= x-domain y-domain))))

    (testing "returns nil when too few exceedances"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 6.0]))
            threshold 5.0  ; Only 1 exceedance
            spec
            (charts.tail/exponential-qq-plot
             samples
             threshold
             identity-transforms)]
        (is (nil? spec))))

    (testing "returns nil for nil samples"
      (let [spec (charts.tail/exponential-qq-plot nil 5.0 identity-transforms)]
        (is (nil? spec))))))

;;; GPD Q-Q Plot Tests

(deftest gpd-qq-plot-test
  ;; Tests GPD Q-Q plot generation for tail analysis.
  ;; Verifies layer structure and GPD quantile usage.
  (testing "gpd-qq-plot"
    (testing "generates layered spec with reference line and scatter"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            gpd-fit {:xi 0.3 :sigma 2.5}
            spec (charts.tail/gpd-qq-plot
                  samples
                  threshold
                  gpd-fit
                  identity-transforms)]
        (is (map? spec))
        (is (contains? spec :layer))
        (is (= 2 (count (:layer spec))))
        ;; First layer is reference line
        (is (= "line" (get-in (first (:layer spec)) [:mark :type])))
        ;; Second layer is scatter
        (is (= "point" (get-in (second (:layer spec)) [:mark :type])))))

    (testing "returns nil when gpd-fit is nil"
      (let [samples (arr/->double-array (double-array [1.0 2.0 6.0 7.0 8.0]))
            spec (charts.tail/gpd-qq-plot samples 5.0 nil identity-transforms)]
        (is (nil? spec))))

    (testing "returns nil when xi is missing"
      (let [samples (arr/->double-array (double-array [1.0 2.0 6.0 7.0 8.0]))
            gpd-fit {:sigma 2.5}
            spec (charts.tail/gpd-qq-plot
                  samples
                  5.0
                  gpd-fit
                  identity-transforms)]
        (is (nil? spec))))

    (testing "returns nil when sigma is zero"
      (let [samples (arr/->double-array (double-array [1.0 2.0 6.0 7.0 8.0]))
            gpd-fit {:xi 0.3 :sigma 0.0}
            spec (charts.tail/gpd-qq-plot
                  samples
                  5.0
                  gpd-fit
                  identity-transforms)]
        (is (nil? spec))))))

;;; Complete Vega Spec Tests

(deftest tail-analysis-vega-spec-test
  ;; Tests complete Vega-Lite spec generation for tail analysis visualization.
  ;; Verifies overall structure and chart composition.
  (testing "tail-analysis-vega-spec"
    (testing "produces valid structure with vconcat"
      (let [data-map (test-data/tail-analysis-data-map)
            spec (charts.tail/tail-analysis-vega-spec
                  data-map {} {:width 400 :height 200})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))))

    (testing "generates charts for each metric"
      (let [data-map (test-data/tail-analysis-data-map)
            spec (charts.tail/tail-analysis-vega-spec
                  data-map {} {:width 400 :height 200})
            metric-charts (filterv some? (:vconcat spec))]
        ;; Should have at least one metric with charts
        (is (pos? (count metric-charts)))
        ;; Each metric should have nested vconcat of individual charts
        (is (every? #(contains? % :vconcat) metric-charts))))

    (testing "each metric has title"
      (let [data-map (test-data/tail-analysis-data-map)
            spec (charts.tail/tail-analysis-vega-spec
                  data-map {} {:width 400 :height 200})
            metric-charts (filterv some? (:vconcat spec))]
        (is (every? #(contains? % :title) metric-charts))))

    (testing "returns empty vconcat when no tail-analysis data"
      (let [data-map {:samples (:samples (test-data/tail-analysis-data-map))}
            spec (charts.tail/tail-analysis-vega-spec
                  data-map {} {:width 400 :height 200})]
        (is (map? spec))
        (is (empty? (:vconcat spec)))))))

;;; Schema Validation Tests

(deftest tail-ratios-table-schema-validation-test
  ;; Validates tail-ratios-table output against Vega-Lite v6 schema.
  (testing "tail-ratios-table"
    (testing "produces valid Vega-Lite spec"
      (let [tail-data {:tail-ratios {:p99-p95 1.5 :p999-p99 1.8 :p999-p95 2.7}
                       :empirical-quantiles {:p95 10.0 :p99 15.0 :p999 27.0}}
            spec (charts.tail/tail-ratios-table tail-data)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "tail-ratios-table validation failed: "
                 (pr-str (:errors result))))))))

(deftest hill-plot-schema-validation-test
  ;; Validates hill-plot output against Vega-Lite v6 schema.
  (testing "hill-plot"
    (testing "produces valid Vega-Lite spec"
      (let [tail-data {:hill {:k-range [3 4 5 6 7 8]
                              :estimates [0.8 0.85 0.82 0.81 0.83 0.84]
                              :stable-estimate 0.82}}
            spec (charts.tail/hill-plot tail-data)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "hill-plot validation failed: "
                 (pr-str (:errors result))))))))

(deftest mrl-plot-schema-validation-test
  ;; Validates mrl-plot output against Vega-Lite v6 schema.
  (testing "mrl-plot"
    (testing "produces valid Vega-Lite spec"
      (let [tail-data {:threshold 5.0
                       :mrl {:thresholds [2.0 3.0 4.0 5.0 6.0]
                             :values [4.5 5.2 6.0 7.0 8.5]}}
            spec (charts.tail/mrl-plot tail-data identity-transforms)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "mrl-plot validation failed: "
                 (pr-str (:errors result))))))))

(deftest zipf-plot-schema-validation-test
  ;; Validates zipf-plot output against Vega-Lite v6 schema.
  (testing "zipf-plot"
    (testing "produces valid Vega-Lite spec"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            spec (charts.tail/zipf-plot samples identity-transforms)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "zipf-plot validation failed: "
                 (pr-str (:errors result))))))))

(deftest exponential-qq-plot-schema-validation-test
  ;; Validates exponential-qq-plot output against Vega-Lite v6 schema.
  (testing "exponential-qq-plot"
    (testing "produces valid Vega-Lite spec"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            spec (charts.tail/exponential-qq-plot
                  samples
                  threshold
                  identity-transforms)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "exponential-qq-plot validation failed: "
                 (pr-str (:errors result))))))))

(deftest gpd-qq-plot-schema-validation-test
  ;; Validates gpd-qq-plot output against Vega-Lite v6 schema.
  (testing "gpd-qq-plot"
    (testing "produces valid Vega-Lite spec"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            gpd-fit {:xi 0.3 :sigma 2.5}
            spec (charts.tail/gpd-qq-plot
                  samples
                  threshold
                  gpd-fit
                  identity-transforms)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "gpd-qq-plot validation failed: "
                 (pr-str (:errors result))))))))

(deftest tail-analysis-vega-spec-schema-validation-test
  ;; Validates tail-analysis-vega-spec output against Vega-Lite v6 schema.
  (testing "tail-analysis-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/tail-analysis-data-map)
            spec (charts.tail/tail-analysis-vega-spec
                  data-map {} {:width 400 :height 200})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "tail-analysis-vega-spec validation failed: "
                 (pr-str (:errors result))))))))
