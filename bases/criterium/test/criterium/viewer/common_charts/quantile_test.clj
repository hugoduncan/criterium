(ns criterium.viewer.common-charts.quantile-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.test-data :as test-data]
   [criterium.viewer.common-charts.quantile :as charts.quantile]
   [criterium.viewer.schema-validation :as schema]))

;;; Shared test data

(def identity-transforms
  "Identity transforms for testing - `:sample->` must be a list of functions."
  {:sample-> (list identity)
   :->sample [identity]})

;;; Q-Q Plot Chart Tests

(deftest qq-points-test
  ;; Tests Q-Q point generation comparing sample quantiles to theoretical quantiles.
  ;; Verifies correct output structure and Hazen plotting position.
  (testing "qq-points"
    (testing "generates correct structure"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            ;; Simple identity quantile function for testing
            quantile-fn identity
            points (charts.quantile/qq-points samples quantile-fn identity-transforms)]
        (is (= 5 (count points)))
        (is (every? #(contains? % "theoretical") points))
        (is (every? #(contains? % "observed") points))))

    (testing "uses Hazen plotting position"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            quantile-fn identity
            points (charts.quantile/qq-points samples quantile-fn identity-transforms)]
        ;; Hazen: (i - 0.5) / n for i = 1, 2, 3 and n = 3
        ;; p1 = 0.5/3 = 0.167, p2 = 1.5/3 = 0.5, p3 = 2.5/3 = 0.833
        (is (< (Math/abs (- (/ 0.5 3.0) (double (get (nth points 0) "theoretical")))) 0.001))
        (is (< (Math/abs (- 0.5 (double (get (nth points 1) "theoretical")))) 0.001))
        (is (< (Math/abs (- (/ 2.5 3.0) (double (get (nth points 2) "theoretical")))) 0.001))))

    (testing "preserves sorted sample values"
      (let [samples (arr/->double-array (double-array [3.0 1.0 2.0]))  ; unsorted input
            quantile-fn identity
            points (charts.quantile/qq-points samples quantile-fn identity-transforms)]
        ;; Observed values should be sorted
        (is (= 1.0 (get (nth points 0) "observed")))
        (is (= 2.0 (get (nth points 1) "observed")))
        (is (= 3.0 (get (nth points 2) "observed")))))

    (testing "applies transforms"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            quantile-fn identity
            transforms {:sample-> (list #(* 1000.0 (double %)))
                        :->sample [#(/ (double %) 1000.0)]}
            points (charts.quantile/qq-points samples quantile-fn transforms)]
        ;; Values should be transformed to ns from s
        (is (= 1000.0 (get (nth points 0) "observed")))))))

(deftest distribution-qq-layer-test
  ;; Tests Q-Q scatter layer generation for fitted distributions.
  ;; Verifies layer structure, mark properties, and encoding.
  (testing "distribution-qq-layer"
    (testing "generates valid layer for fitted distribution"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            fit-result {:params {:shape 2.0 :scale 1.5}
                        :best-model :gamma}
            layer (charts.quantile/distribution-qq-layer
                   :gamma fit-result samples identity-transforms)]
        (is (some? layer))
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))
        (is (= "point" (get-in layer [:mark :type])))))

    (testing "uses larger filled marks for best model"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            fit-result {:params {:shape 2.0 :scale 1.5}
                        :best-model :gamma}
            layer (charts.quantile/distribution-qq-layer
                   :gamma fit-result samples identity-transforms)]
        (is (= 60 (get-in layer [:mark :size])))
        (is (true? (get-in layer [:mark :filled])))))

    (testing "uses smaller hollow marks for non-best model"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            fit-result {:params {:mu 0.5 :sigma 0.6}
                        :best-model :gamma}  ; lognormal is not best
            layer (charts.quantile/distribution-qq-layer
                   :lognormal fit-result samples identity-transforms)]
        (is (= 40 (get-in layer [:mark :size])))
        (is (false? (get-in layer [:mark :filled])))))

    (testing "returns nil for skipped distribution"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            fit-result {:skipped :moment-match-failed}
            layer (charts.quantile/distribution-qq-layer
                   :inverse-gaussian fit-result samples identity-transforms)]
        (is (nil? layer))))

    (testing "returns nil for distribution without params"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            fit-result {}
            layer (charts.quantile/distribution-qq-layer
                   :gamma fit-result samples identity-transforms)]
        (is (nil? layer))))))

(deftest qq-reference-line-layer-test
  ;; Tests reference line (y=x diagonal) generation for Q-Q plots.
  ;; Verifies line structure and range extension.
  (testing "qq-reference-line-layer"
    (testing "generates valid layer structure"
      (let [layer (charts.quantile/qq-reference-line-layer 1.0 5.0)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))
        (is (= "line" (get-in layer [:mark :type])))))

    (testing "uses dashed line style"
      (let [layer (charts.quantile/qq-reference-line-layer 1.0 5.0)]
        (is (= [4 4] (get-in layer [:mark :strokeDash])))))

    (testing "extends range for visual clarity"
      (let [layer (charts.quantile/qq-reference-line-layer 1.0 5.0)
            values (get-in layer [:data :values])]
        ;; Range 1.0-5.0, margin = 0.05 * 4 = 0.2
        ;; Start = 1.0 - 0.2 = 0.8, End = 5.0 + 0.2 = 5.2
        (is (= 2 (count values)))
        (is (< (Math/abs (- 0.8 (double (get (first values) "x")))) 0.001))
        (is (< (Math/abs (- 5.2 (double (get (second values) "x")))) 0.001))))))

(deftest distribution-qq-overlay-layers-test
  ;; Tests building Q-Q overlay layers for all fitted distributions.
  ;; Verifies filtering of skipped distributions and layer count.
  (testing "distribution-qq-overlay-layers"
    (testing "generates layers for fitted distributions"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            fit-data {:best-model :gamma
                      :distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:params {:mu 0.5 :sigma 0.6}}
                       :weibull {:params {:shape 2.0 :scale 3.0}}}}
            layers (charts.quantile/distribution-qq-overlay-layers
                    fit-data samples identity-transforms)]
        (is (= 3 (count layers)))
        (is (every? map? layers))))

    (testing "filters out skipped distributions"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            fit-data {:best-model :gamma
                      :distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :inverse-gaussian {:skipped :moment-match-failed}}}
            layers (charts.quantile/distribution-qq-overlay-layers
                    fit-data samples identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "returns empty vector when no distributions fitted"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            fit-data {:best-model nil :distributions {}}
            layers (charts.quantile/distribution-qq-overlay-layers
                    fit-data samples identity-transforms)]
        (is (empty? layers))))))

(deftest distribution-qq-vega-spec-test
  ;; Tests complete Vega-Lite spec generation for Q-Q plots with subplots per distribution.
  ;; Verifies subplot grid structure and individual subplot composition.
  (testing "distribution-qq-vega-spec"
    (testing "produces valid structure with subplot grid"
      (let [data-map (test-data/distribution-qq-data-map)
            spec (charts.quantile/distribution-qq-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        ;; First metric produces a grid of subplots
        (let [metric-grid (first (:vconcat spec))]
          (is (contains? metric-grid :vconcat) "Grid uses vconcat for rows"))))

    (testing "creates subplot for each distribution"
      (let [data-map (test-data/distribution-qq-data-map)
            spec (charts.quantile/distribution-qq-vega-spec
                  data-map {} {:width 400 :height 300})
            metric-grid (first (:vconcat spec))
            rows (:vconcat metric-grid)
            all-subplots (mapcat :hconcat rows)]
        ;; Test data has 3 distributions (gamma, lognormal, weibull)
        (is (= 3 (count all-subplots)))
        ;; Each subplot has a title with the distribution name
        (is (every? #(get-in % [:title :text]) all-subplots))))

    (testing "each subplot includes reference line and Q-Q scatter layers"
      (let [data-map (test-data/distribution-qq-data-map)
            spec (charts.quantile/distribution-qq-vega-spec
                  data-map {} {:width 400 :height 300})
            metric-grid (first (:vconcat spec))
            rows (:vconcat metric-grid)
            first-subplot (first (:hconcat (first rows)))
            layers (:layer first-subplot)]
        ;; Each subplot has 2 layers: reference line + Q-Q scatter
        (is (= 2 (count layers)))
        ;; First layer is reference line (dashed)
        (is (= [4 4] (get-in (first layers) [:mark :strokeDash])))
        ;; Second layer is scatter plot
        (is (= "point" (get-in (second layers) [:mark :type])))))

    (testing "subplot axes constrained to observed data range"
      (let [data-map (test-data/distribution-qq-data-map)
            spec (charts.quantile/distribution-qq-vega-spec
                  data-map {} {:width 400 :height 300})
            metric-grid (first (:vconcat spec))
            rows (:vconcat metric-grid)
            first-subplot (first (:hconcat (first rows)))
            layers (:layer first-subplot)
            scatter-layer (second layers)
            x-domain (get-in scatter-layer [:encoding :x :scale :domain])
            y-domain (get-in scatter-layer [:encoding :y :scale :domain])]
        ;; Both axes should have explicit domain constraints
        (is (vector? x-domain))
        (is (vector? y-domain))
        ;; Domains should be equal (square plot for y=x reference)
        (is (= x-domain y-domain))))

    (testing "returns nil chart when no distribution-fit data"
      (let [data-map {:samples (:samples (test-data/distribution-qq-data-map))}
            spec (charts.quantile/distribution-qq-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        ;; Chart should be nil when no fit data
        (is (nil? (first (:vconcat spec))))))))

(deftest distribution-qq-vega-spec-schema-validation-test
  ;; Validates distribution-qq-vega-spec output against Vega-Lite v6 schema.
  ;; Tests Q-Q plot with distribution overlay visualization.
  (testing "distribution-qq-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/distribution-qq-data-map)
            spec (charts.quantile/distribution-qq-vega-spec
                  data-map {} {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "distribution-qq-vega-spec validation failed: "
                 (pr-str (:errors result))))))))
