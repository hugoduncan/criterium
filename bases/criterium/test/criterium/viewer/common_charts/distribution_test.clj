(ns criterium.viewer.common-charts.distribution-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.test-data :as test-data]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.schema-validation :as schema]))

;;; Shared test data

(def identity-transforms
  "Identity transforms for testing - `:sample->` must be a list of functions."
  {:sample-> (list identity)
   :->sample [identity]})

(def sample-fit-result
  "Sample fit result for a single distribution."
  {:params {:shape 2.0 :scale 1.5}
   :log-likelihood -150.0
   :aic 304.0
   :delta-aic 0.0})

(def sample-grid
  "Sample KDE grid for testing."
  [1.0 2.0 3.0 4.0 5.0])

(def sample-cdf-grid
  "Sample x-values grid for CDF testing."
  [1.0 2.0 3.0 4.0 5.0])

;;; KDE schema validation test

(deftest kde-vega-spec-schema-validation-test
  ;; Validates kde-vega-spec output against Vega-Lite v6 schema.
  ;; Tests KDE density curve visualization.
  (testing "kde-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/kde-data-map)
            view {}
            chart-options {:width 400 :height 300}
            spec (charts.distribution/kde-vega-spec data-map view chart-options)
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "kde-vega-spec validation failed: "
                 (pr-str (:errors result))))))))

;;; Distribution PDF overlay tests.
;;; Verifies PDF overlay layer generation for fitted distributions.

(deftest distribution-pdf-layer-test
  ;; Tests PDF layer generation for a single fitted distribution.
  ;; Verifies layer structure, data points, and styling.
  (testing "distribution-pdf-layer"
    (testing "produces valid layer for fitted distribution"
      (let [layer (charts.distribution/distribution-pdf-layer
                   :gamma sample-fit-result sample-grid "elapsed-time"
                   identity-transforms false)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))))

    (testing "includes PDF density values in data"
      (let [layer (charts.distribution/distribution-pdf-layer
                   :gamma sample-fit-result sample-grid "elapsed-time"
                   identity-transforms false)
            data (get-in layer [:data :values])]
        (is (= 5 (count data)))
        (is (every? #(contains? % "elapsed-time") data))
        (is (every? #(contains? % "pdf-density") data))
        ;; PDF values should be positive
        (is (every? #(pos? (double (get % "pdf-density"))) data))))

    (testing "uses line mark"
      (let [layer (charts.distribution/distribution-pdf-layer
                   :gamma sample-fit-result sample-grid "elapsed-time"
                   identity-transforms false)]
        (is (= "line" (get-in layer [:mark :type])))))

    (testing "best model has solid line"
      (let [best-result (assoc sample-fit-result :best-model :gamma)
            layer (charts.distribution/distribution-pdf-layer
                   :gamma best-result sample-grid "elapsed-time"
                   identity-transforms false)]
        (is (= [1 0] (get-in layer [:mark :strokeDash])))
        (is (= 2.5 (get-in layer [:mark :strokeWidth])))))

    (testing "non-best model has dashed line"
      (let [non-best-result (assoc sample-fit-result :best-model :lognormal)
            layer (charts.distribution/distribution-pdf-layer
                   :gamma non-best-result sample-grid "elapsed-time"
                   identity-transforms false)]
        (is (= [4 4] (get-in layer [:mark :strokeDash])))
        (is (= 1.5 (get-in layer [:mark :strokeWidth])))))

    (testing "returns nil for failed fit"
      (let [failed-result {:error "Fitting failed"}
            layer (charts.distribution/distribution-pdf-layer
                   :gamma failed-result sample-grid "elapsed-time"
                   identity-transforms false)]
        (is (nil? layer))))

    (testing "returns nil for skipped distribution"
      (let [skipped-result {:skipped :moment-match-failed}
            layer (charts.distribution/distribution-pdf-layer
                   :gamma skipped-result sample-grid "elapsed-time"
                   identity-transforms false)]
        (is (nil? layer))))

    (testing "works for all distribution types"
      (doseq [[dist params] [[:gamma {:shape 2.0 :scale 1.5}]
                             [:lognormal {:mu 0.5 :sigma 0.8}]
                             [:weibull {:shape 1.8 :scale 3.2}]
                             [:inverse-gaussian {:mu 3.0 :lambda 2.0}]]]
        (let [result {:params params}
              layer (charts.distribution/distribution-pdf-layer
                     dist result sample-grid "elapsed-time"
                     identity-transforms false)]
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
            layers (charts.distribution/distribution-pdf-overlay-layers
                    fit-data sample-grid "elapsed-time" identity-transforms false)]
        (is (= 3 (count layers)))
        (is (every? map? layers))))

    (testing "filters out failed distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:error "Fitting failed"}}
                      :best-model :gamma}
            layers (charts.distribution/distribution-pdf-overlay-layers
                    fit-data sample-grid "elapsed-time" identity-transforms false)]
        (is (= 1 (count layers)))))

    (testing "filters out skipped distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :inverse-gaussian {:skipped :moment-match-failed}}
                      :best-model :gamma}
            layers (charts.distribution/distribution-pdf-overlay-layers
                    fit-data sample-grid "elapsed-time" identity-transforms false)]
        (is (= 1 (count layers)))))

    (testing "returns empty vector when all fail"
      (let [fit-data {:distributions
                      {:gamma {:error "Fitting failed"}
                       :lognormal {:skipped :moment-match-failed}}
                      :best-model nil}
            layers (charts.distribution/distribution-pdf-overlay-layers
                    fit-data sample-grid "elapsed-time" identity-transforms false)]
        (is (empty? layers))))))

(deftest distribution-pdf-vega-spec-test
  ;; Tests complete Vega-Lite spec generation for KDE with PDF overlays.
  ;; Verifies layer composition and structure.
  (testing "distribution-pdf-vega-spec"
    (testing "produces valid structure"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts.distribution/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes chart dimensions"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts.distribution/distribution-pdf-vega-spec
                  data-map {} {:width 500 :height 350})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 350 (:height chart)))))

    (testing "includes KDE and distribution layers"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts.distribution/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})
            chart (first (:vconcat spec))
            inner-group (first (:layer chart))
            inner-layers (:layer inner-group)]
        ;; Should have KDE density + distribution PDFs
        ;; (gamma, lognormal, weibull - inverse-gaussian is skipped)
        ;; Note: KDE confidence band is intentionally not included to avoid
        ;; scale mismatch with fitted PDFs
        (is (>= (count inner-layers) 3))))

    (testing "works without distribution-fit data"
      (let [data-map (test-data/kde-data-map)
            spec (charts.distribution/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))))))

(deftest distribution-pdf-vega-spec-schema-validation-test
  ;; Validates distribution-pdf-vega-spec output against Vega-Lite v6 schema.
  ;; Tests KDE + distribution PDF overlay visualization.
  (testing "distribution-pdf-vega-spec"
    (testing "produces valid Vega-Lite spec"
      (let [data-map (test-data/distribution-fit-data-map)
            spec (charts.distribution/distribution-pdf-vega-spec
                  data-map {} {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "distribution-pdf-vega-spec validation failed: "

                 (pr-str (:errors result))))))))

;;; Distribution CDF overlay tests

(deftest ecdf-layer-test
  ;; Tests ECDF (empirical cumulative distribution function) layer generation.
  ;; Verifies step function structure and cumulative probability values.
  (testing "ecdf-layer"
    (testing "produces valid layer structure"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            layer (charts.distribution/ecdf-layer samples identity-transforms)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))))

    (testing "includes correct number of data points"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            layer (charts.distribution/ecdf-layer samples identity-transforms)
            data (get-in layer [:data :values])]
        (is (= 5 (count data)))))

    (testing "computes correct ECDF values"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            layer (charts.distribution/ecdf-layer samples identity-transforms)
            data (get-in layer [:data :values])
            cdf-values (mapv #(get % "cdf") data)]
        ;; ECDF at each point should be i/n
        (is (= [0.2 0.4 0.6 0.8 1.0] cdf-values))))

    (testing "uses step-after interpolation"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0]))
            layer (charts.distribution/ecdf-layer samples identity-transforms)]
        (is (= "step-after" (get-in layer [:mark :interpolate])))))

    (testing "handles unsorted samples"
      (let [samples (arr/->double-array (double-array [5.0 1.0 3.0 2.0 4.0]))
            layer (charts.distribution/ecdf-layer samples identity-transforms)
            data (get-in layer [:data :values])
            x-values (mapv #(get % "x") data)]
        ;; Should be sorted
        (is (= [1.0 2.0 3.0 4.0 5.0] x-values))))))

(deftest distribution-cdf-layer-test
  ;; Tests CDF layer generation for a single fitted distribution.
  ;; Verifies layer structure, data points, and styling.
  (testing "distribution-cdf-layer"
    (testing "produces valid layer for fitted distribution"
      (let [layer (charts.distribution/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)]
        (is (map? layer))
        (is (contains? layer :data))
        (is (contains? layer :mark))
        (is (contains? layer :encoding))))

    (testing "includes CDF values in data"
      (let [layer (charts.distribution/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)
            data (get-in layer [:data :values])]
        (is (= 5 (count data)))
        (is (every? #(contains? % "x") data))
        (is (every? #(contains? % "cdf") data))
        ;; CDF values should be between 0 and 1
        (is (every? #(<= 0.0 (get % "cdf") 1.0) data))))

    (testing "CDF values are monotonically increasing"
      (let [layer (charts.distribution/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)
            data (get-in layer [:data :values])
            cdf-values (mapv #(get % "cdf") data)]
        (is (apply <= cdf-values))))

    (testing "uses line mark"
      (let [layer (charts.distribution/distribution-cdf-layer
                   :gamma sample-fit-result sample-cdf-grid identity-transforms)]
        (is (= "line" (get-in layer [:mark :type])))))

    (testing "best model has solid line"
      (let [best-result (assoc sample-fit-result :best-model :gamma)
            layer (charts.distribution/distribution-cdf-layer
                   :gamma best-result sample-cdf-grid identity-transforms)]
        (is (= [1 0] (get-in layer [:mark :strokeDash])))
        (is (= 2.5 (get-in layer [:mark :strokeWidth])))))

    (testing "non-best model has dashed line"
      (let [non-best-result (assoc sample-fit-result :best-model :lognormal)
            layer (charts.distribution/distribution-cdf-layer
                   :gamma non-best-result sample-cdf-grid identity-transforms)]
        (is (= [4 4] (get-in layer [:mark :strokeDash])))
        (is (= 1.5 (get-in layer [:mark :strokeWidth])))))

    (testing "returns nil for failed fit"
      (let [failed-result {:error "Fitting failed"}
            layer (charts.distribution/distribution-cdf-layer
                   :gamma failed-result sample-cdf-grid identity-transforms)]
        (is (nil? layer))))

    (testing "returns nil for skipped distribution"
      (let [skipped-result {:skipped :moment-match-failed}
            layer (charts.distribution/distribution-cdf-layer
                   :gamma skipped-result sample-cdf-grid identity-transforms)]
        (is (nil? layer))))

    (testing "works for all distribution types"
      (doseq [[dist params] [[:gamma {:shape 2.0 :scale 1.5}]
                             [:lognormal {:mu 0.5 :sigma 0.8}]
                             [:weibull {:shape 1.8 :scale 3.2}]
                             [:inverse-gaussian {:mu 3.0 :lambda 2.0}]]]
        (let [result {:params params}
              layer (charts.distribution/distribution-cdf-layer
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
            layers (charts.distribution/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (= 3 (count layers)))
        (is (every? map? layers))))

    (testing "filters out failed distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :lognormal {:error "Fitting failed"}}
                      :best-model :gamma}
            layers (charts.distribution/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "filters out skipped distributions"
      (let [fit-data {:distributions
                      {:gamma {:params {:shape 2.0 :scale 1.5}}
                       :inverse-gaussian {:skipped :moment-match-failed}}
                      :best-model :gamma}
            layers (charts.distribution/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (= 1 (count layers)))))

    (testing "returns empty vector when all fail"
      (let [fit-data {:distributions
                      {:gamma {:error "Fitting failed"}
                       :lognormal {:skipped :moment-match-failed}}
                      :best-model nil}
            layers (charts.distribution/distribution-cdf-overlay-layers
                    fit-data sample-cdf-grid identity-transforms)]
        (is (empty? layers))))))

(deftest distribution-cdf-vega-spec-test
  ;; Tests complete Vega-Lite spec generation for ECDF with CDF overlays.
  ;; Verifies layer composition and structure.
  (testing "distribution-cdf-vega-spec"
    (testing "produces valid structure"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts.distribution/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})]
        (is (map? spec))
        (is (contains? spec :vconcat))
        (is (vector? (:vconcat spec)))
        (is (= 1 (count (:vconcat spec))))))

    (testing "includes chart dimensions"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts.distribution/distribution-cdf-vega-spec
                  data-map {} {:width 500 :height 350})
            chart (first (:vconcat spec))]
        (is (= 500 (:width chart)))
        (is (= 350 (:height chart)))))

    (testing "includes ECDF and distribution CDF layers"
      (let [data-map (test-data/distribution-cdf-data-map)
            spec (charts.distribution/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})
            chart (first (:vconcat spec))
            layers (:layer chart)]
        ;; Should have ECDF + distribution CDFs (gamma, lognormal, weibull)
        (is (= 4 (count layers)))))

    (testing "works without distribution-fit data"
      (let [data-map {:samples (:samples (test-data/distribution-cdf-data-map))}
            spec (charts.distribution/distribution-cdf-vega-spec
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
            spec (charts.distribution/distribution-cdf-vega-spec
                  data-map {} {:width 400 :height 300})
            result (schema/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "distribution-cdf-vega-spec validation failed: "
                 (pr-str (:errors result))))))))
