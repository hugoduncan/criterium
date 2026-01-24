(ns criterium.viewer.common-charts.autocorrelation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts.autocorrelation :as acf-chart]
   [criterium.viewer.schema-validation :as sv]))

;; Test ACF chart specification generation and validation.
;; Uses synthetic autocorrelation data to verify chart structure.

(def ^:private sample-acf-data
  "Synthetic autocorrelation analysis data for testing."
  {:acf {1 0.25, 2 0.18, 3 0.12, 4 0.08, 5 0.05,
         6 0.03, 7 0.02, 8 0.01, 9 0.005, 10 0.002}
   :lag-1 {:value 0.25 :severity :moderate}
   :lag-severities {1 :moderate, 2 :minor, 3 :none, 4 :none, 5 :none,
                    6 :none, 7 :none, 8 :none, 9 :none, 10 :none}
   :effective-sample-size {:n-original 100 :n-effective 60 :ratio 0.6}
   :ci-inflation-factor 1.29
   :ljung-box {:q-statistic 15.2 :df 10 :p-value 0.12}
   :pattern :transient-effects
   :classification :warning
   :detected-period nil})

(def ^:private sample-acf-data-clean
  "Synthetic autocorrelation data with all lags at :none severity."
  {:acf {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.005,
         6 0.003, 7 0.002, 8 0.001, 9 0.0005, 10 0.0002}
   :lag-1 {:value 0.05 :severity :none}
   :lag-severities {1 :none, 2 :none, 3 :none, 4 :none, 5 :none,
                    6 :none, 7 :none, 8 :none, 9 :none, 10 :none}
   :effective-sample-size {:n-original 100 :n-effective 100 :ratio 1.0}
   :ci-inflation-factor 1.0
   :ljung-box {:q-statistic 2.1 :df 10 :p-value 0.85}
   :pattern :clean
   :classification :pass
   :detected-period nil})

(def ^:private sample-acf-data-with-period
  "Synthetic autocorrelation data with periodic pattern."
  (assoc sample-acf-data
         :acf {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.005,
               6 0.25, 7 0.02, 8 0.01, 9 0.005, 10 0.002}
         :lag-1 {:value 0.05 :severity :none}
         :lag-severities {1 :none, 2 :none, 3 :none, 4 :none, 5 :none,
                          6 :moderate, 7 :none, 8 :none, 9 :none, 10 :none}
         :pattern :periodic
         :detected-period 6))

(deftest acf->chart-data-test
  (testing "acf->chart-data"
    (testing "transforms ACF map to chart data format"
      (let [result (acf-chart/acf->chart-data sample-acf-data)]
        (is (vector? result))
        (is (= 10 (count result)))
        (is (every? #(contains? % :lag) result))
        (is (every? #(contains? % :acf) result))
        (is (every? #(contains? % :severity) result))))

    (testing "returns data sorted by lag"
      (let [result (acf-chart/acf->chart-data sample-acf-data)
            lags (mapv :lag result)]
        (is (= (range 1 11) lags))))

    (testing "converts severity keywords to strings"
      (let [result (acf-chart/acf->chart-data sample-acf-data)]
        (is (every? string? (map :severity result)))))

    (testing "returns nil for missing data"
      (is (nil? (acf-chart/acf->chart-data {})))
      (is (nil? (acf-chart/acf->chart-data nil))))))

(deftest acf-plot-vega-spec-test
  (testing "acf-plot-vega-spec"
    (testing "generates valid Vega-Lite spec structure"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data
                  {:width 700 :height 350})]
        (is (map? spec))
        (is (contains? spec :title))
        (is (contains? spec :layer))
        (is (contains? spec :width))
        (is (contains? spec :height))
        (is (= 700 (:width spec)))
        (is (= 350 (:height spec)))))

    (testing "includes title with sample size"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data {})]
        (is (re-find #"n=100" (:title spec)))))

    (testing "uses custom title when provided"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data
                  {:title "Custom ACF Title"})]
        (is (= "Custom ACF Title" (:title spec)))))

    (testing "creates layers for zero line, points, and thresholds"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data {})
            layers (:layer spec)]
        ;; Should have: zero line, point chart, threshold rules (variable count)
        (is (>= (count layers) 2))))

    (testing "includes period annotation when detected"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data-with-period {})
            layers (:layer spec)
            ;; Find text layer for period annotation
            text-layers (filter #(= "text" (get-in % [:mark :type])) layers)]
        ;; Should have period annotation text layer
        (is (= 1 (count text-layers)))))

    (testing "returns nil for invalid data"
      (is (nil? (acf-chart/acf-plot-vega-spec {} {})))
      (is (nil? (acf-chart/acf-plot-vega-spec nil {}))))

    (testing "uses point markers instead of bars"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data {})
            layers (:layer spec)
            point-layers (filter #(= "point" (get-in % [:mark :type])) layers)]
        (is (= 1 (count point-layers)))))

    (testing "generates threshold rule layers for crossed thresholds"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data {})
            layers (:layer spec)
            rule-layers (filter #(= "rule" (get-in % [:mark :type])) layers)]
        ;; Should have rule layers (at least zero line plus any crossed thresholds)
        (is (>= (count rule-layers) 1))))))

(deftest acf-plot-schema-validation-test
  (testing "acf-plot-vega-spec"
    (testing "produces valid Vega-Lite spec according to schema"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data
                  {:width 700 :height 350})
            result (sv/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "Expected valid Vega-Lite spec, got errors: " (:errors result)))))

    (testing "produces valid spec with period annotation"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data-with-period
                  {:width 700 :height 350})
            result (sv/validate-vega-lite-spec spec)]
        (is (:valid? result)
            (str "Expected valid Vega-Lite spec with period, got errors: "
                 (:errors result)))))))

(deftest severity-color-scale-test
  (testing "severity-color-scale"
    (testing "has matching domain and range lengths"
      (let [scale acf-chart/severity-color-scale]
        (is (= (count (:domain scale)) (count (:range scale))))))

    (testing "includes all severity levels"
      (let [domain (:domain acf-chart/severity-color-scale)]
        (is (some #(= % "none") domain))
        (is (some #(= % "minor") domain))
        (is (some #(= % "moderate") domain))
        (is (some #(= % "severe") domain))))))

(deftest has-severity-at-or-above?-test
  (testing "has-severity-at-or-above?"
    (testing "returns true when severity meets threshold"
      (is (acf-chart/has-severity-at-or-above?
           {1 :moderate, 2 :none} :moderate))
      (is (acf-chart/has-severity-at-or-above?
           {1 :severe, 2 :none} :moderate))
      (is (acf-chart/has-severity-at-or-above?
           {1 :minor, 2 :moderate} :moderate)))

    (testing "returns false when no severity meets threshold"
      (is (not (acf-chart/has-severity-at-or-above?
                {1 :none, 2 :none} :minor)))
      (is (not (acf-chart/has-severity-at-or-above?
                {1 :minor, 2 :none} :moderate)))
      (is (not (acf-chart/has-severity-at-or-above?
                {1 :moderate, 2 :minor} :severe))))

    (testing "handles alternating severities correctly"
      ;; alternating-moderate should still count as moderate level
      (is (acf-chart/has-severity-at-or-above?
           {1 :alternating-moderate, 2 :none} :moderate))
      (is (not (acf-chart/has-severity-at-or-above?
                {1 :alternating-minor, 2 :none} :moderate))))

    (testing "handles empty or nil input"
      (is (not (acf-chart/has-severity-at-or-above? {} :moderate)))
      (is (not (acf-chart/has-severity-at-or-above? nil :moderate))))))

(deftest acf-plot-min-severity-test
  (testing "acf-plot-vega-spec with :min-severity option"
    (testing "returns spec when threshold met"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data
                  {:min-severity :moderate})]
        (is (some? spec))
        (is (map? spec))))

    (testing "returns nil when threshold not met"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data-clean
                  {:min-severity :moderate})]
        (is (nil? spec))))

    (testing "returns spec with :minor threshold on clean data"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data-clean
                  {:min-severity :none})]
        (is (some? spec))))

    (testing "returns spec when no threshold specified"
      (let [spec (acf-chart/acf-plot-vega-spec
                  sample-acf-data-clean
                  {})]
        (is (some? spec))))))
