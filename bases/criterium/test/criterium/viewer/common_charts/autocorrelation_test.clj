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
   :effective-sample-size {:n-original 100 :n-effective 60 :ratio 0.6}
   :ci-inflation-factor 1.29
   :ljung-box {:q-statistic 15.2 :df 10 :p-value 0.12}
   :pattern :warmup
   :classification :warning
   :detected-period nil})

(def ^:private sample-acf-data-with-period
  "Synthetic autocorrelation data with periodic pattern."
  (assoc sample-acf-data
         :acf {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.005,
               6 0.25, 7 0.02, 8 0.01, 9 0.005, 10 0.002}
         :lag-1 {:value 0.05 :severity :none}
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

    (testing "creates layers for zero line, threshold, and bars"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data {})
            layers (:layer spec)]
        ;; Should have: zero line, threshold rules, bar chart
        (is (>= (count layers) 3))))

    (testing "includes period annotation when detected"
      (let [spec (acf-chart/acf-plot-vega-spec sample-acf-data-with-period {})
            layers (:layer spec)]
        ;; Should have: zero line, threshold rules, bar chart, period annotation
        (is (= 4 (count layers)))))

    (testing "returns nil for invalid data"
      (is (nil? (acf-chart/acf-plot-vega-spec {} {})))
      (is (nil? (acf-chart/acf-plot-vega-spec nil {}))))))

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
