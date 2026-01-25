(ns criterium.viewer.print.tail-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.tail]))

;; Tests multimethod registration, data processing, and output formatting
;; for tail analysis views.
;;
;; Tail analysis includes GPD/Hill parameter estimation, tail ratios
;; (p99/p95, p999/p99), and high quantile extrapolation.

(deftest tail-summary-print-test
  ;; Tests the print viewer output for tail summary (GPD/Hill parameters).
  (testing "tail-summary*"
    (testing "prints GPD and Hill parameters"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-summary* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Tail Summary:") lines))
        (is (some #(str/includes? % "Threshold") lines))
        (is (some #(str/includes? % "GPD") lines))))

    (testing "handles missing tail analysis gracefully"
      (let [output (with-out-str (view/tail-summary* :print {} {}))]
        (is (str/blank? output))))

    (testing "uses custom tail-analysis-id"
      (let [data-map (test-data/tail-analysis-data-map)
            custom-map {:my-tail (:tail-analysis data-map)}
            output (with-out-str
                     (view/tail-summary* :print
                                         {:tail-analysis-id :my-tail}
                                         custom-map))]
        (is (str/includes? output "Tail Summary:"))))))

(deftest tail-ratios-print-test
  ;; Tests the print viewer output for tail ratios.
  (testing "tail-ratios*"
    (testing "prints tail ratios with percentile values"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-ratios* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Tail Ratios:") lines))
        (is (some #(str/includes? % "tail ratios") lines))
        (is (some #(str/includes? % "p99/p95") lines))
        (is (some #(str/includes? % "p999/p99") lines))))

    (testing "handles missing tail analysis gracefully"
      (let [output (with-out-str (view/tail-ratios* :print {} {}))]
        (is (str/blank? output))))))

(deftest tail-high-quantiles-print-test
  ;; Tests the print viewer output for high quantile estimates.
  (testing "tail-high-quantiles*"
    (testing "prints high quantile estimates"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-high-quantiles* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "High Quantile Estimates") lines))
        (is (some #(str/includes? % "p99") lines) "Should show p99 estimate")
        (is (some #(str/includes? % "p99.9") lines) "Should show p99.9 estimate")))

    (testing "handles missing tail analysis gracefully"
      (let [output (with-out-str (view/tail-high-quantiles* :print {} {}))]
        (is (str/blank? output))))))

(deftest tail-chart-views-print-test
  ;; Tests that chart views are no-ops for print viewer.
  (testing "chart views are no-ops for print"
    (let [data-map (test-data/tail-analysis-data-map)]
      (is (nil? (view/tail-ratios-chart* :print {} data-map)))
      (is (nil? (view/hill-plot* :print {} data-map)))
      (is (nil? (view/mrl-plot* :print {} data-map)))
      (is (nil? (view/zipf-plot* :print {} data-map)))
      (is (nil? (view/exponential-qq-plot* :print {} data-map)))
      (is (nil? (view/gpd-qq-plot* :print {} data-map))))))
