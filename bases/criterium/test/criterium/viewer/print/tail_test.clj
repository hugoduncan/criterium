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

;;; Chart View Tests

(deftest tail-ratios-chart-print-test
  ;; Tests ASCII tail ratios bar chart rendering.
  ;; Verifies chart structure and ratio display.
  (testing "tail-ratios-chart*"
    (testing "renders ASCII bar chart with tail ratios"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/tail-ratios-chart* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Tail Ratios") lines)
            "Should have header")
        (is (some #(str/includes? % "p99/p95") lines)
            "Should show p99/p95 ratio")
        (is (some #(str/includes? % "#") lines)
            "Should have bar characters")))

    (testing "produces no output when no data"
      (is (= "" (with-out-str (view/tail-ratios-chart* :print {} {})))))))

(deftest hill-plot-print-test
  ;; Tests ASCII Hill plot rendering.
  ;; Verifies chart structure and axis labels.
  (testing "hill-plot*"
    (testing "renders ASCII Hill plot"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/hill-plot* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Hill Plot") lines)
            "Should have header")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(or (str/includes? % "*") (str/includes? % ".")) lines)
            "Should have chart characters")
        (is (some #(str/includes? % "Stable estimate") lines)
            "Should show stable estimate")))

    (testing "produces no output when no data"
      (is (= "" (with-out-str (view/hill-plot* :print {} {})))))))

(deftest mrl-plot-print-test
  ;; Tests ASCII mean residual life plot rendering.
  ;; Verifies chart structure and threshold display.
  (testing "mrl-plot*"
    (testing "renders ASCII MRL plot"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/mrl-plot* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Mean Residual Life") lines)
            "Should have header")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(or (str/includes? % "*") (str/includes? % ".")) lines)
            "Should have chart characters")
        (is (some #(str/includes? % "threshold") lines)
            "Should show selected threshold")))

    (testing "produces no output when no data"
      (is (= "" (with-out-str (view/mrl-plot* :print {} {})))))))

(deftest zipf-plot-print-test
  ;; Tests ASCII Zipf plot (log-log CCDF) rendering.
  ;; Verifies scatter plot structure.
  (testing "zipf-plot*"
    (testing "renders ASCII Zipf plot"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/zipf-plot* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Zipf Plot") lines)
            "Should have header")
        (is (some #(str/includes? % "log") lines)
            "Should mention log scale")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(str/includes? % "*") lines)
            "Should have point characters")))

    (testing "produces no output when no samples"
      (let [data-map (test-data/tail-analysis-data-map)
            no-samples (assoc data-map :samples nil)]
        (is (= "" (with-out-str (view/zipf-plot* :print {} no-samples))))))))

(deftest exponential-qq-plot-print-test
  ;; Tests ASCII exponential Q-Q plot rendering.
  ;; Verifies scatter plot structure.
  (testing "exponential-qq-plot*"
    (testing "renders ASCII exponential Q-Q plot"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/exponential-qq-plot* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Exponential Q-Q") lines)
            "Should have header")
        (is (some #(str/includes? % "exceedances") lines)
            "Should show exceedance count")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(str/includes? % "*") lines)
            "Should have point characters")))

    (testing "produces no output when no data"
      (is (= "" (with-out-str (view/exponential-qq-plot* :print {} {})))))))

(deftest gpd-qq-plot-print-test
  ;; Tests ASCII GPD Q-Q plot rendering.
  ;; Verifies scatter plot structure.
  (testing "gpd-qq-plot*"
    (testing "renders ASCII GPD Q-Q plot"
      (let [data-map (test-data/tail-analysis-data-map)
            output (with-out-str (view/gpd-qq-plot* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "GPD Q-Q") lines)
            "Should have header")
        (is (some #(str/includes? % "exceedances") lines)
            "Should show exceedance count")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(str/includes? % "*") lines)
            "Should have point characters")))

    (testing "produces no output when no GPD fit"
      (let [data-map (test-data/tail-analysis-data-map)
            no-gpd (update-in data-map [:tail-analysis :tail-analysis [:elapsed-time]]
                              dissoc :gpd)]
        (is (= "" (with-out-str (view/gpd-qq-plot* :print {} no-gpd))))))))
