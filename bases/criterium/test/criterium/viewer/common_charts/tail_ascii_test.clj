(ns criterium.viewer.common-charts.tail-ascii-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.viewer.common-charts.tail-ascii :as tail-ascii]))

;; Tests ASCII chart rendering for tail analysis visualization.
;; Verifies chart structure, content, and edge case handling.

;;; Test Data Fixtures

(def identity-transforms
  "Identity transforms for testing."
  {:sample-> [identity]
   :->sample [identity]})

(def sample-tail-ratios
  "Sample tail ratios data for testing."
  {:tail-ratios {:p99-p95 1.5 :p999-p99 1.8 :p999-p95 2.7}
   :empirical-quantiles {:p95 10.0 :p99 15.0 :p999 27.0}})

(def sample-hill-data
  "Sample Hill estimator data for testing."
  {:hill {:k-range [3 4 5 6 7 8]
          :estimates [0.8 0.85 0.82 0.81 0.83 0.84]
          :stable-estimate 0.82}})

(def sample-mrl-data
  "Sample MRL data for testing."
  {:threshold 5.0
   :mrl {:thresholds [2.0 3.0 4.0 5.0 6.0 7.0]
         :values [4.5 5.2 6.0 7.0 8.5 10.0]}})

(def sample-gpd-fit
  "Sample GPD fit parameters."
  {:xi 0.3 :sigma 2.5})

;;; Tail Ratios Chart Tests

(deftest render-ascii-tail-ratios-test
  ;; Tests ASCII bar chart rendering for tail ratios.
  ;; Verifies chart structure and ratio display.
  (testing "render-ascii-tail-ratios"
    (testing "renders bar chart with all ratios"
      (let [output (tail-ascii/render-ascii-tail-ratios
                    sample-tail-ratios
                    {:width 60})]
        (is (string? output))
        (is (str/includes? output "p99/p95"))
        (is (str/includes? output "p999/p99"))
        (is (str/includes? output "p999/p95"))
        (is (str/includes? output "#"))  ; Bar characters
        (is (str/includes? output "1.5")))) ; Ratio value

    (testing "uses custom header function"
      (let [output (tail-ascii/render-ascii-tail-ratios
                    sample-tail-ratios
                    {:header-fn (fn [n] (str "Custom Header n=" n))})]
        (is (str/includes? output "Custom Header n=3"))))

    (testing "applies indent"
      (let [output (tail-ascii/render-ascii-tail-ratios
                    sample-tail-ratios
                    {:indent "  "})]
        (is (str/includes? output "  p99/p95"))))

    (testing "returns nil when no ratios"
      (is (nil? (tail-ascii/render-ascii-tail-ratios
                 {:tail-ratios {} :empirical-quantiles {}}
                 {}))))

    (testing "handles partial ratios"
      (let [output (tail-ascii/render-ascii-tail-ratios
                    {:tail-ratios {:p99-p95 1.5}
                     :empirical-quantiles {:p95 10.0 :p99 15.0}}
                    {})]
        (is (string? output))
        (is (str/includes? output "p99/p95"))
        (is (not (str/includes? output "p999")))))))

;;; Hill Plot Tests

(deftest render-ascii-hill-plot-test
  ;; Tests ASCII Hill plot rendering.
  ;; Verifies line chart structure and stable estimate display.
  (testing "render-ascii-hill-plot"
    (testing "renders line chart with Hill estimates"
      (let [output (tail-ascii/render-ascii-hill-plot
                    sample-hill-data
                    {:width 60 :height 12})]
        (is (string? output))
        (is (str/includes? output "|"))  ; Axis
        (is (or (str/includes? output "*")
                (str/includes? output ".")))  ; Chart chars
        (is (str/includes? output "Stable estimate"))))

    (testing "uses custom header function"
      (let [output (tail-ascii/render-ascii-hill-plot
                    sample-hill-data
                    {:header-fn (fn [n] (str "Custom Hill n=" n))})]
        (is (str/includes? output "Custom Hill n=6"))))

    (testing "returns nil with empty data"
      (is (nil? (tail-ascii/render-ascii-hill-plot
                 {:hill {:k-range [] :estimates []}}
                 {}))))

    (testing "works without stable estimate"
      (let [output (tail-ascii/render-ascii-hill-plot
                    {:hill {:k-range [3 4 5]
                            :estimates [0.8 0.85 0.82]}}
                    {:width 60})]
        (is (string? output))
        (is (not (str/includes? output "Stable estimate")))))))

;;; MRL Plot Tests

(deftest render-ascii-mrl-plot-test
  ;; Tests ASCII MRL plot rendering.
  ;; Verifies line chart structure and threshold display.
  (testing "render-ascii-mrl-plot"
    (testing "renders line chart with MRL curve"
      (let [output (tail-ascii/render-ascii-mrl-plot
                    sample-mrl-data
                    identity-transforms
                    {:width 60 :height 12})]
        (is (string? output))
        (is (str/includes? output "|"))  ; Axis
        (is (or (str/includes? output "*")
                (str/includes? output ".")))  ; Chart chars
        (is (str/includes? output "threshold"))))

    (testing "returns nil with empty thresholds"
      (is (nil? (tail-ascii/render-ascii-mrl-plot
                 {:threshold 5.0 :mrl {:thresholds [] :values []}}
                 identity-transforms
                 {}))))))

;;; Zipf Plot Tests

(deftest render-ascii-zipf-plot-test
  ;; Tests ASCII Zipf plot (log-log CCDF) rendering.
  ;; Verifies scatter plot structure.
  (testing "render-ascii-zipf-plot"
    (testing "renders scatter plot with log-transformed axes"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            output (tail-ascii/render-ascii-zipf-plot
                    samples
                    identity-transforms
                    {:width 60 :height 12})]
        (is (string? output))
        (is (str/includes? output "|"))  ; Axis
        (is (str/includes? output "*"))))  ; Points

    (testing "returns nil for empty samples"
      (is (nil? (tail-ascii/render-ascii-zipf-plot
                 (arr/->double-array (double-array []))
                 identity-transforms
                 {}))))

    (testing "returns nil for nil samples"
      (is (nil? (tail-ascii/render-ascii-zipf-plot
                 nil
                 identity-transforms
                 {}))))))

;;; Exponential Q-Q Plot Tests

(deftest render-ascii-exponential-qq-test
  ;; Tests ASCII exponential Q-Q plot rendering.
  ;; Verifies scatter plot structure.
  (testing "render-ascii-exponential-qq"
    (testing "renders scatter plot for exceedances"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            output (tail-ascii/render-ascii-exponential-qq
                    samples threshold
                    identity-transforms
                    {:width 60 :height 12})]
        (is (string? output))
        (is (str/includes? output "|"))  ; Axis
        (is (str/includes? output "*")))) ; Points

    (testing "returns nil when too few exceedances"
      (let [samples (arr/->double-array (double-array [1.0 2.0 3.0 6.0]))
            threshold 5.0]  ; Only 1 exceedance
        (is (nil? (tail-ascii/render-ascii-exponential-qq
                   samples threshold
                   identity-transforms
                   {})))))))

;;; GPD Q-Q Plot Tests

(deftest render-ascii-gpd-qq-test
  ;; Tests ASCII GPD Q-Q plot rendering.
  ;; Verifies scatter plot structure.
  (testing "render-ascii-gpd-qq"
    (testing "renders scatter plot for exceedances vs GPD quantiles"
      (let [samples (arr/->double-array
                     (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 10.0 15.0]))
            threshold 5.0
            output (tail-ascii/render-ascii-gpd-qq
                    samples threshold sample-gpd-fit
                    identity-transforms
                    {:width 60 :height 12})]
        (is (string? output))
        (is (str/includes? output "|"))  ; Axis
        (is (str/includes? output "*")))) ; Points

    (testing "returns nil when gpd-fit is nil"
      (let [samples (arr/->double-array (double-array [1.0 2.0 6.0 7.0 8.0]))]
        (is (nil? (tail-ascii/render-ascii-gpd-qq
                   samples 5.0 nil
                   identity-transforms
                   {})))))

    (testing "returns nil when sigma is zero"
      (let [samples (arr/->double-array (double-array [1.0 2.0 6.0 7.0 8.0]))]
        (is (nil? (tail-ascii/render-ascii-gpd-qq
                   samples 5.0 {:xi 0.3 :sigma 0.0}
                   identity-transforms
                   {})))))))
