(ns criterium.viewer.print.autocorrelation-test
  ;; Tests the print viewer output for autocorrelation analysis results.
  ;; Verifies lag-1 display, effective sample size, CI inflation, Ljung-Box,
  ;; assessment, pattern, recommendations, and anomalous lags formatting.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.autocorrelation :as print.acf]))

(deftest print-autocorrelation-test
  ;; Tests print-autocorrelation function for autocorrelation analysis display.
  ;; Covers: lag-1 with severity, effective sample size, CI inflation,
  ;; Ljung-Box p-value, assessment, pattern, and recommendations.
  (testing "print-autocorrelation"
    (testing "displays basic statistics for :pass classification"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.05 :severity :none}
                       :effective-sample-size {:n-original 200
                                               :n-effective 190
                                               :ratio 0.95}
                       :ci-inflation-factor 1.05
                       :ljung-box {:q-statistic 15.2 :df 20 :p-value 0.75}
                       :pattern :clean
                       :classification :pass
                       :detected-period nil}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (= "Sample Independence:" (first lines)))
        (is (some #(str/includes? % "Lag-1 autocorrelation") lines))
        (is (some #(str/includes? % "0.05") lines))
        (is (some #(str/includes? % "(none)") lines))
        (is (some #(str/includes? % "Effective sample size") lines))
        (is (some #(str/includes? % "190 of 200 (95%)") lines))
        (is (some #(str/includes? % "CI inflation factor") lines))
        (is (some #(str/includes? % "1.05×") lines))
        (is (some #(str/includes? % "Ljung-Box p-value") lines))
        (is (some #(str/includes? % "0.75") lines))
        (is (some #(str/includes? % "Assessment") lines))
        (is (some #(str/includes? % "Pass") lines))
        (is (not (some #(str/includes? % "Pattern") lines))
            "Should not show pattern for pass classification")
        (is (not (some #(str/includes? % "Recommendation") lines))
            "Should not show recommendation for pass classification")))

    (testing "displays pattern and recommendation for :warning"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.25 :severity :moderate}
                       :effective-sample-size {:n-original 200
                                               :n-effective 120
                                               :ratio 0.60}
                       :ci-inflation-factor 1.67
                       :ljung-box {:q-statistic 42.5 :df 20 :p-value 0.008}
                       :pattern :transient-effects
                       :classification :warning
                       :detected-period nil}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "moderate") lines)
            "Should show severity as moderate")
        (is (some #(str/includes? % "Warning") lines))
        (is (some #(str/includes? % "Pattern") lines))
        (is (some #(str/includes? % "Transient effects") lines))
        (is (some #(str/includes? % "Recommendation") lines))
        (is (some #(str/includes? % "Check:") lines))))

    (testing "displays pattern and period for :periodic"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.08 :severity :none}
                       :effective-sample-size {:n-original 200
                                               :n-effective 185
                                               :ratio 0.925}
                       :ci-inflation-factor 1.09
                       :ljung-box {:q-statistic 35.0 :df 20 :p-value 0.02}
                       :pattern :periodic
                       :classification :warning
                       :detected-period 47}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Periodic structure detected") lines))
        (is (some #(str/includes? % "Suspected period") lines))
        (is (some #(str/includes? % "47 samples") lines))
        (is (some #(str/includes? % "GC logs") lines)
            "Should recommend investigating GC")))

    (testing "displays severe assessment for :fail"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.45 :severity :severe}
                       :effective-sample-size {:n-original 200
                                               :n-effective 50
                                               :ratio 0.25}
                       :ci-inflation-factor 5.0
                       :ljung-box {:q-statistic 85.0 :df 20 :p-value 0.0001}
                       :pattern :severe
                       :classification :fail
                       :detected-period nil}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "severe") lines))
        (is (some #(str/includes? % "Fail") lines))
        (is (some #(str/includes? % "Severe autocorrelation") lines))
        (is (some #(str/includes? % "unreliable") lines)
            "Should warn results are unreliable")))))

(deftest autocorrelation-view-test
  ;; Tests autocorrelation* print viewer multimethod with data-map.
  ;; Covers: basic view rendering, custom autocorrelation-id, missing data.
  (testing "autocorrelation*"
    (testing "renders autocorrelation summary via view"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.12 2 0.08 3 0.05}
                         :lag-1 {:value 0.12 :severity :minor}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}
                         :ci-inflation-factor 1.13
                         :ljung-box {:q-statistic 18.5 :df 20 :p-value 0.55}
                         :pattern :clean
                         :classification :acceptable
                         :detected-period nil}}}}
            output (with-out-str
                     (view/autocorrelation* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Sample Independence") lines))
        (is (some #(str/includes? % "Elapsed Time") lines))
        (is (some #(str/includes? % "0.12") lines))
        (is (some #(str/includes? % "minor") lines))
        (is (some #(str/includes? % "157 of 200") lines))
        (is (some #(str/includes? % "Acceptable") lines))))

    (testing "uses custom autocorrelation-id"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:my-acf
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.05}
                         :lag-1 {:value 0.05 :severity :none}
                         :effective-sample-size {:n-original 100
                                                 :n-effective 95
                                                 :ratio 0.95}
                         :ci-inflation-factor 1.05
                         :ljung-box {:q-statistic 10.0 :df 10 :p-value 0.8}
                         :pattern :clean
                         :classification :pass
                         :detected-period nil}}}}
            output (with-out-str
                     (view/autocorrelation*
                      :print
                      {:autocorrelation-id :my-acf}
                      data-map))]
        (is (str/includes? output "Sample Independence"))))

    (testing "handles missing autocorrelation data gracefully"
      (let [output (with-out-str
                     (view/autocorrelation* :print {} {}))]
        (is (str/blank? output))))))

(deftest acf-plot-print-test
  ;; Tests ASCII ACF plot rendering for print viewer.
  ;; Verifies output format, severity filtering, bar rendering, and threshold legend.
  (testing "acf-plot*"
    (testing "renders ACF plot when severity threshold met"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.25 2 0.18 3 0.05 12 -0.22}
                         :lag-1 {:value 0.25 :severity :moderate}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}
                         :ci-inflation-factor 1.13
                         :lag-severities {1 :moderate 2 :minor 3 :none 12 :minor}
                         :thresholds {:lag-1 {:minor 0.10 :moderate 0.20 :severe 0.35}
                                      :other {:minor 0.15 :moderate 0.25 :severe 0.40}}
                         :ljung-box {:q-statistic 15.0 :df 10 :p-value 0.5}
                         :pattern :clean
                         :classification :acceptable
                         :detected-period nil}}}}
            output (with-out-str
                     (view/acf-plot* :print {:min-severity :minor} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "ACF Plot") lines)
            "Should show ACF Plot header")
        (is (some #(str/includes? % "n=200") lines)
            "Should show sample count")
        (is (some #(str/includes? % "Lag") lines)
            "Should show Lag column header")
        (is (some #(str/includes? % "ACF") lines)
            "Should show ACF column header")
        (is (some #(str/includes? % "0.25") lines)
            "Should show lag-1 ACF value")
        (is (some #(str/includes? % "(moderate)") lines)
            "Should show severity annotation")
        (is (some #(str/includes? % "|") lines)
            "Should have bar center markers")
        (is (some #(str/includes? % "█") lines)
            "Should have bar characters")))

    (testing "filters lags by min-severity"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.25 2 0.05}
                         :lag-1 {:value 0.25 :severity :moderate}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}
                         :lag-severities {1 :moderate 2 :none}
                         :thresholds {:lag-1 {:minor 0.10 :moderate 0.20 :severe 0.35}
                                      :other {:minor 0.15 :moderate 0.25 :severe 0.40}}}}}}
            ;; With min-severity :moderate, only lag 1 should appear
            output (with-out-str
                     (view/acf-plot* :print {:min-severity :moderate} data-map))
            lines (trimmed-lines output)]
        (is (some #(and (str/includes? % "1") (str/includes? % "0.25")) lines)
            "Should show lag 1")
        ;; Lag 2 with :none severity should not appear
        (is (not (some #(str/includes? % "0.05") lines))
            "Should not show lag 2 with :none severity")))

    (testing "does not render when no lags meet severity threshold"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.05 2 0.03}
                         :lag-1 {:value 0.05 :severity :none}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 190
                                                 :ratio 0.95}
                         :lag-severities {1 :none 2 :none}
                         :thresholds {:lag-1 {:minor 0.10 :moderate 0.20 :severe 0.35}
                                      :other {:minor 0.15 :moderate 0.25 :severe 0.40}}}}}}
            ;; With default min-severity :moderate, should not render
            output (with-out-str
                     (view/acf-plot* :print {} data-map))]
        (is (str/blank? output)
            "Should not render when no lags meet severity threshold")))

    (testing "shows threshold legend with crossed thresholds"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.25 12 0.20}
                         :lag-1 {:value 0.25 :severity :moderate}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}
                         :lag-severities {1 :moderate 12 :minor}
                         :thresholds {:lag-1 {:minor 0.10 :moderate 0.20 :severe 0.35}
                                      :other {:minor 0.15 :moderate 0.25 :severe 0.40}}}}}}
            output (with-out-str
                     (view/acf-plot* :print {:min-severity :minor} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Thresholds:") lines)
            "Should show threshold legend")
        (is (some #(str/includes? % "lag-1") lines)
            "Should mention lag-1 thresholds")
        (is (some #(str/includes? % "0.10") lines)
            "Should show crossed minor threshold value")))

    (testing "handles missing autocorrelation data gracefully"
      (let [output (with-out-str
                     (view/acf-plot* :print {} {}))]
        (is (str/blank? output))))))

(deftest print-autocorrelation-anomalous-lags-test
  ;; Tests display of anomalous lags in autocorrelation output.
  ;; Covers: anomalous lags display when present, no display when empty.
  (testing "print-autocorrelation"
    (testing "displays anomalous lags when present"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.15 :severity :minor}
                       :effective-sample-size {:n-original 200
                                               :n-effective 160
                                               :ratio 0.80}
                       :ci-inflation-factor 1.18
                       :ljung-box {:q-statistic 25.0 :df 20 :p-value 0.20}
                       :pattern :clean
                       :classification :acceptable
                       :detected-period nil
                       :anomalous-lags [1 12]
                       :lag-severities {1 :minor 12 :moderate}}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Anomalous lags") lines)
            "Should display anomalous lags label")
        (is (some #(str/includes? % "1 (minor)") lines)
            "Should show lag 1 with minor severity")
        (is (some #(str/includes? % "12 (moderate)") lines)
            "Should show lag 12 with moderate severity")))

    (testing "does not display anomalous lags when empty"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.05 :severity :none}
                       :effective-sample-size {:n-original 200
                                               :n-effective 190
                                               :ratio 0.95}
                       :ci-inflation-factor 1.05
                       :ljung-box {:q-statistic 15.2 :df 20 :p-value 0.75}
                       :pattern :clean
                       :classification :pass
                       :detected-period nil
                       :anomalous-lags []
                       :lag-severities {1 :none 2 :none}}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (not (some #(str/includes? % "Anomalous lags") lines))
            "Should not display anomalous lags when empty")))

    (testing "does not display anomalous lags when nil"
      (let [output (with-out-str
                     (print.acf/print-autocorrelation
                      {:lag-1 {:value 0.05 :severity :none}
                       :effective-sample-size {:n-original 200
                                               :n-effective 190
                                               :ratio 0.95}
                       :ci-inflation-factor 1.05
                       :ljung-box {:q-statistic 15.2 :df 20 :p-value 0.75}
                       :pattern :clean
                       :classification :pass
                       :detected-period nil}
                      "Elapsed Time"))
            lines (trimmed-lines output)]
        (is (not (some #(str/includes? % "Anomalous lags") lines))
            "Should not display anomalous lags when not present")))))
