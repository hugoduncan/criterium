(ns criterium.viewer.print.autocorrelation
  "Print viewer for autocorrelation analysis.

  Provides views for lag analysis, effective sample size,
  CI inflation factors, pattern classification, and ACF plots."
  (:require
   [clojure.string :as str]
   [criterium.view :as view]
   [criterium.viewer.common-charts.autocorrelation :as acf-charts]
   [criterium.viewer.common.autocorrelation :as acf-common]
   [criterium.viewer.print.core :as print-core]))

(set! *unchecked-math* false)

(defn print-autocorrelation
  "Print autocorrelation analysis summary for a metric.

  Output format:
  Sample Independence:
                      Lag-1 autocorrelation: 0.12 (minor)
                    Effective sample size: 157 of 200 (78%)
                       CI inflation factor: 1.13×
                        Ljung-Box p-value: 0.08
                           Assessment: Acceptable

  For warning/fail, adds pattern and recommendation lines."
  [{:keys [acf lag-1 effective-sample-size ci-inflation-factor
           ljung-box pattern classification detected-period
           anomalous-lags lag-severities]}
   metric-label]
  (println (print-core/format-sublabel "Sample Independence"))
  (println (format "%s %.2f (%s)"
                   (print-core/format-sublabel (str metric-label " Lag-1 autocorrelation"))
                   (:value lag-1)
                   (acf-common/format-severity (:severity lag-1))))
  (println (format "%s %d of %d (%.0f%%)"
                   (print-core/format-sublabel "Effective sample size")
                   (:n-effective effective-sample-size)
                   (:n-original effective-sample-size)
                   (* 100.0 (:ratio effective-sample-size))))
  (println (format "%s %.2f×"
                   (print-core/format-sublabel "CI inflation factor")
                   ci-inflation-factor))
  (println (format "%s %.2f"
                   (print-core/format-sublabel "Ljung-Box p-value")
                   (:p-value ljung-box)))
  (println (format "%s %s"
                   (print-core/format-sublabel "Assessment")
                   (str/capitalize (name classification))))
  (when-let [formatted (acf-common/format-anomalous-lags anomalous-lags lag-severities)]
    (println (format "%s %s"
                     (print-core/format-sublabel "Anomalous lags")
                     formatted)))
  (when (and (#{:warning :fail} classification)
             (acf-common/displayable-patterns pattern))
    (println (format "%s %s"
                     (print-core/format-sublabel "Pattern")
                     (get acf-common/pattern-labels pattern (name pattern))))
    (when-let [period-str (acf-common/format-detected-period
                           detected-period acf lag-severities)]
      (println (format "%s %s"
                       (print-core/format-sublabel "Suspected period")
                       period-str)))
    (when-let [rec (get acf-common/pattern-recommendations pattern)]
      (println (format "%s %s"
                       (print-core/format-sublabel "Recommendation")
                       rec)))))

(defn print-autocorrelations
  "Print autocorrelation analysis for all metrics."
  [view data-map]
  (acf-common/with-autocorrelation-metrics view data-map
    (fn [acf-data mc]
      (print-autocorrelation acf-data (:label mc)))))

(defmethod view/autocorrelation* :print
  [_ view data-map]
  (print-autocorrelations view data-map))

;;; ACF Plot

(defn print-acf-plot
  "Print ASCII ACF plot for a single metric.

  Uses the common render-ascii-acf-plot function with print-viewer formatting."
  [acf-data metric-label opts]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [label n]
                    (format "%s ACF Plot (n=%d):"
                            (print-core/format-sublabel label) n))
        render-opts (assoc opts
                           :header-fn header-fn
                           :indent indent)]
    (when-let [output (acf-charts/render-ascii-acf-plot acf-data metric-label render-opts)]
      (println output))))

(defn print-acf-plots
  "Print ACF plots for all metrics."
  [view data-map]
  (acf-common/with-autocorrelation-metrics view data-map
    (fn [acf-data mc]
      (print-acf-plot acf-data (:label mc) view))))

(defmethod view/acf-plot* :print
  [_ view data-map]
  (print-acf-plots view data-map))

(defn- print-classification-for-metric
  "Print classification analysis for a single metric."
  [{:keys [acf lag-1 lag-severities ljung-box pattern classification detected-period]}
   metric-label]
  (println)
  (println (print-core/format-sublabel metric-label)
           "Sample Independence Classification")
  (println (print-core/format-sublabel "Lag-1 autocorrelation")
           (format
            "%.2f (%s)"
            (:value lag-1)
            (acf-common/format-severity (:severity lag-1))))
  (println (print-core/format-sublabel "Ljung-Box p-value")
           (format "%.2f" (:p-value ljung-box)))
  (println (print-core/format-sublabel "Assessment")
           (str/capitalize (name classification)))
  (when (and (#{:warning :fail} classification)
             (acf-common/displayable-patterns pattern))
    (println (print-core/format-sublabel "Pattern")
             (get acf-common/pattern-labels pattern (name pattern)))
    (when-let [period-str (acf-common/format-detected-period
                           detected-period acf lag-severities)]
      (println (print-core/format-sublabel "Suspected period")
               period-str))
    (when-let [rec (get acf-common/pattern-recommendations pattern)]
      (println (print-core/format-sublabel "Recommendation")
               rec))))

(defn print-autocorrelation-classifications
  "Print autocorrelation classification for all metrics.
  Only outputs if at least one metric is not classified as :pass."
  [view data-map]
  (let [metrics (acf-common/collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (doseq [[class-data acf-data mc] metrics]
        (let [combined (merge class-data
                              (select-keys acf-data [:lag-1 :acf :lag-severities]))]
          (print-classification-for-metric combined (:label mc)))))))

(defmethod view/autocorrelation-classification* :print
  [_ view data-map]
  (print-autocorrelation-classifications view data-map))

(defn- print-effective-sample-size-for-metric
  "Print effective sample size analysis for a single metric."
  [{:keys [lag-1 effective-sample-size ci-inflation-factor]} metric-label]
  (println)
  (println (print-core/format-sublabel metric-label) "Effective Sample Size")
  (when lag-1
    (println (print-core/format-sublabel "Lag-1 autocorrelation (no outliers)")
             (format "%.2f (%s)"
                     (:value lag-1)
                     (acf-common/format-severity (:severity lag-1)))))
  (println (print-core/format-sublabel "Effective sample size")
           (format "%d of %d (%.0f%%)"
                   (:n-effective effective-sample-size)
                   (:n-original effective-sample-size)
                   (* 100.0 (double (:ratio effective-sample-size)))))
  (println (print-core/format-sublabel "CI inflation factor")
           (format "%.2f×" ci-inflation-factor)))

(defn print-effective-sample-sizes
  "Print effective sample size analysis for all metrics.
  Only outputs if at least one metric has CI inflation factor other than 1.0."
  [view data-map]
  (let [metrics (acf-common/collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (doseq [[ess-data acf-data mc] metrics]
        (let [combined (merge ess-data (select-keys acf-data [:lag-1]))]
          (print-effective-sample-size-for-metric combined (:label mc)))))))

(defmethod view/effective-sample-size* :print
  [_ view data-map]
  (print-effective-sample-sizes view data-map))
