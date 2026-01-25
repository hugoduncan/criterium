(ns criterium.viewer.print.autocorrelation
  "Print viewer for autocorrelation analysis.

  Provides views for lag analysis, effective sample size,
  CI inflation factors, pattern classification, and ACF plots."
  (:require
   [clojure.string :as str]
   [criterium.view :as view]
   [criterium.viewer.common-charts.autocorrelation :as acf-charts]
   [criterium.viewer.common.autocorrelation :as acf-common]
   [criterium.viewer.common.core :as core]
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

(defn- format-threshold-legend
  "Format threshold legend showing only crossed thresholds.
  Returns string like 'lag-1 [minor 0.10, moderate 0.20], other [minor 0.15]'."
  [acf-map thresholds]
  (let [lag-1-acf (Math/abs (double (get acf-map 1 0)))
        other-max-acf (if (> (count acf-map) 1)
                        (apply max 0 (map #(Math/abs (double %))
                                          (vals (dissoc acf-map 1))))
                        0)
        lag-1-thresholds (:lag-1 thresholds)
        other-thresholds (:other thresholds)
        ;; Find crossed thresholds for lag-1
        lag-1-crossed (for [[level ^double threshold] (sort-by val lag-1-thresholds)
                            :when (> lag-1-acf threshold)]
                        (format "%s %.2f" (name level) threshold))
        ;; Find crossed thresholds for other lags
        other-crossed (for [[level ^double threshold] (sort-by val other-thresholds)
                            :when (> other-max-acf threshold)]
                        (format "%s %.2f" (name level) threshold))]
    (cond-> []
      (seq lag-1-crossed)
      (conj (str "lag-1 [" (str/join ", " lag-1-crossed) "]"))
      (seq other-crossed)
      (conj (str "other [" (str/join ", " other-crossed) "]")))))

(defn print-acf-plot
  "Print ASCII ACF plot for a single metric.

  Shows autocorrelation coefficients as bidirectional ASCII bar charts.
  Only renders if at least one lag meets the min-severity threshold.

  Options:
    :min-severity - minimum severity to display (default :moderate)
    :bar-width    - half-width of bar in characters (default 15)"
  [{:keys [acf effective-sample-size lag-severities thresholds]} metric-label opts]
  (let [{:keys [min-severity bar-width]
         :or {min-severity :moderate bar-width 15}} opts
        n (get effective-sample-size :n-original)
        ;; Default thresholds if not provided
        thresholds (or thresholds
                       {:lag-1 {:minor 0.10 :moderate 0.20 :severe 0.35}
                        :other {:minor 0.15 :moderate 0.25 :severe 0.40}})]
    (when (and acf n lag-severities
               (acf-charts/has-severity-at-or-above? lag-severities min-severity))
      ;; Filter lags by severity
      (let [min-rank (get acf-charts/severity-rank min-severity 0)
            qualifying-lags (for [[lag sev] lag-severities
                                  :when (>= (long (get acf-charts/severity-rank sev 0))
                                            (long min-rank))]
                              lag)
            ;; Calculate max |acf| for scaling
            max-abs-acf (apply max 0.01 (map #(Math/abs (double (get acf % 0)))
                                             qualifying-lags))
            ;; Sort lags numerically
            sorted-lags (sort qualifying-lags)
            ;; Calculate column widths
            max-lag-width (max 3 (count (str (apply max 1 sorted-lags))))
            ;; Print header
            indent "  "
            ;; Build format strings with dynamic width
            header-fmt (str "%s%" max-lag-width "s   %s")
            row-fmt (str "%s%" max-lag-width "d  %6.2f  %s (%s)")]
        (println (format "%s ACF Plot (n=%d):" (print-core/format-sublabel metric-label) n))
        (println (format header-fmt indent "Lag" "ACF"))
        ;; Print each lag
        (doseq [lag sorted-lags]
          (let [acf-val (double (get acf lag))
                severity (get lag-severities lag :none)
                bar (core/ascii-bar-bidirectional acf-val max-abs-acf bar-width)]
            (println (format row-fmt
                             indent
                             lag
                             acf-val
                             bar
                             (acf-common/format-severity severity)))))
        ;; Print threshold legend
        (let [legend-parts (format-threshold-legend acf thresholds)]
          (when (seq legend-parts)
            (println)
            (println (format "%sThresholds: %s" indent (str/join ", " legend-parts)))))))))

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
