(ns criterium.viewer.print
  "A print viewer

  Core functionality (metrics, stats, extremes, bootstrap, samples, outliers,
  events, GC, OS, runtime) is in criterium.viewer.print.core.

  Domain analysis (grouped, extract, comparison, regression, apply) is in
  criterium.viewer.print.domain.

  Allocation profiling (summary, hotspots, by-type, treemap) is in
  criterium.viewer.print.allocation.

  Distribution fit (models, parameter CIs) is in
  criterium.viewer.print.distribution.

  Tail analysis (summary, ratios, high quantiles) is in
  criterium.viewer.print.tail."
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common.autocorrelation :as acf-common]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.shape :as shape]
   [criterium.viewer.print.allocation]
   [criterium.viewer.print.core :as print.core]
   [criterium.viewer.print.distribution]
   [criterium.viewer.print.domain]
   [criterium.viewer.print.tail]))

;;; Re-exported functions from print.core for backwards compatibility
(def print-stat
  "Print a single metric's stats. Delegates to criterium.viewer.print.core."
  print.core/print-stat)

(def print-extreme
  "Print a single metric's min/max values. Delegates to criterium.viewer.print.core."
  print.core/print-extreme)

(def print-bootstrap-stat
  "Print bootstrap statistics for a metric. Delegates to criterium.viewer.print.core."
  print.core/print-bootstrap-stat)

(def print-outlier-count
  "Print outlier counts for a metric. Delegates to criterium.viewer.print.core."
  print.core/print-outlier-count)

(set! *unchecked-math* false)

;;; Shape Statistics Views

(defn- format-skewness-class
  "Format skewness classification for display."
  [classification]
  (case classification
    :highly-left-skewed "highly left-skewed"
    :moderately-left-skewed "moderately left-skewed"
    :slightly-left-skewed "slightly left-skewed"
    :symmetric "symmetric"
    :slightly-right-skewed "slightly right-skewed"
    :moderately-right-skewed "moderately right-skewed"
    :highly-right-skewed "highly right-skewed"
    (name classification)))

(defn- format-kurtosis-class
  "Format kurtosis classification for display."
  [classification]
  (case classification
    :heavy-tails "heavy tails (leptokurtic)"
    :light-tails "light tails (platykurtic)"
    :normal-tails "normal tails (mesokurtic)"
    (name classification)))

(defn- format-cv-class
  "Format CV classification for display."
  [classification]
  (case classification
    :low-variability "low variability"
    :moderate-variability "moderate variability"
    :high-variability "high variability"
    (name classification)))

(defn print-shape-stats
  "Print shape statistics (skewness, kurtosis, CV) for bootstrap results."
  [{:keys [bootstrap-stats-id] :as _view} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (-> (:metrics-defs bootstrap-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            shape-data (shape/shape-stats-data metric-configs bootstrap)]
        (when (seq shape-data)
          (println "Shape Statistics:")
          (doseq [{:keys [metric skewness skewness-class
                          kurtosis kurtosis-class
                          cv cv-class]} shape-data]
            (println
             (format "%32s: skewness %s (%s)"
                     metric skewness (format-skewness-class skewness-class)))
            (println
             (format "%32s  kurtosis %s (%s)"
                     "" kurtosis (format-kurtosis-class kurtosis-class)))
            (println
             (format "%32s  CV %s (%s)"
                     "" cv (format-cv-class cv-class)))))))))

(defmethod view/shape-stats* :print
  [_ view data-map]
  (print-shape-stats view data-map))

;;; Modal Analysis Views

(defmethod view/multimodal-warning* :print
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config modes transforms]}]
     (println
      (format "%32s: Multimodal distribution detected"
              (:label metric-config)))
     (when (seq modes)
       (let [locations (map #(modal/format-mode-location
                              (:location %)
                              metric-config
                              transforms)
                            modes)]
         (println (format "%32s  Mode locations: %s" ""
                          (str/join ", " locations))))))))

;;; Autocorrelation Views

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
  (println (format "%36s:" "Sample Independence"))
  (println (format "%36s: %.2f (%s)"
                   (str metric-label " Lag-1 autocorrelation")
                   (:value lag-1)
                   (acf-common/format-severity (:severity lag-1))))
  (println (format "%36s: %d of %d (%.0f%%)"
                   "Effective sample size"
                   (:n-effective effective-sample-size)
                   (:n-original effective-sample-size)
                   (* 100.0 (:ratio effective-sample-size))))
  (println (format "%36s: %.2f×"
                   "CI inflation factor"
                   ci-inflation-factor))
  (println (format "%36s: %.2f"
                   "Ljung-Box p-value"
                   (:p-value ljung-box)))
  (println (format "%36s: %s"
                   "Assessment"
                   (str/capitalize (name classification))))
  (when-let [formatted (acf-common/format-anomalous-lags anomalous-lags lag-severities)]
    (println (format "%36s: %s"
                     "Anomalous lags"
                     formatted)))
  (when (and (#{:warning :fail} classification)
             (acf-common/displayable-patterns pattern))
    (println (format "%36s: %s"
                     "Pattern"
                     (get acf-common/pattern-labels pattern (name pattern))))
    (when-let [period-str (acf-common/format-detected-period
                           detected-period acf lag-severities)]
      (println (format "%36s: %s"
                       "Suspected period"
                       period-str)))
    (when-let [rec (get acf-common/pattern-recommendations pattern)]
      (println (format "%36s: %s"
                       "Recommendation"
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

;; ACF plot is a no-op for print viewer (charts not supported)
(defmethod view/acf-plot* :print [_ _ _])

(defn- print-classification-for-metric
  "Print classification analysis for a single metric."
  [{:keys [acf lag-1 lag-severities ljung-box pattern classification detected-period]}
   metric-label]
  (println (format "%36s:" "Sample Independence Classification"))
  (println (format "%36s: %.2f (%s)"
                   (str metric-label " Lag-1 autocorrelation")
                   (:value lag-1)
                   (acf-common/format-severity (:severity lag-1))))
  (println (format "%36s: %.2f"
                   (str metric-label " Ljung-Box p-value")
                   (:p-value ljung-box)))
  (println (format "%36s: %s"
                   "Assessment"
                   (str/capitalize (name classification))))
  (when (and (#{:warning :fail} classification)
             (acf-common/displayable-patterns pattern))
    (println (format "%36s: %s"
                     "Pattern"
                     (get acf-common/pattern-labels pattern (name pattern))))
    (when-let [period-str (acf-common/format-detected-period
                           detected-period acf lag-severities)]
      (println (format "%36s: %s"
                       "Suspected period"
                       period-str)))
    (when-let [rec (get acf-common/pattern-recommendations pattern)]
      (println (format "%36s: %s"
                       "Recommendation"
                       rec)))))

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
  (println (format "%36s:" "Effective Sample Size"))
  (when lag-1
    (println (format "%36s: %.2f (%s)"
                     (str metric-label " Lag-1 autocorrelation")
                     (:value lag-1)
                     (acf-common/format-severity (:severity lag-1)))))
  (println (format "%36s: %d of %d (%.0f%%)"
                   "Effective sample size"
                   (:n-effective effective-sample-size)
                   (:n-original effective-sample-size)
                   (* 100.0 (:ratio effective-sample-size))))
  (println (format "%36s: %.2f×"
                   "CI inflation factor"
                   ci-inflation-factor)))

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

