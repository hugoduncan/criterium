(ns criterium.viewer.common.autocorrelation
  "Autocorrelation view helpers.

  Provides functions for formatting autocorrelation data and iterating over
  metrics for display. Used by print and pprint viewers."
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]))

;;; Label Maps

(def severity-labels
  "Human-readable labels for severity keywords."
  {:none "none"
   :minor "minor"
   :moderate "moderate"
   :severe "severe"
   :alternating-none "alternating"
   :alternating-minor "alternating"
   :alternating-moderate "alternating"
   :alternating-severe "alternating"})

(def pattern-labels
  "Human-readable labels for pattern keywords."
  {:clean "Clean"
   :transient-effects "Transient effects"
   :drift "Drift detected"
   :periodic "Periodic structure detected"
   :severe "Severe autocorrelation"
   :alternating-moderate "Alternating pattern—investigate methodology"
   :alternating-severe "Alternating pattern—investigate methodology"})

(def displayable-patterns
  "Patterns that warrant display of pattern label and recommendations.
  Excludes :clean, :alternating-none, and :alternating-minor."
  #{:transient-effects :drift :periodic :severe
    :alternating-moderate :alternating-severe})

(def pattern-recommendations
  "Recommendations for each displayable pattern."
  {:transient-effects
   "Check: warmup iterations, system load, thermal throttling"
   :drift                "Shorter benchmark duration; check thermal throttling"
   :periodic             "Investigate GC logs; increase heap"
   :severe               "Review methodology; results unreliable"
   :alternating-moderate "Review methodology; results unreliable"
   :alternating-severe   "Review methodology; results unreliable"})

;;; Formatting Functions

(defn format-severity
  "Format a severity keyword for display."
  [severity]
  (get severity-labels severity (name severity)))

(defn format-anomalous-lags
  "Format anomalous lags for display.
  Takes a vector of lag numbers and a map of lag -> severity.
  Returns a string like '1 (minor), 12 (moderate)' or nil if empty."
  [anomalous-lags lag-severities]
  (when (seq anomalous-lags)
    (str/join ", "
              (map (fn [lag]
                     (format "%d (%s)"
                             lag
                             (format-severity (get lag-severities lag :none))))
                   anomalous-lags))))

(defn format-detected-period
  "Format detected period with ACF value and severity.
  Returns a string like '60 samples (r=0.35, moderate)' or nil if period
  is nil."
  [detected-period acf-map lag-severities]
  (when detected-period
    (let [acf-val (get acf-map detected-period)
          severity (get lag-severities detected-period :none)]
      (if acf-val
        (format "%d samples (r=%.2f, %s)"
                detected-period
                (double acf-val)
                (format-severity severity))
        (format "%d samples" detected-period)))))

;;; Data Collection Functions

(defn with-autocorrelation-metrics
  "Helper for autocorrelation views. Calls f for each metric with acf-data.
  f receives acf-data and metric-config."
  [{:keys [autocorrelation-id]} data-map f]
  (let [autocorrelation-id (or autocorrelation-id :autocorrelation)
        autocorr-map (data-map autocorrelation-id)]
    (when autocorr-map
      (let [autocorr (util/autocorrelation autocorr-map)
            metrics-defs (:metrics-defs autocorr-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (doseq [mc metric-configs]
          (when-let [acf-data (get autocorr (:path mc))]
            (f acf-data mc)))))))

(defn collect-classification-metrics
  "Collect classification data for all metrics.
   Returns seq of [class-data acf-data mc].
  Uses :classification-id to get classification analysis results, and looks up
  the source autocorrelation for lag-1 data."
  [{:keys [classification-id autocorrelation-id]} data-map]
  (let [classification-id (or classification-id :autocorrelation-classification)
        class-map (data-map classification-id)]
    (when class-map
      (let [class-data (util/autocorrelation-classification-data class-map)
            ;; Get autocorrelation data for lag-1
            autocorr-id (or autocorrelation-id (:source-id class-map))
            autocorr-map (when autocorr-id (data-map autocorr-id))
            autocorr (when autocorr-map (util/autocorrelation autocorr-map))
            metrics-defs (:metrics-defs class-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (for [mc metric-configs
              :let [p (:path mc)
                    cd (get class-data p)
                    acf (when autocorr (get autocorr p))]
              :when cd]
          [cd acf mc])))))

(defn collect-ess-metrics
  "Collect ESS data for all metrics. Returns seq of [ess-data acf-data mc].
  Uses :ess-id to get ESS analysis results, and looks up
  the source autocorrelation for lag-1 data."
  [{:keys [ess-id autocorrelation-id]} data-map]
  (let [ess-id (or ess-id :effective-sample-size)
        ess-map (data-map ess-id)]
    (when ess-map
      (let [ess-data (util/effective-sample-size-data ess-map)
            ;; Get autocorrelation data for lag-1
            autocorr-id (or autocorrelation-id (:source-id ess-map))
            autocorr-map (when autocorr-id (data-map autocorr-id))
            autocorr (when autocorr-map (util/autocorrelation autocorr-map))
            metrics-defs (:metrics-defs ess-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (for [mc metric-configs
              :let [p (:path mc)
                    ed (get ess-data p)
                    acf (when autocorr (get autocorr p))]
              :when ed]
          [ed acf mc])))))
