(ns criterium.viewer.kindly.autocorrelation
  "Kindly viewer for autocorrelation analysis results.

  Provides view/* multimethod implementations for displaying autocorrelation
  diagnostics in Kindly notebooks, including ACF plots, classification tables,
  and effective sample size statistics."
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common-charts.autocorrelation :as charts.autocorrelation]
   [criterium.viewer.kindly.core :as kindly.core]))

;;; Autocorrelation Views

(defmethod view/autocorrelation* :kindly
  [_ _view _data-map]
  ;; Text summary not needed for kindly - use acf-plot for visual output
  nil)

(defmethod view/acf-plot* :kindly
  [_ {:keys [autocorrelation-id min-severity]} data-map]
  (let [autocorrelation-id (or autocorrelation-id :autocorrelation)
        autocorr-map (data-map autocorrelation-id)]
    (when autocorr-map
      (let [autocorr (util/autocorrelation autocorr-map)
            metrics-defs (:metrics-defs autocorr-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (doseq [mc metric-configs]
          (when-let [acf-data (get autocorr (:path mc))]
            (when-let [spec (charts.autocorrelation/acf-plot-vega-spec
                             acf-data
                             (cond-> {:width kindly.core/chart-width
                                      :height kindly.core/chart-height
                                      :title (str "ACF: " (:label mc)
                                                  " (n="
                                                  (get-in acf-data [:effective-sample-size :n-original])
                                                  ")")}
                               min-severity (assoc :min-severity min-severity)))]
              (kindly.core/kindly-heading (str "Autocorrelation: " (:label mc)))
              (kindly.core/kindly-vega-lite spec))))))))

(def ^:private severity-labels
  {:none "none"
   :minor "minor"
   :moderate "moderate"
   :severe "severe"
   :alternating-none "alternating"
   :alternating-minor "alternating"
   :alternating-moderate "alternating"
   :alternating-severe "alternating"})

(def ^:private displayable-patterns
  "Patterns that warrant display of pattern label and recommendations.
  Excludes :clean, :alternating-none, and :alternating-minor."
  #{:transient-effects :drift :periodic :severe
    :alternating-moderate :alternating-severe})

(defn- collect-classification-metrics
  "Collect classification data for all metrics. Returns seq of [class-data acf-data mc].
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

(defn- collect-ess-metrics
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

(defn- format-anomalous-lags
  "Format anomalous lags for display.
  Returns a string like '1 (minor), 12 (moderate)' or nil if empty."
  [anomalous-lags lag-severities]
  (when (seq anomalous-lags)
    (str/join ", "
              (map (fn [lag]
                     (format "%d (%s)"
                             lag
                             (get severity-labels (get lag-severities lag :none) "unknown")))
                   anomalous-lags))))

(defn- format-detected-period
  "Format detected period with ACF value and severity.
  Returns a string like '60 samples (r=0.35, moderate)' or nil if period is nil."
  [detected-period acf-map lag-severities]
  (when detected-period
    (let [acf-val (get acf-map detected-period)
          severity (get lag-severities detected-period :none)]
      (if acf-val
        (format "%d samples (r=%.2f, %s)"
                detected-period
                (double acf-val)
                (get severity-labels severity "unknown"))
        (format "%d samples" detected-period)))))

(defmethod view/autocorrelation-classification* :kindly
  [_ view data-map]
  (let [metrics (collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (doseq [[class-data acf-data mc] metrics]
        (let [{:keys [ljung-box classification pattern detected-period]} class-data
              lag-1 (:lag-1 acf-data)
              acf-map (:acf acf-data)
              anomalous-lags (:anomalous-lags acf-data)
              lag-severities (:lag-severities acf-data)]
          (kindly.core/kindly-heading "Sample Independence Classification")
          (kindly.core/kindly-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format "%.2f (%s)"
                                      (:value lag-1)
                                      (get severity-labels (:severity lag-1) "unknown"))})
                    {:metric (str (:label mc) " Ljung-Box p-value")
                     :value (format "%.2f" (:p-value ljung-box))}
                    {:metric "Assessment"
                     :value (str/capitalize (name classification))}]
             (seq anomalous-lags)
             (conj {:metric "Anomalous lags"
                    :value (format-anomalous-lags anomalous-lags lag-severities)})

             (and (#{:warning :fail} classification)
                  (displayable-patterns pattern))
             (conj {:metric "Pattern" :value (name pattern)})

             (format-detected-period detected-period acf-map lag-severities)
             (conj {:metric "Suspected period"
                    :value (format-detected-period detected-period acf-map lag-severities)})

             true
             (->> (remove nil?) vec))))))))

(defmethod view/effective-sample-size* :kindly
  [_ view data-map]
  (let [metrics (collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (doseq [[ess-data acf-data mc] metrics]
        (let [{:keys [effective-sample-size ci-inflation-factor]} ess-data
              lag-1 (:lag-1 acf-data)]
          (kindly.core/kindly-heading "Effective Sample Size")
          (kindly.core/kindly-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format
                               "%.2f (%s)"
                               (:value lag-1)
                               (get
                                severity-labels
                                (:severity lag-1)
                                "unknown"))})
                    {:metric "Effective sample size"
                     :value (format "%d of %d (%.0f%%)"
                                    (:n-effective effective-sample-size)
                                    (:n-original effective-sample-size)
                                    (* 100.0
                                       (double
                                        (:ratio effective-sample-size))))}
                    {:metric "CI inflation factor"
                     :value (format "%.2f×" ci-inflation-factor)}]
             true
             (->> (remove nil?) vec))))))))
