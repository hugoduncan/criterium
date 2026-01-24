(ns criterium.viewer.portal.autocorrelation
  "Portal viewer for autocorrelation analysis results.

  Provides view/* multimethod implementations for displaying autocorrelation
  diagnostics in Portal, including ACF plots, classification tables, and
  effective sample size statistics."
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common-charts.autocorrelation :as charts.autocorrelation]
   [criterium.viewer.common.autocorrelation :as common.autocorr]
   [criterium.viewer.portal.core :as portal.core]))

;;; Re-exported from common for local use

(def ^:private severity-labels common.autocorr/severity-labels)
(def ^:private displayable-patterns common.autocorr/displayable-patterns)

;;; Autocorrelation Views

(defmethod view/autocorrelation* :portal
  [_ _view _data-map]
  ;; Text summary not needed for portal - use acf-plot for visual output
  nil)

(defmethod view/acf-plot* :portal
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
                             (cond-> {:height 400
                                      :title (str "ACF: " (:label mc)
                                                  " (n="
                                                  (get-in acf-data [:effective-sample-size :n-original])
                                                  ")")}
                               min-severity (assoc :min-severity min-severity)))]
              (portal.core/heading (str "Autocorrelation: " (:label mc)))
              (portal.core/portal-vega-lite spec))))))))

(defmethod view/autocorrelation-classification* :portal
  [_ view data-map]
  (let [metrics (common.autocorr/collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (doseq [[class-data acf-data mc] metrics]
        (let [{:keys [ljung-box classification pattern detected-period]} class-data
              lag-1 (:lag-1 acf-data)
              acf-map (:acf acf-data)
              anomalous-lags (:anomalous-lags acf-data)
              lag-severities (:lag-severities acf-data)]
          (portal.core/heading "Sample Independence Classification")
          (portal.core/portal-table
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
                    :value (common.autocorr/format-anomalous-lags anomalous-lags lag-severities)})

             (and (#{:warning :fail} classification)
                  (displayable-patterns pattern))
             (conj {:metric "Pattern" :value (name pattern)})

             (common.autocorr/format-detected-period detected-period acf-map lag-severities)
             (conj {:metric "Suspected period"
                    :value (common.autocorr/format-detected-period detected-period acf-map lag-severities)})

             true
             (->> (remove nil?) vec))))))))

(defmethod view/effective-sample-size* :portal
  [_ view data-map]
  (let [metrics (common.autocorr/collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (doseq [[ess-data acf-data mc] metrics]
        (let [{:keys [effective-sample-size ci-inflation-factor]} ess-data
              lag-1 (:lag-1 acf-data)]
          (portal.core/heading "Effective Sample Size")
          (portal.core/portal-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format
                               "%.2f (%s)"
                               (:value lag-1)
                               (get
                                severity-labels
                                (:severity lag-1) "unknown"))})
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
