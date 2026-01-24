(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>.

  Core functionality (tap infrastructure, metrics, stats, extremes, bootstrap,
  samples, outliers, events, KDE) is in criterium.viewer.portal.core.

  Domain analysis views (grouped, extract, comparison, regression, apply)
  are in criterium.viewer.portal.domain.

  Allocation profiling views (summary, hotspots, by-type, treemap)
  are in criterium.viewer.portal.allocation.

  Distribution fit views (models, parameter CIs, PDF, CDF, Q-Q charts)
  are in criterium.viewer.portal.distribution.

  Tail analysis views (summary, ratios, high quantiles, charts)
  are in criterium.viewer.portal.tail.

  Shape statistics views (skewness, kurtosis, CV)
  are in criterium.viewer.portal.shape.

  Modal analysis views (multimodal warnings)
  are in criterium.viewer.portal.modal."
  (:refer-clojure :exclude [flush])
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts.autocorrelation :as charts.autocorrelation]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.portal.allocation]
   [criterium.viewer.portal.core :as portal.core]
   [criterium.viewer.portal.distribution]
   [criterium.viewer.portal.domain]
   [criterium.viewer.portal.modal]
   [criterium.viewer.portal.shape]
   [criterium.viewer.portal.tail]))

;;; Re-exported from portal.core for backwards compatibility

(def tapped
  "Atom storing tapped values and portal-submit function.
   Delegates to criterium.viewer.portal.core."
  portal.core/tapped)

(def submit
  "Tap target function. Delegates to criterium.viewer.portal.core."
  portal.core/submit)

(def flush
  "Flush tapped output. Delegates to criterium.viewer.portal.core."
  portal.core/flush)

(def portal-heading
  "Send hiccup-formatted heading to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-heading)

(def portal-table
  "Send table data to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-table)

(def portal-vega-lite
  "Send Vega-Lite spec to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-vega-lite)

(def portal-vega
  "Send Vega spec to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-vega)

(def heading
  "Send bold heading to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/heading)

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
              (heading (str "Autocorrelation: " (:label mc)))
              (portal-vega-lite spec))))))))

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

(defn- capitalize [^String s]
  (when s
    (str (Character/toUpperCase ^char (first s)) (subs s 1))))

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

(defmethod view/autocorrelation-classification* :portal
  [_ view data-map]
  (let [metrics (collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (doseq [[class-data acf-data mc] metrics]
        (let [{:keys [ljung-box classification pattern detected-period]} class-data
              lag-1 (:lag-1 acf-data)
              acf-map (:acf acf-data)
              anomalous-lags (:anomalous-lags acf-data)
              lag-severities (:lag-severities acf-data)]
          (heading "Sample Independence Classification")
          (portal-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format "%.2f (%s)"
                                      (:value lag-1)
                                      (get severity-labels (:severity lag-1) "unknown"))})
                    {:metric (str (:label mc) " Ljung-Box p-value")
                     :value (format "%.2f" (:p-value ljung-box))}
                    {:metric "Assessment"
                     :value (capitalize (name classification))}]
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

(defmethod view/effective-sample-size* :portal
  [_ view data-map]
  (let [metrics (collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (doseq [[ess-data acf-data mc] metrics]
        (let [{:keys [effective-sample-size ci-inflation-factor]} ess-data
              lag-1 (:lag-1 acf-data)]
          (heading "Effective Sample Size")
          (portal-table
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

;;; Call Tree Views

(defmethod view/call-tree* :portal
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (heading (format "Call Tree (%d total calls)" total-calls))
        (portal-vega (charts.profile/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :portal
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (heading "Call Flame Chart")
        (portal-vega (charts.profile/call-tree-flame-vega-spec call-tree total-calls {}))))))

(defmethod view/most-called* :portal
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (heading (format "Most Called Methods (top %d, %d total calls)"
                         (count methods) total-in-list))
        (portal-vega-lite (charts.profile/most-called-vega-lite-spec most-called-data {}))))))


