(ns criterium.viewer.portal.core
  "Portal viewer core functions for basic metrics display.

  Provides Portal output for:
  - tap infrastructure (submit, flush)
  - metrics, stats, extremes
  - bootstrap statistics
  - samples with outliers
  - outlier counts and significance
  - event stats (class loader, JIT, GC)
  - histograms, KDE, quantiles"
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.common-charts.samples :as charts.samples]
   [criterium.viewer.common.bootstrap :as bootstrap]
   [criterium.viewer.common.core :as core]))

;;; Tap Infrastructure

(defonce tapped (atom {:values '()}))

(defn submit
  "Tap target function.

  This allows criterium to control the order of tapped output.

  ```clojure
  (def submit (criterium.viewer.portal/submit #'portal.api/submit))
  (add-tap #'submit)`
  (remove-tap #'submit)
  ``"
  [portal-submit]
  (swap! tapped assoc :portal-submit portal-submit)
  (fn
    [value]
    (swap! tapped update :values conj value)))

;; Sentinel value for flush synchronization - must match ::criterium.viewer.portal/_
(def ^:private flush-sentinel :criterium.viewer.portal/_)

(defn flush
  "Flush tapped output"
  []
  (tap> flush-sentinel)
  (loop [i 0]
    (when (not= flush-sentinel (first (:values @tapped)))
      (when (< i 1000)
        (Thread/yield)
        (recur (unchecked-inc i)))))

  (let [[{:keys [portal-submit values]}] (swap-vals! tapped assoc :values '())]
    (doseq [value values]
      (when (not= flush-sentinel value)
        (portal-submit value)))))

(defmethod view/flush-viewer :portal [_]
  (flush))

;;; Output Helpers

(defn portal-heading [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/hiccup})))

(defn portal-table [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/table})))

(defn portal-vega-lite [s]
  (tap> (with-meta
          (assoc s :$schema "https://vega.github.io/schema/vega-lite/v5.json")
          {:portal.viewer/default :portal.viewer/vega-lite})))

(defn portal-vega
  "Submit a full Vega spec to Portal."
  [s]
  (tap> (with-meta
          (assoc s :$schema "https://vega.github.io/schema/vega/v5.json")
          {:portal.viewer/default :portal.viewer/vega})))

(defn heading [s]
  (portal-heading [:b s]))

;;; Metrics

(defmethod view/metrics* :portal
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (portal-table
     (core/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

;;; Stats

(defmethod view/stats* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (heading "Summary stats")
          (portal-table
           (core/stats-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

;;; Extremes

(defmethod view/extremes* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (heading "Extremes")
          (portal-table
           (core/extremes-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

;;; Event Stats

(defmethod view/event-stats* :portal
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (have (:metrics-defs event-stats-map))
            stats (core/event-stats
                   metrics-defs
                   (util/event-stats event-stats-map))]
        (when (seq stats)
          (heading "Event stats")
          (portal-table stats))))))

;;; Quantiles

(defmethod view/quantiles* :portal
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (heading "Quantiles")
    (portal-table
     (core/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

;;; Outliers

(defmethod view/outlier-counts* :portal
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outliers")
    (portal-table
     (core/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/outlier-significance* :portal
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outlier Significance")
    (portal-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

;;; Collect Plan

(defmethod view/collect-plan* :portal
  [_ _view data-map]
  (heading "Collect plan")
  (portal-table
   (core/collect-plan-data data-map)))

;;; Samples

(defmethod view/samples* :portal
  [_ view data-map]
  (heading "Samples")
  (portal-vega-lite
   (charts.samples/samples-vega-spec data-map view {:height 800})))

(defmethod view/histogram* :portal
  [_ view data-map]
  (heading "Histogram")
  (portal-vega-lite
   (charts.samples/histogram-vega-spec data-map view {:height 800})))

(defmethod view/kde* :portal
  [_ view data-map]
  (let [kde-id (or (:kde-id view) :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (heading "Kernel Density Estimation")
      (portal-vega-lite
       (charts.distribution/kde-vega-spec data-map view {:height 400})))))

(defmethod view/sample-percentiles* :portal
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quant-samples-id)]
    (heading "Percentiles")
    (portal-vega-lite
     {:data {:values [{}]} ; for portal
      :height 800
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts.samples/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defmethod view/sample-diffs* :portal
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metric-configs (:metric-configs quant-samples)]
    (heading "Sample diffs")
    (portal-vega-lite
     {:data {:values [{}]} ; for portal
      :height 800
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts.samples/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

;;; Bootstrap Stats

(defmethod view/bootstrap-stats* :portal
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)
        metrics-defs (:metrics-defs bootstrap-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        bootstrap (util/bootstrap bootstrap-map)
        transforms (util/get-transforms data-map bootstrap-stats-id)]
    (when (seq metric-configs)
      (heading "Bootstrap Statistics")
      (portal-table
       (vec
        (for [m metric-configs
              :let [stat (get-in bootstrap (:path m))]
              :when stat]
          (bootstrap/bootstrap-stat-row m stat transforms)))))))

;;; No-op Views

(defmethod view/final-gc-warnings* :portal [_ _ _])

(defmethod view/os* :portal [_ _ _])

(defmethod view/runtime* :portal [_ _ _])
