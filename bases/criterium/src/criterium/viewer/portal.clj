(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]
   [criterium.viewer.common-charts :as charts]))

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

(defn flush
  "Flush tapped output"
  []
  (tap> ::_)
  (loop [i 0]
    (when (not= ::_ (first (:values @tapped)))
      (when (< i 1000)
        (Thread/yield)
        (recur (unchecked-inc i)))))

  (let [[{:keys [portal-submit values]}] (swap-vals! tapped assoc :values '())]
    (doseq [value values]
      (when (not= ::_ value)
        (portal-submit value)))))

(defmethod view/flush-viewer :portal [_]
  (flush))

(defn portal-heading [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/hiccup})))

(defn portal-table [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/table})))

(defn portal-vega-lite [s]
  (tap> (with-meta
          (assoc s :$schema  "https://vega.github.io/schema/vega-lite/v5.json")
          {:portal.viewer/default :portal.viewer/vega-lite})))

(defn heading [s]
  (portal-heading [:b s]))

(defmethod view/metrics* :portal
  [_ {:keys [samples-id]} data-map]
  (let [samples-id      (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs    (:metrics-defs metrics-samples)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (portal-table
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id       (or stats-id :stats)
        stats-map      (data-map stats-id)
        metrics-defs   (-> (:metrics-defs stats-map)
                           (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map stats-id)]
    (heading "Summary stats")
    (portal-table
     (viewer-common/stats-map
      (util/stats stats-map)
      metric-configs
      transforms))))

(defmethod view/event-stats* :portal
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id  (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs    (have (:metrics-defs event-stats-map))]
    (heading "Event stats")
    (portal-table
     (viewer-common/event-stats
      metrics-defs
      (util/event-stats event-stats-map)))))

(defmethod view/quantiles* :portal
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id   (or quantiles-id :quantiles)
        quantiles-map  (data-map quantiles-id)
        metrics-defs   (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map quantiles-id)]
    (heading "Quantiles")
    (portal-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :portal
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id    (or outliers-id :outliers)
        outliers-map   (data-map outliers-id)
        metrics-defs   (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outliers")
    (portal-table
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/outlier-significance* :portal
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id  (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig     (util/outlier-significance outlier-sig-map)
        metrics-defs    (:metrics-defs outlier-sig-map)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (heading "Outlier Significance")
    (portal-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/collect-plan* :portal
  [_ _view data-map]
  (heading "Collect plan")
  (portal-table
   (viewer-common/collect-plan-data data-map)))

(defmethod view/samples* :portal
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id     (:samples-id view :samples)
        event-samples-id     (:event-samples-id view quant-samples-id)
        outliers-analysis-id (:outliers-id view :outliers)

        quant-samples (data-map quant-samples-id)
        event-samples (data-map event-samples-id)
        outliers      (data-map outliers-analysis-id)

        q-metrics-defs   (-> (:metrics-defs  quant-samples)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
        e-metrics-defs   (-> (:metrics-defs event-samples)
                             (metric/filter-metrics
                              (metric/type-pred :event)))
        metric-configs   (metric/all-metric-configs q-metrics-defs)
        e-metric-configs (metric/all-metric-configs e-metrics-defs)

        transforms (util/get-transforms data-map quant-samples-id)]
    (heading "Samples")
    (portal-vega-lite
     {:data     {:values [{}]}
      :encoding {:x {:field "index" :type "quantitative"}}
      :resolve  {:scale {:y "independent"}}
      :vconcat
      (into
       [{:height 800
         :layer
         (vec
          (into
           [(charts/metric-layer
             (util/metric->values quant-samples)
             transforms
             (when outliers (util/outliers outliers))
             (have (first metric-configs)))]
           (mapcat
            #(charts/event-layer (util/metric->values event-samples) %)
            e-metrics-defs)))}]
       (mapv
        #(charts/metric-layer
          (util/metric->values quant-samples)
          transforms
          outliers %)
        e-metric-configs))})))

(defmethod view/histogram* :portal
  [_ {:keys [histogram-id samples-id stats-id]} data-map]
  (let [histogram-id     (or histogram-id :histograms)
        stats-id         (or stats-id :stats)
        quant-samples-id (or samples-id :samples)
        quant-samples    (data-map quant-samples-id)
        stats            (data-map stats-id)
        histograms-map   (util/lookup-data data-map histogram-id)
        histograms       (:histograms histograms-map)
        metrics-defs     (-> (:metrics-defs quant-samples)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
        metric-configs   (metric/all-metric-configs metrics-defs)
        hist-transforms  (util/get-transforms data-map histogram-id)
        stats-transforms (util/get-transforms data-map (:source-id stats))
        layer-num        (volatile! 0)]
    (heading "Histogram")
    (portal-vega-lite
     {:data    {:values []}
      :resolve {:scale {:x     "independent"
                        :y     "independent"
                        :color "shared"}}
      :vconcat (mapv
                (fn [metric-config]
                  {:resolve {:scale {:x "shared" :y "independent"}}
                   :height  800
                   :layer
                   (into
                    [(charts/metric-computed-histo-layer
                      hist-transforms
                      (histograms (:path metric-config))
                      metric-config
                      (vswap! layer-num unchecked-inc))]
                    (when stats
                      (->>
                       (charts/metric-sample-stats-layer
                        stats-transforms
                        (get-in (util/stats stats) (:path metric-config))
                        metric-config
                        (vswap! layer-num unchecked-inc)))))})
                metric-configs)})))

(defmethod view/sample-percentiles* :portal
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples    (data-map quant-samples-id)
        metrics-defs     (-> (:metrics-defs quant-samples)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
        metric-configs   (metric/all-metric-configs metrics-defs)
        transforms       (util/get-transforms data-map quant-samples-id)]
    (heading "Percentiles")
    (portal-vega-lite
     {:data    {:values [{}]} ; for portal
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defmethod view/sample-diffs* :portal
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples    (data-map quant-samples-id)
        metric-configs   (:metric-configs quant-samples)]
    (heading "Sample diffs")
    (portal-vega-lite
     {:data    {:values [{}]} ; for portal
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))
