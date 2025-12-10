(ns criterium.viewer.kindly
  "A viewer that outputs Kindly-annotated data structures for Clay notebooks.

  Uses an accumulator pattern where view functions append Kindly-annotated
  values to an atom. The `flush-viewer` multimethod returns a `kind/fragment`
  combining all accumulated values.

  No runtime dependency on scicloj/kindly - produces plain maps with
  appropriate `:kindly/kind` metadata."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]
   [criterium.viewer.common-charts :as charts]))

(defonce ^{:doc "Accumulator for Kindly-annotated values."}
  accumulated
  (atom []))

(defn kindly-add
  "Add a value to the accumulator."
  [value]
  (swap! accumulated conj value)
  nil)

(defn kindly-heading
  "Add a markdown heading to the accumulator."
  [s]
  (kindly-add
   (with-meta
     [(str "**" s "**")]
     {:kindly/kind :kind/md})))

(defn kindly-table
  "Add a table to the accumulator."
  [data]
  (kindly-add
   (with-meta data {:kindly/kind :kind/table})))

(defn kindly-vega-lite
  "Add a Vega-Lite chart to the accumulator."
  [spec]
  (kindly-add
   (with-meta
     (assoc spec :$schema "https://vega.github.io/schema/vega-lite/v5.json")
     {:kindly/kind :kind/vega-lite})))

(defn flush
  "Return accumulated values as a kind/fragment and clear the accumulator."
  []
  (let [[values _] (swap-vals! accumulated (constantly []))]
    (when (seq values)
      (with-meta values {:kindly/kind :kind/fragment}))))

(defmethod view/flush-viewer :kindly [_]
  (flush))

(defmethod view/stats* :kindly
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id       (or stats-id :stats)
        stats-map      (data-map stats-id)
        metrics-defs   (-> (:metrics-defs stats-map)
                           (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map stats-id)]
    (kindly-heading "Summary stats")
    (kindly-table
     (viewer-common/stats-map
      (util/stats stats-map)
      metric-configs
      transforms))))

(defmethod view/quantiles* :kindly
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id   (or quantiles-id :quantiles)
        quantiles-map  (data-map quantiles-id)
        metrics-defs   (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map quantiles-id)]
    (kindly-heading "Quantiles")
    (kindly-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :kindly
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id    (or outliers-id :outliers)
        outliers-map   (data-map outliers-id)
        metrics-defs   (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Outliers")
    (kindly-table
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/collect-plan* :kindly
  [_ _view data-map]
  (kindly-heading "Collect plan")
  (kindly-table
   (viewer-common/collect-plan-data data-map)))

(defmethod view/samples* :kindly
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id     (:samples-id view :samples)
        event-samples-id     (:event-samples-id view quant-samples-id)
        outliers-analysis-id (:outliers-id view :outliers)

        quant-samples (data-map quant-samples-id)
        event-samples (data-map event-samples-id)
        outliers      (data-map outliers-analysis-id)

        q-metrics-defs   (-> (:metrics-defs quant-samples)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
        e-metrics-defs   (-> (:metrics-defs event-samples)
                             (metric/filter-metrics
                              (metric/type-pred :event)))
        metric-configs   (metric/all-metric-configs q-metrics-defs)
        e-metric-configs (metric/all-metric-configs e-metrics-defs)

        transforms (util/get-transforms data-map quant-samples-id)]
    (kindly-heading "Samples")
    (kindly-vega-lite
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

(defmethod view/histogram* :kindly
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
    (kindly-heading "Histogram")
    (kindly-vega-lite
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
