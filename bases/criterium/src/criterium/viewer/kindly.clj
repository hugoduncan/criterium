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
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

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
