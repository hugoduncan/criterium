(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>."
  (:refer-clojure :exclude [flush])
  (:require
   [clojure.string :as str]
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
          (assoc s :$schema "https://vega.github.io/schema/vega-lite/v5.json")
          {:portal.viewer/default :portal.viewer/vega-lite})))

(defn heading [s]
  (portal-heading [:b s]))

(defmethod view/metrics* :portal
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (portal-table
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map stats-id)]
    (when (seq metric-configs)
      (heading "Summary stats")
      (portal-table
       (viewer-common/stats-map
        (util/stats stats-map)
        metric-configs
        transforms)))))

(defmethod view/event-stats* :portal
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs (have (:metrics-defs event-stats-map))
        stats (viewer-common/event-stats
               metrics-defs
               (util/event-stats event-stats-map))]
    (when (seq stats)
      (heading "Event stats")
      (portal-table stats))))

(defmethod view/quantiles* :portal
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (heading "Quantiles")
    (portal-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :portal
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outliers")
    (portal-table
     (viewer-common/outlier-counts
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

(defmethod view/collect-plan* :portal
  [_ _view data-map]
  (heading "Collect plan")
  (portal-table
   (viewer-common/collect-plan-data data-map)))

(defmethod view/samples* :portal
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        event-samples-id (:event-samples-id view quant-samples-id)
        outliers-analysis-id (:outliers-id view :outliers)

        quant-samples (data-map quant-samples-id)
        event-samples (data-map event-samples-id)
        outliers (data-map outliers-analysis-id)

        q-metrics-defs (-> (:metrics-defs quant-samples)
                           (metric/filter-metrics
                            (metric/type-pred :quantitative)))
        e-metrics-defs (-> (:metrics-defs event-samples)
                           (metric/filter-metrics
                            (metric/type-pred :event)))
        metric-configs (metric/all-metric-configs q-metrics-defs)
        event-metric->values (util/metric->values event-samples)
        e-metric-configs (->> (metric/all-metric-configs e-metrics-defs)
                              (filterv #(not-every? zero?
                                                    (get event-metric->values
                                                         (:path %)))))

        transforms (util/get-transforms data-map quant-samples-id)]
    (heading "Samples")
    (portal-vega-lite
     {:data {:values [{}]}
      :encoding {:x {:field "index" :type "quantitative"}}
      :resolve {:scale {:y "independent"}}
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
            #(charts/event-layer event-metric->values %)
            e-metrics-defs)))}]
       (mapv
        #(charts/metric-layer
          event-metric->values
          transforms
          nil %)
        e-metric-configs))})))

(defmethod view/histogram* :portal
  [_ {:keys [histogram-id samples-id stats-id]} data-map]
  (let [histogram-id (or histogram-id :histograms)
        stats-id (or stats-id :stats)
        quant-samples-id (or samples-id :samples)
        quant-samples (data-map quant-samples-id)
        stats (data-map stats-id)
        histograms-map (util/lookup-data data-map histogram-id)
        histograms (:histograms histograms-map)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        hist-transforms (util/get-transforms data-map histogram-id)
        stats-transforms (util/get-transforms data-map (:source-id stats))
        layer-num (volatile! 0)]
    (heading "Histogram")
    (portal-vega-lite
     {:data {:values []}
      :resolve {:scale {:x "independent"
                        :y "independent"
                        :color "shared"}}
      :vconcat (mapv
                (fn [metric-config]
                  {:resolve {:scale {:x "shared" :y "independent"}}
                   :height 800
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
           [(charts/metric-percentile-layer
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
           [(charts/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

(defmethod view/bootstrap-stats* :portal [_ _ _])

(defmethod view/final-gc-warnings* :portal [_ _ _])

(defmethod view/os* :portal [_ _ _])

(defmethod view/runtime* :portal [_ _ _])

;;; Domain Views

(defn- format-coord-label
  "Format a coordinate for chart labels."
  [coord]
  (if (map? coord)
    (str/join " " (map (fn [[k v]] (str (name k) "=" v))
                       (sort-by key coord)))
    (name coord)))

(defmethod view/domain-extract* :portal
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract    (data-map extract-id)]
    (when extract
      (let [{:keys [metric data]} extract
            chart-data (map-indexed
                        (fn [idx [coord value]]
                          {:index idx
                           :coord (format-coord-label coord)
                           :value (or value 0)})
                        data)]
        (heading (str "Domain Extract: " (pr-str metric)))
        (portal-vega-lite
         {:data     {:values (vec chart-data)}
          :height   400
          :encoding {:x       {:field "coord"
                               :type  "nominal"
                               :axis  {:labelAngle -45}
                               :title "Coordinate"}
                     :y       {:field "value"
                               :type  "quantitative"
                               :title (str metric)}
                     :tooltip [{:field "coord" :type "nominal"}
                               {:field "value" :type "quantitative"}]}
          :mark     {:type "point" :size 100}})))))

(defmethod view/domain-grouped* :portal
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped    (data-map grouped-id)]
    (when grouped
      (let [{:keys [axis data]} grouped
            table-data (mapv (fn [[axis-val sub-domain]]
                               {:axis-value (if (nil? axis-val)
                                              "<nil>"
                                              (str axis-val))
                                :run-count  (count (:runs sub-domain))})
                             (sort-by (comp str key) data))]
        (heading (str "Domain Grouped by: " (name axis)))
        (portal-table table-data)))))

(defn- coord-without-axis
  "Remove the axis key from a map coordinate."
  [coord axis-key]
  (if (map? coord)
    (dissoc coord axis-key)
    coord))

(defmethod view/domain-comparison* :portal
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison    (data-map comparison-id)]
    (when comparison
      (let [{:keys [axis metric data]} comparison
            chart-data (mapcat
                        (fn [[axis-val entries]]
                          (map (fn [{:keys [coord value]}]
                                 {:axis-value (if (nil? axis-val)
                                                "<nil>"
                                                (str axis-val))
                                  :row-key    (format-coord-label
                                               (coord-without-axis coord axis))
                                  :value      (or value 0)})
                               entries))
                        data)]
        (heading (str "Domain Comparison by " (name axis) ": " (pr-str metric)))
        (portal-vega-lite
         {:data     {:values (vec chart-data)}
          :height   400
          :encoding {:x       {:field "row-key"
                               :type  "nominal"
                               :axis  {:labelAngle -45}
                               :title "Coordinate"}
                     :y       {:field "value"
                               :type  "quantitative"
                               :title (str metric)}
                     :color   {:field "axis-value"
                               :type  "nominal"
                               :title (name axis)}
                     :xOffset {:field "axis-value"}
                     :tooltip [{:field "row-key" :type "nominal" :title "Coord"}
                               {:field "axis-value" :type "nominal"
                                :title (name axis)}
                               {:field "value" :type "quantitative"}]}
          :mark     {:type "bar"}})))))

(defmethod view/domain-regression* :portal
  [_ {:keys [regression-id]} data-map]
  (let [regression-id (or regression-id :regression)
        regression    (data-map regression-id)]
    (when regression
      (let [{:keys [axis metric models best-fit]} regression]
        (heading (str "Domain Regression (axis: " (name axis) ")"))
        (if (seq models)
          (let [sorted-models (sort-by :r-squared > models)
                table-data    (mapv (fn [{:keys [id label r-squared]}]
                                      {:model    label
                                       :r-squared (format "%.4f" r-squared)
                                       :best-fit (if (= id best-fit) "✓" "")})
                                    sorted-models)]
            (portal-table table-data))
          (tap> "Insufficient data for regression"))))))
