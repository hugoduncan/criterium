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
        transforms (util/get-transforms data-map stats-id)]
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
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (have (:metrics-defs event-stats-map))
            stats (viewer-common/event-stats
                   metrics-defs
                   (util/event-stats event-stats-map))]
        (when (seq stats)
          (heading "Event stats")
          (portal-table stats))))))

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
  [_ view data-map]
  (heading "Samples")
  (portal-vega-lite
   (charts/samples-vega-spec data-map view {:height 800})))

(defmethod view/histogram* :portal
  [_ view data-map]
  (heading "Histogram")
  (portal-vega-lite
   (charts/histogram-vega-spec data-map view {:height 800})))

(defmethod view/kde* :portal
  [_ view data-map]
  (let [kde-id (or (:kde-id view) :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (heading "Kernel Density Estimation")
      (portal-vega-lite
       (charts/kde-vega-spec data-map view {:height 400})))))

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

(defmethod view/bootstrap-stats* :portal
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)
        metrics-defs (:metrics-defs bootstrap-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        bootstrap (util/bootstrap bootstrap-map)]
    (when (seq metric-configs)
      (heading "Bootstrap Statistics")
      (portal-table
       (vec
        (for [m metric-configs
              :let [stat (get-in bootstrap (:path m))]
              :when stat]
          (viewer-common/bootstrap-stat-row m stat)))))))

(defmethod view/final-gc-warnings* :portal [_ _ _])

(defmethod view/os* :portal [_ _ _])

(defmethod view/runtime* :portal [_ _ _])

;;; Domain Views

(defmethod view/domain-extract* :portal
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (viewer-common/visualization-strategy extract)
      :single-point-bar
      (when-let [{:keys [rows] heading-text :heading}
                 (viewer-common/prepare-domain-extract-table-transposed extract)]
        (heading heading-text)
        (portal-table rows)
        (portal-vega-lite
         (charts/single-point-box-chart-spec extract {:height 400})))

      :multi-point-line
      (when-let [table-data (viewer-common/prepare-domain-extract-table
                             extract {:header-sep " "})]
        (heading (:heading table-data))
        (portal-table (:rows table-data))
        (portal-vega-lite
         (charts/domain-line-chart-spec extract {:height 400})))

      :default-table
      (when-let [table-data (viewer-common/prepare-domain-extract-table
                             extract {:header-sep " "})]
        (heading (:heading table-data))
        (portal-table (:rows table-data))))))

(defmethod view/domain-grouped* :portal
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [rows] heading-text :heading}
               (viewer-common/prepare-domain-grouped-table grouped)]
      (heading heading-text)
      (portal-table rows))))

(defmethod view/domain-comparison* :portal
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when-let [tables (viewer-common/prepare-domain-comparison-tables
                       comparison)]
      (doseq [{:keys [rows] heading-text :heading} tables]
        (heading heading-text)
        (portal-table rows))
      (case (viewer-common/comparison-visualization-strategy comparison)
        :single-point-bar
        (portal-vega-lite
         (charts/comparison-box-chart-spec comparison {:height 400}))
        :multi-point-line
        (portal-vega-lite
         (charts/comparison-line-chart-spec comparison {:height 400}))
        :default-table nil))))

(defmethod view/domain-regression* :portal
  [_ {:keys [regression-id extract-id log-log-id tolerance]} data-map]
  (let [tolerance (double (or tolerance 0.01))
        chart-width 600
        chart-height 400]
    (viewer-common/with-domain-regression-data
      data-map
      {:regression-id regression-id
       :extract-id extract-id
       :log-log-id log-log-id
       :tolerance tolerance
       :table-options {:best-fit-marker "✓"
                       :plotted-marker ""
                       :tolerance tolerance}}
      {:render-log-log-charts
       (fn [{:keys [title points line-pts residual-pts chart-opts]}]
         (heading title)
         (when (seq points)
           (portal-vega-lite
            (charts/log-log-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height)))
           (when (seq residual-pts)
             (heading "Log-Log Residual Plot")
             (portal-vega-lite
              (charts/log-log-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (/ chart-height 2)))))))

       :render-model-heading
       (fn [{:keys [title]}]
         (heading title))

       :render-model-table
       (fn [{:keys [table-rows]}]
         (when (seq table-rows)
           (portal-table table-rows)))

       :render-regression-charts
       (fn [{:keys [points line-pts residual-pts y-title residual-title chart-opts]}]
         (when (seq points)
           (portal-vega-lite
            (charts/regression-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :y-title y-title)))
           (when (seq residual-pts)
             (heading "Residual Plot")
             (portal-vega-lite
              (charts/regression-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (/ chart-height 2)
                      :residual-title residual-title))))))})))

;;; Allocation Views

(defmethod view/allocation-summary* :portal
  [_ {:keys [summary-id]} data-map]
  (let [summary-id (or summary-id :allocation-summary)
        summary (data-map summary-id)]
    (when summary
      (let [{:keys [total-allocated total-freed num-allocations num-freed
                    freed-ratio]}
            summary
            total-allocated (long total-allocated)
            total-freed (long total-freed)
            retained (- total-allocated total-freed)]
        (heading "Allocation Summary")
        (portal-table
         [{:metric "Total allocated" :value total-allocated}
          {:metric "Total freed" :value total-freed}
          {:metric "Retained" :value retained}
          {:metric "Allocation count" :value num-allocations}
          {:metric "Freed count" :value num-freed}
          {:metric "Freed ratio"
           :value (format "%.1f%%" (* 100.0 (double freed-ratio)))}])))))

(defmethod view/allocation-hotspots* :portal
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)]
        (when (seq hotspots)
          (heading "Allocation Hotspots")
          (portal-table
           (mapv (fn [{:keys [call-site object-type count bytes freed-count freed-bytes]}]
                   {:call-site (viewer-common/format-call-site call-site nil)
                    :object-type (or object-type "")
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 hotspots)))))))

(defmethod view/allocation-by-type* :portal
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (heading "Allocations by Type")
          (portal-table
           (mapv (fn [[type-name {:keys [count bytes freed-count freed-bytes]}]]
                   {:type type-name
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 sorted)))))))

(defmethod view/allocation-treemap* :portal
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (heading "Allocation Treemap")
      (portal-vega (charts/treemap-vega-spec treemap-data {})))))

;;; Modal Analysis Views

(defmethod view/multimodal-warning* :portal
  [_ {:keys [modes-id]} data-map]
  (viewer-common/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (heading (str "WARNING: Multimodal distribution - "
                   (:label metric-config)))
     (portal-table
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (portal-heading [:em "Mode locations:"])
       (portal-table
        (mapv (fn [{:keys [location density]}]
                {:location (viewer-common/format-mode-location
                            location metric-config transforms)
                 :density (format "%.4g" density)})
              modes))))))
