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

(defonce ^{:doc "Last flushed Kindly fragment for retrieval after bench completes."}
  last-fragment
  (atom nil))

(def ^:private chart-width
  "Width for Kindly vega-lite charts, sized for notebook display."
  700)

(def ^:private chart-height
  "Height for Kindly vega-lite charts, sized for notebook display."
  350)

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

(defn kindly-vega
  "Add a full Vega chart to the accumulator."
  [spec]
  (kindly-add
   (with-meta
     (assoc spec :$schema "https://vega.github.io/schema/vega/v5.json")
     {:kindly/kind :kind/vega})))

(defn flush
  "Return accumulated values as a kind/fragment and clear the accumulator.
  Also stores the fragment in `last-fragment` for retrieval after bench completes."
  []
  (let [[values _] (swap-vals! accumulated (constantly []))]
    (when (seq values)
      (let [fragment (with-meta values {:kindly/kind :kind/fragment})]
        (reset! last-fragment fragment)
        fragment))))

(defmethod view/flush-viewer :kindly [_]
  (flush))

(defmethod view/stats* :kindly
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map stats-id)]
    (when (seq metric-configs)
      (kindly-heading "Summary stats")
      (kindly-table
       (viewer-common/stats-map
        (util/stats stats-map)
        metric-configs
        transforms)))))

(defmethod view/quantiles* :kindly
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (kindly-heading "Quantiles")
    (kindly-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :kindly
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
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
  [_ view data-map]
  (kindly-heading "Samples")
  (kindly-vega-lite
   (charts/samples-vega-spec data-map view {:width chart-width
                                            :height chart-height})))

(defmethod view/histogram* :kindly
  [_ view data-map]
  (kindly-heading "Histogram")
  (kindly-vega-lite
   (charts/histogram-vega-spec data-map view {:width chart-width
                                              :height chart-height})))

(defmethod view/sample-percentiles* :kindly
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quant-samples-id)]
    (kindly-heading "Percentiles")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defmethod view/metrics* :kindly
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Metrics")
    (kindly-table
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/event-stats* :kindly
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs (have (:metrics-defs event-stats-map))
        stats (viewer-common/event-stats
               metrics-defs
               (util/event-stats event-stats-map))]
    (when (seq stats)
      (kindly-heading "Event stats")
      (kindly-table stats))))

(defmethod view/outlier-significance* :kindly
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Outlier Significance")
    (kindly-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/sample-diffs* :kindly
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metric-configs (:metric-configs quant-samples)]
    (kindly-heading "Sample diffs")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

;;; Domain view implementations

(defmethod view/domain-extract* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when-let [table-data (viewer-common/prepare-domain-extract-table
                           extract {:header-sep "\n"})]
      (kindly-heading (:heading table-data))
      (kindly-table (:rows table-data)))))

(defmethod view/domain-grouped* :kindly
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (viewer-common/prepare-domain-grouped-table
                                       grouped)]
      (kindly-heading heading)
      (kindly-table rows))))

(defmethod view/domain-comparison* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when-let [tables (viewer-common/prepare-domain-comparison-tables
                       comparison)]
      (doseq [{:keys [heading rows]} tables]
        (kindly-heading heading)
        (kindly-table rows)))))

(defmethod view/domain-regression* :kindly
  [_ {:keys [regression-id extract-id tolerance]} data-map]
  (let [regression-id (or regression-id :regression)
        regression (data-map regression-id)
        tolerance (double (or tolerance 0.01))
        table-options {:best-fit-marker "✓"
                       :plotted-marker ""
                       :tolerance tolerance}
        legend-options {:orient "none"
                        :legendX 10
                        :legendY 10}]
    (when regression
      (let [{:keys [axis regressions impl-axis implementations]} regression
            extract-id (or extract-id :extract)
            extract (data-map extract-id)
            multi-impl? (> (count implementations) 1)]

        (if multi-impl?
          ;; Multi-implementation mode
          (doseq [[metric-id {:keys [metric by-impl with-error-bounds]}]
                  regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  impl-keys (sort (keys by-impl))]
              (kindly-heading (str "Domain Regression (axis: " (name axis)
                                   ", metric: " (pr-str metric)
                                   ", by: " (name impl-axis) ")"))
              ;; Model table
              (when (seq by-impl)
                (kindly-table
                 (viewer-common/prepare-regression-model-table-multi-impl
                  by-impl impl-keys table-options)))
              ;; Charts
              (when-let [point-data (viewer-common/prepare-regression-points
                                     metric-extract-data
                                     {:axis axis
                                      :impl-axis impl-axis
                                      :has-error-bounds? with-error-bounds
                                      :metric metric})]
                (let [{:keys [points unit]}
                      point-data
                      line-pts
                      (viewer-common/prepare-regression-fit-lines
                       point-data
                       {:by-impl by-impl :impl-keys impl-keys})
                      y-title (if (seq unit)
                                (str (pr-str metric) " (" unit ")")
                                (pr-str metric))
                      residual-title (if (seq unit)
                                       (str "Residual (" unit ")")
                                       "Residual")]
                  (when (seq points)
                    (kindly-vega-lite
                     (charts/regression-chart-spec
                      points line-pts
                      {:width chart-width
                       :height chart-height
                       :axis-name (name axis) :y-title y-title
                       :color-field "impl"
                       :legend-options legend-options
                       :has-error-bounds? with-error-bounds}))
                    ;; Residual plot
                    (let [residual-pts
                          (viewer-common/prepare-regression-residuals
                           point-data
                           {:axis axis
                            :impl-axis impl-axis
                            :has-error-bounds? with-error-bounds
                            :by-impl by-impl
                            :impl-keys impl-keys})]
                      (kindly-heading "Residual Plot")
                      (kindly-vega-lite
                       (charts/regression-residual-spec
                        residual-pts
                        {:width chart-width
                         :height (long (/ (long chart-height) 2))
                         :axis-name (name axis)
                         :residual-title residual-title
                         :color-field "impl"
                         :legend-options legend-options}))))))))

          ;; Single-implementation mode
          (doseq [[metric-id {:keys [metric models best-fit with-error-bounds]}]
                  regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  best-r-squared (when best-fit
                                   (->> models
                                        (filter #(= (:id %) best-fit))
                                        first :r-squared))
                  models-to-plot (when best-r-squared
                                   (->> models
                                        (filter
                                         #(>= (double (:r-squared %))
                                              (* (double best-r-squared)
                                                 (- 1.0 tolerance))))
                                        (sort-by :r-squared >)))]
              (kindly-heading (str "Domain Regression (axis: " (name axis)
                                   ", metric: " (pr-str metric) ")"))
              ;; Model table
              (when (seq models)
                (kindly-table
                 (viewer-common/prepare-regression-model-table
                  {:models models :best-fit best-fit}
                  table-options)))
              ;; Charts
              (when (and metric-extract-data (seq models-to-plot))
                (when-let [point-data (viewer-common/prepare-regression-points
                                       metric-extract-data
                                       {:axis axis
                                        :has-error-bounds? with-error-bounds
                                        :metric metric})]
                  (let [{:keys [points unit]}
                        point-data
                        line-pts
                        (viewer-common/prepare-regression-fit-lines
                         point-data {:models models-to-plot})
                        y-title (if (seq unit)
                                  (str (pr-str metric) " (" unit ")")
                                  (pr-str metric))
                        residual-title (if (seq unit)
                                         (str "Residual (" unit ")")
                                         "Residual")]
                    (when (seq points)
                      (kindly-vega-lite
                       (charts/regression-chart-spec
                        points line-pts
                        {:width chart-width
                         :height chart-height
                         :axis-name (name axis)
                         :y-title y-title
                         :color-field "model"
                         :legend-options legend-options
                         :has-error-bounds? with-error-bounds}))
                      ;; Residual plot
                      (let [residual-pts
                            (viewer-common/prepare-regression-residuals
                             point-data
                             {:axis axis
                              :has-error-bounds? with-error-bounds
                              :models models-to-plot})]
                        (kindly-heading "Residual Plot")
                        (kindly-vega-lite
                         (charts/regression-residual-spec
                          residual-pts
                          {:width chart-width
                           :height (long (/ (long chart-height) 2))
                           :axis-name (name axis)
                           :residual-title residual-title
                           :color-field "model"
                           :legend-options legend-options}))))))))))))))

;;; Allocation view implementations

(defmethod view/allocation-summary* :kindly
  [_ {:keys [summary-id]} data-map]
  (let [summary-id (or summary-id :allocation-summary)
        summary (data-map summary-id)]
    (when summary
      (let [{:keys [total-allocated total-freed num-allocations num-freed
                    freed-ratio]}
            summary
            total-allocated (long total-allocated)
            total-freed (long total-freed)
            retained (- total-allocated total-freed)
            freed-ratio (or freed-ratio
                            (when (pos? total-allocated)
                              (/ (double total-freed) (double total-allocated))))
            freed-ratio-str (if (some? freed-ratio)
                              (clojure.core/format "%.1f%%" (* 100.0 (double freed-ratio)))
                              "N/A")]
        (kindly-heading "Allocation Summary")
        (kindly-table
         [{:metric "Total allocated"
           :value (str total-allocated " bytes")}
          {:metric "Total freed"
           :value (str total-freed " bytes")}
          {:metric "Retained"
           :value (str retained " bytes")}
          {:metric "Allocation count"
           :value num-allocations}
          {:metric "Freed count"
           :value num-freed}
          {:metric "Freed ratio"
           :value freed-ratio-str}])))))

(defmethod view/allocation-hotspots* :kindly
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)]
        (when (seq hotspots)
          (kindly-heading "Allocation Hotspots")
          (kindly-table
           (mapv (fn [{:keys [call-site object-type count bytes freed-count freed-bytes]}]
                   {:call-site (viewer-common/format-call-site call-site nil)
                    :object-type (or object-type "")
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 hotspots)))))))

(defmethod view/allocation-by-type* :kindly
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (kindly-heading "Allocations by Type")
          (kindly-table
           (mapv (fn [[type-name {:keys [count bytes freed-count freed-bytes]}]]
                   {:type type-name
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 sorted)))))))

(defmethod view/allocation-treemap* :kindly
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (kindly-heading "Allocation Treemap")
      (kindly-vega (charts/treemap-vega-spec treemap-data {})))))

;;; Noop implementations for views not applicable to Kindly output

(defmethod view/bootstrap-stats* :kindly [_ _ _])
(defmethod view/final-gc-warnings* :kindly [_ _ _])
(defmethod view/os* :kindly [_ _ _])
(defmethod view/runtime* :kindly [_ _ _])
