(ns criterium.viewer.kindly
  "A viewer that outputs Kindly-annotated data structures for Clay notebooks.

  Uses an accumulator pattern where view functions append Kindly-annotated
  values to an atom. The `flush-viewer` multimethod returns a `kind/fragment`
  combining all accumulated values.

  No runtime dependency on scicloj/kindly - produces plain maps with
  appropriate `:kindly/kind` metadata."
  (:refer-clojure :exclude [flush])
  (:require
   [clojure.string :as str]
   [criterium.domain.types :as domain.types]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts.autocorrelation :as charts.autocorrelation]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.common-charts.quantile :as charts.quantile]
   [criterium.viewer.common-charts.regression :as charts.regression]
   [criterium.viewer.common-charts.samples :as charts.samples]
   [criterium.viewer.common-charts.tail :as charts.tail]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.common.bootstrap :as bootstrap]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.regression :as regression]
   [criterium.viewer.common.shape :as shape]))

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
  "Add a table to the accumulator.
  Optionally accepts :column-names in opts for explicit column ordering."
  ([data] (kindly-table data nil))
  ([data {:keys [column-names]}]
   (kindly-add
    (with-meta
      (if column-names
        {:row-maps data :column-names column-names}
        data)
      {:kindly/kind :kind/table}))))

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
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (kindly-heading "Summary stats")
          (kindly-table
           (core/stats-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

(defmethod view/extremes* :kindly
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (kindly-heading "Extremes")
          (kindly-table
           (core/extremes-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

(defmethod view/quantiles* :kindly
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (kindly-heading "Quantiles")
    (kindly-table
     (core/quantiles
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
     (core/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/collect-plan* :kindly
  [_ _view data-map]
  (kindly-heading "Collect plan")
  (kindly-table
   (core/collect-plan-data data-map)))

(defmethod view/samples* :kindly
  [_ view data-map]
  (kindly-heading "Samples")
  (kindly-vega-lite
   (charts.samples/samples-vega-spec data-map view {:width chart-width
                                                    :height chart-height})))

(defmethod view/histogram* :kindly
  [_ view data-map]
  (kindly-heading "Histogram")
  (kindly-vega-lite
   (charts.samples/histogram-vega-spec data-map view {:width chart-width
                                                      :height chart-height})))

(defmethod view/kde* :kindly
  [_ view data-map]
  (let [kde-id (or (:kde-id view) :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (kindly-heading "Kernel Density Estimation")
      (kindly-vega-lite
       (charts.distribution/kde-vega-spec data-map view {:width chart-width
                                                         :height chart-height})))))

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
           [(charts.samples/metric-percentile-layer
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
     (core/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/event-stats* :kindly
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (have (:metrics-defs event-stats-map))
            stats (core/event-stats
                   metrics-defs
                   (util/event-stats event-stats-map))]
        (when (seq stats)
          (kindly-heading "Event stats")
          (kindly-table stats))))))

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
           [(charts.samples/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

;;; Domain view implementations

(defmethod view/domain-extract-table* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [{:keys [heading col-headers rows]}
                 (extract/prepare-domain-extract-table-transposed extract)]
        (kindly-heading heading)
        (kindly-table rows {:column-names col-headers}))

      :multi-point
      (when-let [{:keys [heading coord-header col-headers rows]}
                 (extract/prepare-domain-extract-table
                  extract {:header-sep "\n"})]
        (kindly-heading heading)
        (kindly-table rows {:column-names (into [coord-header] col-headers)}))

      :default-table
      (when-let [{:keys [heading coord-header col-headers rows]}
                 (extract/prepare-domain-extract-table
                  extract {:header-sep "\n"})]
        (kindly-heading heading)
        (kindly-table rows {:column-names (into [coord-header] col-headers)})))))

(defmethod view/domain-extract-chart* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when extract
        (let [box-spec (charts.comparison/single-point-box-chart-spec extract {:width chart-width
                                                                               :height chart-height})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (kindly-vega-lite box-spec)
            (kindly-vega-lite
             (charts.comparison/single-point-bar-chart-spec extract {:width chart-width
                                                                     :height chart-height})))))

      :multi-point
      (when extract
        (kindly-vega-lite
         (charts.comparison/domain-line-chart-spec extract {:width chart-width
                                                            :height chart-height})))

      ;; :default-table - no chart output
      nil)))

(defmethod view/domain-grouped* :kindly
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (extract/prepare-domain-grouped-table
                                       grouped)]
      (kindly-heading heading)
      (kindly-table rows))))

(defmethod view/domain-comparison-table* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when-let [{:keys [heading col-headers rows]}
                 (comparison/prepare-domain-comparison-table-transposed comparison)]
        (kindly-heading heading)
        (kindly-table rows {:column-names col-headers}))

      :multi-point
      (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
        (doseq [{:keys [heading coord-header col-headers rows]} tables]
          (kindly-heading heading)
          (kindly-table rows {:column-names (into [coord-header] col-headers)})))

      :default-table
      (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
        (doseq [{:keys [heading coord-header col-headers rows]} tables]
          (kindly-heading heading)
          (kindly-table rows {:column-names (into [coord-header] col-headers)}))))))

(defmethod view/domain-comparison-chart* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when comparison
        (let [box-spec (charts.comparison/comparison-box-chart-spec comparison {:width chart-width
                                                                                :height chart-height})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (kindly-vega-lite box-spec)
            (kindly-vega-lite
             (charts.comparison/comparison-bar-chart-spec comparison {:width chart-width
                                                                      :height chart-height})))))

      :multi-point
      (when comparison
        (kindly-vega-lite
         (charts.comparison/comparison-line-chart-spec comparison {:width chart-width
                                                                   :height chart-height})))

      ;; :default-table - no chart output
      nil)))

(defmethod view/domain-regression* :kindly
  [_ {:keys [regression-id extract-id log-log-id tolerance]} data-map]
  (let [tolerance (double (or tolerance 0.01))
        legend-options {:orient "none"
                        :legendX 10
                        :legendY 10}]
    (regression/with-domain-regression-data
      data-map
      {:regression-id regression-id
       :extract-id extract-id
       :log-log-id log-log-id
       :tolerance tolerance
       :table-options {:best-fit-marker "✓"
                       :plotted-marker ""
                       :tolerance tolerance}}
      {:render-log-log-charts
       (fn [{:keys [title metric points line-pts residual-pts chart-opts]}]
         (kindly-heading title)
         (when (seq points)
           (kindly-vega-lite
            (charts.regression/log-log-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :metric-name (name (second metric))
                    :legend-options legend-options)))
           (when (seq residual-pts)
             (kindly-heading "Log-Log Residual Plot")
             (kindly-vega-lite
              (charts.regression/log-log-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (long (/ (long chart-height) 2))
                      :legend-options legend-options))))))

       :render-model-heading
       (fn [{:keys [title]}]
         (kindly-heading title))

       :render-model-table
       (fn [{:keys [table-rows]}]
         (when (seq table-rows)
           (kindly-table table-rows)))

       :render-regression-charts
       (fn [{:keys [points line-pts residual-pts y-title residual-title chart-opts]}]
         (when (seq points)
           (kindly-vega-lite
            (charts.regression/regression-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :y-title y-title
                    :legend-options legend-options)))
           (when (seq residual-pts)
             (kindly-heading "Residual Plot")
             (kindly-vega-lite
              (charts.regression/regression-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (long (/ (long chart-height) 2))
                      :residual-title residual-title
                      :legend-options legend-options))))))})))

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
            retained (- total-allocated total-freed)]
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
           :value (clojure.core/format "%.1f%%" (* 100.0 (double freed-ratio)))}])))))

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
                   {:call-site (allocation/format-call-site call-site nil)
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
      (kindly-vega (charts.profile/treemap-vega-spec treemap-data {})))))

;;; Bootstrap statistics view

(defmethod view/bootstrap-stats* :kindly
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (:metrics-defs bootstrap-map)
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            transforms (util/get-transforms data-map bootstrap-stats-id)]
        (when (seq metric-configs)
          (kindly-heading "Bootstrap Statistics")
          (kindly-table
           (vec
            (for [m metric-configs
                  :let [stat (get-in bootstrap (:path m))]
                  :when stat]
              (bootstrap/bootstrap-stat-row m stat transforms)))
           {:column-names [:metric :median :median-ci-lower :median-ci-upper
                           :mean :mean-ci-lower :mean-ci-upper
                           :p10 :p90]}))))))

;;; Shape statistics view

(defmethod view/shape-stats* :kindly
  [_ {:keys [bootstrap-stats-id] :as _view} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (-> (:metrics-defs bootstrap-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            shape-data (shape/shape-stats-data metric-configs bootstrap)]
        (when (seq shape-data)
          (kindly-heading "Shape Statistics")
          (kindly-table
           (mapv (fn [{:keys [metric skewness skewness-class
                              kurtosis kurtosis-class cv cv-class]}]
                   {:metric metric
                    :skewness skewness
                    :skewness-interpretation (name skewness-class)
                    :kurtosis kurtosis
                    :kurtosis-interpretation (name kurtosis-class)
                    :cv cv
                    :cv-interpretation (name cv-class)})
                 shape-data)))))))

;;; Call Tree Views

(defmethod view/call-tree* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading (clojure.core/format "Call Tree (%d total calls)" total-calls))
        (kindly-vega (charts.profile/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading "Call Flame Chart")
        (kindly-vega (charts.profile/call-tree-flame-vega-spec call-tree total-calls {}))))))

(defmethod view/most-called* :kindly
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (kindly-heading (clojure.core/format "Most Called Methods (top %d, %d total calls)"
                                             (count methods) total-in-list))
        (kindly-vega-lite (charts.profile/most-called-vega-lite-spec most-called-data {}))))))

;;; Distribution Fit Views

(def ^:private distribution-labels
  "Human-readable labels for distributions."
  {:gamma "Gamma"
   :lognormal "Log-normal"
   :inverse-gaussian "Inverse Gaussian"
   :weibull "Weibull"})

(defn- format-distribution-table-row
  "Format a distribution fit result as a table row."
  [dist result best-model]
  (let [label (get distribution-labels dist (name dist))
        is-best? (= dist best-model)]
    (cond
      (:error result)
      {:distribution label
       :status "error"
       :aic "-"
       :delta-aic "-"
       :bic "-"
       :ks-stat "-"
       :ks-pvalue "-"
       :cvm-stat "-"
       :cvm-pvalue "-"
       :best? false}

      (:skipped result)
      {:distribution label
       :status (name (:skipped result))
       :aic "-"
       :delta-aic "-"
       :bic "-"
       :ks-stat "-"
       :ks-pvalue "-"
       :cvm-stat "-"
       :cvm-pvalue "-"
       :best? false}

      :else
      {:distribution label
       :status "fitted"
       :aic (clojure.core/format "%.1f" (:aic result))
       :delta-aic (clojure.core/format "%.1f" (or (:delta-aic result) 0.0))
       :bic (clojure.core/format "%.1f" (:bic result))
       :ks-stat (if-let [ks (:ks-test result)]
                  (clojure.core/format "%.4f" (:statistic ks)) "-")
       :ks-pvalue (if-let [ks (:ks-test result)]
                    (clojure.core/format "%.4f" (:p-value ks)) "-")
       :cvm-stat (if-let [cvm (:cvm-test result)]
                   (clojure.core/format "%.4f" (:statistic cvm)) "-")
       :cvm-pvalue (if-let [cvm (:cvm-test result)]
                     (clojure.core/format "%.4f" (:p-value cvm)) "-")
       :best? is-best?})))

(defn- format-parameter-ci-rows
  "Format parameter CIs as table rows."
  [best-model parameter-cis]
  (when (and best-model (get parameter-cis best-model))
    (let [label (get distribution-labels best-model (name best-model))
          cis (get parameter-cis best-model)]
      (mapv (fn [[param {:keys [point-estimate ci-lower ci-upper]}]]
              {:distribution label
               :parameter (name param)
               :estimate (clojure.core/format "%.4g" point-estimate)
               :ci-lower (clojure.core/format "%.4g" ci-lower)
               :ci-upper (clojure.core/format "%.4g" ci-upper)})
            cis))))

(defmethod view/distribution-models* :kindly
  [_ {:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)]
        (when (seq fits)
          (doseq [[path fit-data] fits]
            (let [{:keys [n warning distributions best-model]} fit-data
                  metric-label (name (first path))]
              (kindly-heading (str "Distribution Models: " metric-label
                                   " (n=" n (when warning " - small sample") ")"))
              (kindly-table
               (mapv (fn [[dist result]]
                       (format-distribution-table-row dist result best-model))
                     (sort-by (fn [[_ r]] (or (:delta-aic r) Double/MAX_VALUE))
                              distributions))))))))))

(defmethod view/distribution-parameter-cis* :kindly
  [_ {:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)]
        (when (seq fits)
          (doseq [[path fit-data] fits]
            (let [{:keys [best-model parameter-cis]} fit-data
                  metric-label (name (first path))]
              (when-let [ci-rows (format-parameter-ci-rows best-model parameter-cis)]
                (kindly-heading (str "Parameter CIs: " metric-label))
                (kindly-table ci-rows)))))))))

(defmethod view/distribution-pdf* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly-heading "Distribution PDF")
      (kindly-vega-lite
       (charts.distribution/distribution-pdf-vega-spec
        data-map
        (assoc view :histogram-id :histograms)
        {:width chart-width
         :height chart-height})))))

(defmethod view/distribution-cdf* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly-heading "Distribution CDF")
      (kindly-vega-lite
       (charts.distribution/distribution-cdf-vega-spec data-map view {:width chart-width
                                                                      :height chart-height})))))

(defmethod view/distribution-qq* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly-heading "Q-Q Plot")
      (kindly-vega-lite
       (charts.quantile/distribution-qq-vega-spec data-map view {:width chart-width
                                                                 :height chart-height})))))

;;; Tail Analysis Views

(defn- get-tail-context
  "Extract common context for tail analysis views.
  Returns map with :tail-analysis-map, :tail-results, :metric-configs, :transforms,
  :samples-map, :metric->values, or nil if no data."
  [{:keys [tail-analysis-id samples-id]} data-map]
  (let [tail-analysis-id (or tail-analysis-id :tail-analysis)
        samples-id (or samples-id :samples)
        tail-analysis-map (get data-map tail-analysis-id)
        samples-map (get data-map samples-id)]
    (when tail-analysis-map
      (let [tail-results (:tail-analysis tail-analysis-map)
            metrics-defs (-> (:metrics-defs tail-analysis-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            raw-transform (:transform tail-analysis-map)
            transforms (when raw-transform
                         {:sample-> (let [s (:sample-> raw-transform)]
                                      (if (fn? s) (list s) s))
                          :->sample (let [s (:->sample raw-transform)]
                                      (if (fn? s) [s] s))})
            metric->values (when samples-map (util/metric->values samples-map))]
        (when (seq tail-results)
          {:tail-analysis-map tail-analysis-map
           :tail-results tail-results
           :metric-configs metric-configs
           :transforms transforms
           :samples-map samples-map
           :metric->values metric->values})))))

(defmethod view/tail-summary* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (core/tail-summary-table tail-data transforms)]
          (when (seq summary-rows)
            (kindly-heading (str "Tail Summary: " (:label mc)))
            (kindly-table summary-rows
                          {:column-names [:parameter :value]})))))))

(defmethod view/tail-ratios* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [ratios-rows (core/tail-ratios-table-data tail-data)]
          (when (seq ratios-rows)
            (kindly-heading (str "Tail Ratios: " (:label mc)))
            (kindly-table ratios-rows
                          {:column-names [:ratio :value :p95 :p99 :p999]})))))))

(defmethod view/tail-high-quantiles* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [quantiles-rows (core/tail-high-quantiles-table tail-data transforms)]
          (when (seq quantiles-rows)
            (kindly-heading (str "High Quantile Estimates: " (:label mc)))
            (kindly-table quantiles-rows
                          {:column-names [:quantile :estimate]})))))))

(defmethod view/tail-ratios-chart* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/tail-ratios-table tail-data)]
          (kindly-heading (str "Tail Ratios Chart: " (:label mc)))
          (kindly-vega-lite (merge {:width chart-width :height 200} chart)))))))

(defmethod view/hill-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/hill-plot tail-data)]
          (kindly-heading (str "Hill Plot: " (:label mc)))
          (kindly-vega-lite (merge {:width chart-width :height chart-height} chart)))))))

(defmethod view/mrl-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/mrl-plot tail-data transforms)]
          (kindly-heading (str "Mean Residual Life Plot: " (:label mc)))
          (kindly-vega-lite (merge {:width chart-width :height chart-height} chart)))))))

(defmethod view/zipf-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [_tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [chart (charts.tail/zipf-plot samples transforms)]
            (kindly-heading (str "Zipf Plot: " (:label mc)))
            (kindly-vega-lite (merge {:width chart-width :height chart-height} chart))))))))

(defmethod view/exponential-qq-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [chart (charts.tail/exponential-qq-plot samples threshold transforms)]
              (kindly-heading (str "Exponential Q-Q Plot: " (:label mc)))
              (kindly-vega-lite (merge {:width chart-width :height chart-height} chart)))))))))

(defmethod view/gpd-qq-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [gpd (:gpd tail-data)]
              (when-let [chart (charts.tail/gpd-qq-plot samples threshold gpd transforms)]
                (kindly-heading (str "GPD Q-Q Plot: " (:label mc)))
                (kindly-vega-lite (merge {:width chart-width :height chart-height} chart))))))))))

;;; Noop implementations for views not applicable to Kindly output

(defmethod view/final-gc-warnings* :kindly [_ _ _])
(defmethod view/os* :kindly [_ _ _])
(defmethod view/runtime* :kindly [_ _ _])

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
                             (cond-> {:width chart-width
                                      :height chart-height
                                      :title (str "ACF: " (:label mc)
                                                  " (n="
                                                  (get-in acf-data [:effective-sample-size :n-original])
                                                  ")")}
                               min-severity (assoc :min-severity min-severity)))]
              (kindly-heading (str "Autocorrelation: " (:label mc)))
              (kindly-vega-lite spec))))))))

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
          (kindly-heading "Sample Independence Classification")
          (kindly-table
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
          (kindly-heading "Effective Sample Size")
          (kindly-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format "%.2f (%s)"
                                      (:value lag-1)
                                      (get severity-labels (:severity lag-1) "unknown"))})
                    {:metric "Effective sample size"
                     :value (format "%d of %d (%.0f%%)"
                                    (:n-effective effective-sample-size)
                                    (:n-original effective-sample-size)
                                    (* 100.0 (:ratio effective-sample-size)))}
                    {:metric "CI inflation factor"
                     :value (format "%.2f×" ci-inflation-factor)}]
             true
             (->> (remove nil?) vec))))))))

;;; Modal Analysis Views

(defmethod view/multimodal-warning* :kindly
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (kindly-heading (str "WARNING: Multimodal distribution - "
                          (:label metric-config)))
     (kindly-table
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (kindly-add
        (with-meta
          ["*Mode locations:*"]
          {:kindly/kind :kind/md}))
       (kindly-table
        (mapv (fn [{:keys [location density]}]
                {:location (modal/format-mode-location
                            location metric-config transforms)
                 :density (format "%.4g" density)})
              modes))))))

;;; Domain Apply View

(defn- resolve-view-fn-without-flush
  "Resolve a view spec to a function without automatic flushing.
  Returns nil and logs a warning if the view-spec cannot be resolved.
  Used by domain-apply to accumulate all run outputs before flushing."
  [view-spec]
  (let [[view-kw opts] (if (sequential? view-spec)
                         [(first view-spec) (second view-spec)]
                         [view-spec {}])
        view-fn-var (ns-resolve 'criterium.view (symbol (name view-kw)))]
    (if view-fn-var
      (view-fn-var (or opts {}))
      (binding [*out* *err*]
        (println (format "WARNING: Unknown view-spec '%s' - no such view function in criterium.view"
                         view-kw))))))

(defmethod view/domain-apply* :kindly
  [viewer {:keys [domain-id view-spec]} data-map]
  (let [domain-id (or domain-id :domain)
        domain (get data-map domain-id)]
    (cond
      (nil? view-spec)
      (binding [*out* *err*]
        (println "WARNING: domain-apply requires :view-spec option"))

      (nil? domain)
      nil

      :else
      ;; Use direct view function resolution to avoid automatic flush
      ;; that benchmark/->view performs after each call.
      ;; For kindly, we want to accumulate all runs' output first.
      (when-let [view-fn (resolve-view-fn-without-flush view-spec)]
        (doseq [{:keys [coord data]} (domain.types/runs domain)]
          (kindly-heading (format "Run: %s" (pr-str coord)))
          (view-fn viewer data))))))
