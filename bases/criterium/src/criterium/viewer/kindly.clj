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
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts :as charts]
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
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map stats-id)]
    (when (seq metric-configs)
      (kindly-heading "Summary stats")
      (kindly-table
       (core/stats-map
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
   (charts/samples-vega-spec data-map view {:width chart-width
                                            :height chart-height})))

(defmethod view/histogram* :kindly
  [_ view data-map]
  (kindly-heading "Histogram")
  (kindly-vega-lite
   (charts/histogram-vega-spec data-map view {:width chart-width
                                              :height chart-height})))

(defmethod view/kde* :kindly
  [_ view data-map]
  (let [kde-id (or (:kde-id view) :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (kindly-heading "Kernel Density Estimation")
      (kindly-vega-lite
       (charts/kde-vega-spec data-map view {:width chart-width
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
           [(charts/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

;;; Domain view implementations

(defmethod view/domain-extract* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [{:keys [heading col-headers rows]}
                 (extract/prepare-domain-extract-table-transposed extract)]
        (kindly-heading heading)
        (kindly-table rows {:column-names col-headers})
        (let [box-spec (charts/single-point-box-chart-spec extract {:width chart-width
                                                                    :height chart-height})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (kindly-vega-lite box-spec)
            (kindly-vega-lite
             (charts/single-point-bar-chart-spec extract {:width chart-width
                                                          :height chart-height})))))

      :multi-point
      (when-let [{:keys [heading coord-header col-headers rows]}
                 (extract/prepare-domain-extract-table
                  extract {:header-sep "\n"})]
        (kindly-heading heading)
        (kindly-table rows {:column-names (into [coord-header] col-headers)})
        (kindly-vega-lite
         (charts/domain-line-chart-spec extract {:width chart-width
                                                 :height chart-height})))

      :default-table
      (when-let [{:keys [heading coord-header col-headers rows]}
                 (extract/prepare-domain-extract-table
                  extract {:header-sep "\n"})]
        (kindly-heading heading)
        (kindly-table rows {:column-names (into [coord-header] col-headers)})))))

(defmethod view/domain-grouped* :kindly
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (extract/prepare-domain-grouped-table
                                       grouped)]
      (kindly-heading heading)
      (kindly-table rows))))

(defmethod view/domain-comparison* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when-let [tables (comparison/prepare-domain-comparison-tables
                       comparison)]
      (doseq [{:keys [heading coord-header col-headers rows]} tables]
        (kindly-heading heading)
        (kindly-table rows {:column-names (into [coord-header] col-headers)}))
      (case (detection/comparison-visualization-strategy comparison)
        :single-point
        (let [box-spec (charts/comparison-box-chart-spec comparison {:width chart-width
                                                                     :height chart-height})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (kindly-vega-lite box-spec)
            (kindly-vega-lite
             (charts/comparison-bar-chart-spec comparison {:width chart-width
                                                           :height chart-height}))))
        :multi-point
        (kindly-vega-lite
         (charts/comparison-line-chart-spec comparison {:width chart-width
                                                        :height chart-height}))
        :default-table nil))))

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
       (fn [{:keys [title points line-pts residual-pts chart-opts]}]
         (kindly-heading title)
         (when (seq points)
           (kindly-vega-lite
            (charts/log-log-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :legend-options legend-options)))
           (when (seq residual-pts)
             (kindly-heading "Log-Log Residual Plot")
             (kindly-vega-lite
              (charts/log-log-residual-spec
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
            (charts/regression-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :y-title y-title
                    :legend-options legend-options)))
           (when (seq residual-pts)
             (kindly-heading "Residual Plot")
             (kindly-vega-lite
              (charts/regression-residual-spec
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
      (kindly-vega (charts/treemap-vega-spec treemap-data {})))))

;;; Bootstrap statistics view

(defmethod view/bootstrap-stats* :kindly
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (:metrics-defs bootstrap-map)
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)]
        (when (seq metric-configs)
          (kindly-heading "Bootstrap Statistics")
          (kindly-table
           (vec
            (for [m metric-configs
                  :let [stat (get-in bootstrap (:path m))]
                  :when stat]
              (bootstrap/bootstrap-stat-row m stat)))
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
        (kindly-vega (charts/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading "Call Flame Chart")
        (kindly-vega (charts/call-tree-flame-vega-spec call-tree total-calls {}))))))

(defmethod view/most-called* :kindly
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (kindly-heading (clojure.core/format "Most Called Methods (top %d, %d total calls)"
                                             (count methods) total-in-list))
        (kindly-vega-lite (charts/most-called-vega-lite-spec most-called-data {}))))))

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

(defmethod view/distribution-fit* :kindly
  [_ {:keys [distribution-fit-id kde-id] :as view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        kde-id (or kde-id :kde)
        distribution-fit-map (data-map distribution-fit-id)
        kde-map (data-map kde-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)]
        (when (seq fits)
          (doseq [[path fit-data] fits]
            (let [{:keys [n warning distributions best-model parameter-cis]} fit-data
                  metric-label (name (first path))]
              ;; Heading with sample size and warning
              (kindly-heading (str "Distribution Fit: " metric-label
                                   " (n=" n (when warning " - small sample") ")"))

              ;; Model comparison table
              (kindly-table
               (mapv (fn [[dist result]]
                       (format-distribution-table-row dist result best-model))
                     (sort-by (fn [[_ r]] (or (:delta-aic r) Double/MAX_VALUE))
                              distributions)))

              ;; Parameter CIs for best model
              (when-let [ci-rows (format-parameter-ci-rows best-model parameter-cis)]
                (kindly-heading "Best Model Parameter CIs")
                (kindly-table ci-rows))))

          ;; Charts - only if KDE data is available
          (when kde-map
            (kindly-heading "Distribution PDF")
            (kindly-vega-lite
             (charts/distribution-pdf-vega-spec
              data-map
              (assoc view :histogram-id :histograms)
              {:width chart-width
               :height chart-height}))

            (kindly-heading "Distribution CDF")
            (kindly-vega-lite
             (charts/distribution-cdf-vega-spec data-map view {:width chart-width
                                                               :height chart-height}))

            (kindly-heading "Q-Q Plot")
            (kindly-vega-lite
             (charts/distribution-qq-vega-spec data-map view {:width chart-width
                                                              :height chart-height}))))))))

;;; Noop implementations for views not applicable to Kindly output

(defmethod view/final-gc-warnings* :kindly [_ _ _])
(defmethod view/os* :kindly [_ _ _])
(defmethod view/runtime* :kindly [_ _ _])

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
