(ns criterium.viewer.pprint
  "A pretty print viewer"
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.common.autocorrelation :as acf-common]
   [criterium.viewer.common.bootstrap :as bootstrap]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.regression :as regression]
   ;; Required for delegating :pprint methods to :print implementations
   [criterium.viewer.print]))

(defmethod view/metrics* :pprint
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:metric :value]
     (core/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :pprint
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (pprint/print-table
           [:_metric :mean-minus-3sigma :mean :mean-plus-3sigma :min-val :max-val]
           (core/stats-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

(defmethod view/extremes* :pprint
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (println "Extremes:")
          (pprint/print-table
           [:metric :min :max]
           (core/extremes-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

(defmethod view/quantiles* :pprint
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)
        table (core/quantiles
               metric-configs
               (util/quantiles quantiles-map)
               transforms)]
    (pprint/print-table
     (into [:metric]
           (->> table first keys (filter #(not= % :metric)) sort))
     table)))

(defmethod view/event-stats* :pprint
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (:metrics-defs event-stats-map)
            res (core/event-stats
                 metrics-defs
                 (util/event-stats event-stats-map))
            ks (reduce into [] (map keys res))]
        (when (seq res)
          (pprint/print-table (distinct ks) res))))))

(defmethod view/outlier-counts* :pprint
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:_metric :low-severe :low-mild :high-mild :high-severe]
     (core/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defn print-outlier-significances
  [{:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        significances (for [m metric-configs
                            :let [data (get-in outlier-sig (:path m))]
                            :when (:significance data)]
                        data)]
    (when (seq significances)
      (pprint/print-table significances))))

(defmethod view/outlier-significance* :pprint
  [_ view data-map]
  (print-outlier-significances view data-map))

(defmethod view/bootstrap-stats* :pprint
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)
        metrics-defs (:metrics-defs bootstrap-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        bootstrap (util/bootstrap bootstrap-map)
        transforms (util/get-transforms data-map bootstrap-stats-id)]
    (when (seq metric-configs)
      (println "\nBootstrap Statistics:")
      (pprint/print-table
       [:metric :median :median-ci-lower :median-ci-upper
        :mean :mean-ci-lower :mean-ci-upper
        :p10 :p90]
       (for [m metric-configs
             :let [stat (get-in bootstrap (:path m))]
             :when stat]
         (bootstrap/bootstrap-stat-row m stat transforms))))))

(defn- flatten-events [sample metrics-defs index]
  (reduce-kv
   (fn [res k metric-group]
     (reduce
      (fn [res metric-config]
        (let [v (get (get sample (:path metric-config)) index)]
          (if (pos? (long v))
            (assoc res
                   (core/composite-key
                    [(if-let [group (:group metric-config)]
                       group
                       k)
                     (last (:path metric-config))])
                   v)
            res)))
      res
      (or (:values metric-group)
          (mapcat :values (vals (:groups metric-group))))))
   {}
   metrics-defs))

(defn- outlier-values [outlier-analysis path index]
  (when-let [v (some-> outlier-analysis
                       (get-in path)
                       :outliers
                       (get index))]
    [[(core/composite-key [(last path) :outlier])
      v]]))

(defmethod view/collect-plan* :pprint
  [_ _view data-map]
  (pprint/print-table
   [:phase :num-samples :batch-size :num-evals]
   (core/collect-plan-data data-map)))

;; When :show-chart is true, delegates to :print for ASCII chart rendering
(defmethod view/samples* :pprint
  [viewer {:keys [show-chart] :as view} bench-map]
  (if show-chart
    ;; Delegate to :print for chart rendering
    ((get-method view/samples* :print) viewer view bench-map)
    ;; Default pprint table output
    (let [quant-samples-id (:samples-id view :samples)
          event-samples-id (:event-samples-id view quant-samples-id)
          outlier-analysis-id (:outlier-id view :outliers)
          quant-samples (bench-map quant-samples-id)
          event-samples (bench-map event-samples-id)
          outlier-analysis (bench-map outlier-analysis-id)

          metric-defs (metric/filter-metrics
                       (:metrics-defs quant-samples)
                       (metric/type-pred :quantitative))
          event-metrics-defs (metric/filter-metrics
                              (:metrics-defs event-samples)
                              (metric/type-pred :event))

          metric-configs (metric/all-metric-configs metric-defs)
          transforms (util/get-transforms bench-map quant-samples-id)

          quant-ids (mapv (comp last :path) metric-configs)
          event-keys (into
                      []
                      (mapcat
                       (fn [[k metric-group]]
                         (reduce
                          (fn [res metric-config]
                            (conj res
                                  (core/composite-key
                                   [(if-let [group (:group metric-config)]
                                      group
                                      k)
                                    (last (:path metric-config))])))
                          []
                          (or (:values metric-group)
                              (mapcat :values
                                      (vals (:groups metric-group))))))
                       event-metrics-defs))
          outlier-keys (when outlier-analysis
                         (mapv
                          #(core/composite-key [(last %) :outlier])
                          (mapv :path metric-configs)))

          all-keys (reduce into [:index] [quant-ids outlier-keys event-keys])
          data (mapv
                (fn [index]
                  (reduce
                   merge
                   {:index index}
                   [(reduce
                     (fn [res path]
                       (into
                        (assoc res (last path)
                               (util/transform-sample->
                                (get (get quant-samples path) index)
                                transforms))
                        (outlier-values outlier-analysis path index)))
                     {}
                     (mapv :path metric-configs))
                    (flatten-events
                     event-samples event-metrics-defs index)]))
                (range (-> quant-samples
                           (get (:path (first metric-configs)))
                           count)))]
      (pprint/print-table all-keys data))))

(defmethod view/histogram* :pprint
  [_ {:keys [histogram-id] :as _view} data-map]
  (let [histogram-id (or histogram-id :histograms)
        histograms (util/lookup-data data-map histogram-id)
        metrics-defs (-> (:metrics-defs histograms)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map histogram-id)
        histograms (->> metric-configs
                        (mapv
                         #(core/histogram
                           (have
                            some?
                            ((:histograms histograms) (:path %))
                            {:keys (keys (:histograms histograms))
                             :path %})
                           transforms
                           %)))]
    (doseq [h histograms]
      (println (format "\nHistogram of %s %s"
                       (-> h :metric-config :label)
                       (:unit h)))
      (pprint/print-table
       [:centers :counts :density]
       (core/column-data->maps
        h
        [:centers :counts :density]
        {:centers #(format "%-7.3g" %)
         :density #(format "%-7.3g" %)}))
      (println))))

(defn- kde-modes-table
  "Prepare modes data for pprint table display."
  [modes metric-config transforms]
  (mapv (fn [{:keys [location density ci-lower ci-upper]}]
          {:location (modal/format-mode-location
                      location metric-config transforms)
           :density (format "%.4g" density)
           :ci-lower (modal/format-mode-location
                      ci-lower metric-config transforms)
           :ci-upper (modal/format-mode-location
                      ci-upper metric-config transforms)})
        modes))

(defn- pprint-kde-metric
  "Pretty-print KDE summary for a single metric."
  [metric-config kde-data transforms]
  (let [{:keys [bandwidth modes n]} kde-data
        {:keys [label _dimension scale]} metric-config
        bw (* (double scale) (util/transform-sample-> bandwidth transforms))]
    (println (format "\nKDE of %s (n=%d, bandwidth=%.4g)"
                     label n bw))
    (when (seq modes)
      (pprint/print-table
       [:location :density :ci-lower :ci-upper]
       (kde-modes-table modes metric-config transforms)))
    (println)))

(defmethod view/kde* :pprint
  [_ {:keys [kde-id] :as _view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (let [metrics-defs (-> (:metrics-defs kde-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map kde-id)
            kdes (:kdes kde-map)]
        (doseq [metric-config metric-configs]
          (when-let [kde-data (get kdes (:path metric-config))]
            (pprint-kde-metric metric-config kde-data transforms)))))))

;; Delegates to :print for ASCII chart rendering
(defmethod view/sample-percentiles* :pprint
  [viewer view-opts data-map]
  ((get-method view/sample-percentiles* :print) viewer view-opts data-map))

;; Delegates to :print for ASCII chart rendering
(defmethod view/sample-diffs* :pprint
  [viewer view-opts data-map]
  ((get-method view/sample-diffs* :print) viewer view-opts data-map))
(defmethod view/domain-extract-table* :pprint
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when-let [{:keys [heading coord-header col-headers rows]}
               (extract/prepare-domain-extract-table extract {:header-sep " "})]
      (println heading)
      ;; Rows already use string keys matching column headers
      (pprint/print-table (into [coord-header] col-headers) rows))))

;; Delegates to :print for ASCII chart rendering
(defmethod view/domain-extract-chart* :pprint
  [viewer view-opts data-map]
  ((get-method view/domain-extract-chart* :print) viewer view-opts data-map))

(defmethod view/domain-grouped* :pprint
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (extract/prepare-domain-grouped-table
                                       grouped)]
      (println heading)
      (pprint/print-table [:axis-value :run-count] rows))))

(defmethod view/domain-comparison-table* :pprint
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
      (doseq [{:keys [heading coord-header col-headers rows]} tables]
        (println heading)
        (pprint/print-table (into [coord-header] col-headers) rows)))))

;; Delegates to :print for ASCII chart rendering
(defmethod view/domain-comparison-chart* :pprint
  [viewer view-opts data-map]
  ((get-method view/domain-comparison-chart* :print) viewer view-opts data-map))

(defmethod view/domain-regression* :pprint
  [_ {:keys [regression-id tolerance]} data-map]
  (let [regression-id (or regression-id :regression)
        regression (data-map regression-id)
        tolerance (or tolerance 0.01)
        table-options {:best-fit-marker "<- best"
                       :plotted-marker "[plotted]"
                       :tolerance tolerance}]
    (when regression
      (let [{:keys [axis regressions impl-axis implementations]} regression
            multi-impl? (> (count implementations) 1)]
        (if multi-impl?
          ;; Multi-implementation mode
          (doseq [[_metric-id {:keys [metric by-impl]}] regressions]
            (println (format "Domain Regression (axis: %s, metric: %s, by: %s)"
                             (name axis) (pr-str metric) (name impl-axis)))
            (if (seq by-impl)
              (let [impl-keys (sort (keys by-impl))
                    table-rows (regression/prepare-regression-model-table-multi-impl
                                by-impl impl-keys table-options)]
                (pprint/print-table
                 [:implementation :model :r-squared :equation :best-fit]
                 table-rows))
              (println "  (no implementations)"))
            (println))

          ;; Single-implementation mode
          (doseq [[_metric-id {:keys [metric models best-fit]}] regressions]
            (println (format "Domain Regression (axis: %s, metric: %s)"
                             (name axis) (pr-str metric)))
            (if (seq models)
              (let [table-rows (regression/prepare-regression-model-table
                                {:models models :best-fit best-fit}
                                table-options)]
                (pprint/print-table [:model :r-squared :equation :best-fit] table-rows))
              (println "  (insufficient data for regression)"))
            (println)))))))

;;; Allocation Views

(defmethod view/allocation-summary* :pprint
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
        (println "Allocation Summary:")
        (pprint/print-table
         [:metric :value]
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
           :value (format "%.1f%%" (* 100.0 (double freed-ratio)))}])))))

(defmethod view/allocation-hotspots* :pprint
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)]
        (when (seq hotspots)
          (println "Allocation Hotspots:")
          (pprint/print-table
           [:count :bytes :freed-count :freed-bytes :object-type :call-site]
           (mapv (fn [{:keys [call-site object-type count bytes freed-count freed-bytes]}]
                   {:count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes
                    :object-type (or object-type "")
                    :call-site (allocation/format-call-site call-site nil)})
                 hotspots)))))))

(defmethod view/allocation-by-type* :pprint
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (println "Allocations by Type:")
          (pprint/print-table
           [:type :count :bytes :freed-count :freed-bytes]
           (mapv (fn [[type-name {:keys [count bytes freed-count freed-bytes]}]]
                   {:type type-name
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 sorted)))))))

(defmethod view/allocation-treemap* :pprint
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (println)
      (println (allocation/render-ascii-treemap treemap-data)))))

;;; Modal Analysis Views

(defn- modes-table
  "Prepare modes data for pprint table display in warning output."
  [modes metric-config transforms]
  (mapv (fn [{:keys [location density]}]
          {:location (modal/format-mode-location
                      location metric-config transforms)
           :density (format "%.4g" density)})
        modes))

(defmethod view/multimodal-warning* :pprint
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (println)
     (println (format "WARNING: Multimodal distribution detected for %s"
                      (:label metric-config)))
     (pprint/print-table
      [:metric :value]
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (println "\nMode locations:")
       (pprint/print-table
        [:location :density]
        (modes-table modes metric-config transforms))))))

;;; Autocorrelation Views

(defn- autocorrelation-summary-table
  "Build autocorrelation summary table rows for pprint."
  [acf-data _metric-label]
  (let [{:keys [acf lag-1 effective-sample-size ci-inflation-factor
                ljung-box classification pattern detected-period
                anomalous-lags lag-severities]} acf-data
        rows [{:metric "Lag-1 autocorrelation"
               :value (format "%.2f (%s)"
                              (:value lag-1)
                              (acf-common/format-severity (:severity lag-1)))}
              {:metric "Effective sample size"
               :value (format "%d of %d (%.0f%%)"
                              (:n-effective effective-sample-size)
                              (:n-original effective-sample-size)
                              (* 100.0
                                 (double (:ratio effective-sample-size))))}
              {:metric "CI inflation factor"
               :value (format "%.2f×" ci-inflation-factor)}
              {:metric "Ljung-Box p-value"
               :value (format "%.2f" (:p-value ljung-box))}
              {:metric "Assessment"
               :value (str/capitalize (name classification))}]]
    (cond-> rows
      (seq anomalous-lags)
      (conj {:metric "Anomalous lags"
             :value (acf-common/format-anomalous-lags anomalous-lags lag-severities)})

      (and (#{:warning :fail} classification)
           (acf-common/displayable-patterns pattern))
      (conj {:metric "Pattern"
             :value (get acf-common/pattern-labels pattern (name pattern))})

      (acf-common/format-detected-period detected-period acf lag-severities)
      (conj {:metric "Suspected period"
             :value (acf-common/format-detected-period detected-period acf lag-severities)})

      (and (#{:warning :fail} classification)
           (acf-common/displayable-patterns pattern)
           (get acf-common/pattern-recommendations pattern))
      (conj {:metric "Recommendation"
             :value (get acf-common/pattern-recommendations pattern)}))))

(defmethod view/autocorrelation* :pprint
  [_ view data-map]
  (acf-common/with-autocorrelation-metrics view data-map
    (fn [acf-data mc]
      (println (format "\nSample Independence (%s):" (:label mc)))
      (pprint/print-table
       [:metric :value]
       (autocorrelation-summary-table acf-data (:label mc))))))

;; ACF plot is a no-op for pprint viewer (charts not supported)
(defmethod view/acf-plot* :pprint [_ _ _])

(defn- classification-table-rows
  "Build classification table rows for pprint."
  [class-data acf-data metric-label]
  (let [combined (merge class-data
                        (select-keys acf-data [:lag-1 :acf :lag-severities]))
        {:keys [acf lag-1 lag-severities ljung-box classification pattern
                detected-period anomalous-lags]} combined
        rows [{:metric (str metric-label " Lag-1")
               :value (format "%.2f (%s)"
                              (:value lag-1)
                              (acf-common/format-severity (:severity lag-1)))}
              {:metric "Ljung-Box p-value"
               :value (format "%.2f" (:p-value ljung-box))}
              {:metric "Assessment"
               :value (str/capitalize (name classification))}]]
    (cond-> rows
      (seq anomalous-lags)
      (conj {:metric "Anomalous lags"
             :value (acf-common/format-anomalous-lags anomalous-lags lag-severities)})

      (and (#{:warning :fail} classification)
           (acf-common/displayable-patterns pattern))
      (conj {:metric "Pattern"
             :value (get acf-common/pattern-labels pattern (name pattern))})

      (acf-common/format-detected-period detected-period acf lag-severities)
      (conj {:metric "Suspected period"
             :value (acf-common/format-detected-period detected-period acf lag-severities)})

      (and (#{:warning :fail} classification)
           (acf-common/displayable-patterns pattern)
           (get acf-common/pattern-recommendations pattern))
      (conj {:metric "Recommendation"
             :value (get acf-common/pattern-recommendations pattern)}))))

(defmethod view/autocorrelation-classification* :pprint
  [_ view data-map]
  (let [metrics (acf-common/collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (println "\nSample Independence Classification:")
      (doseq [[class-data acf-data mc] metrics]
        (pprint/print-table
         [:metric :value]
         (classification-table-rows class-data acf-data (:label mc)))))))

(defn- ess-table-rows
  "Build effective sample size table rows for pprint."
  [ess-data acf-data metric-label]
  (let [combined (merge ess-data (select-keys acf-data [:lag-1]))
        {:keys [lag-1 effective-sample-size ci-inflation-factor]} combined
        rows (cond-> []
               lag-1
               (conj {:metric (str metric-label " Lag-1")
                      :value (format "%.2f (%s)"
                                     (:value lag-1)
                                     (acf-common/format-severity
                                      (:severity lag-1)))}))]
    (conj rows
          {:metric "Effective sample size"
           :value (format "%d of %d (%.0f%%)"
                          (:n-effective effective-sample-size)
                          (:n-original effective-sample-size)
                          (* 100.0 (double (:ratio effective-sample-size))))}
          {:metric "CI inflation factor"
           :value (format "%.2f×" ci-inflation-factor)})))

(defmethod view/effective-sample-size* :pprint
  [_ view data-map]
  (let [metrics (acf-common/collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (println "\nEffective Sample Size:")
      (doseq [[ess-data acf-data mc] metrics]
        (pprint/print-table
         [:metric :value]
         (ess-table-rows ess-data acf-data (:label mc)))))))

;;; Domain Apply View

;; Delegates to :print since the output format is identical
(defmethod view/domain-apply* :pprint
  [viewer view-opts data-map]
  ((get-method view/domain-apply* :print) viewer view-opts data-map))

;;; Missing :pprint implementations that delegate to :print

;; Shape statistics
(defmethod view/shape-stats* :pprint
  [viewer view-opts data-map]
  ((get-method view/shape-stats* :print) viewer view-opts data-map))

;; GC warnings
(defmethod view/final-gc-warnings* :pprint
  [viewer view-opts data-map]
  ((get-method view/final-gc-warnings* :print) viewer view-opts data-map))

;; OS info
(defmethod view/os* :pprint
  [viewer view-opts data-map]
  ((get-method view/os* :print) viewer view-opts data-map))

;; Runtime info
(defmethod view/runtime* :pprint
  [viewer view-opts data-map]
  ((get-method view/runtime* :print) viewer view-opts data-map))

;; Distribution model comparison
(defmethod view/distribution-models* :pprint
  [viewer view-opts data-map]
  ((get-method view/distribution-models* :print) viewer view-opts data-map))

;; Distribution parameter CIs
(defmethod view/distribution-parameter-cis* :pprint
  [viewer view-opts data-map]
  ((get-method view/distribution-parameter-cis* :print) viewer view-opts data-map))

;; Tail summary (GPD/Hill)
(defmethod view/tail-summary* :pprint
  [viewer view-opts data-map]
  ((get-method view/tail-summary* :print) viewer view-opts data-map))

;; Tail ratios
(defmethod view/tail-ratios* :pprint
  [viewer view-opts data-map]
  ((get-method view/tail-ratios* :print) viewer view-opts data-map))

;; High quantile estimates
(defmethod view/tail-high-quantiles* :pprint
  [viewer view-opts data-map]
  ((get-method view/tail-high-quantiles* :print) viewer view-opts data-map))

;; Call tree (ASCII)
(defmethod view/call-tree* :pprint
  [viewer view-opts data-map]
  ((get-method view/call-tree* :print) viewer view-opts data-map))

;; Flame chart (message only)
(defmethod view/call-flame* :pprint
  [viewer view-opts data-map]
  ((get-method view/call-flame* :print) viewer view-opts data-map))

;; Most called methods table
(defmethod view/most-called* :pprint
  [viewer view-opts data-map]
  ((get-method view/most-called* :print) viewer view-opts data-map))

;;; Chart no-ops (charts cannot render in pprint text output)

;; Distribution charts
(defmethod view/distribution-pdf* :pprint [_ _ _])
(defmethod view/distribution-cdf* :pprint [_ _ _])
(defmethod view/distribution-qq* :pprint [_ _ _])

;; Tail analysis charts
(defmethod view/tail-ratios-chart* :pprint [_ _ _])
(defmethod view/hill-plot* :pprint [_ _ _])
(defmethod view/mrl-plot* :pprint [_ _ _])
(defmethod view/zipf-plot* :pprint [_ _ _])
(defmethod view/exponential-qq-plot* :pprint [_ _ _])
(defmethod view/gpd-qq-plot* :pprint [_ _ _])
