(ns criterium.viewer.pprint
  "A pretty print viewer"
  (:require
   [clojure.pprint :as pprint]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

(defmethod view/metrics* :pprint
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:metric :value]
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :pprint
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map stats-id)]
    (when (seq metric-configs)
      (pprint/print-table
       [:_metric :mean-minus-3sigma :mean :mean-plus-3sigma :min-val :max-val]
       (viewer-common/stats-map
        (util/stats stats-map)
        metric-configs
        transforms)))))

(defmethod view/quantiles* :pprint
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)
        table (viewer-common/quantiles
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
        event-stats-map (data-map event-stats-id)
        metrics-defs (:metrics-defs event-stats-map)
        res (viewer-common/event-stats
             metrics-defs
             (util/event-stats event-stats-map))
        ks (reduce into [] (map keys res))]
    (when (seq res)
      (pprint/print-table (distinct ks) res))))

(defmethod view/outlier-counts* :pprint
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:_metric :low-severe :low-mild :high-mild :high-severe]
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defn print-outlier-significances
  [{:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     (for [m metric-configs]
       (get-in outlier-sig (:path m))))))

(defmethod view/outlier-significance* :pprint
  [_ view data-map]
  (print-outlier-significances view data-map))

(defn- flatten-events [sample metrics-defs index]
  (reduce-kv
   (fn [res k metric-group]
     (reduce
      (fn [res metric-config]
        (let [v (get (get sample (:path metric-config)) index)]
          (if (pos? (long v))
            (assoc res
                   (viewer-common/composite-key
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
    [[(viewer-common/composite-key [(last path) :outlier])
      v]]))

(defmethod view/collect-plan* :pprint
  [_ _view data-map]
  (pprint/print-table
   [:phase :num-samples :batch-size :num-evals]
   (viewer-common/collect-plan-data data-map)))

(defmethod view/samples* :pprint
  [_ {:keys [] :as view} banech-map]
  (let [quant-samples-id (:samples-id view :samples)
        event-samples-id (:event-samples-id view quant-samples-id)
        outlier-analysis-id (:outlier-id view :outliers)
        quant-samples (banech-map quant-samples-id)
        event-samples (banech-map event-samples-id)
        outlier-analysis (banech-map outlier-analysis-id)

        metric-defs (metric/filter-metrics
                     (:metrics-defs quant-samples)
                     (metric/type-pred :quantitative))
        event-metrics-defs (metric/filter-metrics
                            (:metrics-defs event-samples)
                            (metric/type-pred :event))

        metric-configs (metric/all-metric-configs metric-defs)
        transforms (util/get-transforms banech-map quant-samples-id)

        quant-ids (mapv (comp last :path) metric-configs)
        event-keys (into
                    []
                    (mapcat
                     (fn [[k metric-group]]
                       (reduce
                        (fn [res metric-config]
                          (conj res
                                (viewer-common/composite-key
                                 [(if-let [group (:group metric-config)]
                                    group
                                    k)
                                  (last (:path metric-config))])))
                        []
                        (or (:values metric-group)
                            (mapcat :values
                                    (vals (:groups metric-group)))))))
                    event-metrics-defs)
        outlier-keys (when outlier-analysis
                       (mapv
                        #(viewer-common/composite-key [(last %) :outlier])
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
    (pprint/print-table all-keys data)))

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
                         #(viewer-common/histogram
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
       (viewer-common/column-data->maps
        h
        [:centers :counts :density]
        {:centers #(format "%-7.3g" %)
         :density #(format "%-7.3g" %)}))
      (println))))

(defn- kde-modes-table
  "Prepare modes data for pprint table display."
  [modes metric-config transforms]
  (mapv (fn [{:keys [location density ci-lower ci-upper]}]
          {:location (viewer-common/format-mode-location
                      location metric-config transforms)
           :density (format "%.4g" density)
           :ci-lower (viewer-common/format-mode-location
                      ci-lower metric-config transforms)
           :ci-upper (viewer-common/format-mode-location
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

(defmethod view/sample-percentiles* :pprint
  [_ _view _banch-map]
  ;; TODO
  )
(defmethod view/domain-extract* :pprint
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when-let [{:keys [heading coord-header col-headers rows]}
               (viewer-common/prepare-domain-extract-table extract {:header-sep " "})]
      (println heading)
      ;; pprint/print-table needs string keys for column headers to display
      ;; without colon prefix; transform the coord key from keyword to string
      (let [coord-key (keyword coord-header)
            pprint-rows (mapv #(-> %
                                   (assoc coord-header (get % coord-key))
                                   (dissoc coord-key))
                              rows)]
        (pprint/print-table (into [coord-header] col-headers) pprint-rows)))))

(defmethod view/domain-grouped* :pprint
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (viewer-common/prepare-domain-grouped-table
                                       grouped)]
      (println heading)
      (pprint/print-table [:axis-value :run-count] rows))))

(defmethod view/domain-comparison* :pprint
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when-let [tables (viewer-common/prepare-domain-comparison-tables comparison)]
      (doseq [{:keys [heading coord-header col-headers rows]} tables]
        (println heading)
        (pprint/print-table (into [coord-header] col-headers) rows)))))

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
                    table-rows (viewer-common/prepare-regression-model-table-multi-impl
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
              (let [table-rows (viewer-common/prepare-regression-model-table
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
                    :call-site (viewer-common/format-call-site call-site nil)})
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
      (println (viewer-common/render-ascii-treemap treemap-data)))))

;;; Modal Analysis Views

(defn- modes-table
  "Prepare modes data for pprint table display in warning output."
  [modes metric-config transforms]
  (mapv (fn [{:keys [location density]}]
          {:location (viewer-common/format-mode-location
                      location metric-config transforms)
           :density (format "%.4g" density)})
        modes))

(defmethod view/multimodal-warning* :pprint
  [_ {:keys [modes-id]} data-map]
  (let [modes-id (or modes-id :modes)
        modes-map (get data-map modes-id)]
    (when modes-map
      (let [transforms (util/get-transforms data-map modes-id)
            metrics-defs (:metrics-defs modes-map)
            metric-configs (when metrics-defs
                             (metric/all-metric-configs metrics-defs))
            all-modes (:modes modes-map)]
        (doseq [metric-config metric-configs]
          (when-let [modes-data (get all-modes (:path metric-config))]
            (let [n-modes (:n-modes modes-data)
                  modes (:modes modes-data)]
              (when (and n-modes (> n-modes 1))
                (println)
                (println (format "WARNING: Multimodal distribution detected for %s"
                                 (:label metric-config)))
                (pprint/print-table
                 [:metric :value]
                 [{:metric "Mode count" :value n-modes}
                  {:metric "Status" :value "Consider investigating the source of variation"}])
                (when (seq modes)
                  (println "\nMode locations:")
                  (pprint/print-table
                   [:location :density]
                   (modes-table modes metric-config transforms)))))))))))
