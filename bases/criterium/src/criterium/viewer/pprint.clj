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
  (let [samples-id      (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs    (:metrics-defs metrics-samples)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:metric :value]
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :pprint
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id       (or stats-id :stats)
        stats-map      (data-map stats-id)
        metrics-defs   (-> (:metrics-defs stats-map)
                           (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map stats-id)]
    (pprint/print-table
     [:_metric :mean-minus-3sigma :mean :mean-plus-3sigma :min-val :max-val]
     (viewer-common/stats-map
      (util/stats stats-map)
      metric-configs
      transforms))))

(defmethod view/quantiles* :pprint
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id   (or quantiles-id :quantiles)
        quantiles-map  (data-map quantiles-id)
        metrics-defs   (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map quantiles-id)
        table          (viewer-common/quantiles
                        metric-configs
                        (util/quantiles quantiles-map)
                        transforms)]
    (pprint/print-table
     (into [:metric]
           (->> table first keys (filter #(not= % :metric)) sort))
     table)))

(defmethod view/event-stats* :pprint
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id  (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs    (:metrics-defs event-stats-map)
        res             (viewer-common/event-stats
                         metrics-defs
                         (util/event-stats event-stats-map))
        ks              (reduce into [] (map keys res))]
    (pprint/print-table (distinct ks) res)))

(defmethod view/outlier-counts* :pprint
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id    (or outliers-id :outliers)
        outliers-map   (data-map outliers-id)
        metrics-defs   (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:_metric :low-severe :low-mild :high-mild :high-severe]
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defn print-outlier-significances
  [{:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id  (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig     (util/outlier-significance outlier-sig-map)
        metrics-defs    (:metrics-defs outlier-sig-map)
        metric-configs  (metric/all-metric-configs metrics-defs)]
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
        (let [v (get (get sample  (:path metric-config)) index)]
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
  (let [quant-samples-id    (:samples-id view :samples)
        event-samples-id    (:event-samples-id view quant-samples-id)
        outlier-analysis-id (:outlier-id view :outliers)
        quant-samples       (banech-map quant-samples-id)
        event-samples       (banech-map event-samples-id)
        outlier-analysis    (banech-map outlier-analysis-id)

        metric-defs        (metric/filter-metrics
                            (:metrics-defs quant-samples)
                            (metric/type-pred :quantitative))
        event-metrics-defs (metric/filter-metrics
                            (:metrics-defs event-samples)
                            (metric/type-pred :event))

        metric-configs       (metric/all-metric-configs metric-defs)
        event-metric-configs (metric/all-metric-configs event-metrics-defs)

        transforms (util/get-transforms banech-map quant-samples-id)

        quant-ids    (mapv (comp last :path) metric-configs)
        event-keys   (into
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
        data     (mapv
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
  (let [histogram-id   (or histogram-id :histograms)
        histograms     (util/lookup-data data-map histogram-id)
        metrics-defs   (-> (:metrics-defs histograms)
                           (metric/filter-metrics
                            (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map histogram-id)
        histograms     (->> metric-configs
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
      (println))) )

(defmethod view/sample-percentiles* :pprint
  [_ _view _banch-map]
  ;; TODO
  )
