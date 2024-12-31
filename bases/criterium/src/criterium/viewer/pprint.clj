(ns criterium.viewer.pprint
  "A pretty print viewer"
  (:require
   [clojure.pprint :as pprint]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

(defmethod view/metrics* :pprint
  [{:keys [samples-id]} bench-map]
  (let [samples-id      (or samples-id :samples)
        metrics-samples (-> bench-map :data samples-id)
        metrics-defs    (:metrics-defs metrics-samples)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:metric :value]
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :pprint
  [{:keys [stats-id]} bench-map]
  (let [stats-id       (or stats-id :stats)
        stats-map      (-> bench-map :data stats-id)
        metrics-defs   (:metrics-defs stats-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:metric :mean-minus-3sigma :mean :mean-plus-3sigma :min-val :max-val]
     (viewer-common/stats-map
      (util/stats stats-map)
      metric-configs))))

(defmethod view/quantiles* :pprint
  [{:keys [quantiles-id]} bench-map]
  (let [quantiles-id   (or quantiles-id :quantiles)
        quantiles-map  (-> bench-map :data quantiles-id)
        metric-configs (:metric-configs quantiles-map)
        table          (viewer-common/quantiles
                        metric-configs
                        (util/quantiles quantiles-map))]
    (pprint/print-table
     (into [:metric]
           (->> table first keys (filter #(not= % :metric)) sort))
     table)))

(defmethod view/event-stats* :pprint
  [{:keys [event-stats-id]} bench-map]
  (let [event-stats-id  (or event-stats-id :event-stats)
        event-stats-map (-> bench-map :data event-stats-id)
        metrics-defs    (:metrics-defs event-stats-map)
        res             (viewer-common/event-stats
                         metrics-defs
                         (util/event-stats event-stats-map))
        ks              (reduce into [] (map keys res))]
    (pprint/print-table (distinct ks) res)))

(defmethod view/outlier-counts* :pprint
  [{:keys [outliers-id] :as _view} bench-map]
  (let [outliers-id    (or outliers-id :outliers)
        outliers-map   (-> bench-map :data outliers-id)
        metrics-defs   (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     [:metric :low-severe :low-mild :high-mild :high-severe]
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defn print-outlier-significances
  [{:keys [outlier-significance-id] :as _view} bench-map]
  (let [outlier-sig-id  (or outlier-significance-id :outlier-significance)
        outlier-sig-map (-> bench-map :data outlier-sig-id)
        outlier-sig     (util/outlier-significance outlier-sig-map)
        metrics-defs    (:metrics-defs outlier-sig-map)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (pprint/print-table
     (for [m metric-configs]
       (get-in outlier-sig (:path m))))))

(defmethod view/outlier-significance* :pprint
  [view bench-map]
  (print-outlier-significances view bench-map))

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
  [_view bench-map]
  (pprint/print-table
   [:phase :num-samples :batch-size :num-evals]
   (viewer-common/collect-plan-data bench-map)))

(defmethod view/samples* :pprint
  [{:keys [] :as view} banech-map]
  (let [quant-samples-id    (:samples-id view :samples)
        event-samples-id    (:event-samples-id view quant-samples-id)
        outlier-analysis-id (:outlier-id view :outliers)
        quant-samples       (-> banech-map :data quant-samples-id)
        event-samples       (-> banech-map :data event-samples-id)
        outlier-analysis    (-> banech-map :data outlier-analysis-id)

        metric-configs     (:metrics-defs quant-samples)
        event-metrics-defs (:metrics-defs event-samples)

        transforms (util/get-transforms (:data banech-map) quant-samples-id)

        quant-ids    (mapv (comp last :path) metric-configs)
        event-keys   (vec
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
                                      (vals (:groups metric-group))))))
                       event-metrics-defs))
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
  [_view _bench-map]
  ;; TODO
  )

(defmethod view/sample-percentiles* :pprint
  [_view _banch-map]
  ;; TODO
  )
