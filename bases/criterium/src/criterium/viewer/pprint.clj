(ns criterium.viewer.pprint
  "A pretty print viewer"
  (:require
   [clojure.pprint :as pprint]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

(defmethod view/metrics* :pprint
  [{:keys [samples-id metric-ids]} sampled]
  (let [samples-id     (or samples-id :samples)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (pprint/print-table
     [:metric :value]
     (viewer-common/metrics-map
      (sampled samples-id)
      metric-configs))))

(defmethod view/stats* :pprint
  [{:keys [stats-id metric-ids]} sampled]
  (let [stats-id       (or stats-id :stats)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (pprint/print-table
     [:metric :mean-minus-3sigma :mean :mean-plus-3sigma :min-val :max-val]
     (viewer-common/stats-map (get sampled stats-id) metric-configs))))

(defmethod view/quantiles* :pprint
  [{:keys [quantiles-id metric-ids]} sampled]
  (let [quantiles-id   (or quantiles-id :quantiles)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)
        table          (viewer-common/quantiles
                        metric-configs
                        (get sampled quantiles-id))]
    (pprint/print-table
     (into [:metric]
           (->> table first keys (filter #(not= % :metric)) sort))
     table)))

(defmethod view/event-stats* :pprint
  [{:keys [event-stats-id metric-ids]} sampled]
  (let [event-stats-id  (or event-stats-id :event-stats)
        metrics-configs (metric/metrics-of-type
                         (:metrics-configs sampled)
                         :event metric-ids)
        res             (viewer-common/event-stats
                         metrics-configs
                         (get sampled event-stats-id))
        ks              (reduce into [] (map keys res))]
    (pprint/print-table (distinct ks) res)))

(defmethod view/outlier-counts* :pprint
  [{:keys [metric-ids outliers-id] :as _view} sampled]
  (let [outliers-id    (or outliers-id :outliers)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (pprint/print-table
     [:metric :low-severe :low-mild :high-mild :high-severe]
     (viewer-common/outlier-counts
      metric-configs
      (get sampled outliers-id)))))

(defn print-outlier-significances
  [{:keys [metric-ids outlier-significance-id] :as _view} sampled]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig    (get sampled outlier-sig-id)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (pprint/print-table
     (for [m metric-configs]
       (get-in outlier-sig (:path m))))))

(defmethod view/outlier-significance* :pprint
  [view sampled]
  (print-outlier-significances view sampled))

(defn- flatten-events [sample metrics-configs index]
  (reduce-kv
   (fn [res k metric-group]
     (reduce
      (fn [res metric-config]
        (let [v (get (get sample  (:path metric-config)) index)]
          (if (pos? v)
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
   metrics-configs))

(defn- outlier-values [outlier-analysis path index]
  (when-let [v (some-> outlier-analysis
                       (get-in path)
                       :outliers
                       (get index))]
    [[(viewer-common/composite-key [(last path) :outlier])
      v]]))

(defmethod view/collect-plan* :pprint
  [_view sampled]
  (pprint/print-table
   [:phase :num-samples :batch-size :num-evals]
   (viewer-common/collect-plan-data sampled)))

(defmethod view/samples* :pprint
  [{:keys [metric-ids] :as view} sampled]
  (let [quant-samples-id    (:samples-id view :samples)
        event-samples-id    (:event-samples-id view quant-samples-id)
        outlier-analysis-id (:outlier-id view :outliers)
        quant-samples       (get sampled quant-samples-id)
        event-samples       (get sampled event-samples-id)
        outlier-analysis    (get sampled outlier-analysis-id)

        metric-configs        (metric/metric-configs-of-type
                               (:metrics-configs sampled)
                               :quantitative metric-ids)
        event-metrics-configs (metric/metrics-of-type
                               (:metrics-configs sampled)
                               :event metric-ids)

        transforms (util/get-transforms sampled quant-samples-id)

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
                       event-metrics-configs))
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
                       event-samples event-metrics-configs index)]))
                  (range (-> quant-samples
                             (get (:path (first metric-configs)))
                             count)))]
    (pprint/print-table all-keys data)))

(defmethod view/histogram* :pprint
  [_view _sampled]
  ;; TODO
  )

(defmethod view/sample-percentiles* :pprint
  [_view _sampled]
  ;; TODO
  )
