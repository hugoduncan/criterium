(ns criterium.viewer.common.tail
  "Common tail analysis context extraction for portal and kindly viewers.

  Provides shared data extraction that both graphical viewers need for
  tail analysis views including samples access for charts."
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]))

(defn get-tail-context
  "Extract common context for tail analysis views.

  Returns map with :tail-analysis-map, :tail-results, :metric-configs,
  :transforms, :samples-map, :metric->values, or nil if no data.

  This function is shared by portal and kindly viewers. The print viewer
  uses a simpler version (get-tail-analysis-context) that doesn't require
  samples data."
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
