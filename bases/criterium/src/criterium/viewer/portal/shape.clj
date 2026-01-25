(ns criterium.viewer.portal.shape
  "Portal viewer for shape statistics.

  Displays skewness, kurtosis, and coefficient of variation (CV) for
  bootstrap results in a Portal table."
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common.shape :as shape]
   [criterium.viewer.portal.core :as portal.core]))

(defmethod view/shape-stats* :portal
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
          (portal.core/heading "Shape Statistics")
          (portal.core/portal-table
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
