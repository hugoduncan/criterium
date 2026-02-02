(ns criterium.viewer.print.shape
  "Print viewer for shape statistics.

  Displays skewness, kurtosis, and coefficient of variation (CV) for
  bootstrap results."
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common.shape :as shape]
   [criterium.viewer.print.core :as print-core :refer [get-analysis-context]]))

(defn print-shape-stats
  "Print shape statistics (skewness, kurtosis, CV) for bootstrap results."
  [view data-map]
  (when-let [{:keys [analysis-map metric-configs]}
             (get-analysis-context
              :bootstrap-stats-id
              :bootstrap-stats
              view
              data-map
              (metric/type-pred :quantitative))]
    (let [bootstrap  (util/bootstrap analysis-map)
          shape-data (shape/shape-stats-data metric-configs bootstrap)]
      (when (seq shape-data)
        (doseq [{:keys [metric skewness skewness-class
                        kurtosis kurtosis-class
                        cv cv-class]} shape-data]
          (println
           (format "%s %s (%s)"
                   (print-core/format-sublabel (str metric " skewness"))
                   skewness
                   (shape/format-classification skewness-class)))
          (println
           (format "%s %s (%s)"
                   (print-core/format-sublabel "kurtosis")
                   kurtosis
                   (shape/format-classification kurtosis-class)))
          (println
           (format "%s %s (%s)"
                   (print-core/format-sublabel "CV")
                   cv
                   (shape/format-classification cv-class))))))))

(defmethod view/shape-stats* :print
  [_ view data-map]
  (print-shape-stats view data-map))
