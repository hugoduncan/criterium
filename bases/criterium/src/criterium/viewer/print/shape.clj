(ns criterium.viewer.print.shape
  "Print viewer for shape statistics.

  Displays skewness, kurtosis, and coefficient of variation (CV) for
  bootstrap results."
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common.shape :as shape]
   [criterium.viewer.print.core :as print-core]))

(defn- format-skewness-class
  "Format skewness classification for display."
  [classification]
  (case classification
    :highly-left-skewed "highly left-skewed"
    :moderately-left-skewed "moderately left-skewed"
    :slightly-left-skewed "slightly left-skewed"
    :symmetric "symmetric"
    :slightly-right-skewed "slightly right-skewed"
    :moderately-right-skewed "moderately right-skewed"
    :highly-right-skewed "highly right-skewed"
    (name classification)))

(defn- format-kurtosis-class
  "Format kurtosis classification for display."
  [classification]
  (case classification
    :heavy-tails "heavy tails (leptokurtic)"
    :light-tails "light tails (platykurtic)"
    :normal-tails "normal tails (mesokurtic)"
    (name classification)))

(defn- format-cv-class
  "Format CV classification for display."
  [classification]
  (case classification
    :low-variability "low variability"
    :moderate-variability "moderate variability"
    :high-variability "high variability"
    (name classification)))

(defn print-shape-stats
  "Print shape statistics (skewness, kurtosis, CV) for bootstrap results."
  [{:keys [bootstrap-stats-id] :as _view} data-map]
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
          (println "Shape Statistics:")
          (doseq [{:keys [metric skewness skewness-class
                          kurtosis kurtosis-class
                          cv cv-class]} shape-data]
            (println
             (format "%s skewness %s (%s)"
                     (print-core/format-label metric) skewness (format-skewness-class skewness-class)))
            (println
             (format "%s  kurtosis %s (%s)"
                     (print-core/label-str "") kurtosis (format-kurtosis-class kurtosis-class)))
            (println
             (format "%s  CV %s (%s)"
                     (print-core/label-str "") cv (format-cv-class cv-class)))))))))

(defmethod view/shape-stats* :print
  [_ view data-map]
  (print-shape-stats view data-map))
