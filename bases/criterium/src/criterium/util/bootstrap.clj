(ns criterium.util.bootstrap
  "Bootstrap statistics for criterium.

  Core bootstrap algorithms are provided by stats.interface. This namespace
  provides criterium-specific integration with metrics and collect plans."
  (:require
   [criterium.collect-plan :as collect-plan]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [random.interface :as random]
   [stats.interface :as stats]))

;;; Re-exports from stats.interface for backward compatibility

(def bootstrap-sample
  "Bootstrap sampling of a statistic, using resampling with replacement."
  stats/bootstrap-sample)

(def bootstrap-estimate
  "Mean, variance and confidence interval."
  stats/bootstrap-estimate)

(def scale-bootstrap-estimate
  "Scale a BcaEstimate by the given scale factor."
  stats/scale-bootstrap-estimate)

(def drop-at
  "Return coll with element at index n removed."
  stats/drop-at)

(def jacknife
  "Jacknife statistics on data."
  stats/jacknife)

(def bca-nonparametric-eval
  "Calculate bootstrap values for given estimate and samples."
  stats/bca-nonparametric-eval)

(def bca-nonparametric
  "Non-parametric BCa estimate of a statistic on data."
  stats/bca-nonparametric)

(def ->BcaEstimate stats/->BcaEstimate)
(def map->BcaEstimate stats/map->BcaEstimate)

(def bootstrap-bca
  "Bootstrap a statistic with BCa confidence intervals."
  stats/bootstrap-bca)

(def bootstrap
  "Bootstrap a statistic."
  stats/bootstrap)

(def assoc-bootstrap-mean-3-sigma
  "Add :mean-plus-3sigma and :mean-minus-3sigma to stats map."
  stats/assoc-bootstrap-mean-3-sigma)

(def scale-bootstrap-values
  "Apply function f to all values in stats map."
  stats/scale-bootstrap-values)

(def stats-fn-map
  "Map of stat keywords to their corresponding functions."
  stats/stats-fn-map)

(def stats-fns
  "Build vector of stat functions including quantile functions."
  stats/stats-fns)

(def stats-fn
  "Combine multiple stat functions into one."
  stats/stats-fn)

;;; Criterium-specific bootstrap functions

(defn- scale-bootstrap-stat
  "Scale a bootstrap stat using the given scale function."
  [scale-f stat]
  (stats/scale-bootstrap-stat scale-f stat))

(defn bootstrap-stats-for
  "Compute bootstrap statistics for samples with given options and transforms.

  Computes mean, variance, and quantiles with BCa confidence intervals.
  Does not include min-val, max-val, or 3-sigma bounds."
  [samples opts transforms]
  {:pre [(:quantiles opts)
         (:estimate-quantiles opts)]}
  (let [vs        (mapv double samples)
        quantiles (into [0.1 0.25 0.5 0.75 0.9] (:quantiles opts))
        stats-fn  (stats/stats-fn (stats/stats-fns quantiles))
        stats     (stats/bootstrap-bca
                   vs
                   stats-fn
                   (:bootstrap-size opts (long (* (count vs) 0.8)))
                   (into [0.5] (:estimate-quantiles opts))
                   random/well-rng-1024a)
        scale-1   (fn [v] (util/transform-sample-> v transforms))
        scale-f   (partial scale-bootstrap-stat scale-1)
        ks        (keys stats/stats-fn-map)]
    (-> (zipmap ks stats)
        (dissoc :min-val :max-val)
        (stats/scale-bootstrap-values scale-f)
        (assoc :quantiles
               (zipmap quantiles (map scale-f (drop (count ks) stats)))))))

(defn bootstrap-stats*
  "Compute bootstrap stats for all metric paths."
  [metric->values metric-configs transforms config]
  (reduce
   (fn [res path]
     (assoc-in
      res path
      (bootstrap-stats-for
       (get metric->values path)
       config
       transforms)))
   {}
   (map :path metric-configs)))

(defn bootstrap-stats
  "Analysis function that adds bootstrap stats to the result."
  ([] (bootstrap-stats {}))
  ([{:keys [id metric-ids samples-id] :as analysis}]
   (fn [data-map]
     (let [id              (or id :bootstrap-stats)
           samples-id      (or samples-id :samples)
           metrics-samples (data-map samples-id)
           metrics-defs    (-> (:metrics-defs metrics-samples)
                               (metric/select-metrics metric-ids)
                               (metric/filter-metrics
                                (metric/type-pred :quantitative)))
           metric-configs  (metric/all-metric-configs metrics-defs)
           transforms      (util/get-transforms data-map samples-id)
           result          (bootstrap-stats*
                            (util/metric->values metrics-samples)
                            metric-configs
                            transforms
                            analysis)]
       (assoc
        data-map
        id
        {:type         :criterium/bootstrap
         :bootstrap    result
         :metrics-defs metrics-defs
         :transform    collect-plan/identity-transforms
         :batch-size   (:batch-size metrics-samples)
         :source-id    samples-id})))))
