(ns criterium.util.bootstrap
  "Bootstrap statistics for criterium.

  Core bootstrap algorithms are provided by stats.interface. This namespace
  provides criterium-specific integration with metrics and collect plans."
  (:require
   [criterium.collect-plan :as collect-plan]
   [criterium.metric :as metric]
   [criterium.random.interface :as random]
   [criterium.util.helpers :as util]
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

(def ^:private default-min-samples
  "Default minimum sample size for bootstrap resampling.
  Below this threshold, BCa confidence intervals may be unreliable."
  30)

(defn bootstrap-stats-for
  "Compute bootstrap statistics for samples with given options.

  Computes mean, variance, and quantiles with BCa confidence intervals.
  Does not include min-val, max-val, or 3-sigma bounds.

  Raw values are returned without transform application. Transforms are
  applied later via the source-id chain when viewing results.

  Options:
    :bootstrap-size - Number of bootstrap resamples (default: sample count)
    :min-samples    - Minimum sample size threshold (default: 30). When sample
                      count is below this, a warning is printed and results
                      include :low-sample-count? true.

  The :bootstrap-size option controls the number of bootstrap resamples.
  Defaults to the number of samples if not specified."
  [samples opts]
  {:pre [(:quantiles opts)
         (:estimate-quantiles opts)]}
  (let [vs            (mapv double samples)
        n             (count vs)
        min-samples   (long (:min-samples opts default-min-samples))
        low-samples?  (< n min-samples)
        _             (when low-samples?
                        (util/report
                         "Warning: bootstrap sample count (%d) below minimum (%d). Results may be unreliable.\n"
                         n min-samples))
        quantiles     (into [0.1 0.25 0.5 0.75 0.9] (:quantiles opts))
        stats-fn      (stats/stats-fn (stats/stats-fns quantiles))
        stats         (stats/bootstrap-bca
                       vs
                       stats-fn
                       (:bootstrap-size opts n)
                       (into [0.5] (:estimate-quantiles opts))
                       random/well-rng-1024a)
        ks            (keys stats/stats-fn-map)]
    (cond-> (-> (zipmap ks stats)
                (dissoc :min-val :max-val)
                (assoc :quantiles
                       (zipmap quantiles (drop (count ks) stats))))
      low-samples? (assoc :low-sample-count? true))))

(defn- filter-outliers
  "Remove outlier samples from values vector based on outlier indices.
  Returns values unchanged if no outliers for this path."
  [values outliers path]
  (let [ols (:outliers (get-in outliers path) {})]
    (if (seq ols)
      (into []
            (comp
             (map-indexed (fn [i v] (when-not (ols i) v)))
             (filter some?))
            values)
      values)))

(defn bootstrap-stats*
  "Compute bootstrap stats for all metric paths.
  When outliers is non-nil, removes outlier samples before bootstrap resampling."
  [metric->values outliers metric-configs config]
  (reduce
   (fn [res path]
     (let [values (get metric->values path)
           filtered-values (filter-outliers values outliers path)]
       (if (seq filtered-values)
         (assoc-in
          res path
          (bootstrap-stats-for filtered-values config))
         res)))
   {}
   (map :path metric-configs)))

(defn bootstrap-stats
  "Analysis function that adds bootstrap stats to the result.

  Parameters:
    opts - Optional map with keys:
      :id            - Key for bootstrap stats in output (default: :bootstrap-stats)
      :samples-id    - Key for source samples (default: :samples)
      :outliers-id   - Key for outlier analysis if available (default: :outliers)
      :metric-ids    - Set of metric ids to analyze (default: all quantitative)
      :quantiles     - Additional quantiles to compute (default: none)
      :estimate-quantiles - Confidence interval quantiles (default: [0.025 0.975])
      :bootstrap-size     - Number of bootstrap resamples (default: sample count)
      :min-samples        - Minimum sample size for reliable results (default: 30).
                            When sample count is below this threshold, a warning
                            is printed and results include :low-sample-count? true.

  When :outliers-id is provided, outliers identified in the outlier analysis
  are removed from samples before bootstrap resampling. This prevents outliers
  from propagating and amplifying in resamples."
  ([] (bootstrap-stats {}))
  ([{:keys [id metric-ids samples-id outliers-id] :as analysis}]
   (fn [data-map]
     (let [id              (or id :bootstrap-stats)
           samples-id      (or samples-id :samples)
           outliers-id     (or outliers-id :outliers)
           metrics-samples (data-map samples-id)
           outliers-data   (data-map outliers-id)
           outliers        (when outliers-data
                             (util/outliers outliers-data))
           metrics-defs    (-> (:metrics-defs metrics-samples)
                               (metric/select-metrics metric-ids)
                               (metric/filter-metrics
                                (metric/type-pred :quantitative)))
           metric-configs  (metric/all-metric-configs metrics-defs)
           result          (bootstrap-stats*
                            (util/metric->values metrics-samples)
                            outliers
                            metric-configs
                            analysis)]
       (assoc
        data-map
        id
        {:type         :criterium/bootstrap
         :bootstrap    result
         :metrics-defs metrics-defs
         :transform    collect-plan/identity-transforms
         :batch-size   (:batch-size metrics-samples)
         :source-id    samples-id
         :outliers-id  (when outliers-data outliers-id)})))))
