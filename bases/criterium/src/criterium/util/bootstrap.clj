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

(def ^:private default-min-samples
  "Default minimum sample size for bootstrap resampling.
  Below this threshold, BCa confidence intervals may be unreliable."
  30)

;;; Selective stats computation for robust/non-robust split

(def ^:private base-stat-keys
  "Base statistics keys (excluding quantiles)."
  [:mean :variance])

(defn- quantile-robust?
  "Check if a quantile should use unfiltered data."
  [robust-stats-set q]
  (or (contains? robust-stats-set :quantiles)
      (contains? robust-stats-set q)))

(defn- partition-stats
  "Partition base stats and quantiles into robust and non-robust groups.

  robust-stats can contain:
    - :mean, :variance - these specific stats are robust
    - :quantiles - all quantiles are robust
    - specific quantile values like 0.5 - these specific quantiles are robust"
  [robust-stats quantiles]
  (let [robust-set (set robust-stats)]
    {:robust {:stats (filterv #(contains? robust-set %) base-stat-keys)
              :quantiles (filterv #(quantile-robust? robust-set %) quantiles)}
     :non-robust {:stats (filterv #(not (contains? robust-set %)) base-stat-keys)
                  :quantiles (filterv #(not (quantile-robust? robust-set %)) quantiles)}}))

(defn- build-selective-stats-fn
  "Build a stats function for selected base stats and quantiles."
  [base-stats quantiles]
  (let [stat-fns (keep #(get stats/stats-fn-map %) base-stats)
        quantile-fns (map (fn [q] (fn [vs] (stats/quantile q vs))) quantiles)]
    (stats/stats-fn (into (vec stat-fns) quantile-fns))))

(defn- process-bootstrap-result
  "Process bootstrap-bca result into a map with stat keys and quantiles."
  [base-stats quantiles result scale-f]
  (let [n-stats (count base-stats)
        stat-results (take n-stats result)
        quantile-results (drop n-stats result)]
    (cond-> {}
      (seq base-stats)
      (merge (zipmap base-stats (map scale-f stat-results)))

      (seq quantiles)
      (assoc :quantiles (zipmap quantiles (map scale-f quantile-results))))))

(defn- compute-all-stats
  "Single-pass bootstrap computation for all stats (original behavior)."
  [vs quantiles bootstrap-n estimate-qs scale-f]
  (let [stats-fn (stats/stats-fn (stats/stats-fns quantiles))
        stats    (stats/bootstrap-bca
                  vs stats-fn bootstrap-n estimate-qs
                  random/well-rng-1024a)
        ks       (keys stats/stats-fn-map)]
    (-> (zipmap ks stats)
        (dissoc :min-val :max-val)
        (stats/scale-bootstrap-values scale-f)
        (assoc :quantiles
               (zipmap quantiles (map scale-f (drop (count ks) stats)))))))

(defn- compute-selective-stats
  "Compute bootstrap stats for selected base-stats and quantiles."
  [vs base-stats quantiles bootstrap-n estimate-qs scale-f]
  (when (or (seq base-stats) (seq quantiles))
    (let [stats-fn (build-selective-stats-fn base-stats quantiles)
          result   (stats/bootstrap-bca
                    vs stats-fn bootstrap-n estimate-qs
                    random/well-rng-1024a)]
      (process-bootstrap-result base-stats quantiles result scale-f))))

(defn- merge-stats-results
  "Merge robust and non-robust stats results."
  [robust-result non-robust-result]
  (merge
   (dissoc robust-result :quantiles)
   (dissoc non-robust-result :quantiles)
   {:quantiles (merge (:quantiles robust-result)
                      (:quantiles non-robust-result))}))

(defn bootstrap-stats-for
  "Compute bootstrap statistics for samples with given options and transforms.

  Computes mean, variance, and quantiles with BCa confidence intervals.
  Does not include min-val, max-val, or 3-sigma bounds.

  Options:
    :bootstrap-size - Number of bootstrap resamples (default: sample count)
    :min-samples    - Minimum sample size threshold (default: 30). When sample
                      count is below this, a warning is printed and results
                      include :low-sample-count? true.
    :robust-stats   - Stats to compute without outlier filtering.
                      Can include :mean, :variance, :quantiles (all quantiles),
                      or specific quantile values like 0.5.
                      When specified and samples differ from unfiltered-samples,
                      robust stats use unfiltered data, others use filtered.

  The :bootstrap-size option controls the number of bootstrap resamples.
  Defaults to the number of samples if not specified."
  [samples unfiltered-samples opts transforms]
  {:pre [(:quantiles opts)
         (:estimate-quantiles opts)]}
  (let [vs            (mapv double samples)
        unfilt-vs     (mapv double unfiltered-samples)
        n             (count vs)
        min-samples   (long (:min-samples opts default-min-samples))
        low-samples?  (< n min-samples)
        _             (when low-samples?
                        (util/report
                         "Warning: bootstrap sample count (%d) below minimum (%d). Results may be unreliable.\n"
                         n min-samples))
        quantiles     (into [0.1 0.25 0.5 0.75 0.9] (:quantiles opts))
        robust-stats  (:robust-stats opts)
        scale-1       (fn [v] (util/transform-sample-> v transforms))
        scale-f       (partial scale-bootstrap-stat scale-1)
        estimate-qs   (into [0.5] (:estimate-quantiles opts))
        bootstrap-n   (:bootstrap-size opts n)
        ;; Need split computation when robust-stats specified and data differs
        needs-split?  (and (seq robust-stats)
                           (not= vs unfilt-vs))]
    (cond->
      (if needs-split?
          ;; Two-pass: robust stats from unfiltered, non-robust from filtered
        (let [{:keys [robust non-robust]} (partition-stats robust-stats quantiles)
              robust-result (compute-selective-stats
                             unfilt-vs
                             (:stats robust)
                             (:quantiles robust)
                             bootstrap-n estimate-qs scale-f)
              non-robust-result (compute-selective-stats
                                 vs
                                 (:stats non-robust)
                                 (:quantiles non-robust)
                                 bootstrap-n estimate-qs scale-f)]
          (merge-stats-results robust-result non-robust-result))
          ;; Single pass: all stats from same data (original behavior)
        (compute-all-stats vs quantiles bootstrap-n estimate-qs scale-f))
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

  When outliers is non-nil, removes outlier samples before bootstrap resampling.
  When :robust-stats is specified in config, those stats use unfiltered data
  while other stats use filtered data."
  [metric->values outliers metric-configs transforms config]
  (reduce
   (fn [res path]
     (let [unfiltered-values (get metric->values path)
           filtered-values (filter-outliers unfiltered-values outliers path)]
       (if (seq filtered-values)
         (assoc-in
          res path
          (bootstrap-stats-for filtered-values unfiltered-values config transforms))
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
      :robust-stats       - Stats to compute without outlier filtering. Can include:
                            - :mean, :variance - these specific stats are robust
                            - :quantiles - all quantiles are robust
                            - specific quantile values like 0.5 (median)
                            When specified with :outliers-id, robust stats use
                            unfiltered data while other stats use filtered data.
                            This enables computing median on raw data (robust to
                            outliers) while protecting mean from outlier influence.

  When :outliers-id is provided, outliers identified in the outlier analysis
  are removed from samples before bootstrap resampling. This prevents outliers
  from propagating and amplifying in resamples.

  Example usage for robust median with filtered mean:
    [:bootstrap-stats {:outliers-id :outliers
                       :robust-stats [:quantiles]}]"
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
           transforms      (util/get-transforms data-map samples-id)
           result          (bootstrap-stats*
                            (util/metric->values metrics-samples)
                            outliers
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
         :source-id    samples-id
         :outliers-id  (when outliers-data outliers-id)})))))
