(ns criterium.util.bootstrap
  "Bootstrap statistics for criterium.

  Core bootstrap algorithms are provided by criterium.stats.bootstrap. This namespace
  provides criterium-specific integration with metrics and collect plans."
  (:require
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]
   [criterium.metric :as metric]
   [criterium.random.interface :as random]
   [criterium.stats.bootstrap :as stats]
   [criterium.util.helpers :as util])
  (:import
   [criterium.array DoubleArray LongArray ObjectArray]))

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

(defn- ensure-double-array
  "Convert input to a DoubleArray if not already one.
  Handles DoubleArray, LongArray, and ObjectArray."
  ^DoubleArray [samples]
  (cond
    (instance? DoubleArray samples)
    samples

    (instance? LongArray samples)
    (let [n   (arr/length samples)
          out (double-array n)]
      (dotimes [i n]
        (aset out i (double (arr/get-long samples i))))
      (arr/->double-array out))

    (instance? ObjectArray samples)
    (arr/->double-array
     (double-array
      (arr/fold samples
                (fn [acc v] (conj acc (double v)))
                [])))

    :else
    (throw (ex-info "Expected TypedArray, got unexpected type"
                    {:type (type samples)}))))

(defn- typed-array->double-vec
  "Convert a typed array to a vector of doubles.
  Handles DoubleArray, LongArray, and ObjectArray."
  [samples]
  (cond
    (or (instance? DoubleArray samples)
        (instance? LongArray samples))
    (persistent!
     (arr/dfold samples
                (fn [acc ^double v] (conj! acc v))
                (transient [])))

    (instance? ObjectArray samples)
    (persistent!
     (arr/fold samples
               (fn [acc v] (conj! acc (double v)))
               (transient [])))

    :else
    (throw (ex-info "Expected TypedArray, got unexpected type"
                    {:type (type samples)}))))

(def ^:private default-min-samples
  "Default minimum sample size for bootstrap resampling.
  Below this threshold, BCa confidence intervals may be unreliable."
  30)

(defn bootstrap-stats-for
  "Compute bootstrap statistics for samples with given options.

  Accepts DoubleArray or vector of samples.
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
  (let [darr          (ensure-double-array samples)
        n             (arr/length darr)
        ;; Convert to vector for stats functions that need sequences
        vs            (typed-array->double-vec darr)
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
                       random/make-well-rng-1024a)
        ks            (keys stats/stats-fn-map)]
    (cond-> (-> (zipmap ks stats)
                (dissoc :min-val :max-val)
                (assoc :quantiles
                       (zipmap quantiles (drop (count ks) stats))))
      low-samples? (assoc :low-sample-count? true))))

(defn- filter-outliers
  "Remove outlier samples from values based on outlier indices.
  Accepts DoubleArray, LongArray, or ObjectArray.
  Returns a DoubleArray with outliers removed."
  ^DoubleArray [values outliers path]
  (let [ols      (:outliers (get-in outliers path) {})
        darr     (ensure-double-array values)]
    (if (seq ols)
      (arr/filter-indices darr (set (keys ols)))
      darr)))

(defn bootstrap-stats*
  "Compute bootstrap stats for all metric paths.
  When outliers is non-nil, removes outlier samples before bootstrap resampling."
  [metric->values outliers metric-configs config]
  (reduce
   (fn [res path]
     (let [values          (get metric->values path)
           filtered-values (filter-outliers values outliers path)]
       (if (pos? (arr/length filtered-values))
         (assoc-in
          res path
          (bootstrap-stats-for filtered-values config))
         res)))
   {}
   (map :path metric-configs)))

(defn- inflate-estimate-quantiles
  "Inflate confidence interval bounds using the CI inflation factor.

  For original CI [lo, hi] around point estimate p:
  - New lower bound: p - inflation * (p - lo)
  - New upper bound: p + inflation * (hi - p)

  Returns a vector of adjusted quantile maps with the same structure
  as the input estimate-quantiles."
  [^double point-estimate estimate-quantiles ^double inflation]
  (mapv
   (fn [{:keys [value] :as eq}]
     (let [value (double value)
           adjusted (if (< value point-estimate)
                      ;; Lower bound: p - inflation * (p - lo)
                      (- point-estimate (* inflation (- point-estimate value)))
                      ;; Upper bound: p + inflation * (hi - p)
                      (+ point-estimate (* inflation (- value point-estimate))))]
       (assoc eq :value adjusted)))
   estimate-quantiles))

(defn- add-adjusted-cis-to-stat
  "Add :adjusted-estimate-quantiles to a BcaEstimate-like stat map."
  [stat ^double inflation]
  (if-let [eq (:estimate-quantiles stat)]
    (let [p (double (:point-estimate stat))
          adjusted (inflate-estimate-quantiles p eq inflation)]
      (assoc stat :adjusted-estimate-quantiles adjusted))
    stat))

(defn- adjust-stat-within-metric
  "Recursively adjust all stats within a metric's result using the given inflation factor.
  Handles both top-level stats (mean, variance) and nested :quantiles map."
  [metric-stats ^double inflation]
  (reduce-kv
   (fn [result k v]
     (cond
       ;; If this is a stat entry (has :estimate-quantiles), adjust it
       (and (map? v) (contains? v :estimate-quantiles))
       (assoc result k (add-adjusted-cis-to-stat v inflation))

       ;; If this is a nested map (like :quantiles), recurse
       (map? v)
       (assoc result k (adjust-stat-within-metric v inflation))

       ;; Otherwise keep the value
       :else
       (assoc result k v)))
   {}
   metric-stats))

(defn- add-adjusted-cis
  "Add adjusted CIs to bootstrap results using ESS inflation factors.

  For each metric path, looks up the ci-inflation-factor from effective
  sample size analysis and applies it to all stats for that metric.

  The bootstrap result uses nested maps (from assoc-in with vector paths like
  [:elapsed-time]), while ESS data uses vector path keys like
  {[:elapsed-time] {...}}. This function processes each top-level key in the
  bootstrap result as a metric path.

  Returns the modified bootstrap result with :adjusted-estimate-quantiles
  added alongside original :estimate-quantiles where ESS data exists."
  [bootstrap-result ess-data]
  (reduce-kv
   (fn [result metric-key metric-stats]
     ;; Convert keyword key to vector path for ESS lookup
     (let [metric-path [metric-key]
           ess-analysis (get ess-data metric-path)]
       (if ess-analysis
         (let [inflation (double (:ci-inflation-factor ess-analysis 1.0))]
           (if (> inflation 1.0)
             (assoc result metric-key (adjust-stat-within-metric metric-stats inflation))
             (assoc result metric-key metric-stats)))
         (assoc result metric-key metric-stats))))
   {}
   bootstrap-result))

(defn bootstrap-stats
  "Analysis function that adds bootstrap stats to the result.

  Parameters:
    opts - Optional map with keys:
      :id            - Key for bootstrap stats in output (default: :bootstrap-stats)
      :samples-id    - Key for source samples (default: :samples)
      :outliers-id   - Key for outlier analysis if available (default: :outliers)
      :ess-id        - Key for effective sample size analysis (default: nil). When
                       provided and ESS data exists, CI widths are inflated by the
                       ci-inflation-factor and stored as :adjusted-estimate-quantiles.
      :metric-ids    - Set of metric ids to analyze (default: all quantitative)
      :quantiles     - Additional quantiles to compute (default: none)
      :estimate-quantiles - Confidence interval quantiles (default: [0.025 0.975])
      :bootstrap-size     - Number of bootstrap resamples (default: sample count)
      :min-samples        - Minimum sample size for reliable results (default: 30).
                            When sample count is below this threshold, a warning
                            is printed and results include :low-sample-count? true.

  When :outliers-id is provided, outliers identified in the outlier analysis
  are removed from samples before bootstrap resampling. This prevents outliers
  from propagating and amplifying in resamples.

  When :ess-id is provided and the data-map contains effective sample size
  analysis under that key, confidence intervals are inflated by the
  ci-inflation-factor for each metric. The adjusted CIs are stored as
  :adjusted-estimate-quantiles alongside the original :estimate-quantiles.
  If ESS data is not present for a metric, only the original CIs are included."
  ([] (bootstrap-stats {}))
  ([{:keys [id metric-ids samples-id outliers-id ess-id] :as analysis}]
   (fn [data-map]
     (let [id              (or id :bootstrap-stats)
           samples-id      (or samples-id :samples)
           outliers-id     (or outliers-id :outliers)
           metrics-samples (data-map samples-id)
           outliers-data   (data-map outliers-id)
           outliers        (when outliers-data
                             (util/outliers outliers-data))
           ess-data        (when ess-id
                             (when-let [ess-entry (data-map ess-id)]
                               (util/effective-sample-size-data ess-entry)))
           metrics-defs    (-> (:metrics-defs metrics-samples)
                               (metric/select-metrics metric-ids)
                               (metric/filter-metrics
                                (metric/type-pred :quantitative)))
           metric-configs  (metric/all-metric-configs metrics-defs)
           result          (bootstrap-stats*
                            (util/metric->values metrics-samples)
                            outliers
                            metric-configs
                            analysis)
           ;; Apply ESS-adjusted CIs if effective sample size data is available
           adjusted-result (if ess-data
                             (add-adjusted-cis result ess-data)
                             result)]
       (assoc
        data-map
        id
        (cond-> {:type         :criterium/bootstrap
                 :bootstrap    adjusted-result
                 :metrics-defs metrics-defs
                 :transform    collect-plan/identity-transforms
                 :batch-size   (:batch-size metrics-samples)
                 :source-id    samples-id
                 :outliers-id  (when outliers-data outliers-id)}
          ess-id (assoc :ess-id (when ess-data ess-id))))))))
