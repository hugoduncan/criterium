(ns criterium.analyse
  (:require
   [criterium.analyse.digest-samples]
   [criterium.analyse.methods :as methods]
   [criterium.analyse.metrics-samples]
   [criterium.collect-plan :as collect-plan]
   [criterium.metric :as metric]
   [criterium.types :as types]
   [criterium.util.debug :as debug]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]))

(defn exp [v]
  (Math/exp v))

(defn log [v]
  (Math/log v))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn transform-log
  "Performs logarithmic transformation on time-based samples.

  Returns a function that takes a sampled data map and adds
  log-transformed samples under a new key. Only transforms samples
  with :time dimension from quantitative metrics.

  Parameters:
    opts - Optional map with keys:

      :id         - Key for transformed samples in result
                    (default: :log-samples)
      :samples-id - Key for source samples in input (default: :samples)
      :metric-ids - Set of metric ids to transform (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing samples and metric configs
  - Returns the map with transformed samples added under :id key
  - Preserves original samples and adds transform metadata

  Example:
  (let [transform (transform-log {:id :my-logs})
        result (transform {:samples {...} :metrics-defs {...}})]
    (:my-logs result)) ;; Contains log-transformed values"
  ([] (transform-log {}))
  ([{:keys [id samples-id metric-ids] :as options}]
   (fn transform-log [data-map]
     (let [samples-id      (or samples-id :samples)
           id              (or id (keyword (str "log-" (name samples-id))))
           metrics-samples (have types/generic-data-map? (data-map samples-id))
           metrics-defs    (-> (:metrics-defs metrics-samples)
                               (metric/select-metrics metric-ids)
                               (metric/filter-metrics
                                (every-pred
                                 (metric/type-pred :quantitative)
                                 (metric/dimension-pred :time))))
           metric-configs  (metric/all-metric-configs metrics-defs)
           transformed     (->
                            (methods/transform
                             metrics-samples
                             metric-configs
                             log
                             exp
                             options)
                            (merge
                             {:source-id samples-id :metrics-defs metrics-defs}))]
       (assoc data-map id transformed)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn quantiles
  "Calculates statistical quantiles for quantitative sample measurements.

  Returns a function that computes sample quantiles (including quartiles and
  custom percentiles) for each quantitative metric in the input data.

  Parameters:
    opts - Optional map with keys:
      :id         - Key for quantile results in output (default: :quantiles)
      :samples-id - Key for source samples in input (default: :samples)
      :metric-ids - Set of metric ids to analyze (default: all quantitative)
      :quantiles  - Vector of quantile values to calculate [0-1]
                   (default: [0.25 0.5 0.75])

  The returned function:
  - Takes a sampled data map containing samples and metric configs
  - Returns the map with quantile analysis added under :id key
  - Preserves source data transforms for correct value scaling

  Example:
  (let [analyze (quantiles {:quantiles [0.05 0.95]})
        result (analyze {:samples {...} :metrics-defs {...}})]
    (get-in result [:quantiles :elapsed-time]))
  ;; Returns map of quantiles for elapsed time metric"
  ([] (quantiles {}))
  ([{:keys [id samples-id metric-ids] :as analysis}]
   (fn quantiles [data-map]
     {:pre [(have? types/result-map? data-map)]}
     (let [samples-id      (or samples-id :samples)
           id              (or id :quantiles)
           metrics-samples (data-map samples-id)
           metrics-defs    (-> (:metrics-defs metrics-samples)
                               (metric/select-metrics metric-ids)
                               (metric/filter-metrics
                                (metric/type-pred :quantitative)))
           metric-configs  (metric/all-metric-configs metrics-defs)
           quantiles       (methods/quantiles
                            metrics-samples
                            metric-configs
                            analysis)
           quantiles-map   (have
                            types/quantiles-map?
                            (merge
                             {:type         :criterium/quantiles
                              :source-id    samples-id
                              :metrics-defs metrics-defs}
                             quantiles))]
       (assoc data-map id quantiles-map)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn outliers
  "Detects statistical outliers in sample measurements using boxplot criteria.

  Returns a function that identifies outliers based on the interquartile range
  (IQR) method, classifying them as mild or severe deviations.

  Parameters:
    opts - Optional map with keys:
      :id           - Key for outlier results (default: :outliers)
      :samples-id   - Key for source samples (default: :samples)
      :quantiles-id - Key for required quantile analysis (default: :quantiles)
      :metric-ids   - Set of metric ids to analyze (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing samples, metrics config and quantiles
  - Returns the map with outlier analysis added under :id key
  - For each metric provides:
    - outlier thresholds (IQR boundaries)
    - identified outliers with indices
    - counts by severity (low/high, mild/severe)
  - Requires prior quantile analysis in input data

  Outlier Classification:
  - Mild: between 1.5 and 3.0 IQR from quartiles
  - Severe: beyond 3.0 IQR from quartiles

  Example:
  (let [analyze (outliers)
        result (analyze {:samples {...}
                        :metrics-defs {...}
                        :quantiles {...}})]
    (get-in result [:outliers :elapsed-time]))
  ;; Returns {:thresholds [...] :outliers {...} :outlier-counts {...}}"
  ([] (outliers {}))
  ([{:keys [id samples-id quantiles-id metric-ids]}]
   (fn [data-map]
     (let [id              (or id :outliers)
           quantiles-id    (or quantiles-id :quantiles)
           samples-id      (or samples-id :samples)
           all-quantiles   (have (data-map quantiles-id))
           metrics-samples (have (data-map samples-id))
           metrics-defs    (-> (:metrics-defs all-quantiles)
                               (metric/select-metrics metric-ids))
           metric-configs  (metric/all-metric-configs metrics-defs)]
       (when-not all-quantiles
         (throw (ex-info
                 "outlier analysis requires quantiles analysis"
                 {:quantiles-id  quantiles-id
                  :available-ids (keys data-map)})))
       (let [outliers     (methods/outliers
                           metrics-samples
                           all-quantiles
                           metric-configs
                           {})
             outliers-map (have
                           types/outliers-map?
                           (merge
                            {:type         :criterium/outliers
                             :source-id    samples-id
                             :quantiles-id quantiles-id
                             :metrics-defs metrics-defs}
                            outliers))]
         (assoc data-map id outliers-map))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn stats
  "Calculates comprehensive descriptive statistics for sample measurements.

  Returns a function that computes key statistics including mean, variance,
  standard deviation bounds (±3σ), and min/max values for quantitative metrics.

  Parameters:
    opts - Optional map with keys:
      :id          - Key for stats in output (default: :stats)
      :samples-id  - Key for source samples (default: :samples)
      :outliers-id - Key for outlier analysis if available
      :metric-ids  - Set of metric ids to analyze (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing samples and metric configs
  - Returns the map with statistics added under :id key
  - For each metric, calculates:
    - mean, variance
    - mean ±3σ bounds
    - min/max values
  - Preserves data transforms for correct scaling

  Example:
  (let [analyze (stats)
        result (analyze {:samples {...} :metrics-defs {...}})]
    (get-in result [:stats :elapsed-time]))
  ;; Returns {:mean 100.0 :variance 16.0 ...}"
  ([] (stats {}))
  ([{:keys [id samples-id outliers-id metric-ids]
     :as   analysis}]
   (let [samples-id  (or samples-id :samples)
         id          (or id :stats)
         outliers-id (or outliers-id :outliers)]
     (fn [data-map]
       (debug/dtap> {:stats id})
       (let [outliers        (when outliers-id
                               (data-map outliers-id))
             metrics-samples (have (data-map samples-id))
             metrics-defs    (-> (have (:metrics-defs metrics-samples))
                                 (metric/select-metrics metric-ids)
                                 (metric/filter-metrics
                                  (metric/type-pred :quantitative)))
             metric-configs  (metric/all-metric-configs metrics-defs)
             stats           (methods/stats
                              metrics-samples
                              outliers
                              metric-configs
                              analysis)
             stats-map       (have
                              types/stats-map?
                              (merge
                               {:type         :criterium/stats
                                :metrics-defs metrics-defs
                                :source-id    samples-id
                                :outliers-id  outliers-id
                                :batch-size   (:batch-size metrics-samples)}
                               stats))]
         (assoc data-map id stats-map))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn event-stats
  "Calculates statistics for discrete events captured during sampling.

  Returns a function that aggregates event metrics like JIT compilation,
  garbage collection, and class loading events that occur during benchmarking.

  Parameters:
    opts - Optional map with keys:
      :id          - Key for event stats in output (default: :event-stats)
      :samples-id  - Key for source samples (default: :samples)
      :metric-ids  - Set of event metric ids to analyze (default: all events)

  The returned function:
  - Takes a sampled data map containing samples and metrics configs
  - Returns the map with event statistics added under :id key
  - For each event metric collects:
    - Total counts/durations
    - Number of samples containing events
    - Metric-specific aggregations (e.g., loaded/unloaded classes)
  - Only processes metrics of type :event

  Example:
  (let [analyze (event-stats)
        result (analyze {:samples {...} :metrics-defs {...}})]
    (:event-stats result))
  ;; Returns {:compilation {:time-ms 8 :sample-count 2} ...}"
  ([] (event-stats {}))
  ([{:keys [id samples-id metric-ids] :as analysis}]
   (let [id         (or id :event-stats)
         samples-id (or samples-id :samples)]
     (fn [data-map]
       (debug/dtap> {:event-stats id})
       (let [metrics-samples (data-map samples-id)
             metrics-defs    (-> (:metrics-defs metrics-samples)
                                 (metric/select-metrics metric-ids)
                                 (metric/filter-metrics
                                  (metric/type-pred :event)))
             event-stats     (methods/event-stats
                              metrics-samples
                              metrics-defs
                              analysis)
             es-map          (have
                              types/event-stats-map?
                              (merge
                               {:type         :criterium/event-stats
                                :source-id    samples-id
                                :metrics-defs metrics-defs}
                               event-stats))]
         (assoc data-map id es-map))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn histogram
  "Calculate a histogram for sample measurements.

  Returns a function that computes histograms for quantitative metrics.

  Parameters:
    opts - Optional map with keys:
      :id          - Key for stats in output (default: :stats)
      :samples-id  - Key for source samples (default: :samples)
      :outliers-id - Key for outlier analysis if available
      :metric-ids  - Set of metric ids to analyze (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing samples
  - Returns the map with histograms added under :id key
  - Preserves data transforms for correct scaling

  Example:
  (let [analyze (histogram)
        result (analyze {:samples {...} :outliers {...}})]
    (get-in result [:histogram :elapsed-time]))"
  ([] (histogram {}))
  ([{:keys [id samples-id quantiles-id outliers-id metric-ids]
     :as   analysis}]
   (let [samples-id   (or samples-id :samples)
         id           (or id :histograms)
         quantiles-id (or quantiles-id :quantiles)
         outliers-id  (or outliers-id :outliers)         ]
     (fn [data-map]
       (let [outliers        (when outliers-id
                               (data-map outliers-id))
             quantiles       (util/lookup-data data-map quantiles-id)
             metrics-samples (util/lookup-data data-map samples-id)
             metrics-defs    (-> (have (:metrics-defs metrics-samples))
                                 (metric/select-metrics metric-ids)
                                 (metric/filter-metrics
                                  (metric/type-pred :quantitative)))
             metric-configs  (metric/all-metric-configs metrics-defs)
             histogram       (methods/histogram
                              metrics-samples
                              quantiles
                              outliers
                              metric-configs
                              analysis)
             histogram-map   (have
                              types/histogram-map?
                              (merge
                               {:type         :criterium/histogram
                                :metrics-defs metrics-defs
                                :source-id    samples-id
                                :outliers-id  outliers-id
                                :batch-size   (:batch-size metrics-samples)}
                               histogram))]
         (assoc data-map id histogram-map))))))

(defn- min-f
  ^double [f ^double q ^double r]
  (min ^double (f q) ^double (f r)))

(defn outlier-significance*
  "Calculate statistical significance of outliers using gaussian fit analysis.

  Determines how well a gaussian distribution describes the sample statistics by
  comparing the sample variance to the variance of a fitted gaussian model.  A
  high significance indicates the outliers substantially affect the
  distribution.

  Based on the methodology described in:
  http://www.ellipticgroup.com/misc/article_supplement.pdf, p17

  Parameters:
    mean     - Sample mean (must be non-zero)
    variance - Sample variance
    batch-size - Number of measurements per sample (must be >= 16)

  Returns:
    A value between 0 and 1 representing outlier significance:
    - 0: No significant effect from outliers
    - 1: Outliers heavily influence the distribution

  Throws:
    AssertionError if preconditions on inputs are not met

  Example:
  (outlier-significance* 100.0 16.0 67108864) ;; => 0.25"
  [^double mean ^double variance ^long batch-size]
  {:pre [(number? mean) (number? variance) (nat-int? batch-size)]}
  (if (or (zero? variance) (< batch-size 16))
    0
    (let [variance-block (* batch-size variance)
          std-dev-block  (Math/sqrt variance-block)
          mean-g-min     (/ mean 2)
          sigma-g        (min (/ mean-g-min 4)
                              (/ std-dev-block (Math/sqrt batch-size)))
          variance-g     (* sigma-g sigma-g)
          batch-size-sqr (util/sqr batch-size)
          c-max-f        (fn ^long [^double t-min]    ; Eq 38
                           (let [j0-sqr (util/sqr (- mean t-min))
                                 k0     (- (* batch-size-sqr j0-sqr))
                                 k1     (+ variance-block
                                           (- (* batch-size variance-g))
                                           (* batch-size j0-sqr))
                                 det    (- (* k1 k1)
                                           (* 4 variance-g k0))]
                             (long (Math/floor (/ (* -2 k0)
                                                  (+ k1 (Math/sqrt det)))))))
          var-out        (fn ^double [^long c]        ; Eq 45
                           (let [nmc (- batch-size c)]
                             (* (/ nmc (double batch-size))
                                (- variance-block (* nmc variance-g)))))
          c-max          (min-f c-max-f 0.0 mean-g-min)]
      (/ (min-f var-out 1.0 c-max) variance-block))))

(defn outlier-effect
  "Return a keyword describing the effect of outliers on a point estimate."
  [^double significance]
  (cond
    (< significance 0.01) :unaffected
    (< significance 0.1)  :slight
    (< significance 0.5)  :moderate
    :else                 :severe))

(defn- samples-outlier-significance [batch-size outliers stats metric-configs]
  (reduce
   (fn sample-m [result metric]
     (let [path           (:path metric)
           outlier-data   (get-in outliers path)
           stat           (get-in stats path)
           _              (assert (map? outlier-data) outlier-data)
           outlier-counts (:outlier-counts outlier-data)
           significance   (when (some pos? (vals outlier-counts))
                            (outlier-significance*
                             (:mean stat)
                             (:variance stat)
                             batch-size))]
       (update-in result path
                  assoc
                  :significance significance
                  :effect (outlier-effect significance))))
   {}
   metric-configs))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn outlier-significance
  "Analyzes the statistical significance of detected outliers.

  Returns a function that calculates how much outliers affect the sample
  distribution by comparing actual variance to an idealized gaussian model.

  Parameters:
    opts - Optional map with keys:
      :id          - Key for significance results (default: :outlier-significance)
      :outliers-id - Key for outlier analysis (default: :outliers)
      :stats-id    - Key for statistical analysis (default: :stats)
      :metric-ids  - Set of metric ids to analyze (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing outlier analysis and statistics
  - Returns the map with significance analysis added under :id key
  - For each metric provides:
    - significance: A value between 0-1 indicating outlier impact
    - effect: Keyword describing impact (:unaffected, :slight, :moderate, :severe)
  - Requires prior outlier and statistical analysis in input data

  Effects are classified as:
  - :unaffected - significance < 0.01
  - :slight     - significance < 0.1
  - :moderate   - significance < 0.5
  - :severe     - significance >= 0.5

  Example:
  (let [analyze (outlier-significance)
        result (analyze {:outliers {...}
                        :stats {...}})]
    (get-in result [:outlier-significance :elapsed-time]))
  ;; Returns {:significance 0.25 :effect :moderate}"
  ([] (outlier-significance {}))
  ([{:keys [id outliers-id stats-id metric-ids] :as _analysis}]
   (fn [data-map]
     (let [id             (or id :outlier-significance)
           outliers-id    (or outliers-id :outliers)
           stats-id       (or stats-id :stats)
           outliers       (data-map outliers-id)
           stats          (data-map stats-id)
           metrics-defs   (-> (:metrics-defs stats)
                              (metric/select-metrics metric-ids)
                              (metric/filter-metrics
                               (metric/type-pred :quantitative)))
           metric-configs (metric/all-metric-configs metrics-defs)]
       (when-not outliers
         (throw (ex-info
                 "outlier significance requires outlier analysis"
                 {:outliers-id   outliers-id
                  :available-ids (keys data-map)})))
       (let [significance (samples-outlier-significance
                           (:batch-size stats)
                           (util/outliers outliers)
                           (util/stats stats)
                           metric-configs)
             os-map       (have
                           types/outlier-significance-map?
                           {:type                 :criterium/outlier-significance
                            :transform            collect-plan/identity-transforms
                            :outlier-significance significance
                            :metrics-defs         metrics-defs
                            :outliers-id          outliers-id
                            :source-id            stats-id})]
         (assoc data-map id os-map))))))
