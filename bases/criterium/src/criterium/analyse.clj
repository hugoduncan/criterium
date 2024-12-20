(ns criterium.analyse
  (:require
   [criterium.metric :as metric]
   [criterium.util.debug :as debug]
   [criterium.util.helpers :as util]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]))

(defn exp [v]
  (Math/exp v))

(defn log [v]
  (Math/log v))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn transform-log
  "Performs logarithmic transformation on time-based samples.

  Returns a function that takes a sampled data map and adds log-transformed samples
  under a new key. Only transforms samples with :time dimension from quantitative
  metrics.

  Parameters:
    opts - Optional map with keys:
      :id         - Key for transformed samples in result (default: :log-samples)
      :samples-id - Key for source samples in input (default: :samples)
      :metric-ids - Set of metric ids to transform (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing samples and metric configs
  - Returns the map with transformed samples added under :id key
  - Preserves original samples and adds transform metadata

  Example:
  (let [transform (transform-log {:id :my-logs})
        result (transform {:samples {...} :metrics-configs {...}})]
    (:my-logs result)) ;; Contains log-transformed values"
  ([] (transform-log {}))
  ([{:keys [id samples-id metric-ids]}]
   (fn transform-log [sampled]
     (let [samples-id        (or samples-id :samples)
           id                (or id (keyword (str "log-" (name samples-id))))
           metric-configs    (metric/metric-configs-of-type
                              (:metrics-configs sampled)
                              :quantitative metric-ids)
           transform-samples (fn x-sample [samples]
                               (reduce
                                (fn x-path [result path]
                                  (assoc result
                                         path (mapv log (get samples path))))
                                {}
                                (->> metric-configs
                                     (filterv #(#{:time} (:dimension %)))
                                     (mapv :path))))]
       (->  sampled
            (assoc id (with-meta
                        (transform-samples (samples-id sampled))
                        {:source-id samples-id
                         :type      :criterium/samples
                         :transform {:sample-> exp :->sample log}})))))))

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
        result (analyze {:samples {...} :metrics-configs {...}})]
    (get-in result [:quantiles :elapsed-time]))
  ;; Returns map of quantiles for elapsed time metric"
  ([] (quantiles {}))
  ([{:keys [id samples-id metric-ids] :as analysis}]
   (fn quantiles [sampled]
     (let [samples-id     (or samples-id :samples)
           id             (or id :quantiles)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)
           samples        (get sampled samples-id)
           transforms     (util/get-transforms sampled samples-id)
           res            (sampled-stats/quantiles
                           samples
                           metric-configs
                           transforms
                           analysis)]
       (assoc sampled id
              (with-meta res
                {:source-id samples-id
                 :type      :criterium/stats
                 :transform {:sample-> identity :->sample identity}}))))))

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  {:low-severe  low-severe
   :low-mild    low-mild
   :high-mild   high-mild
   :high-severe high-severe})

(defn classifier
  [[^double low-severe ^double low-mild ^double high-mild ^double high-severe]]
  (fn [^double x i]
    (when-not (<= low-mild x high-mild)
      [i (cond
           (<= x low-severe)           :low-severe
           (< low-severe x low-mild)   :low-mild
           (> high-severe x high-mild) :high-mild
           (>= x high-severe)          :high-severe)])))

(defn samples-outliers [metric-configs all-quantiles samples transforms]
  (reduce
   (fn sample-m [result metric-config]
     (let [path           (:path metric-config)
           quantiles      (get-in all-quantiles path)
           _              (assert (map? quantiles) quantiles)
           thresholds     (stats/boxplot-outlier-thresholds
                           (get quantiles 0.25)
                           (get quantiles 0.75))
           thresholds     (mapv
                           #(util/transform->sample % transforms)
                           thresholds)
           classifier     (classifier thresholds)
           outliers       (when (apply not= thresholds)
                            (into {}
                                  (mapv classifier
                                        (get samples path)
                                        (range))))
           outlier-counts (reduce-kv
                           (fn [counts _i v]
                             (update counts v inc))
                           (outlier-count 0 0 0 0)
                           outliers)]
       (update-in result path
                  assoc
                  :thresholds thresholds
                  :outliers outliers
                  :outlier-counts outlier-counts)))
   {}
   metric-configs))

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
                        :metrics-configs {...}
                        :quantiles {...}})]
    (get-in result [:outliers :elapsed-time]))
  ;; Returns {:thresholds [...] :outliers {...} :outlier-counts {...}}"
  ([] (outliers {}))
  ([{:keys [id samples-id quantiles-id metric-ids]}]
   (fn [sampled]
     (let [id             (or id :outliers)
           quantiles-id   (or quantiles-id :quantiles)
           samples-id     (or samples-id :samples)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)
           all-quantiles  (get sampled quantiles-id)
           samples        (get sampled samples-id)
           transforms     (util/get-transforms sampled samples-id)]
       (when-not all-quantiles
         (throw (ex-info
                 "outlier analysis requires quantiles analysis"
                 {:quantiles-id  quantiles-id
                  :available-ids (keys sampled)})))
       (let [result (samples-outliers
                     metric-configs all-quantiles samples transforms)]
         (assoc sampled id
                (with-meta
                  result
                  {:type      :criterium/outliers
                   :source-id samples-id})))))))

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
        result (analyze {:samples {...} :metrics-configs {...}})]
    (get-in result [:stats :elapsed-time]))
  ;; Returns {:mean 100.0 :variance 16.0 ...}"
  ([] (stats {}))
  ([{:keys [id samples-id outliers-id metric-ids]
     :as   analysis}]
   (let [samples-id  (or samples-id :samples)
         id          (or id :stats)
         outliers-id (or outliers-id :outliers)]
     (fn [sampled]
       (debug/dtap> {:stats id})
       (let [metric-configs (metric/metric-configs-of-type
                             (:metrics-configs sampled)
                             :quantitative
                             metric-ids)
             outliers       (when outliers-id
                              (get sampled outliers-id))
             res            (sampled-stats/sample-stats
                             sampled
                             samples-id
                             outliers
                             metric-configs
                             analysis)]
         (assoc sampled id
                (with-meta res
                  {:source-id samples-id
                   :type      :criterium/stats
                   :transform {:sample-> identity :->sample identity}})))))))

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
        result (analyze {:samples {...} :metrics-configs {...}})]
    (:event-stats result))
  ;; Returns {:compilation {:time-ms 8 :sample-count 2} ...}"
  ([] (event-stats {}))
  ([{:keys [id samples-id metric-ids] :as _analysis}]
   (let [id         (or id :event-stats)
         samples-id (or samples-id :samples)]
     (fn [sampled]
       (debug/dtap> {:event-stats id})
       (let [metrics-configs (metric/metrics-of-type
                              (:metrics-configs sampled)
                              :event metric-ids)
             samples         (sampled samples-id)
             res             (sampled-stats/event-stats
                              metrics-configs
                              samples)]
         (assoc sampled id (with-meta res {:type :criterium/event-stats})))))))

(defn- min-f
  ^double [f ^double q ^double r]
  (min ^double (f q) ^double (f r)))

(defn outlier-significance*
  "Calculates the statistical significance of outliers using gaussian fit analysis.

  Determines how well a gaussian distribution describes the sample statistics by
  comparing the sample variance to the variance of a fitted gaussian model.
  A high significance indicates the outliers substantially affect the distribution.

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
   (fn [sampled]
     (let [id             (or id :outlier-significance)
           outliers-id    (or outliers-id :outliers)
           stats-id       (or stats-id :stats)
           outliers       (get sampled outliers-id)
           stats          (get sampled stats-id)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)]
       (when-not outliers
         (throw (ex-info
                 "outlier significance requires outlier analysis"
                 {:outliers-id   outliers-id
                  :available-ids (keys sampled)})))
       (let [result (samples-outlier-significance
                     (:batch-size sampled) outliers stats metric-configs)]
         (assoc sampled id
                (with-meta
                  result
                  {:type :criterium/outlier-significance})))))))
