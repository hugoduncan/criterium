(ns criterium.analyse
  (:require
   [criterium.allocation.analysis :as allocation-analysis]
   [criterium.analyse.call-graph :as call-graph-analysis]
   [criterium.analyse.digest-samples]
   [criterium.analyse.methods :as methods]
   [criterium.analyse.metrics-samples]
   [criterium.collect-plan :as collect-plan]
   [criterium.metric :as metric]
   [criterium.stats.interface :as stats]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.util.debug :as debug]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]))

(defn exp
  ^double [^double v]
  (Math/exp v))

(defn log
  ^double [^double v]
  (Math/log v))

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
     (let [samples-id (or samples-id :samples)
           id (or id (keyword (str "log-" (name samples-id))))
           metrics-samples (util/get-generic-data-entry data-map samples-id)
           metrics-defs (-> (:metrics-defs metrics-samples)
                            (metric/select-metrics metric-ids)
                            (metric/filter-metrics
                             (every-pred
                              (metric/type-pred :quantitative)
                              (metric/dimension-pred :time))))
           metric-configs (metric/all-metric-configs metrics-defs)
           transformed (->
                        (methods/transform
                         metrics-samples
                         metric-configs
                         log
                         exp
                         options)
                        (merge
                         {:source-id samples-id :metrics-defs metrics-defs}))]
       (assoc data-map id transformed)))))

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
     (let [samples-id (or samples-id :samples)
           id (or id :quantiles)
           metrics-samples (data-map samples-id)
           metrics-defs (-> (:metrics-defs metrics-samples)
                            (metric/select-metrics metric-ids)
                            (metric/filter-metrics
                             (metric/type-pred :quantitative)))
           metric-configs (metric/all-metric-configs metrics-defs)
           quantiles (methods/quantiles
                      metrics-samples
                      metric-configs
                      analysis)
           quantiles-map (util/->quantiles-map
                          (merge
                           {:type :criterium/quantiles
                            :source-id samples-id
                            :metrics-defs metrics-defs}
                           quantiles))]
       (assoc data-map id quantiles-map)))))

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
      :outlier-method - Method for computing outlier thresholds:
                        :adjusted (default) - adjusted boxplot for skewed data
                        :standard - symmetric 1.5×IQR whiskers
                        :auto - uses :adjusted for metrics-samples,
                                :standard for digest (same as default)
                        Note: digest-based samples always use :standard
                        regardless of this setting.

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
  ([{:keys [id samples-id quantiles-id metric-ids outlier-method]}]
   (fn [data-map]
     (let [id (or id :outliers)
           quantiles-id (or quantiles-id :quantiles)
           samples-id (or samples-id :samples)
           all-quantiles (have (data-map quantiles-id))
           metrics-samples (have (data-map samples-id))
           metrics-defs (-> (:metrics-defs all-quantiles)
                            (metric/select-metrics metric-ids))
           metric-configs (metric/all-metric-configs metrics-defs)]
       (when-not all-quantiles
         (throw (ex-info
                 "outlier analysis requires quantiles analysis"
                 {:quantiles-id quantiles-id
                  :available-ids (keys data-map)})))
       (let [outliers (methods/outliers
                       metrics-samples
                       all-quantiles
                       metric-configs
                       {:outlier-method outlier-method})
             outliers-map (util/->outliers-map
                           (merge
                            {:type :criterium/outliers
                             :source-id samples-id
                             :quantiles-id quantiles-id
                             :metrics-defs metrics-defs}
                            outliers))]
         (assoc data-map id outliers-map))))))

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
     :as analysis}]
   (let [samples-id (or samples-id :samples)
         id (or id :stats)
         outliers-id (or outliers-id :outliers)]
     (fn [data-map]
       (debug/dtap> {:stats id})
       (let [outliers (when outliers-id
                        (data-map outliers-id))
             metrics-samples (have (data-map samples-id))
             metrics-defs (-> (have (:metrics-defs metrics-samples))
                              (metric/select-metrics metric-ids)
                              (metric/filter-metrics
                               (metric/type-pred :quantitative)))
             metric-configs (metric/all-metric-configs metrics-defs)
             stats (methods/stats
                    metrics-samples
                    outliers
                    metric-configs
                    analysis)
             stats-map (util/->stats-map
                        (merge
                         {:type :criterium/stats
                          :metrics-defs metrics-defs
                          :source-id samples-id
                          :outliers-id outliers-id
                          :batch-size (:batch-size metrics-samples)}
                         stats))]
         (assoc data-map id stats-map))))))

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
   (let [id (or id :event-stats)
         samples-id (or samples-id :samples)]
     (fn [data-map]
       (debug/dtap> {:event-stats id})
       (let [metrics-samples (data-map samples-id)
             metrics-defs (-> (:metrics-defs metrics-samples)
                              (metric/select-metrics metric-ids)
                              (metric/filter-metrics
                               (metric/type-pred :event)))
             event-stats (methods/event-stats
                          metrics-samples
                          metrics-defs
                          analysis)
             es-map (util/->event-stats-map
                     (merge
                      {:type :criterium/event-stats
                       :source-id samples-id
                       :metrics-defs metrics-defs}
                      event-stats))]
         (assoc data-map id es-map))))))

(defn histogram
  "Calculate a histogram for sample measurements.

  Returns a function that computes histograms for quantitative metrics.

  Parameters:
    opts - Optional map with keys:
      :id           - Key for histograms in output (default: :histograms)
      :samples-id   - Key for source samples (default: :samples)
      :quantiles-id - Key for quantile analysis (default: :quantiles)
      :outliers-id  - Key for outlier analysis if available (default: :outliers)
      :metric-ids   - Set of metric ids to analyze (default: all quantitative)
      :method       - Binning method:
                      - :freedman-diaconis (default) - IQR-based bin width
                      - :knuth - Bayesian optimal bin count selection
      :max-bins     - Maximum bins to search (only for :knuth, default: 50)

  The returned function:
  - Takes a sampled data map containing samples
  - Returns the map with histograms added under :id key
  - Preserves data transforms for correct scaling
  - For :knuth method, histogram includes :optimal-bins and :log-posterior

  Example:
  (let [analyze (histogram)
        result (analyze {:samples {...} :outliers {...}})]
    (get-in result [:histograms :elapsed-time]))

  Example with Knuth binning:
  (let [analyze (histogram {:method :knuth})
        result (analyze {:samples {...} :outliers {...}})]
    (:optimal-bins (get-in result [:histograms :elapsed-time])))"
  ([] (histogram {}))
  ([{:keys [id samples-id quantiles-id outliers-id metric-ids]
     :as analysis}]
   (let [samples-id (or samples-id :samples)
         id (or id :histograms)
         quantiles-id (or quantiles-id :quantiles)
         outliers-id (or outliers-id :outliers)]
     (fn [data-map]
       (let [outliers (when outliers-id
                        (data-map outliers-id))
             quantiles (util/lookup-data data-map quantiles-id)
             metrics-samples (util/lookup-data data-map samples-id)
             metrics-defs (-> (have (:metrics-defs metrics-samples))
                              (metric/select-metrics metric-ids)
                              (metric/filter-metrics
                               (metric/type-pred :quantitative)))
             metric-configs (metric/all-metric-configs metrics-defs)
             histogram (methods/histogram
                        metrics-samples
                        quantiles
                        outliers
                        metric-configs
                        analysis)
             histogram-map (util/->histogram-map
                            (merge
                             {:type :criterium/histogram
                              :metrics-defs metrics-defs
                              :source-id samples-id
                              :outliers-id outliers-id
                              :batch-size (:batch-size metrics-samples)}
                             histogram))]
         (assoc data-map id histogram-map))))))

(defn kde
  "Calculate kernel density estimation for sample measurements.

  Returns a function that computes KDE for quantitative metrics, providing
  density estimates, bandwidth selection, and confidence bands.

  Parameters:
    opts - Optional map with keys:
      :id          - Key for KDE results in output (default: :kde)
      :samples-id  - Key for source samples (default: :log-samples)
      :outliers-id - Key for outlier analysis if available (default: :outliers)
      :metric-ids  - Set of metric ids to analyze (default: all quantitative)
      :n-points    - Grid size for density evaluation (default: 512)
      :n-bootstrap - Bootstrap samples for confidence bands (default: 200)
      :alpha       - Confidence level (default: 0.05)

  The returned function:
  - Takes a sampled data map containing samples
  - Returns the map with KDE analysis added under :id key
  - Returns data-map unchanged if samples unavailable (e.g., digest-based)
  - For each metric provides:
    - bandwidth: ISJ-selected bandwidth
    - grid/density: evaluation points and density values
    - lower-band/upper-band: bootstrap confidence bands

  Note: Mode detection is now a separate analysis step. Use `modes` function
  after KDE to compute statistically validated modes with Silverman's test.

  Example:
  (let [analyze (kde {:n-points 256})
        result (analyze {:log-samples {...} :outliers {...}})]
    (get-in result [:kde :elapsed-time]))"
  ([] (kde {}))
  ([{:keys [id samples-id outliers-id metric-ids n-points n-bootstrap alpha]
     :as _options}]
   (let [samples-id (or samples-id :log-samples)
         id (or id :kde)
         outliers-id (or outliers-id :outliers)]
     (fn [data-map]
       (let [metrics-samples (get data-map samples-id)]
         (if-not metrics-samples
           data-map
           (let [outliers (when outliers-id
                            (data-map outliers-id))
                 metrics-defs (-> (have (:metrics-defs metrics-samples))
                                  (metric/select-metrics metric-ids)
                                  (metric/filter-metrics
                                   (metric/type-pred :quantitative)))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 kde-options (cond-> {}
                               n-points (assoc :n-points n-points)
                               n-bootstrap (assoc :n-bootstrap n-bootstrap)
                               alpha (assoc :alpha alpha))
                 kde-result (methods/kde
                             metrics-samples
                             outliers
                             metric-configs
                             kde-options)]
             (if kde-result
               (let [kde-map (util/->kde-map
                              (merge
                               {:type :criterium/kde
                                :metrics-defs metrics-defs
                                :source-id samples-id
                                :outliers-id outliers-id
                                :batch-size (:batch-size metrics-samples)}
                               kde-result))]
                 (assoc data-map id kde-map))
               data-map))))))))

(defn modes
  "Calculate statistically validated mode analysis for KDE output.

  Returns a function that computes modes with multimodality testing for significance.
  Tests from k=1 up to max-modes to determine the statistically supported
  number of modes.

  Parameters:
    opts - Optional map with keys:
      :id          - Key for modes results in output (default: :modes)
      :kde-id      - Key for source KDE data (default: :kde)
      :samples-id  - Key for raw samples (default: :log-samples)
      :outliers-id - Key for outlier data (default: :outliers)
      :metric-ids  - Set of metric ids to analyze (default: all from KDE)
      :max-modes   - Maximum modes to test (default: 5)
      :n-bootstrap - Bootstrap samples for CIs and test (default: 200)
      :alpha       - Significance level (default: 0.05)
      :method      - Test method, :acr (default) or :silverman
                     :acr uses excess mass statistic (better calibrated)
                     :silverman uses bootstrap mode count
      :mode-method - Mode finding method (default: :isj)
                     :isj - find modes from KDE density at ISJ bandwidth
                     :critical - find modes at critical bandwidth for validated k
                     When :critical, output includes :antimodes and :mode-bandwidth

  The returned function:
  - Takes a data map containing KDE and samples
  - Returns the map with mode analysis added under :id key
  - For each metric provides:
    - modes: detected peaks with CIs and significance flags
    - n-modes: statistically validated mode count
    - test-results: p-values, critical bandwidths, and method used
    - When :mode-method is :critical:
      - antimodes: local minima between modes
      - mode-bandwidth: critical bandwidth used for mode finding

  Example:
  (let [analyze (comp (modes) (kde))
        result (analyze {:log-samples {...}})]
    (get-in result [:modes :elapsed-time :n-modes]))"
  ([] (modes {}))
  ([{:keys [id kde-id samples-id outliers-id metric-ids max-modes n-bootstrap alpha
            method mode-method]
     :as _options}]
   (let [id (or id :modes)
         kde-id (or kde-id :kde)
         samples-id (or samples-id :log-samples)
         outliers-id (or outliers-id :outliers)]
     (fn [data-map]
       (let [kde-map (get data-map kde-id)
             samples (get data-map samples-id)]
         (if-not (and kde-map samples)
           data-map
           (let [outliers (when outliers-id (get data-map outliers-id))
                 metrics-defs (-> (have (:metrics-defs kde-map))
                                  (metric/select-metrics metric-ids)
                                  (metric/filter-metrics
                                   (metric/type-pred :quantitative)))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 modes-options (cond-> {}
                                 max-modes (assoc :max-modes max-modes)
                                 n-bootstrap (assoc :n-bootstrap n-bootstrap)
                                 alpha (assoc :alpha alpha)
                                 method (assoc :method method)
                                 mode-method (assoc :mode-method mode-method))
                 modes-result (methods/modes
                               kde-map
                               samples
                               outliers
                               metric-configs
                               modes-options)]
             (if modes-result
               (let [modes-map (util/->modes-map
                                (merge
                                 {:type :criterium/modes
                                  :metrics-defs metrics-defs
                                  :source-id kde-id
                                  :samples-id samples-id
                                  :outliers-id outliers-id}
                                 modes-result))]
                 (assoc data-map id modes-map))
               data-map))))))))

(defn kde-stats
  "Calculates descriptive statistics derived from KDE density estimates.

  Returns a function that computes statistics by integrating over the KDE
  density function. This provides a smoothed view of the underlying distribution
  compared to sample-based statistics.

  Parameters:
    opts - Optional map with keys:
      :id         - Key for stats in output (default: :kde-stats)
      :kde-id     - Key for source KDE data (default: :kde)
      :metric-ids - Set of metric ids to analyze (default: all quantitative)

  The returned function:
  - Takes a data map containing KDE analysis results
  - Returns the map with statistics added under :id key
  - Returns data-map unchanged if KDE data unavailable
  - For each metric, calculates:
    - mean: density-weighted mean ∫ x·f(x) dx
    - variance: density-weighted variance ∫ (x-μ)²·f(x) dx
    - min-val/max-val: grid bounds (effective support)
    - mean ±3σ bounds
    - n: sample count from KDE metadata

  Example:
  (let [analyze (kde-stats)
        result (analyze {:kde {...}})]
    (get-in result [:kde-stats :elapsed-time]))
  ;; Returns {:mean 100.0 :variance 16.0 ...}"
  ([] (kde-stats {}))
  ([{:keys [id kde-id metric-ids]}]
   (let [id (or id :kde-stats)
         kde-id (or kde-id :kde)]
     (fn [data-map]
       (let [kde-map (get data-map kde-id)]
         (if-not kde-map
           data-map
           (let [metrics-defs (-> (have (:metrics-defs kde-map))
                                  (metric/select-metrics metric-ids)
                                  (metric/filter-metrics
                                   (metric/type-pred :quantitative)))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 stats (methods/stats
                        kde-map
                        nil ; no outliers - already filtered in KDE
                        metric-configs
                        {})
                 stats-map (util/->stats-map
                            (merge
                             {:type :criterium/stats
                              :metrics-defs metrics-defs
                              :source-id kde-id
                              :outliers-id nil
                              :batch-size (:batch-size kde-map)}
                             stats))]
             (assoc data-map id stats-map))))))))

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
          std-dev-block (Math/sqrt variance-block)
          mean-g-min (/ mean 2)
          sigma-g (min (/ mean-g-min 4)
                       (/ std-dev-block (Math/sqrt batch-size)))
          variance-g (* sigma-g sigma-g)
          batch-size-sqr (util/sqr batch-size)
          c-max-f (fn ^long [^double t-min] ; Eq 38
                    (let [j0-sqr (util/sqr (- mean t-min))
                          k0 (- (* batch-size-sqr j0-sqr))
                          k1 (+ variance-block
                                (- (* batch-size variance-g))
                                (* batch-size j0-sqr))
                          det (- (* k1 k1)
                                 (* 4 variance-g k0))]
                      (long (Math/floor (/ (* -2 k0)
                                           (+ k1 (Math/sqrt det)))))))
          var-out (fn ^double [^long c] ; Eq 45
                    (let [nmc (- batch-size c)]
                      (* (/ nmc (double batch-size))
                         (- variance-block (* nmc variance-g)))))
          c-max (min-f c-max-f 0.0 mean-g-min)]
      (/ (min-f var-out 1.0 c-max) variance-block))))

(defn outlier-effect
  "Return a keyword describing the effect of outliers on a point estimate."
  [^double significance]
  (cond
    (< significance 0.01) :unaffected
    (< significance 0.1) :slight
    (< significance 0.5) :moderate
    :else :severe))

(defn- samples-outlier-significance [batch-size outliers stats metric-configs]
  (reduce
   (fn sample-m [result metric]
     (let [path (:path metric)
           outlier-data (get-in outliers path)
           stat (get-in stats path)
           _ (assert (map? outlier-data) outlier-data)
           outlier-counts (:outlier-counts outlier-data)
           significance (when (some pos? (vals outlier-counts))
                          (outlier-significance*
                           (:mean stat)
                           (:variance stat)
                           batch-size))]
       (update-in result path
                  assoc
                  :significance significance
                  :effect (when significance (outlier-effect significance)))))
   {}
   metric-configs))

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
     (let [id (or id :outlier-significance)
           outliers-id (or outliers-id :outliers)
           stats-id (or stats-id :stats)
           outliers (data-map outliers-id)
           stats (data-map stats-id)
           metrics-defs (-> (:metrics-defs stats)
                            (metric/select-metrics metric-ids)
                            (metric/filter-metrics
                             (metric/type-pred :quantitative)))
           metric-configs (metric/all-metric-configs metrics-defs)]
       (when-not outliers
         (throw (ex-info
                 "outlier significance requires outlier analysis"
                 {:outliers-id outliers-id
                  :available-ids (keys data-map)})))
       (let [significance (samples-outlier-significance
                           (:batch-size stats)
                           (util/outliers outliers)
                           (util/stats stats)
                           metric-configs)
             os-map (util/->outlier-significance-map
                     {:type :criterium/outlier-significance
                      :transform collect-plan/identity-transforms
                      :outlier-significance significance
                      :metrics-defs metrics-defs
                      :outliers-id outliers-id
                      :source-id stats-id})]
         (assoc data-map id os-map))))))

;;; Allocation Analysis

(defn allocation-summary
  "Computes allocation summary statistics from an allocation trace.

  Delegates to criterium.allocation.analysis/summary-fn. Returns data-map
  unchanged if allocation trace is not present (no-op behavior).

  Parameters:
    opts - Optional map with keys:
      :id       - Key for result in output (default: :allocation-summary)
      :trace-id - Path for source trace in input (default: [:samples :allocation-trace])"
  ([] (allocation-summary {}))
  ([opts]
   (allocation-analysis/summary-fn opts)))

(defn allocation-hotspots
  "Identifies allocation hotspots by call-site from an allocation trace.

  Delegates to criterium.allocation.analysis/hotspots-fn. Returns data-map
  unchanged if allocation trace is not present (no-op behavior).

  Parameters:
    opts - Optional map with keys:
      :id       - Key for result in output (default: :allocation-hotspots)
      :trace-id - Path for source trace in input (default: [:samples :allocation-trace])
      :limit    - Maximum number of hotspots to return (default: 10)
      :order-by - Sort key, :bytes or :count (default: :bytes)"
  ([] (allocation-hotspots {}))
  ([opts]
   (allocation-analysis/hotspots-fn opts)))

(defn allocation-by-type
  "Groups allocations by object type from an allocation trace.

  Delegates to criterium.allocation.analysis/by-type-fn. Returns data-map
  unchanged if allocation trace is not present (no-op behavior).

  Parameters:
    opts - Optional map with keys:
      :id       - Key for result in output (default: :allocation-by-type)
      :trace-id - Path for source trace in input (default: [:samples :allocation-trace])"
  ([] (allocation-by-type {}))
  ([opts]
   (allocation-analysis/by-type-fn opts)))

(defn allocation-treemap
  "Transforms allocation records into hierarchical treemap data.

  Delegates to criterium.allocation.analysis/treemap-fn. Returns data-map
  unchanged if allocation trace is not present (no-op behavior).

  Parameters:
    opts - Optional map with keys:
      :id        - Key for result in output (default: :allocation-treemap)
      :trace-id  - Path for source trace in input (default: [:samples :allocation-trace])
      :group-by  - Hierarchy: :class→line→type or :type→class→line (default: :class→line→type)
      :size-by   - Value sizing: :count, :bytes, :bytes-per-allocation (default: :bytes)
      :filter-by - Filter: :freed, :not-freed, :all (default: :all)"
  ([] (allocation-treemap {}))
  ([opts]
   (allocation-analysis/treemap-fn opts)))

(defn distribution-fit
  "Fits parametric distributions to sample data using MLE.

  Returns a function that computes maximum likelihood estimates for
  gamma, log-normal, inverse-gaussian, and Weibull distributions,
  with model selection via AIC/BIC and goodness-of-fit testing.

  Parameters:
    opts - Optional map with keys:
      :id           - Key for fit results in output (default: :distribution-fit)
      :samples-id   - Key for source samples (default: :samples)
      :outliers-id  - Key for outlier analysis to filter (default: :outliers)
      :metric-ids   - Set of metric ids to analyze (default: all quantitative)
      :distributions - Vector of distributions to fit (default: all four)
                      Options: :gamma, :lognormal, :inverse-gaussian, :weibull
      :n-bootstrap  - Bootstrap samples for parameter CIs (default: 200)
      :alpha        - Significance level for CIs (default: 0.05)

  The returned function:
  - Takes a sampled data map containing samples
  - Returns the map with distribution fits added under :id key
  - For each metric provides:
    - :n - sample size
    - :warning - :small-sample if n < 30
    - :distributions - map of distribution to fit results:
      - :params - fitted parameters
      - :log-likelihood - maximized log-likelihood
      - :aic, :bic, :aicc - information criteria
      - :delta-aic - difference from best model's AIC
      - :ks-test, :cvm-test - goodness-of-fit test results
      - Or :error/:skipped if fitting failed
    - :best-model - distribution with lowest AIC
    - :parameter-cis - bootstrap CIs for best model parameters

  Uses moment-matching prefilter to skip distributions with invalid
  initial parameter estimates. Bootstraps parameter CIs only for
  the best model by AIC.

  Example:
  (let [analyze (distribution-fit {:distributions [:gamma :lognormal]})
        result (analyze {:samples {...} :outliers {...}})]
    (get-in result [:distribution-fit :elapsed-time :best-model]))
  ;; Returns :gamma or :lognormal"
  ([] (distribution-fit {}))
  ([{:keys [id samples-id metric-ids distributions n-bootstrap alpha]
     :as options}]
   (let [samples-id (or samples-id :samples)
         id (or id :distribution-fit)
         ;; Use :outliers as default only if key not provided
         ;; Explicit nil means no outlier filtering
         outliers-id (if (contains? options :outliers-id)
                       (:outliers-id options)
                       :outliers)]
     (fn [data-map]
       (let [metrics-samples (get data-map samples-id)]
         (if-not metrics-samples
           data-map
           (let [outliers (when outliers-id (get data-map outliers-id))
                 metrics-defs (-> (have (:metrics-defs metrics-samples))
                                  (metric/select-metrics metric-ids)
                                  (metric/filter-metrics
                                   (metric/type-pred :quantitative)))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 fit-options (cond-> {}
                               distributions (assoc :distributions distributions)
                               n-bootstrap (assoc :n-bootstrap n-bootstrap)
                               alpha (assoc :alpha alpha))
                 fit-result (methods/distribution-fit
                             metrics-samples
                             outliers
                             metric-configs
                             fit-options)]
             (if fit-result
               (let [fit-map (util/->distribution-fit-map
                              (merge
                               {:type :criterium/distribution-fit
                                :metrics-defs metrics-defs
                                :source-id samples-id
                                :outliers-id outliers-id
                                :batch-size (:batch-size metrics-samples)}
                               fit-result))]
                 (assoc data-map id fit-map))
               data-map))))))))

(defn tail-analysis
  "Computes tail statistics for extreme value analysis.

  Returns a function that performs tail analysis including Hill estimator,
  GPD fitting, mean residual life, tail ratios, and high quantile estimation.
  Uses raw samples WITHOUT outlier filtering - tail analysis requires the
  extreme values that would normally be considered outliers.

  Parameters:
    opts - Optional map with keys:
      :id               - Key for result in output (default: :tail-analysis)
      :samples-id       - Key for source samples (default: :samples)
      :metric-ids       - Set of metric ids to analyze (default: all quantitative)
      :threshold        - Explicit threshold value for POT analysis
      :threshold-quantile - Quantile to use as threshold (default: 0.9)
      :k-range          - Range of k values for Hill estimator
      :high-quantiles   - Quantiles to estimate via GPD (default: [0.99 0.999 0.9999])

  The returned function:
  - Takes a sampled data map containing samples
  - Returns the map with tail analysis added under :id key
  - For each metric provides:
    - :tail-ratios - p99/p95, p999/p99 ratios indicating tail heaviness
    - :hill - Hill estimator results with k-range, estimates, and stable-estimate
    - :gpd - GPD fit parameters (xi, sigma) for exceedances over threshold
    - :mrl - Mean residual life values for threshold selection guidance
    - :high-quantiles - Extreme quantile estimates using GPD extrapolation
    - :empirical-quantiles - Raw percentiles (p95, p99, p999)

  Note: Unlike other analyses, tail analysis does NOT filter outliers because
  extreme values ARE the tail we want to analyze.

  Example:
  (let [analyze (tail-analysis {:threshold-quantile 0.95})
        result (analyze {:samples {...}})]
    (get-in result [:tail-analysis :elapsed-time :gpd :xi]))"
  ([] (tail-analysis {}))
  ([{:keys [id samples-id metric-ids] :as options}]
   (let [samples-id (or samples-id :samples)
         id (or id :tail-analysis)]
     (fn [data-map]
       (let [metrics-samples (get data-map samples-id)]
         (if-not metrics-samples
           data-map
           (let [metrics-defs (-> (have (:metrics-defs metrics-samples))
                                  (metric/select-metrics metric-ids)
                                  (metric/filter-metrics
                                   (metric/type-pred :quantitative)))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 tail-result (methods/tail-analysis
                              metrics-samples
                              metric-configs
                              options)]
             (if tail-result
               (let [tail-map (util/->tail-analysis-map
                               (merge
                                {:type :criterium/tail-analysis
                                 :metrics-defs metrics-defs
                                 :source-id samples-id
                                 :batch-size (:batch-size metrics-samples)}
                                tail-result))]
                 (assoc data-map id tail-map))
               data-map))))))))

(defn autocorrelation
  "Computes autocorrelation function (ACF) to detect sample non-independence.

  Returns a function that analyzes lag-1 and higher lag autocorrelations.
  Use effective-sample-size-analysis and autocorrelation-classification
  separately to compute ESS and pattern/classification results.

  Parameters:
    opts - Optional map with keys:
      :id               - Key for result in output (default: :autocorrelation)
      :samples-id       - Key for source samples (default: :samples)
      :outlier-id       - Key for outlier data to filter (default: nil, no filtering)
      :metric-ids       - Set of metric ids to analyze (default: all quantitative)

  The returned function:
  - Takes a sampled data map containing samples
  - Returns the map with autocorrelation analysis added under :id key
  - For each metric provides:
    - :acf - Map of lag -> autocorrelation coefficient for lags 1 to n/2
    - :lag-1 - {:value r1 :severity <:none|:minor|:moderate|:severe>}
    - :effective-sample-size - {:n-original n} (for downstream analyses)

  When :outlier-id is nil (default), uses all samples for pattern detection
  before outlier removal. When :outlier-id is provided, filters outlier samples
  before computing ACF for more accurate effective sample size estimation.

  Example:
  (let [analyze (autocorrelation {})
        result (analyze {:samples {...}})]
    (get-in result [:autocorrelation :elapsed-time :acf]))"
  ([] (autocorrelation {}))
  ([{:keys [id samples-id outlier-id metric-ids] :as _options}]
   (let [samples-id (or samples-id :samples)
         id (or id :autocorrelation)]
     (fn [data-map]
       (let [metrics-samples (get data-map samples-id)]
         (if-not metrics-samples
           data-map
           (let [outliers (when outlier-id (get data-map outlier-id))
                 metrics-defs (-> (have (:metrics-defs metrics-samples))
                                  (metric/select-metrics metric-ids)
                                  (metric/filter-metrics
                                   (metric/type-pred :quantitative)))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 autocorr-result (methods/autocorrelation
                                  metrics-samples
                                  outliers
                                  metric-configs
                                  {})]
             (if autocorr-result
               (let [autocorr-map (util/->autocorrelation-map
                                   (merge
                                    {:type :criterium/autocorrelation
                                     :metrics-defs metrics-defs
                                     :source-id samples-id
                                     :outlier-id outlier-id}
                                    autocorr-result))]
                 (assoc data-map id autocorr-map))
               data-map))))))))

(defn effective-sample-size-analysis
  "Computes effective sample size and CI inflation from autocorrelation results.

  Returns a function that takes autocorrelation analysis output and computes
  effective sample size statistics. This enables computing effective sample size
  on filtered data independently from pattern detection.

  Parameters:
    opts - Optional map with keys:
      :id               - Key for result in output (default: :effective-sample-size)
      :autocorrelation-id - Key for source autocorrelation analysis (default: :autocorrelation)
      :metric-ids       - Set of metric ids to analyze (default: all from source)

  The returned function:
  - Takes a data map containing autocorrelation analysis results
  - Returns the map with effective sample size analysis added under :id key
  - For each metric provides:
    - :effective-sample-size - {:n-original n :n-effective n_eff :ratio ratio}
    - :ci-inflation-factor - Multiplier for confidence interval widths

  Example:
  (let [analyze (effective-sample-size-analysis {})
        result (analyze {:autocorrelation {...}})]
    (get-in result [:effective-sample-size :elapsed-time :ci-inflation-factor]))"
  ([] (effective-sample-size-analysis {}))
  ([{:keys [id autocorrelation-id metric-ids] :as _options}]
   (let [autocorrelation-id (or autocorrelation-id :autocorrelation)
         id (or id :effective-sample-size)]
     (fn [data-map]
       (let [autocorr-map (get data-map autocorrelation-id)]
         (if-not autocorr-map
           data-map
           (let [autocorr-data (util/autocorrelation autocorr-map)
                 metrics-defs (-> (:metrics-defs autocorr-map)
                                  (metric/select-metrics metric-ids))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 ess-results
                 (reduce
                  (fn [result metric-config]
                    (let [p (:path metric-config)
                          acf-data (get-in autocorr-data [p :acf])
                          n (get-in autocorr-data [p :effective-sample-size :n-original])]
                      (if (and acf-data n)
                        (let [ess (stats/effective-sample-size-analysis acf-data n)]
                          (assoc result p ess))
                        result)))
                  {}
                  metric-configs)
                 ess-map (util/->effective-sample-size-map
                          {:type :criterium/effective-sample-size
                           :effective-sample-size-data ess-results
                           :metrics-defs metrics-defs
                           :source-id autocorrelation-id})]
             (assoc data-map id ess-map))))))))

(defn autocorrelation-classification
  "Computes pattern detection and classification from autocorrelation results.

  Returns a function that takes autocorrelation analysis output and computes
  pattern detection and classification. This enables computing classification
  on unfiltered data independently from effective sample size.

  Parameters:
    opts - Optional map with keys:
      :id               - Key for result in output (default: :autocorrelation-classification)
      :autocorrelation-id - Key for source autocorrelation analysis (default: :autocorrelation)
      :metric-ids       - Set of metric ids to analyze (default: all from source)

  The returned function:
  - Takes a data map containing autocorrelation analysis results
  - Returns the map with classification analysis added under :id key
  - For each metric provides:
    - :ljung-box - {:q-statistic Q :df h :p-value p}
    - :pattern - :clean, :transient-effects, :drift, :periodic, :severe, or :alternating-*
    - :classification - :pass, :acceptable, :warning, or :fail
    - :detected-period - Integer period for :periodic pattern, nil otherwise

  Example:
  (let [analyze (autocorrelation-classification {})
        result (analyze {:autocorrelation {...}})]
    (get-in result [:autocorrelation-classification :elapsed-time :pattern]))"
  ([] (autocorrelation-classification {}))
  ([{:keys [id autocorrelation-id metric-ids] :as _options}]
   (let [autocorrelation-id (or autocorrelation-id :autocorrelation)
         id (or id :autocorrelation-classification)]
     (fn [data-map]
       (let [autocorr-map (get data-map autocorrelation-id)]
         (if-not autocorr-map
           data-map
           (let [autocorr-data (util/autocorrelation autocorr-map)
                 metrics-defs (-> (:metrics-defs autocorr-map)
                                  (metric/select-metrics metric-ids))
                 metric-configs (metric/all-metric-configs metrics-defs)
                 class-results
                 (reduce
                  (fn [result metric-config]
                    (let [p (:path metric-config)
                          acf-data (get-in autocorr-data [p :acf])
                          n (get-in autocorr-data [p :effective-sample-size :n-original])]
                      (if (and acf-data n)
                        (let [classification (stats/autocorrelation-classification acf-data n)]
                          (assoc result p classification))
                        result)))
                  {}
                  metric-configs)
                 class-map (util/->autocorrelation-classification-map
                            {:type :criterium/autocorrelation-classification
                             :classification-data class-results
                             :metrics-defs metrics-defs
                             :source-id autocorrelation-id})]
             (assoc data-map id class-map))))))))

(def bootstrap-stats
  "Analysis function that adds bootstrap statistics to the result.

  Computes bootstrap BCa confidence intervals for mean, variance, and
  configurable quantiles (0.1, 0.25, 0.5, 0.75, 0.9 by default plus any
  additional quantiles specified in opts).

  Parameters:
    opts - Optional map with keys:
      :id                 - Key for result in output (default: :bootstrap-stats)
      :samples-id         - Key for source samples (default: :samples)
      :outliers-id        - Key for outlier analysis (default: :outliers)
      :ess-id             - Key for effective sample size analysis (default: nil).
                            When provided, CI widths are inflated by ci-inflation-factor.
      :metric-ids         - Set of metric ids to analyze (default: all quantitative)
      :quantiles          - Additional quantiles beyond defaults (e.g., [0.99])
      :estimate-quantiles - Confidence interval bounds (e.g., [0.025 0.975])
      :bootstrap-size     - Number of bootstrap resamples (default: sample count)
      :min-samples        - Minimum sample size threshold (default: 30)

  The returned function:
  - Takes a data map containing samples (and optionally outliers)
  - Returns the map with bootstrap statistics added under :id key
  - For each metric, calculates bootstrapped estimates with BCa CIs for:
    - mean, variance
    - quantiles (0.1, 0.25, 0.5, 0.75, 0.9 plus configured)

  When :outliers-id is provided, outliers are removed from samples before
  bootstrap resampling.

  When :ess-id is provided and effective sample size analysis exists,
  CI widths are inflated by ci-inflation-factor and stored as
  :adjusted-estimate-quantiles."
  bootstrap/bootstrap-stats)

;;; Call Graph Analysis

(def most-called
  "Analysis function that identifies the most frequently called methods.

  Flattens the call tree and aggregates by class+method, returning a sorted
  vector of the top N methods by total call count.

  Parameters:
    opts - Optional map with keys:
      :id           - Key for result in output (default: :most-called)
      :call-tree-id - Key for source call tree (default: :call-tree)
      :limit        - Maximum methods to return (default: 20)

  The returned function:
  - Takes a data-map containing :call-tree
  - Returns the data-map with :most-called added
  - Returns data-map unchanged if call-tree is not present

  Result structure:
  {:type :criterium/most-called
   :most-called [{:class \"com.example.Foo\"
                  :method \"bar\"
                  :file \"Foo.java\"
                  :line 42
                  :total-calls 1500}
                 ...]}"
  call-graph-analysis/most-called)

(def filter-calls
  "Analysis function that filters the call graph and stores the result.

  Applies filter-call-tree to the source call tree and stores the filtered
  result under a new identifier. Multiple filter-calls can be chained to
  create different filtered views of the same call tree.

  Parameters:
    opts - Optional map with keys:
      :id               - Key for result in output (default: :filtered)
      :call-tree-id     - Key for source call tree (default: :call-tree)
      :exclude-packages - Set of package prefixes to exclude entirely
      :stop-at-packages - Set of package prefixes where traversal stops
      :max-depth        - Maximum depth to include (1 = root only)

  The returned function:
  - Takes a data-map containing :call-tree (or custom :call-tree-id)
  - Returns the data-map with filtered tree added under :id key
  - Returns data-map unchanged if call-tree is not present

  Result structure:
  {:type :criterium/filtered-call-tree
   :source-id :call-tree
   :filter-opts {...}
   :call-tree <filtered-tree>}"
  call-graph-analysis/filter-calls)
