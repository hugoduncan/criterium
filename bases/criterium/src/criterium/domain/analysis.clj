(ns criterium.domain.analysis
  "Analysis functions for domain data.

  Provides functions for extracting metrics, comparing across dimensions,
  fitting complexity models, and composable analysis pipelines."
  (:require
   [criterium.bench.config :as bench-config]
   [criterium.domain.types :as types]
   [criterium.util.helpers :as helpers]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]))

;;; Analysis

(defn- discover-quantitative-metrics
  "Discover quantitative metric-ids from a benchmark result.
  Prefers [:stats :metrics-defs], falls back to [:samples :metrics-defs]
  for :one-shot benchmarks that skip statistical analysis.

  Returns a sequence of metric-ids (keywords
  like :elapsed-time, :thread-allocation)."
  [bench-result]
  (let [metrics-defs (or (get-in bench-result [:stats :metrics-defs])
                         (get-in bench-result [:samples :metrics-defs]))]
    (->> metrics-defs
         (filter (fn [[_k v]] (= :quantitative (:type v))))
         (map first))))

;;; Metric Value Extraction Helpers

(defn- extract-metric-value
  "Extract the primary metric value from a benchmark data map.

  Prefers bootstrapped median (quantile 0.5) when available, falls back to
  mean from stats, then to raw samples for :one-shot benchmarks.

  Returns the transformed value, or nil if no source has the metric."
  [data-map metric-id]
  (or (helpers/bootstrap-quantile-value data-map metric-id 0.5)
      (helpers/stats-value data-map :stats metric-id :mean)
      (helpers/samples-single-value data-map metric-id)))

(defn- extract-error-bounds
  "Extract error bounds for a metric from a benchmark data map.

  Prefers bootstrap CI from quantile 0.5's estimate-quantiles when available,
  falls back to ±3σ from stats when bootstrap stats are unavailable.

  Returns [lower upper] tuple, or nil if no error bounds are available."
  [data-map metric-id]
  (if-let [bootstrap-ci (helpers/bootstrap-quantile-ci data-map metric-id 0.5)]
    [(:ci-lower bootstrap-ci) (:ci-upper bootstrap-ci)]
    (let [lower (helpers/stats-value data-map :stats metric-id :mean-minus-3sigma)
          upper (helpers/stats-value data-map :stats metric-id :mean-plus-3sigma)]
      (when (and lower upper)
        [lower upper]))))

(defn extract
  "Extract metric values from all runs in a domain.
  Returns a domain-extract result with a :metrics map.

  When metric-path is provided, extracts that single metric.
  When metric-path is nil or omitted, extracts all quantitative metrics
  discovered from the first run's :stats :metrics-defs.

  metric-path is a vector like [:stats :elapsed-time :mean].

  By default, extraction uses the bootstrapped median (quantile 0.5) when
  available, falling back to mean from stats when bootstrap stats are
  unavailable. When a metric-path is provided that explicitly requests
  :mean (e.g., [:stats :elapsed-time :mean]), the mean is extracted directly.

  Options:
    :with-error-bounds - When true, extracts error bounds for each value.
                         Prefers bootstrap CI from quantile 0.5 when available,
                         falls back to ±3σ from stats. Values become maps with
                         :value, :lower, and :upper keys.
    :metric-ids        - When metric-path is nil, filter to only these
                         metric-ids (e.g., [:elapsed-time :thread-allocation]).
                         If nil, extracts all quantitative metrics.

  For stats paths, transforms are applied to convert raw values to their
  display form.

  Returns nil for value if the metric is missing from a run.

  If the source domain has multiple implementations, :impl-axis and
  :implementations are preserved in the extract result for use by
  downstream analysis functions.

  Example - single metric:
  (extract domain [:stats :elapsed-time :mean])
  ;; => {:type :criterium/domain-extract
  ;;     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                              :data [[{:n 100} 1.23e-6] ...]}}}

  Example - all metrics (uses bootstrapped median):
  (extract domain)
  ;; => {:type :criterium/domain-extract
  ;;     :metrics {:elapsed-time {:metric [:stats :elapsed-time :median] :data [...]}
  ;;               :thread-allocation {:metric [:stats :thread-allocation :median] :data [...]}}}"
  ([domain]
   (extract domain nil {}))
  ([domain metric-path]
   (extract domain metric-path {}))
  ([domain metric-path {:keys [with-error-bounds metric-ids]}]
   ;; Validate metric-path structure when provided
   (when metric-path
     (have vector? metric-path
           {:reason "metric-path must be a vector like [:stats :metric-id :value-key]"
            :metric-path metric-path})
     (have #(= 3 (count %)) metric-path
           {:reason "metric-path must have exactly 3 elements: [stats-id metric-id value-key]"
            :metric-path metric-path}))
   (let [runs (types/runs domain)
         impl-axis-key (types/impl-axis domain)
         impls (types/implementations domain)
         multi-impl? (> (count impls) 1)
         ;; Determine which metrics to extract
         metric-ids-to-extract
         (if metric-path
           ;; Single metric-path provided - extract the metric-id from path
           [(second metric-path)]
           ;; Discover from first run
           (let [first-run-data (:data (first runs))
                 discovered (discover-quantitative-metrics first-run-data)]
             (if metric-ids
               (filter (set metric-ids) discovered)
               discovered)))

         ;; When metric-path is nil, use median extraction with helper functions
         ;; When metric-path is provided, use explicit path extraction
         use-median? (nil? metric-path)

         ;; Extract using median helper (when metric-path is nil)
         extract-with-median
         (fn [metric-id]
           {:metric [:stats metric-id :median]
            :with-error-bounds (boolean with-error-bounds)
            :data (mapv (fn [{:keys [coord data]}]
                          (let [value (extract-metric-value data metric-id)]
                            (if with-error-bounds
                              (let [[lower upper] (extract-error-bounds
                                                   data metric-id)]
                                [coord (when value
                                         {:value value
                                          :lower lower
                                          :upper upper})])
                              [coord value])))
                        runs)})

         ;; Extract using explicit path (when metric-path is provided)
         extract-with-path
         (fn [mpath]
           (let [[stats-id metric-id value-key] mpath
                 extract-bounds? (and with-error-bounds
                                      (= value-key :mean))]
             {:metric mpath
              :with-error-bounds (boolean extract-bounds?)
              :data (mapv (fn [{:keys [coord data]}]
                            (let [value (helpers/stats-value
                                         data
                                         stats-id
                                         metric-id
                                         value-key)]
                              (if extract-bounds?
                                (let [lower (helpers/stats-value
                                             data
                                             stats-id
                                             metric-id
                                             :mean-minus-3sigma)
                                      upper (helpers/stats-value
                                             data
                                             stats-id
                                             metric-id
                                             :mean-plus-3sigma)]
                                  [coord (when value
                                           {:value value
                                            :lower lower
                                            :upper upper})])
                                [coord value])))
                          runs)}))]
     (cond-> {:type :criterium/domain-extract
              :metrics (into {}
                             (if use-median?
                               (map (fn [mid] [mid (extract-with-median mid)]))
                               (map (fn [mid]
                                      [mid (extract-with-path metric-path)])))
                             metric-ids-to-extract)}
       multi-impl? (assoc :impl-axis impl-axis-key
                          :implementations impls)))))

(defn group-by-axis
  "Partition domain runs by values of an axis key.
  Returns a domain-grouped result containing sub-domains by axis value.

  Runs with map coordinates are grouped by the value of axis-key.
  Runs without the axis-key (including keyword coordinates) are grouped
  under nil.

  Example:
  (group-by-axis domain :impl)
  ;; => {:type :criterium/domain-grouped
  ;;     :axis :impl
  ;;     :data {:foo <domain with :impl :foo runs>
  ;;            :bar <domain with :impl :bar runs>
  ;;            nil  <domain with runs lacking :impl>}}"
  [domain axis-key]
  (let [runs (types/runs domain)
        grouped (group-by (fn [{:keys [coord]}]
                            (when (map? coord)
                              (get coord axis-key)))
                          runs)]
    {:type :criterium/domain-grouped
     :axis axis-key
     :data (into {}
                 (map (fn [[k runs]]
                        [k (reduce (fn [d {:keys [coord data]}]
                                     (types/add-run d coord data))
                                   (types/domain)
                                   runs)]))
                 grouped)}))

(defn compare-by
  "Compare metric values across an axis dimension.
  Returns a domain-comparison result showing how the metric varies across
  axis values.

  axis-key is the dimension to compare across.
  metric-path is [stats-id metric-id value-key] as used by extract.

  When metric-path is nil, discovers all quantitative metrics from the first
  run's :stats :metrics-defs, similar to extract. In multi-metric mode, the
  domain must have an :implementations key.

  By default, comparison uses the bootstrapped median (quantile 0.5) when
  available, falling back to mean from stats when bootstrap stats are
  unavailable. When a metric-path is provided that explicitly requests
  :mean (e.g., [:stats :elapsed-time :mean]), the mean is extracted directly.

  Options:
    :metric-ids        - When metric-path is nil, filter to only these metric-ids.
    :with-error-bounds - When true, extracts error bounds for each value.
                         Prefers bootstrap CI from quantile 0.5 when available,
                         falls back to ±3σ from stats. Values become maps with
                         :value, :lower, and :upper keys.

  In multi-metric mode, bootstrap quantile statistics (:median, :p10, :p90,
  :ci-lower, :ci-upper) are automatically included when available. This
  enables box plot visualization alongside the default bar charts.

  Single metric example:
  (compare-by domain :impl [:stats :elapsed-time :mean])
  ;; => {:type :criterium/domain-comparison
  ;;     :axis :impl
  ;;     :metric [:stats :elapsed-time :mean]
  ;;     :data {:foo [{:coord {:n 100 :impl :foo} :value 1.2e-6} ...]
  ;;            :bar [{:coord {:n 100 :impl :bar} :value 2.3e-6} ...]}}

  Multi-metric example:
  (compare-by domain :impl nil)
  ;; => {:type :criterium/domain-comparison
  ;;     :axis :impl
  ;;     :implementations [:foo :bar]
  ;;     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                              :data {:foo [...] :bar [...]}}
  ;;               :thread-allocation {:metric [...] :data {...}}}}"
  ([domain axis-key metric-path]
   (compare-by domain axis-key metric-path {}))
  ([domain axis-key metric-path {:keys [metric-ids with-error-bounds]}]
   ;; Validate metric-path structure when provided
   (when metric-path
     (have vector? metric-path
           {:reason "metric-path must be a vector like [:stats :metric-id :value-key]"
            :metric-path metric-path})
     (have #(= 3 (count %)) metric-path
           {:reason "metric-path must have exactly 3 elements: [stats-id metric-id value-key]"
            :metric-path metric-path}))
   (let [runs (types/runs domain)
         impls (:implementations domain)
         grouped (:data (group-by-axis domain axis-key))]
     (if metric-path
       ;; Single metric mode
       (let [[stats-id metric-id value-key] metric-path
             extract-bounds? (and with-error-bounds
                                  (= value-key :mean))]
         (cond-> {:type :criterium/domain-comparison
                  :axis axis-key
                  :metric metric-path
                  :with-error-bounds (boolean extract-bounds?)
                  :data (into {}
                              (map (fn [[axis-val sub-domain]]
                                     [axis-val
                                      (mapv (fn [{:keys [coord data]}]
                                              (let [value (helpers/stats-value
                                                           data stats-id
                                                           metric-id value-key)]
                                                {:coord coord
                                                 :value (if extract-bounds?
                                                          (let [lower (helpers/stats-value
                                                                       data stats-id
                                                                       metric-id
                                                                       :mean-minus-3sigma)
                                                                upper (helpers/stats-value
                                                                       data stats-id
                                                                       metric-id
                                                                       :mean-plus-3sigma)]
                                                            (when value
                                                              {:value value
                                                               :lower lower
                                                               :upper upper}))
                                                          value)}))
                                            (types/runs sub-domain))]))
                              grouped)}
           impls (assoc :implementations impls)))
       ;; Multi-metric mode - uses median extraction with bootstrap stats
       (let [first-run-data (:data (first runs))
             discovered (discover-quantitative-metrics
                         first-run-data)
             metric-ids-to-extract (if metric-ids
                                     (filter (set metric-ids) discovered)
                                     discovered)
             ;; Function to extract median and bootstrap stats for a metric
             compare-with-median
             (fn [metric-id]
               (let [extract-bounds? with-error-bounds]
                 {:metric [:stats metric-id :median]
                  :with-error-bounds (boolean extract-bounds?)
                  :data (into {}
                              (map (fn [[axis-val sub-domain]]
                                     [axis-val
                                      (mapv (fn [{:keys [coord data]}]
                                              (let [median-value (extract-metric-value
                                                                  data metric-id)
                                                    ;; Base value with median
                                                    base-value
                                                    (if extract-bounds?
                                                      (let [[lower upper]
                                                            (extract-error-bounds
                                                             data metric-id)]
                                                        (when median-value
                                                          (cond-> {:value median-value}
                                                            lower (assoc :lower lower)
                                                            upper (assoc :upper upper))))
                                                      median-value)
                                                    ;; Bootstrap stats (when available)
                                                    bootstrap (helpers/bootstrap-box-plot-stats
                                                               data metric-id)]
                                                {:coord coord
                                                 :value (if bootstrap
                                                          (merge (if (map? base-value)
                                                                   base-value
                                                                   {:value base-value})
                                                                 bootstrap)
                                                          base-value)}))
                                            (types/runs sub-domain))]))
                              grouped)}))]
         (cond-> {:type :criterium/domain-comparison
                  :axis axis-key
                  :metrics (into {}
                                 (map (fn [metric-id]
                                        [metric-id (compare-with-median metric-id)]))
                                 metric-ids-to-extract)}
           impls (assoc :implementations impls)))))))

;;; Analysis Pipeline
;;
;; These functions return transformers for use in composable analysis pipelines.
;; Each takes an options map and returns a function that transforms a data-map,
;; following the same pattern as criterium.analyse functions.

(defn domain-extract-fn
  "Returns a function that extracts metric values from a domain in a data-map.

  Parameters:
    opts - Map with keys:
      :id               - Key for result in output (default: :extract)
      :domain-id        - Key for source domain in input (default: :domain)
      :metric-path      - Vector path to metric,
                             e.g. [:stats :elapsed-time :mean].
                          When nil, extracts all quantitative metrics.
      :metric-ids       - When metric-path is nil, filter to these metric-ids.
                          e.g., [:elapsed-time :thread-allocation].
      :with-error-bounds - When true and extracting :mean values, also
                           extracts error bounds (±3σ) for each value.

  The returned function:
  - Takes a data-map containing a domain under :domain-id
  - Returns the data-map with a domain-extract result added under :id

  Example - single metric:
  (-> {:domain my-domain}
      ((domain-extract-fn {:id :mean
                           :metric-path [:stats :elapsed-time :mean]})))

  Example - all metrics:
  (-> {:domain my-domain}
      ((domain-extract-fn {:id :metrics})))"
  ([] (domain-extract-fn {}))
  ([{:keys [id domain-id metric-path metric-ids with-error-bounds]}]
   (fn [data-map]
     (let [domain-id (or domain-id :domain)
           id (or id :extract)
           domain (data-map domain-id)
           result (extract
                   domain
                   metric-path
                   {:with-error-bounds with-error-bounds
                    :metric-ids metric-ids})]
       (assoc data-map id result)))))

(defn domain-group-by-fn
  "Returns a function that groups a domain by axis key in a data-map.

  Parameters:
    opts - Map with keys:
      :id        - Key for result in output (default: :grouped)
      :domain-id - Key for source domain in input (default: :domain)
      :axis-key  - Dimension key to group by

  The returned function:
  - Takes a data-map containing a domain under :domain-id
  - Returns the data-map with a domain-grouped result added under :id

  Example:
  (-> {:domain my-domain}
      ((domain-group-by-fn {:id :by-impl :axis-key :impl})))
  ;; => {:domain my-domain
  ;;     :by-impl {:type :criterium/domain-grouped ...}}"
  ([] (domain-group-by-fn {}))
  ([{:keys [id domain-id axis-key]}]
   (fn [data-map]
     (let [domain-id (or domain-id :domain)
           id (or id :grouped)
           domain (data-map domain-id)
           result (group-by-axis domain axis-key)]
       (assoc data-map id result)))))

(defn domain-compare-fn
  "Returns a function that compares metric values across an axis in a data-map.

  Parameters:
    opts - Map with keys:
      :id              - Key for result in output (default: :comparison)
      :domain-id       - Key for source domain in input (default: :domain)
      :axis-key        - Dimension key to compare across
      :metric-path     - Vector path to metric, e.g. [:stats :elapsed-time :mean].
                         When nil, extracts all quantitative metrics.
      :metric-ids      - When metric-path is nil, filter to these metric-ids.
                         E.g., [:elapsed-time :thread-allocation].
      :with-error-bounds - When true and extracting :mean values, also
                           extracts error bounds (±3σ) for each value.

  In multi-metric mode, bootstrap quantile statistics (median, p10, p90, CI)
  are automatically included when available, enabling box plot visualization.

  The returned function:
  - Takes a data-map containing a domain under :domain-id
  - Returns the data-map with a domain-comparison result added under :id

  Example - single metric:
  (-> {:domain my-domain}
      ((domain-compare-fn {:id :impl-vs-time
                           :axis-key :impl
                           :metric-path [:stats :elapsed-time :mean]})))

  Example - all metrics:
  (-> {:domain my-domain}
      ((domain-compare-fn {:id :impl-comparison
                           :axis-key :impl})))"
  ([] (domain-compare-fn {}))
  ([{:keys [id domain-id axis-key metric-path metric-ids with-error-bounds]}]
   (fn [data-map]
     (let [domain-id (or domain-id :domain)
           id (or id :comparison)
           domain (data-map domain-id)
           result (compare-by
                   domain
                   axis-key
                   metric-path
                   {:metric-ids metric-ids
                    :with-error-bounds with-error-bounds})]
       (assoc data-map id result)))))

;;; Regression Analysis
;;
;; Functions for fitting complexity models to domain extract data.
;; Enables quantitative determination of algorithmic complexity from
;; benchmark measurements across varying input sizes.

(defn compute-aic
  "Compute AICc (Akaike Information Criterion with small-sample correction).

  AICc = n*ln(RSS/n) + 2k + (2k(k+1))/(n-k-1)

  Where n = sample size, k = number of parameters, RSS = residual sum of squares.

  Returns nil if n <= k+1 (insufficient data for the correction term)."
  [^double rss ^long n ^long k]
  (when (> n (inc k))
    (let [base-aic (+ (* n (Math/log (/ rss n)))
                      (* 2.0 k))
          correction (/ (* 2.0 k (inc k))
                        (- n k 1))]
      (+ base-aic correction))))

(defn compute-bic
  "Compute BIC (Bayesian Information Criterion).

  BIC = n*ln(RSS/n) + k*ln(n)

  Where n = sample size, k = number of parameters, RSS = residual sum of squares.

  Returns nil if n = 0."
  [^double rss ^long n ^long k]
  (when (pos? n)
    (+ (* n (Math/log (/ rss n)))
       (* k (Math/log n)))))

(def default-complexity-models
  "Default complexity models for regression fitting.

  Simple models use :transform to map input size n to a single predictor.

  Composite models use :transforms for multiple predictors (e.g.,
  n*log(n) + n).

  Each model also includes:
    :predict-fn - Takes coefficients map → (fn [x] predicted-y)
    :equation-fn - Takes coefficients map → formatted equation string"
  {:constant     {:transform   (constantly 1.0)
                  :label       "O(1)"
                  :predict-fn  (fn [{:keys [^double a ^double b]}]
                                 (fn [_x] (+ a b)))
                  :equation-fn (fn [{:keys [^double a ^double b]}]
                                 (format "y = %.4g" (+ a b)))}
   :logarithmic  {:transform   (fn [^double n] (Math/log n))
                  :label       "O(log n)"
                  :predict-fn  (fn [{:keys [^double a ^double b]}]
                                 (fn [^double x] (+ (* a (Math/log x)) b)))
                  :equation-fn (fn [{:keys [a ^double b]}]
                                 (let [sign (if (neg? b) "-" "+")]
                                   (format "y = %.4g*log(n) %s %.4g"
                                           a sign (Math/abs ^double b))))}
   :linear       {:transform   identity
                  :label       "O(n)"
                  :predict-fn  (fn [{:keys [^double a ^double b]}]
                                 (fn [^double x] (+ (* a x) b)))
                  :equation-fn (fn [{:keys [a ^double b]}]
                                 (let [sign (if (neg? b) "-" "+")]
                                   (format "y = %.4g*n %s %.4g"
                                           a sign (Math/abs ^double b))))}
   :n-log-n      {:transform   (fn [^double n] (* n (Math/log n)))
                  :label       "O(n log n)"
                  :predict-fn  (fn [{:keys [^double a ^double b]}]
                                 (fn [^double x] (+ (* a x (Math/log x)) b)))
                  :equation-fn (fn [{:keys [a ^double b]}]
                                 (let [sign (if (neg? b) "-" "+")]
                                   (format "y = %.4g*n*log(n) %s %.4g"
                                           a sign (Math/abs ^double b))))}
   :nlogn-linear {:transforms  [(fn [^double n] (* n (Math/log n)))
                                (fn [^double n] n)]
                  :label       "O(n log n + n)"
                  :predict-fn  (fn [{:keys [^double a ^double b ^double c]}]
                                 (fn [^double x]
                                   (+ (* a x (Math/log x)) (* b x) c)))
                  :equation-fn (fn [{:keys [^double a ^double b ^double c]}]
                                 (let [sign-b (if (neg? b) "-" "+")
                                       sign-c (if (neg? c) "-" "+")]
                                   (format "y = %.4g*n*log(n) %s %.4g*n %s %.4g"
                                           a sign-b (Math/abs ^double b)
                                           sign-c (Math/abs ^double c))))}
   :quadratic
   {:transform   (fn [^double n] (* n n))
    :label       "O(n²)"
    :predict-fn  (fn [{:keys [^double a ^double b]}]
                   (fn [^double x]
                     (+ (* a x x) b)))
    :equation-fn (fn [{:keys [^double a ^double b]}]
                   (let [sign (if (neg? b) "-" "+")]
                     (format "y = %.4g*n² %s %.4g"
                             a sign (Math/abs ^double b))))}})

(defn- linear-regression
  "Perform simple linear regression: y = a*x + b.
  Returns {:a slope :b intercept :r-squared coefficient-of-determination}."
  [xs ys]
  (let [n (count xs)
        sum-x (double (reduce + xs))
        sum-y (double (reduce + ys))
        sum-xy (double (reduce + (map * xs ys)))
        sum-x2 (double (reduce
                        (fn [^double s ^double x] (+ s (* x x))) 0.0 xs))
        sum-y2 (double (reduce
                        (fn [^double s ^double x] (+ s (* x x))) 0.0 ys))
        ;; Calculate slope and intercept
        denom (- (* n sum-x2) (* sum-x sum-x))
        a (if (zero? denom)
            0.0
            (/ (- (* n sum-xy) (* sum-x sum-y)) denom))
        b (/ (- sum-y (* a sum-x)) n)
        ;; Calculate R²
        ss-tot (- sum-y2 (/ (* sum-y sum-y) n))
        ^double ss-res (reduce + (map (fn [^double x ^double y]
                                        (let [pred (+ (* a x) b)
                                              res (- y pred)]
                                          (* res res)))
                                      xs ys))
        r-sq (if (zero? ss-tot)
               (if (zero? ss-res) 1.0 0.0)
               (- 1.0 (/ ss-res ss-tot)))]
    {:a a
     :b b
     :r-squared r-sq}))

(defn log-log-regression
  "Perform linear regression on log-transformed data.

  Transforms (x, y) to (log(x), log(y)) and fits a linear model:
    log(y) = slope * log(x) + intercept

  The slope directly indicates algorithmic complexity:
    - slope ≈ 0   → O(1)
    - slope ≈ 0.5 → O(√n)
    - slope ≈ 1   → O(n)
    - slope ≈ 1.5 → O(n√n)
    - slope ≈ 2   → O(n²)

  Returns:
    {:slope      - complexity exponent
     :intercept  - log-space intercept
     :r-squared  - coefficient of determination in log-log space
     :log-xs     - log-transformed x values
     :log-ys     - log-transformed y values
     :residuals  - residuals in log space
     :predict-fn - (fn [x] predicted-y) in original space}"
  [xs ys]
  {:pre [(seq xs) (seq ys) (= (count xs) (count ys))
         (every? pos? xs) (every? pos? ys)]}
  (let [log-xs (mapv #(Math/log (double %)) xs)
        log-ys (mapv #(Math/log (double %)) ys)
        {:keys [^double a ^double b ^double r-squared]}
        (linear-regression log-xs log-ys)
        residuals (mapv (fn [^double lx ^double ly]
                          (- ly (+ (* a lx) b)))
                        log-xs log-ys)]
    {:slope a
     :intercept b
     :r-squared r-squared
     :log-xs log-xs
     :log-ys log-ys
     :residuals residuals
     :predict-fn (fn [^double x]
                   (Math/exp (+ (* a (Math/log x)) b)))}))

(defn- linear-regression-2
  "Perform multiple linear regression with 2 predictors: y = a*x1 + b*x2 + c.
  Uses centered variables for numerical stability, then adjusts intercept.
  Returns {:a coef-x1 :b coef-x2 :c intercept :r-squared r²}."
  [x1s x2s ys]
  (let [n (count ys)
        ;; Compute means for centering
        mean-x1 (/ (double (reduce + x1s)) n)
        mean-x2 (/ (double (reduce + x2s)) n)
        mean-y (/ (double (reduce + ys)) n)
        ;; Center variables (improves numerical stability)
        cx1s (mapv #(- (double %) mean-x1) x1s)
        cx2s (mapv #(- (double %) mean-x2) x2s)
        cys (mapv #(- (double %) mean-y) ys)
        ;; Compute X'X matrix elements (for centered data, no intercept column
        ;; needed)
        sum-cx1cx1 (double (reduce + (map * cx1s cx1s)))
        sum-cx2cx2 (double (reduce + (map * cx2s cx2s)))
        sum-cx1cx2 (double (reduce + (map * cx1s cx2s)))
        sum-cx1cy (double (reduce + (map * cx1s cys)))
        sum-cx2cy (double (reduce + (map * cx2s cys)))
        ;; Solve 2x2 system: [sum-cx1cx1  sum-cx1cx2] [a]   [sum-cx1cy]
        ;;                   [sum-cx1cx2  sum-cx2cx2] [b] = [sum-cx2cy]
        det (- (* sum-cx1cx1 sum-cx2cx2) (* sum-cx1cx2 sum-cx1cx2))
        [^double a ^double b]
        (if (< (Math/abs (double det)) 1e-10)
          ;; Near-singular: fall back to simple regression on first predictor
          (let [a1 (if (zero? sum-cx1cx1) 0.0 (/ sum-cx1cy sum-cx1cx1))]
            [a1 0.0])
          [(/ (- (* sum-cx1cy sum-cx2cx2) (* sum-cx2cy sum-cx1cx2)) det)
           (/ (- (* sum-cx2cy sum-cx1cx1) (* sum-cx1cy sum-cx1cx2)) det)])
        ;; Compute intercept: c = mean-y - a*mean-x1 - b*mean-x2
        c (- mean-y (* a mean-x1) (* b mean-x2))
        ;; Calculate R²
        sum-y2 (double (reduce (fn [^double s ^double y] (+ s (* y y))) 0.0 ys))
        ss-tot (- sum-y2 (/ (* (double (reduce + ys))
                               (double (reduce + ys)))
                            n))
        ss-res (double
                (reduce + (mapv (fn [^double x1 ^double x2 ^double y]
                                  (let [pred (+ (* a x1) (* b x2) c)
                                        res (- y pred)]
                                    (* res res)))
                                x1s x2s ys)))
        r-sq (if (zero? ss-tot)
               (if (zero? ss-res) 1.0 0.0)
               (- 1.0 (/ ss-res ss-tot)))]
    {:a a :b b :c c :r-squared r-sq}))

(defn- fit-complexity-model
  "Fit a single complexity model to data points.

  Supports simple models (single :transform) and composite
  models (two :transforms).

  Returns map with :id, :label, :coefficients, :r-squared, :residuals,
  :aic, :bic, and optionally :predict-fn (instantiated) and :equation-str.

  AIC/BIC may be nil if there is insufficient data for the correction term."
  [model-id {:keys [transform transforms label predict-fn equation-fn]} xs ys]
  (if transforms
    ;; Composite model: y = a*f1(x) + b*f2(x) + c
    (let [[t1 t2] transforms
          x1s (mapv (comp double t1) xs)
          x2s (mapv (comp double t2) xs)
          {:keys [^double a ^double b ^double c ^double r-squared]}
          (linear-regression-2 x1s x2s ys)
          coefficients {:a a :b b :c c}
          residuals (mapv (fn [^double x1 ^double x2 ^double y]
                            (- y (+ (* a x1) (* b x2) c)))
                          x1s x2s ys)
          n (count ys)
          k 3  ; 3 parameters for composite model
          rss (double (reduce (fn [^double acc ^double r] (+ acc (* r r)))
                              0.0 residuals))
          aic (compute-aic rss n k)
          bic (compute-bic rss n k)]
      (cond-> {:id model-id
               :label label
               :coefficients coefficients
               :r-squared r-squared
               :residuals residuals
               :aic aic
               :bic bic}
        predict-fn (assoc :predict-fn (predict-fn coefficients))
        equation-fn (assoc :equation-str (equation-fn coefficients))))
    ;; Simple model: y = a*f(x) + b
    (let [transformed-xs (mapv (comp double transform) xs)
          {:keys [^double a ^double b ^double r-squared]}
          (linear-regression transformed-xs ys)
          coefficients {:a a :b b}
          residuals (mapv (fn [^double tx ^double y]
                            (- y (+ (* a tx) b)))
                          transformed-xs ys)
          n (count ys)
          k 2  ; 2 parameters for simple model
          rss (double (reduce (fn [^double acc ^double r] (+ acc (* r r)))
                              0.0 residuals))
          aic (compute-aic rss n k)
          bic (compute-bic rss n k)]
      (cond-> {:id model-id
               :label label
               :coefficients coefficients
               :r-squared r-squared
               :residuals residuals
               :aic aic
               :bic bic}
        predict-fn (assoc :predict-fn (predict-fn coefficients))
        equation-fn (assoc :equation-str (equation-fn coefficients))))))

(defn- within-threshold?
  "Check if diff is within threshold, handling -Infinity values.
  When both values are -Infinity (perfect fit), they are considered equal."
  [^double best-val ^double val ^double threshold]
  (let [diff (- val best-val)]
    (or (Double/isNaN diff)  ; Both -Infinity → NaN diff → treat as equal
        (< diff threshold))))

(defn- select-by-r-squared
  "Select best model by highest R², preferring simpler when close."
  [fitted threshold]
  (let [sorted (sort-by (juxt #(- (double (:r-squared %)))
                              :param-count)
                        fitted)
        best (first sorted)
        best-val (double (:r-squared best))
        within-threshold (filterv #(within-threshold?
                                    (- best-val)
                                    (- (double (:r-squared %)))
                                    threshold)
                                  sorted)]
    (:id (first (sort-by :param-count within-threshold)))))

(defn- select-best-model
  "Select best model based on selection method.

  selection-method can be:
    :aic       - select model with lowest AICc (default)
    :bic       - select model with lowest BIC
    :r-squared - select model with highest R²

  In all cases, prefers simpler models (fewer parameters) when values
  are essentially equal (within threshold).

  When using :aic or :bic, falls back to :r-squared selection if no
  models have valid AIC/BIC values (insufficient data for information
  criterion computation)."
  [fitted selection-method]
  (when (seq fitted)
    (let [threshold 1e-6]
      (case selection-method
        :r-squared
        (select-by-r-squared fitted threshold)

        :bic
        ;; Lowest BIC, prefer simpler when close
        ;; Filter out models with nil BIC, fall back to R² if none
        (let [with-bic (filterv #(some? (:bic %)) fitted)]
          (if (seq with-bic)
            (let [sorted (sort-by (juxt #(double (:bic %))
                                        :param-count)
                                  with-bic)
                  best (first sorted)
                  best-val (double (:bic best))
                  within-threshold (filterv #(within-threshold?
                                              best-val
                                              (double (:bic %))
                                              threshold)
                                            sorted)]
              (:id (first (sort-by :param-count within-threshold))))
            ;; Fall back to R² selection when no BIC values
            (select-by-r-squared fitted threshold)))

        ;; Default: :aic - lowest AICc, prefer simpler when close
        ;; Filter out models with nil AIC, fall back to R² if none
        (let [with-aic (filterv #(some? (:aic %)) fitted)]
          (if (seq with-aic)
            (let [sorted (sort-by (juxt #(double (:aic %))
                                        :param-count)
                                  with-aic)
                  best (first sorted)
                  best-val (double (:aic best))
                  within-threshold (filterv #(within-threshold?
                                              best-val
                                              (double (:aic %))
                                              threshold)
                                            sorted)]
              (:id (first (sort-by :param-count within-threshold))))
            ;; Fall back to R² selection when no AIC values
            (select-by-r-squared fitted threshold)))))))

(defn fit-complexity
  "Fit complexity models to domain extract data.

  Returns a domain-regression result identifying how metrics scale with
  input size.

  extract is a domain-extract result (with :metrics map).
  axis is the coordinate key to use for x-values (e.g., :n for input size).

  Options (third argument can be a models map for backward compatibility,
  or an options map):
    :models           - Map of model-id to {:transform fn :label string},
                        or nil for defaults
    :selection-method - Method for selecting best model:
                        :aic (default) - lowest AICc
                        :bic           - lowest BIC
                        :r-squared     - highest R²

  Filters out data points with nil values before fitting.
  Selects best-fit model using the specified selection method,
  preferring simpler models when values are essentially equal.

  When the extract has multiple implementations (count of :implementations > 1),
  data is grouped by implementation and models are fit separately for each.
  The result includes :impl-axis key and regression data in :by-impl maps.

  Example - single implementation:
  (fit-complexity extract :n)
  ;; => {:type :criterium/domain-regression
  ;;     :axis :n
  ;;     :regressions {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                                  :models [{:id :linear :r-squared 0.98 ...}]
  ;;                                  :best-fit :linear}}}

  Example - with selection method:
  (fit-complexity extract :n {:selection-method :bic})

  Example - multiple implementations:
  (fit-complexity extract-with-impls :n)
  ;; => {:type :criterium/domain-regression
  ;;     :axis :n
  ;;     :impl-axis :impl
  ;;     :implementations [:vec :list]
  ;;     :regressions {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                                  :by-impl {:vec {:models [...] :best-fit :linear}
  ;;                                            :list {:models [...] :best-fit :quadratic}}}}}"
  ([extract axis] (fit-complexity extract axis {}))
  ([extract axis opts]
   ;; Support backward compatibility: if opts looks like a models map
   ;; (has :transform or :transforms in values), treat it as models
   (let [is-options-map? (or (nil? opts)
                             (empty? opts)
                             (contains? opts :models)
                             (contains? opts :selection-method))
         {:keys [models selection-method]}
         (if is-options-map?
           opts
           {:models opts})
         models (or models default-complexity-models)
         selection-method (or selection-method :aic)
         impl-axis-key (:impl-axis extract)
         impls (:implementations extract)
         multi-impl? (> (count impls) 1)

         ;; Helper to fit models to a set of data points
         fit-data-points
         (fn [data with-error-bounds]
           (let [get-value (if with-error-bounds
                             (fn [v] (when (map? v) (:value v)))
                             identity)
                 ;; Filter out nil y values and coords missing axis
                 valid-data (filterv (fn [[coord value]]
                                       (and (some? (get-value value))
                                            (map? coord)
                                            (contains? coord axis)))
                                     data)
                 xs (mapv
                     (fn [[coord _]] (double (get coord axis)))
                     valid-data)
                 ys (mapv
                     (fn [[_ value]] (double (get-value value)))
                     valid-data)
                 ;; Fit each model
                 fitted (when (>= (count xs) 2)
                          (mapv (fn [[model-id model-def]]
                                  (let [result (fit-complexity-model
                                                model-id
                                                model-def
                                                xs
                                                ys)
                                        param-count (if (:transforms model-def)
                                                      3
                                                      2)]
                                    (assoc
                                     result
                                     :param-count
                                     param-count)))
                                models))
                 best-fit (select-best-model fitted selection-method)]
             {:models (or fitted [])
              :best-fit best-fit}))

         ;; Fit a single metric's data (no implementation grouping)
         fit-single
         (fn [[_metric-id metric-data]]
           (let [{:keys [metric data with-error-bounds]} metric-data
                 result (fit-data-points
                         data
                         with-error-bounds)]
             (assoc result
                    :metric metric
                    :with-error-bounds (boolean with-error-bounds))))

         ;; Fit a single metric's data with implementation grouping
         fit-single-by-impl
         (fn [[_metric-id metric-data]]
           (let [{:keys [metric data with-error-bounds]} metric-data
                 ;; Group data by implementation
                 by-impl
                 (group-by
                  (fn [[coord _]] (get coord impl-axis-key))
                  data)
                 ;; Fit each implementation separately
                 impl-results
                 (into {}
                       (map (fn [[impl-val impl-data]]
                              [impl-val
                               (fit-data-points impl-data with-error-bounds)]))
                       by-impl)]
             {:metric metric
              :with-error-bounds (boolean with-error-bounds)
              :by-impl impl-results}))]

     (if multi-impl?
       ;; Multi-implementation: group and fit separately
       {:type :criterium/domain-regression
        :axis axis
        :impl-axis impl-axis-key
        :implementations impls
        :regressions (into {}
                           (map (fn [[metric-id metric-data]]
                                  [metric-id
                                   (fit-single-by-impl
                                    [metric-id metric-data])]))
                           (:metrics extract))}
       ;; Single implementation: original behavior
       {:type :criterium/domain-regression
        :axis axis
        :regressions (into {}
                           (map (fn [[metric-id metric-data]]
                                  [metric-id
                                   (fit-single [metric-id metric-data])]))
                           (:metrics extract))}))))

(defn domain-regression-fn
  "Returns a function that fits complexity models to a domain extract in a data-map.

  Parameters:
    opts - Map with keys:
      :id               - Key for result in output (default: :regression)
      :extract-id       - Key for source domain-extract in input (default: :extract)
      :axis             - Coordinate key to use for x-values (required, e.g., :n)
      :models           - Map of model definitions, or nil for defaults
      :selection-method - Method for selecting best model:
                          :aic (default) - lowest AICc
                          :bic           - lowest BIC
                          :r-squared     - highest R²

  The returned function:
  - Takes a data-map containing a domain-extract under :extract-id
  - Returns the data-map with a domain-regression result added under :id

  Example:
  (-> {:domain my-domain}
      ((domain-extract-fn {:id :extract
                           :metric-path [:stats :elapsed-time :mean]}))
      ((domain-regression-fn {:id :scaling :axis :n})))
  ;; => {:domain my-domain
  ;;     :extract {:type :criterium/domain-extract ...}
  ;;     :scaling {:type :criterium/domain-regression ...}}"
  ([] (domain-regression-fn {}))
  ([{:keys [id extract-id axis models selection-method]}]
   (fn [data-map]
     (let [extract-id (or extract-id :extract)
           id (or id :regression)
           extract (data-map extract-id)
           result (fit-complexity extract axis {:models models
                                                :selection-method selection-method})]
       (assoc data-map id result)))))

;;; Log-Log Regression Analysis
;;
;; Functions for computing log-log regression on domain extract data.
;; The log-log plot provides intuitive understanding of complexity class:
;; the slope directly indicates the complexity exponent.

(defn fit-log-log
  "Fit log-log linear regression to domain extract data.

  Returns a domain-log-log-regression result with log-transformed data
  and regression statistics for each metric.

  extract is a domain-extract result (with :metrics map).
  axis is the coordinate key to use for x-values (e.g., :n for input size).

  The log-log regression fits: log(y) = slope * log(x) + intercept
  The slope directly indicates complexity class:
    - slope ≈ 1   → O(n)
    - slope ≈ 2   → O(n²)

  Filters out data points with nil or non-positive values before fitting.

  When the extract has multiple implementations, data is grouped by
  implementation and regression is fit separately for each.

  Example - single implementation:
  (fit-log-log extract :n)
  ;; => {:type :criterium/domain-log-log-regression
  ;;     :axis :n
  ;;     :regressions {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                                  :slope 1.02
  ;;                                  :intercept -15.3
  ;;                                  :r-squared 0.998
  ;;                                  :log-xs [...]
  ;;                                  :log-ys [...]
  ;;                                  :xs [...]
  ;;                                  :ys [...]
  ;;                                  :residuals [...]}}}

  Example - multiple implementations:
  (fit-log-log extract-with-impls :n)
  ;; => {:type :criterium/domain-log-log-regression
  ;;     :axis :n
  ;;     :impl-axis :impl
  ;;     :implementations [:vec :list]
  ;;     :regressions {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                                  :by-impl {:vec {:slope 1.0 ...}
  ;;                                            :list {:slope 2.0 ...}}}}}"
  [extract axis]
  (let [impl-axis-key (:impl-axis extract)
        impls (:implementations extract)
        multi-impl? (> (count impls) 1)

        ;; Helper to fit log-log regression to a set of data points
        fit-data-points
        (fn [data with-error-bounds]
          (let [get-value (if with-error-bounds
                            (fn [v] (when (map? v) (:value v)))
                            identity)
                get-lower (when with-error-bounds
                            (fn [v] (when (map? v) (:lower v))))
                get-upper (when with-error-bounds
                            (fn [v] (when (map? v) (:upper v))))
                ;; Filter: valid coord with axis, positive x and y
                valid-data (filterv (fn [[coord value]]
                                      (let [y (get-value value)
                                            x (when (map? coord) (get coord axis))]
                                        (and (some? y) (pos? (double y))
                                             (some? x) (pos? (double x)))))
                                    data)]
            (when (>= (count valid-data) 2)
              (let [xs (mapv (fn [[coord _]] (double (get coord axis))) valid-data)
                    ys (mapv (fn [[_ v]] (double (get-value v))) valid-data)
                    {:keys [slope intercept r-squared log-xs log-ys residuals predict-fn]}
                    (log-log-regression xs ys)
                    ;; Transform error bounds to log space if present
                    log-lowers (when with-error-bounds
                                 (mapv (fn [[_ v]]
                                         (let [lower (get-lower v)]
                                           (when (and lower (pos? (double lower)))
                                             (Math/log (double lower)))))
                                       valid-data))
                    log-uppers (when with-error-bounds
                                 (mapv (fn [[_ v]]
                                         (let [upper (get-upper v)]
                                           (when (and upper (pos? (double upper)))
                                             (Math/log (double upper)))))
                                       valid-data))]
                (cond-> {:slope slope
                         :intercept intercept
                         :r-squared r-squared
                         :xs xs
                         :ys ys
                         :log-xs log-xs
                         :log-ys log-ys
                         :residuals residuals
                         :predict-fn predict-fn}
                  (and with-error-bounds (some some? log-lowers))
                  (assoc :log-lowers log-lowers :log-uppers log-uppers))))))

        ;; Fit a single metric's data (no implementation grouping)
        fit-single
        (fn [[_metric-id metric-data]]
          (let [{:keys [metric data with-error-bounds]} metric-data
                result (fit-data-points data with-error-bounds)]
            (cond-> {:metric metric
                     :with-error-bounds (boolean with-error-bounds)}
              result (merge result))))

        ;; Fit a single metric's data with implementation grouping
        fit-single-by-impl
        (fn [[_metric-id metric-data]]
          (let [{:keys [metric data with-error-bounds]} metric-data
                ;; Group data by implementation
                by-impl (group-by (fn [[coord _]] (get coord impl-axis-key)) data)
                ;; Fit each implementation separately
                impl-results
                (into {}
                      (keep (fn [[impl-val impl-data]]
                              (when-let [result (fit-data-points
                                                 impl-data with-error-bounds)]
                                [impl-val result])))
                      by-impl)]
            {:metric metric
             :with-error-bounds (boolean with-error-bounds)
             :by-impl impl-results}))]

    (if multi-impl?
      ;; Multi-implementation: group and fit separately
      {:type :criterium/domain-log-log-regression
       :axis axis
       :impl-axis impl-axis-key
       :implementations impls
       :regressions (into {}
                          (map (fn [[metric-id metric-data]]
                                 [metric-id
                                  (fit-single-by-impl [metric-id metric-data])]))
                          (:metrics extract))}
      ;; Single implementation
      {:type :criterium/domain-log-log-regression
       :axis axis
       :regressions (into {}
                          (map (fn [[metric-id metric-data]]
                                 [metric-id
                                  (fit-single [metric-id metric-data])]))
                          (:metrics extract))})))

(defn domain-log-log-fn
  "Returns a function that fits log-log regression to a domain extract in a data-map.

  Parameters:
    opts - Map with keys:
      :id         - Key for result in output (default: :log-log)
      :extract-id - Key for source domain-extract in input (default: :extract)
      :axis       - Coordinate key to use for x-values (required, e.g., :n)

  The returned function:
  - Takes a data-map containing a domain-extract under :extract-id
  - Returns the data-map with a domain-log-log-regression result added under :id

  Example:
  (-> {:domain my-domain}
      ((domain-extract-fn {:id :extract
                           :metric-path [:stats :elapsed-time :mean]}))
      ((domain-log-log-fn {:id :log-log :axis :n})))
  ;; => {:domain my-domain
  ;;     :extract {:type :criterium/domain-extract ...}
  ;;     :log-log {:type :criterium/domain-log-log-regression ...}}"
  ([] (domain-log-log-fn {}))
  ([{:keys [id extract-id axis]}]
   (fn [data-map]
     (let [extract-id (or extract-id :extract)
           id (or id :log-log)
           extract (data-map extract-id)
           result (fit-log-log extract axis)]
       (assoc data-map id result)))))

;;; Domain Plan Execution
;;
;; Functions for executing domain analysis plans, following the same
;; pattern as criterium.benchmark for sample analysis.

(defn- resolve-domain-analyse-fn
  "Resolves a single domain analysis function specification.
  If x is a sequence, treats first element as function and rest as args.
  Otherwise treats x as a function name to resolve.
  Returns a function of one argument (the data-map)."
  [x]
  (let [options {:default-ns 'criterium.domain.analysis}]
    (if (sequential? x)
      (apply (helpers/maybe-var-get (first x) options) (rest x))
      ((helpers/maybe-var-get x options)))))

(defn- resolve-domain-view-fn
  "Resolves a single domain view function specification.
  If x is a sequence, treats first element as function and rest as args.
  Otherwise treats x as a function name to resolve.
  Returns a function of two arguments (viewer, data-map)."
  [x]
  (let [options {:default-ns 'criterium.view}]
    (have
     fn?
     (if (sequential? x)
       (apply (helpers/maybe-var-get (first x) options) (rest x))
       ((helpers/maybe-var-get x options)))
     {:x x})))

(defn ->domain-analyse
  "Creates a composite analysis function from a sequence of analysis specs.

  Each spec is either a keyword/symbol to resolve a function, or a vector
  with a keyword/symbol first element followed by an options map.

  Analysis functions are resolved from the criterium.domain.analysis namespace.
  They are composed in sequence, each taking and returning a data-map.

  Example specs: [[:domain-extract-fn {:metric-path [...]}]
                  [:domain-regression-fn {:axis :n}]]

  Returns a function that takes a data-map and returns the analyzed data-map."
  [analyse-plan]
  (when-not (or (nil? analyse-plan) (sequential? analyse-plan))
    (throw
     (ex-info "analyse must be a sequence of specs" {:analyse analyse-plan})))
  (let [fns (mapv resolve-domain-analyse-fn analyse-plan)]
    (reduce comp (reverse fns))))

(defn ->domain-view
  "Creates a composite view function from a sequence of view specs.

  Each spec is either a keyword/symbol to resolve a function, or a vector
  with a keyword/symbol first element followed by an options map.

  View functions are resolved from the criterium.view namespace.
  They are called in order with the data-map, producing side effects.

  Example specs: [[:domain-extract-table {}]
                  [:domain-regression {}]]

  Returns a function that takes a viewer keyword and data-map, returns
  the viewer output (e.g., kindly fragment for :kindly viewer)."
  [view-plan]
  (when-not (or (nil? view-plan) (sequential? view-plan))
    (throw
     (ex-info "view must be a sequence of specs" {:view view-plan})))
  (let [fns (mapv resolve-domain-view-fn view-plan)]
    (fn [viewer data-map]
      {:pre [(have? keyword? viewer)]}
      (run! #(% viewer data-map) fns)
      (view/flush-viewer viewer))))

(defn options->domain-plan
  "Merge options into a base domain plan.

  Takes a base plan map and keyword/value pairs to override specific fields.

  Example:
    (options->domain-plan domain-plans/complexity-analysis
                          :viewer :portal)"
  [base-plan & {:as options}]
  (merge base-plan options))

(defn analyse-domain
  "Analyze a domain using a domain plan.

  The domain plan is a map with:
    :analyse      - Vector of analysis specs resolved from criterium.domain.analysis
    :view         - Vector of view specs resolved from criterium.view
    :viewer       - Keyword specifying output format (:print, :portal, :kindly, :none)
                    If not specified, uses the default viewer from
                    criterium.bench/set-default-viewer!
    :return-value - Path to extract from result data-map (default varies by viewer:
                    [:viewer :output] for :kindly, nil for others which returns
                    the full data-map)

  Returns the value at :return-value path, or the full data-map if not specified.

  Example:
    (analyse-domain domain-plans/complexity-analysis my-domain)

    (analyse-domain (options->domain-plan
                      domain-plans/complexity-analysis
                      :viewer :portal)
                    my-domain)"
  [domain-plan domain]
  (let [analyse-fn (->domain-analyse (:analyse domain-plan))
        view-fn (->domain-view (:view domain-plan))
        viewer (or (:viewer domain-plan) bench-config/*default-viewer* :print)
        default-return (when (= viewer :kindly) [:viewer :output])
        return-path (or (:return-value domain-plan) default-return)
        data-map (analyse-fn {:domain domain})
        viewer-output (view-fn viewer data-map)
        data-map (assoc data-map :viewer {:type :criterium/viewer-output
                                          :output viewer-output})]
    (if return-path
      (get-in data-map return-path)
      data-map)))
