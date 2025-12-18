(ns criterium.domain.analysis
  "Analysis functions for domain data.

  Provides functions for extracting metrics, comparing across dimensions,
  fitting complexity models, and composable analysis pipelines."
  (:require
   [criterium.bench.config :as bench-config]
   [criterium.domain.types :as types]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]))

;;; Analysis

(defn- discover-quantitative-metrics
  "Discover quantitative metric-ids from a benchmark result's stats.

  Returns a sequence of metric-ids (keywords
  like :elapsed-time, :thread-allocation)."
  [bench-result]
  (let [metrics-defs (get-in bench-result [:stats :metrics-defs])]
    (->> metrics-defs
         (filter (fn [[_k v]] (= :quantitative (:type v))))
         (map first))))

(defn extract
  "Extract metric values from all runs in a domain.
  Returns a domain-extract result with a :metrics map.

  When metric-path is provided, extracts that single metric.
  When metric-path is nil or omitted, extracts all quantitative metrics
  discovered from the first run's :stats :metrics-defs.

  metric-path is a vector like [:stats :elapsed-time :mean].

  Options:
    :with-error-bounds - When true and extracting :mean values, also
                         extracts :mean-plus-3sigma and :mean-minus-3sigma
                         as error bounds. Values become maps with :value,
                         :lower, and :upper keys.
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

  Example - all metrics:
  (extract domain)
  ;; => {:type :criterium/domain-extract
  ;;     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean] :data [...]}
  ;;               :thread-allocation {:metric [:stats :thread-allocation :mean] :data [...]}}}"
  ([domain]
   (extract domain nil {}))
  ([domain metric-path]
   (extract domain metric-path {}))
  ([domain metric-path {:keys [with-error-bounds metric-ids]}]
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

         ;; Build metric-path for each metric-id (default to :mean)
         metric-paths
         (if metric-path
           {(second metric-path) metric-path}
           (into {}
                 (map (fn [mid] [mid [:stats mid :mean]]))
                 metric-ids-to-extract))

         ;; Extract data for each metric
         extract-single
         (fn [metric-path]
           (let [[stats-id metric-id value-key] metric-path
                 extract-bounds? (and with-error-bounds
                                      (= value-key :mean))]
             {:metric metric-path
              :with-error-bounds (boolean extract-bounds?)
              :data (mapv (fn [{:keys [coord data]}]
                            (let [value (util/stats-value
                                         data
                                         stats-id
                                         metric-id
                                         value-key)]
                              (if extract-bounds?
                                (let [lower (util/stats-value
                                             data
                                             stats-id
                                             metric-id
                                             :mean-minus-3sigma)
                                      upper (util/stats-value
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
                             (map (fn [[metric-id mpath]]
                                    [metric-id (extract-single mpath)]))
                             metric-paths)}
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

  Options:
    :metric-ids - When metric-path is nil, filter to only these metric-ids.

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
  ([domain axis-key metric-path {:keys [metric-ids]}]
   (let [runs (types/runs domain)
         impls (:implementations domain)
         grouped (:data (group-by-axis domain axis-key))
         compare-single
         (fn [metric-path]
           (let [[stats-id metric-id value-key] metric-path]
             {:metric metric-path
              :data (into {}
                          (map (fn [[axis-val sub-domain]]
                                 [axis-val
                                  (mapv (fn [{:keys [coord data]}]
                                          {:coord coord
                                           :value (util/stats-value
                                                   data stats-id
                                                   metric-id value-key)})
                                        (types/runs sub-domain))]))
                          grouped)}))]
     (if metric-path
       ;; Single metric mode
       (let [[stats-id metric-id value-key] metric-path]
         (cond-> {:type :criterium/domain-comparison
                  :axis axis-key
                  :metric metric-path
                  :data (into {}
                              (map (fn [[axis-val sub-domain]]
                                     [axis-val
                                      (mapv (fn [{:keys [coord data]}]
                                              {:coord coord
                                               :value (util/stats-value
                                                       data stats-id
                                                       metric-id value-key)})
                                            (types/runs sub-domain))]))
                              grouped)}
           impls (assoc :implementations impls)))
       ;; Multi-metric mode
       (let [first-run-data (:data (first runs))
             discovered (discover-quantitative-metrics
                         first-run-data)
             metric-ids-to-extract (if metric-ids
                                     (filter (set metric-ids) discovered)
                                     discovered)
             metric-paths (into {}
                                (map
                                 (fn [mid] [mid [:stats mid :mean]]))
                                metric-ids-to-extract)]
         (cond-> {:type :criterium/domain-comparison
                  :axis axis-key
                  :metrics (into {}
                                 (map (fn [[metric-id mpath]]
                                        [metric-id (compare-single mpath)]))
                                 metric-paths)}
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
      :id          - Key for result in output (default: :comparison)
      :domain-id   - Key for source domain in input (default: :domain)
      :axis-key    - Dimension key to compare across
      :metric-path - Vector path to metric, e.g. [:stats :elapsed-time :mean].
                     When nil, extracts all quantitative metrics.
      :metric-ids  - When metric-path is nil, filter to these metric-ids.
                     E.g., [:elapsed-time :thread-allocation].

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
  ([{:keys [id domain-id axis-key metric-path metric-ids]}]
   (fn [data-map]
     (let [domain-id (or domain-id :domain)
           id (or id :comparison)
           domain (data-map domain-id)
           result (compare-by
                   domain
                   axis-key
                   metric-path
                   {:metric-ids metric-ids})]
       (assoc data-map id result)))))

;;; Regression Analysis
;;
;; Functions for fitting complexity models to domain extract data.
;; Enables quantitative determination of algorithmic complexity from
;; benchmark measurements across varying input sizes.

(def default-complexity-models
  "Default complexity models for regression fitting.

  Simple models use :transform to map input size n to a single predictor.

  Composite models use :transforms for multiple predictors (e.g.,
  n*log(n) + n)."
  {:constant {:transform (constantly 1.0)
              :label "O(1)"}
   :logarithmic {:transform (fn [^double n] (Math/log n))
                 :label "O(log n)"}
   :linear {:transform identity
            :label "O(n)"}
   :n-log-n {:transform (fn [^double n] (* n (Math/log n)))
             :label "O(n log n)"}
   :nlogn-linear {:transforms [(fn [^double n] (* n (Math/log n)))
                               (fn [^double n] n)]
                  :label "O(n log n + n)"}
   :quadratic {:transform (fn [^double n] (* n n))
               :label "O(n²)"}})

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

  Returns map with :id, :label, :coefficients, :r-squared, :residuals."
  [model-id {:keys [transform transforms label]} xs ys]
  (if transforms
    ;; Composite model: y = a*f1(x) + b*f2(x) + c
    (let [[t1 t2] transforms
          x1s (mapv (comp double t1) xs)
          x2s (mapv (comp double t2) xs)
          {:keys [^double a ^double b ^double c ^double r-squared]}
          (linear-regression-2 x1s x2s ys)
          residuals (mapv (fn [^double x1 ^double x2 ^double y]
                            (- y (+ (* a x1) (* b x2) c)))
                          x1s x2s ys)]
      {:id model-id
       :label label
       :coefficients {:a a :b b :c c}
       :r-squared r-squared
       :residuals residuals})
    ;; Simple model: y = a*f(x) + b
    (let [transformed-xs (mapv (comp double transform) xs)
          {:keys [^double a ^double b ^double r-squared]}
          (linear-regression transformed-xs ys)
          residuals (mapv (fn [^double tx ^double y]
                            (- y (+ (* a tx) b)))
                          transformed-xs ys)]
      {:id model-id
       :label label
       :coefficients {:a a :b b}
       :r-squared r-squared
       :residuals residuals})))

(defn fit-complexity
  "Fit complexity models to domain extract data.

  Returns a domain-regression result identifying how metrics scale with
  input size.

  extract is a domain-extract result (with :metrics map).
  axis is the coordinate key to use for x-values (e.g., :n for input size).
  models is a map of model-id to {:transform fn :label string}, or nil
  for defaults.

  Filters out data points with nil values before fitting.
  Selects best-fit model by highest R² value, preferring simpler models when
  R² values are essentially equal (within 0.0001).

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

  Example - multiple implementations:
  (fit-complexity extract-with-impls :n)
  ;; => {:type :criterium/domain-regression
  ;;     :axis :n
  ;;     :impl-axis :impl
  ;;     :implementations [:vec :list]
  ;;     :regressions {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                                  :by-impl {:vec {:models [...] :best-fit :linear}
  ;;                                            :list {:models [...] :best-fit :quadratic}}}}}"
  ([extract axis] (fit-complexity extract axis nil))
  ([extract axis models]
   (let [models (or models default-complexity-models)
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
                 ;; Find best fit by R², preferring simpler models when R² is
                 ;; close
                 best-fit (when (seq fitted)
                            (let [r-sq-threshold 1e-6
                                  sorted (sort-by
                                          (juxt
                                           #(- (double
                                                (:r-squared %)))
                                           :param-count)
                                          fitted)
                                  best (first sorted)
                                  within-threshold (filterv
                                                    #(<
                                                      (-
                                                       (double
                                                        (:r-squared best))
                                                       (double
                                                        (:r-squared %)))
                                                      r-sq-threshold)
                                                    sorted)]
                              (:id (first
                                    (sort-by
                                     :param-count
                                     within-threshold)))))]
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
      :id         - Key for result in output (default: :regression)
      :extract-id - Key for source domain-extract in input (default: :extract)
      :axis       - Coordinate key to use for x-values (required, e.g., :n)
      :models     - Map of model definitions, or nil for defaults

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
  ([{:keys [id extract-id axis models]}]
   (fn [data-map]
     (let [extract-id (or extract-id :extract)
           id (or id :regression)
           extract (data-map extract-id)
           result (fit-complexity extract axis models)]
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
      (apply (util/maybe-var-get (first x) options) (rest x))
      ((util/maybe-var-get x options)))))

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
       (apply (util/maybe-var-get (first x) options) (rest x))
       ((util/maybe-var-get x options)))
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

  Example specs: [[:domain-extract {}]
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
