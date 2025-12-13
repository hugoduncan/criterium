(ns criterium.domain
  "Abstraction for working with multiple related benchmark runs.

  A domain is an immutable collection of benchmark runs indexed by coordinates.
  It enables analysis of performance behaviour across a parameter space rather
  than at a single point.

  Supports:
  - Scaling behaviour analysis (e.g., O(N)) with varying argument values
  - Identifying parts of argument space with different metric behaviours
  - Comparing different implementation behaviours
  - Tracking behaviour over time (in-memory session)

  Example domain structure:
  {:type :criterium/domain
   :runs [{:coord {:n 100} :data <bench-result>}
          {:coord {:n 1000} :data <bench-result>}
          {:coord {:n 100 :impl :foo} :data <bench-result>}]}"
  (:require
   [criterium.bench :as bench]
   [criterium.bench.config :as bench-config]
   [criterium.measured :as measured]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]))

;;; Predicates

(defn run?
  "Returns true if x is a valid run map with :coord and :data keys."
  [x]
  (and (map? x)
       (contains? x :coord)
       (contains? x :data)))

(defn domain?
  "Returns true if x is a domain."
  [x]
  (and (map? x)
       (= :criterium/domain (:type x))
       (vector? (:runs x))))

(defn domain-extract?
  "Returns true if x is a domain extract result.
  Domain extracts contain a :metrics map with metric-id keys."
  [x]
  (and (map? x)
       (= :criterium/domain-extract (:type x))
       (map? (:metrics x))))

(defn domain-grouped?
  "Returns true if x is a domain grouped result."
  [x]
  (and (map? x)
       (= :criterium/domain-grouped (:type x))
       (contains? x :axis)
       (contains? x :data)))

(defn domain-comparison?
  "Returns true if x is a domain comparison result."
  [x]
  (and (map? x)
       (= :criterium/domain-comparison (:type x))
       (contains? x :axis)
       (contains? x :metric)
       (contains? x :data)))

;;; Construction

(defn domain
  "Create a domain from runs.

  With no arguments, creates an empty domain.
  With arguments, creates a domain containing those runs.

  Each run must be a map with :coord and :data keys where:
  - :coord is either a keyword label or a map of dimension keys to values
  - :data is the full benchmark result from bench

  Options (as final map argument):
  - :implementations - Keyword specifying which coordinate axis represents
                       different implementations (e.g., :impl). When set,
                       analysis functions like fit-complexity will group
                       results by implementation.

  Examples:
  (domain)                                    ; empty domain
  (domain {:coord :baseline :data result1})  ; single run with keyword coord
  (domain {:coord {:n 100} :data result1}    ; runs with map coords
          {:coord {:n 1000} :data result2})
  (domain {:coord {:n 100 :impl :vec} :data result1}
          {:coord {:n 100 :impl :list} :data result2}
          {:implementations :impl})           ; with implementations axis"
  [& args]
  (let [[runs opts] (if (and (seq args)
                             (map? (last args))
                             (contains? (last args) :implementations))
                      [(butlast args) (last args)]
                      [args nil])]
    (cond-> {:type :criterium/domain
             :runs (vec runs)}
      (:implementations opts) (assoc :implementations (:implementations opts)))))

;;; Accumulation

(defn add-run
  "Add a benchmark result to a domain with the given coordinates.
  Returns a new domain. If a run with the same coordinates already exists,
  it is replaced.

  coord can be either a keyword label or a map of dimension keys to values.
  data is the full benchmark result from bench."
  [domain coord data]
  (let [new-run {:coord coord :data data}
        runs (:runs domain)
        idx (reduce-kv (fn [_ i run]
                         (when (= coord (:coord run))
                           (reduced i)))
                       nil
                       runs)]
    (if idx
      (assoc domain :runs (assoc runs idx new-run))
      (assoc domain :runs (conj runs new-run)))))

(defn remove-run
  "Remove a run from a domain by coordinates.
  Returns a new domain. If no run with the given coordinates exists,
  returns the domain unchanged."
  [domain coord]
  (assoc domain :runs (into [] (remove #(= coord (:coord %))) (:runs domain))))

;;; Query

(defn- coord-matches?
  "Returns true if coord matches the partial-coord.
  For keyword coords, matches only if equal.
  For map coords, matches if partial-coord is a subset."
  [coord partial-coord]
  (cond
    (keyword? partial-coord)
    (= coord partial-coord)

    (map? partial-coord)
    (if (map? coord)
      (every? (fn [[k v]] (= (get coord k) v)) partial-coord)
      false)

    :else false))

(defn runs
  "Return runs from a domain.
  With one argument, returns all runs.
  With two arguments, returns runs matching the partial coordinate.

  For map coordinates, partial matching is supported:
  (runs domain {:n 100}) matches {:n 100}, {:n 100 :impl :foo}, etc."
  ([domain]
   (:runs domain))
  ([domain partial-coord]
   (filterv #(coord-matches? (:coord %) partial-coord) (:runs domain))))

(defn coords
  "Return all coordinates from a domain as a vector.
  Preserves the order of runs."
  [domain]
  (mapv :coord (:runs domain)))

(defn axes
  "Infer dimension keys from all map coordinates in a domain.
  Returns a set of keys. Keyword coordinates contribute no keys.

  Example:
  Given coords [{:n 100} {:n 1000 :impl :foo} :baseline]
  Returns #{:n :impl}"
  [domain]
  (into #{}
        (comp (map :coord)
              (filter map?)
              (mapcat keys))
        (:runs domain)))

(defn implementations
  "Returns the implementations axis key for a domain, or nil if not set.
  The implementations key specifies which coordinate axis represents
  different implementations for comparison analysis."
  [domain]
  (:implementations domain))

;;; Analysis

(defn- discover-quantitative-metrics
  "Discover quantitative metric-ids from a benchmark result's stats.
  Returns a sequence of metric-ids (keywords like :elapsed-time, :thread-allocation)."
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

  If the source domain has an :implementations key, it is preserved in
  the extract result for use by downstream analysis functions.

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
   (let [runs (:runs domain)
         impl-axis (:implementations domain)
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
                 extract-bounds? (and with-error-bounds (= value-key :mean))]
             {:metric metric-path
              :with-error-bounds (boolean extract-bounds?)
              :data (mapv (fn [{:keys [coord data]}]
                            (let [value (util/stats-value data stats-id metric-id value-key)]
                              (if extract-bounds?
                                (let [lower (util/stats-value data stats-id metric-id :mean-minus-3sigma)
                                      upper (util/stats-value data stats-id metric-id :mean-plus-3sigma)]
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
       impl-axis (assoc :implementations impl-axis)))))

(defn select
  "Filter domain to runs matching a partial coordinate.
  Returns a new domain containing only runs that match.

  For map coordinates, partial matching is supported:
  (select domain {:n 100}) returns domain with runs matching {:n 100},
  {:n 100 :impl :foo}, etc.

  For keyword coordinates, exact match is required.

  Example:
  (select domain {:impl :foo})
  ;; => domain with only :impl :foo runs"
  [domain partial-coord]
  (assoc domain :runs (filterv #(coord-matches? (:coord %) partial-coord)
                               (:runs domain))))

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
  (let [grouped (group-by (fn [{:keys [coord]}]
                            (when (map? coord)
                              (get coord axis-key)))
                          (:runs domain))]
    {:type :criterium/domain-grouped
     :axis axis-key
     :data (into {}
                 (map (fn [[k runs]]
                        [k (assoc domain :runs (vec runs))]))
                 grouped)}))

(defn compare-by
  "Compare metric values across an axis dimension.
  Returns a domain-comparison result showing how the metric varies across
  axis values.

  axis-key is the dimension to compare across.
  metric-path is [stats-id metric-id value-key] as used by extract.

  Example:
  (compare-by domain :impl [:stats :elapsed-time :mean])
  ;; => {:type :criterium/domain-comparison
  ;;     :axis :impl
  ;;     :metric [:stats :elapsed-time :mean]
  ;;     :data {:foo [{:coord {:n 100 :impl :foo} :value 1.2e-6} ...]
  ;;            :bar [{:coord {:n 100 :impl :bar} :value 2.3e-6} ...]}}"
  [domain axis-key metric-path]
  (let [[stats-id metric-id value-key] metric-path
        grouped (:data (group-by-axis domain axis-key))]
    {:type :criterium/domain-comparison
     :axis axis-key
     :metric metric-path
     :data (into {}
                 (map (fn [[axis-val sub-domain]]
                        [axis-val
                         (mapv (fn [{:keys [coord data]}]
                                 {:coord coord
                                  :value (util/stats-value data stats-id
                                                           metric-id value-key)})
                               (:runs sub-domain))]))
                 grouped)}))

;;; Input Sequence Generators
;;
;; These functions generate sequences of values useful for scaling analysis.
;; They are designed to work with domain coordinates, producing input sizes
;; that reveal algorithmic complexity patterns.

(defn powers-of-2
  "Generate a sequence of powers of 2.
  Returns (2^from 2^(from+1) ... 2^to) inclusive.

  Useful for testing algorithms where input size doubling reveals
  O(n), O(n log n), O(n²) patterns clearly.

  Examples:
  (powers-of-2 0 4)  ;=> (1 2 4 8 16)
  (powers-of-2 4 8)  ;=> (16 32 64 128 256)"
  [from to]
  (map #(bit-shift-left 1 %) (range from (inc to))))

(defn powers-of
  "Generate a sequence of powers of base.
  Returns (base^from base^(from+1) ... base^to) inclusive.

  Useful for testing with specific base increments.

  Examples:
  (powers-of 10 1 4)  ;=> (10 100 1000 10000)
  (powers-of 3 0 4)   ;=> (1 3 9 27 81)"
  [base from to]
  (map #(long (Math/pow base %)) (range from (inc to))))

(defn log-range
  "Generate a logarithmically-spaced sequence of n values from start to end.
  Values are rounded to integers.

  Produces values where the ratio between successive elements is constant,
  useful for covering a wide range of input sizes efficiently.

  Examples:
  (log-range 1 1000 4)  ;=> (1 10 100 1000)
  (log-range 10 10000 5) ;=> (10 56 316 1778 10000)"
  [start end n]
  (if (= n 1)
    (list (long start))
    (let [log-start (Math/log start)
          log-end (Math/log end)
          step (/ (- log-end log-start) (dec n))]
      (map #(long (Math/round (Math/exp (+ log-start (* % step)))))
           (range n)))))

(defn linear-range
  "Generate a linearly-spaced sequence of n values from start to end.
  Values are rounded to integers.

  Produces evenly-spaced values, useful for detecting linear scaling
  or when uniform sampling across a range is needed.

  Examples:
  (linear-range 100 500 5)  ;=> (100 200 300 400 500)
  (linear-range 0 1000 5)   ;=> (0 250 500 750 1000)"
  [start end n]
  (if (= n 1)
    (list (long start))
    (let [step (/ (- end start) (dec n))]
      (map #(long (Math/round (double (+ start (* % step))))) (range n)))))

(defn- invert-n-log-n
  "Solve x*ln(x) = y for x using Newton-Raphson iteration.
  f(x) = x*ln(x) - y, f'(x) = ln(x) + 1
  x_{n+1} = x_n - f(x_n)/f'(x_n)"
  [y initial-guess]
  (let [max-iterations 50
        tolerance 1e-10]
    (loop [x initial-guess
           i 0]
      (if (>= i max-iterations)
        x
        (let [ln-x (Math/log x)
              fx (- (* x ln-x) y)
              fpx (+ ln-x 1.0)]
          (if (< (Math/abs fx) tolerance)
            x
            (recur (- x (/ fx fpx)) (inc i))))))))

(defn n-log-n-range
  "Generate n values from start to end spaced along an n*log(n) curve.
  Values are rounded to integers.

  Useful for testing algorithms with O(n log n) complexity where you want
  denser sampling at smaller sizes and sparser sampling at larger sizes.

  Start must be >= e^-1 (approximately 0.368) where x*ln(x) has its minimum.

  Examples:
  (n-log-n-range 10 10000 5)  ;=> sequence of 5 values from 10 to 10000"
  [start end n]
  (have #(>= % (/ 1.0 Math/E)) start {:msg "start must be >= e^-1"})
  (if (= n 1)
    (list (long start))
    (let [f (fn [x] (* x (Math/log x)))
          y-start (f start)
          y-end (f end)
          y-step (/ (- y-end y-start) (dec n))]
      (map (fn [i]
             (let [y (+ y-start (* i y-step))]
               (if (zero? i)
                 (long start)
                 (if (= i (dec n))
                   (long end)
                   (long (Math/round (invert-n-log-n y start)))))))
           (range n)))))

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
      :metric-path      - Vector path to metric, e.g. [:stats :elapsed-time :mean].
                          When nil, extracts all quantitative metrics.
      :metric-ids       - When metric-path is nil, filter to these metric-ids.
                          E.g., [:elapsed-time :thread-allocation].
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
           result (extract domain metric-path {:with-error-bounds with-error-bounds
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
      :metric-path - Vector path to metric, e.g. [:stats :elapsed-time :mean]

  The returned function:
  - Takes a data-map containing a domain under :domain-id
  - Returns the data-map with a domain-comparison result added under :id

  Example:
  (-> {:domain my-domain}
      ((domain-compare-fn {:id :impl-vs-time
                           :axis-key :impl
                           :metric-path [:stats :elapsed-time :mean]})))
  ;; => {:domain my-domain
  ;;     :impl-vs-time {:type :criterium/domain-comparison ...}}"
  ([] (domain-compare-fn {}))
  ([{:keys [id domain-id axis-key metric-path]}]
   (fn [data-map]
     (let [domain-id (or domain-id :domain)
           id (or id :comparison)
           domain (data-map domain-id)
           result (compare-by domain axis-key metric-path)]
       (assoc data-map id result)))))

;;; Regression Analysis
;;
;; Functions for fitting complexity models to domain extract data.
;; Enables quantitative determination of algorithmic complexity from
;; benchmark measurements across varying input sizes.

(def default-complexity-models
  "Default complexity models for regression fitting.
  Simple models use :transform to map input size n to a single predictor.
  Composite models use :transforms for multiple predictors (e.g., n*log(n) + n)."
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
        sum-x (reduce + xs)
        sum-y (reduce + ys)
        sum-xy (reduce + (map * xs ys))
        sum-x2 (reduce + (map #(* % %) xs))
        sum-y2 (reduce + (map #(* % %) ys))
        ;; Calculate slope and intercept
        denom (- (* n sum-x2) (* sum-x sum-x))
        a (if (zero? denom)
            0.0
            (/ (- (* n sum-xy) (* sum-x sum-y)) denom))
        b (/ (- sum-y (* a sum-x)) n)
        ;; Calculate R²
        ss-tot (- sum-y2 (/ (* sum-y sum-y) n))
        ss-res (reduce + (map (fn [x y]
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
        mean-x1 (/ (reduce + x1s) n)
        mean-x2 (/ (reduce + x2s) n)
        mean-y (/ (reduce + ys) n)
        ;; Center variables (improves numerical stability)
        cx1s (mapv #(- % mean-x1) x1s)
        cx2s (mapv #(- % mean-x2) x2s)
        cys (mapv #(- % mean-y) ys)
        ;; Compute X'X matrix elements (for centered data, no intercept column needed)
        sum-cx1cx1 (reduce + (map * cx1s cx1s))
        sum-cx2cx2 (reduce + (map * cx2s cx2s))
        sum-cx1cx2 (reduce + (map * cx1s cx2s))
        sum-cx1cy (reduce + (map * cx1s cys))
        sum-cx2cy (reduce + (map * cx2s cys))
        ;; Solve 2x2 system: [sum-cx1cx1  sum-cx1cx2] [a]   [sum-cx1cy]
        ;;                   [sum-cx1cx2  sum-cx2cx2] [b] = [sum-cx2cy]
        det (- (* sum-cx1cx1 sum-cx2cx2) (* sum-cx1cx2 sum-cx1cx2))
        [a b] (if (< (Math/abs (double det)) 1e-10)
                ;; Near-singular: fall back to simple regression on first predictor
                (let [a1 (if (zero? sum-cx1cx1) 0.0 (/ sum-cx1cy sum-cx1cx1))]
                  [a1 0.0])
                [(/ (- (* sum-cx1cy sum-cx2cx2) (* sum-cx2cy sum-cx1cx2)) det)
                 (/ (- (* sum-cx2cy sum-cx1cx1) (* sum-cx1cy sum-cx1cx2)) det)])
        ;; Compute intercept: c = mean-y - a*mean-x1 - b*mean-x2
        c (- mean-y (* a mean-x1) (* b mean-x2))
        ;; Calculate R²
        sum-y2 (reduce + (map * ys ys))
        ss-tot (- sum-y2 (/ (* (reduce + ys) (reduce + ys)) n))
        ss-res (reduce + (map (fn [x1 x2 y]
                                (let [pred (+ (* a x1) (* b x2) c)
                                      res (- y pred)]
                                  (* res res)))
                              x1s x2s ys))
        r-sq (if (zero? ss-tot)
               (if (zero? ss-res) 1.0 0.0)
               (- 1.0 (/ ss-res ss-tot)))]
    {:a a :b b :c c :r-squared r-sq}))

(defn- fit-complexity-model
  "Fit a single complexity model to data points.
  Supports simple models (single :transform) and composite models (two :transforms).
  Returns map with :id, :label, :coefficients, :r-squared, :residuals."
  [model-id {:keys [transform transforms label]} xs ys]
  (if transforms
    ;; Composite model: y = a*f1(x) + b*f2(x) + c
    (let [[t1 t2] transforms
          x1s (mapv (comp double t1) xs)
          x2s (mapv (comp double t2) xs)
          {:keys [a b c r-squared]} (linear-regression-2 x1s x2s ys)
          residuals (mapv (fn [x1 x2 y]
                            (- y (+ (* a x1) (* b x2) c)))
                          x1s x2s ys)]
      {:id model-id
       :label label
       :coefficients {:a a :b b :c c}
       :r-squared r-squared
       :residuals residuals})
    ;; Simple model: y = a*f(x) + b
    (let [transformed-xs (mapv (comp double transform) xs)
          {:keys [a b r-squared]} (linear-regression transformed-xs ys)
          residuals (mapv (fn [tx y]
                            (- y (+ (* a tx) b)))
                          transformed-xs ys)]
      {:id model-id
       :label label
       :coefficients {:a a :b b}
       :r-squared r-squared
       :residuals residuals})))

(defn domain-regression?
  "Returns true if x is a domain regression result.
  Domain regressions contain a :regressions map with metric-id keys."
  [x]
  (and (map? x)
       (= :criterium/domain-regression (:type x))
       (contains? x :axis)
       (map? (:regressions x))))

(defn fit-complexity
  "Fit complexity models to domain extract data.
  Returns a domain-regression result identifying how metrics scale with input size.

  extract is a domain-extract result (with :metrics map).
  axis is the coordinate key to use for x-values (e.g., :n for input size).
  models is a map of model-id to {:transform fn :label string}, or nil for defaults.

  Filters out data points with nil values before fitting.
  Selects best-fit model by highest R² value, preferring simpler models when
  R² values are essentially equal (within 0.0001).

  When the extract has an :implementations key (from a multi-implementation domain),
  data is grouped by implementation and models are fit separately for each.
  The result includes :implementations key and regression data in :by-impl maps.

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
  ;;     :implementations :impl
  ;;     :regressions {:elapsed-time {:metric [:stats :elapsed-time :mean]
  ;;                                  :by-impl {:vec {:models [...] :best-fit :linear}
  ;;                                            :list {:models [...] :best-fit :quadratic}}}}}"
  ([extract axis] (fit-complexity extract axis nil))
  ([extract axis models]
   (let [models (or models default-complexity-models)
         impl-axis (:implementations extract)

         ;; Helper to fit models to a set of data points
         fit-data-points
         (fn [data with-error-bounds]
           (let [get-value (if with-error-bounds
                             (fn [v] (when (map? v) (:value v)))
                             identity)
                 ;; Filter out nil y values and coords missing axis
                 valid-data (filter (fn [[coord value]]
                                      (and (some? (get-value value))
                                           (map? coord)
                                           (contains? coord axis)))
                                    data)
                 xs (mapv (fn [[coord _]] (double (get coord axis))) valid-data)
                 ys (mapv (fn [[_ value]] (double (get-value value))) valid-data)
                 ;; Fit each model
                 fitted (when (>= (count xs) 2)
                          (mapv (fn [[model-id model-def]]
                                  (let [result (fit-complexity-model model-id model-def xs ys)
                                        param-count (if (:transforms model-def) 3 2)]
                                    (assoc result :param-count param-count)))
                                models))
                 ;; Find best fit by R², preferring simpler models when R² is close
                 best-fit (when (seq fitted)
                            (let [r-sq-threshold 1e-6
                                  sorted (sort-by (juxt #(- (:r-squared %)) :param-count) fitted)
                                  best (first sorted)
                                  within-threshold (filter #(< (- (:r-squared best) (:r-squared %))
                                                               r-sq-threshold)
                                                           sorted)]
                              (:id (first (sort-by :param-count within-threshold)))))]
             {:models (or fitted [])
              :best-fit best-fit}))

         ;; Fit a single metric's data (no implementation grouping)
         fit-single
         (fn [[_metric-id metric-data]]
           (let [{:keys [metric data with-error-bounds]} metric-data
                 result (fit-data-points data with-error-bounds)]
             (assoc result
                    :metric metric
                    :with-error-bounds (boolean with-error-bounds))))

         ;; Fit a single metric's data with implementation grouping
         fit-single-by-impl
         (fn [[_metric-id metric-data]]
           (let [{:keys [metric data with-error-bounds]} metric-data
                 ;; Group data by implementation
                 by-impl (group-by (fn [[coord _]] (get coord impl-axis)) data)
                 ;; Fit each implementation separately
                 impl-results (into {}
                                    (map (fn [[impl-val impl-data]]
                                           [impl-val (fit-data-points impl-data with-error-bounds)]))
                                    by-impl)]
             {:metric metric
              :with-error-bounds (boolean with-error-bounds)
              :by-impl impl-results}))]

     (if impl-axis
       ;; Multi-implementation: group and fit separately
       {:type :criterium/domain-regression
        :axis axis
        :implementations impl-axis
        :regressions (into {}
                           (map (fn [[metric-id metric-data]]
                                  [metric-id (fit-single-by-impl [metric-id metric-data])]))
                           (:metrics extract))}
       ;; Single implementation: original behavior
       {:type :criterium/domain-regression
        :axis axis
        :regressions (into {}
                           (map (fn [[metric-id metric-data]]
                                  [metric-id (fit-single [metric-id metric-data])]))
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
  (let [options {:default-ns 'criterium.domain}]
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

  Analysis functions are resolved from the criterium.domain namespace.
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
    :analyse      - Vector of analysis specs resolved from criterium.domain
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

;;; Domain Builder

(def ^:private default-initial-limit-time-s 10)

(defn- cartesian-product
  "Return cartesian product of axis values as sequence of maps.
  axes is a map of axis-key to sequence of values."
  [axes]
  (if (empty? axes)
    [{}]
    (let [[k vs] (first axes)
          rest-product (cartesian-product (dissoc axes k))]
      (for [v vs
            m rest-product]
        (assoc m k v)))))

(defn- predict-time-ns
  "Predict execution time for x using fitted model coefficients."
  [model x]
  (let [{:keys [coefficients]} model
        {:keys [a b c]} coefficients
        model-def (get default-complexity-models (:id model))]
    (if-let [transforms (:transforms model-def)]
      ;; Composite model: y = a*f1(x) + b*f2(x) + c
      (let [[t1 t2] transforms]
        (+ (* a (t1 x)) (* b (t2 x)) c))
      ;; Simple model: y = a*f(x) + b
      (let [transform (:transform model-def)]
        (+ (* a (transform x)) b)))))

(defn- estimate-limit-time-s
  "Estimate limit-time-s for next run based on collected data.
  Uses projected time for time-limited runs, total benchmark time otherwise.
  Returns initial-limit-time-s if insufficient data for estimation."
  [impl-runs time-axis next-coord initial-limit-time-s]
  (if (< (count impl-runs) 2)
    initial-limit-time-s
    (let [;; Build extract-like data for fit-complexity
          ;; Use projected time for limited runs, total benchmark time otherwise
          extract-data (mapv (fn [{:keys [coord data]}]
                               (let [time-ns (if-let [projected (get-in data [:samples :time-limit :projected-time-ns])]
                                               projected
                                               (get-in data [:samples :total-benchmark-time-ns]))]
                                 [coord time-ns]))
                             impl-runs)
          extract {:type :criterium/domain-extract
                   :metric [:samples :total-benchmark-time-ns]
                   :data extract-data}
          regression (fit-complexity extract time-axis)
          best-model (first (filter #(= (:id %) (:best-fit regression))
                                    (:models regression)))
          next-x (get next-coord time-axis)]
      (if (and best-model next-x (> (:r-squared best-model) 0.5))
        (let [predicted-ns (predict-time-ns best-model next-x)
              ;; Convert ns to seconds with margin
              predicted-s (* 2.5 (/ predicted-ns 1e9))]
          (max predicted-s initial-limit-time-s))
        initial-limit-time-s))))

;;; Reporter Protocol

(defprotocol DomainBuilderReporter
  "Protocol for reporting domain-builder progress."
  (report-start [reporter impl-key total-runs]
    "Called when starting to benchmark an implementation.")
  (report-run [reporter impl-key coord run-index]
    "Called after completing a benchmark run.")
  (report-end [reporter impl-key]
    "Called when finished benchmarking an implementation."))

(defrecord DotReporter []
  DomainBuilderReporter
  (report-start [_ impl-key total-runs]
                (print (str (name impl-key) " (" total-runs " runs): "))
                (flush))
  (report-run [_ _impl-key _coord _run-index]
              (print ".")
              (flush))
  (report-end [_ _impl-key]
              (println)))

(defn dot-reporter
  "Create a dot reporter that prints progress dots."
  []
  (->DotReporter))

(defn domain-builder
  "Build a domain by running benchmarks across a parameter space.

  axes is a map of axis names to ordinate sequences:
    {:n (log-range 10 10000 5)}

  implementations is a map of impl-key to impl-spec:
    {:sort {:measured (measured/expr (sort coll))
            :args-builder (fn [{:keys [n]}] (fn [] [(vec (range n))]))}}

  Each impl-spec contains:
    :measured     - A Measured created with example args (establishes type hints)
    :args-builder - (fn [axis-map] (fn [] [args...])) returns zero-arg fn
                    producing argument vector for the given coordinates

  Options:
    :initial-limit-time-s - Time limit for runs before estimation kicks in
                            (default: 10 seconds)
    :time-axis           - Axis key for complexity modeling (default: first axis)
    :reporter            - Progress reporter (default: dot-reporter, nil for silent)
    :bench-options       - Additional options passed to bench-measured

  Benchmarks run in order: all coordinates for first implementation,
  then all for second, etc. Within each implementation, coordinates are
  sorted by :time-axis ascending (smallest values first) to enable
  adaptive time estimation.

  When a benchmark hits its time limit and the projected time differs from
  the limit by more than 5%, the benchmark is re-run with the projected time.

  Returns a domain with runs indexed by {:impl impl-key ...axis-coords...}.
  When multiple implementations are provided, the domain's :implementations
  key is set to :impl, enabling automatic grouping in analysis functions."
  [axes implementations & {:keys [initial-limit-time-s
                                  time-axis
                                  reporter
                                  bench-options]
                           :or {initial-limit-time-s default-initial-limit-time-s}}]
  (let [time-axis (or time-axis (first (keys axes)))
        reporter (if (contains? #{nil false} reporter)
                   nil
                   (or reporter (dot-reporter)))
        coords (cartesian-product axes)
        ;; Sort by time-axis ascending
        sorted-coords (sort-by #(get % time-axis) coords)
        total-runs (count sorted-coords)
        ;; Create initial domain with :implementations when multiple impls
        initial-domain (if (> (count implementations) 1)
                         (domain {:implementations :impl})
                         (domain))
        ;; Helper to run a single benchmark
        run-bench (fn [coord-measured limit-time-s]
                    (let [bench-plan (bench/options->bench-plan
                                      (merge bench-options
                                             {:limit-time-s limit-time-s
                                              :viewer :none}))]
                      (bench/bench-measured bench-plan coord-measured)
                      (:data (bench/last-bench))))
        ;; Helper to check if re-run is needed (>5% difference)
        needs-rerun? (fn [bench-result limit-time-s]
                       (when-let [time-limit (get-in bench-result [:samples :time-limit])]
                         (let [projected-s (/ (:projected-time-ns time-limit) 1e9)
                               diff-pct (Math/abs (/ (- projected-s limit-time-s)
                                                     limit-time-s))]
                           (> diff-pct 0.05))))]

    (reduce
     (fn [domain [impl-key impl-spec]]
       (let [{:keys [measured args-builder]} impl-spec]
         (when reporter
           (report-start reporter impl-key total-runs))

         (let [result
               (reduce
                (fn [{:keys [domain impl-runs]} [idx coord]]
                  (let [;; Estimate time limit based on previous runs
                        limit-time-s (estimate-limit-time-s
                                      impl-runs time-axis coord
                                      initial-limit-time-s)
                        ;; Create args-fn for this coordinate
                        args-fn (args-builder coord)
                        ;; Create measured with new args-fn
                        coord-measured (measured/with-args-fn measured args-fn)
                        ;; Run benchmark
                        bench-result (run-bench coord-measured limit-time-s)
                        ;; Re-run if time-limited and projected differs by >5%
                        bench-result (if (needs-rerun? bench-result limit-time-s)
                                       (let [new-limit-s (* 1.1
                                                            (/ (get-in bench-result
                                                                       [:samples :time-limit :projected-time-ns])
                                                               1e9))]
                                         (run-bench coord-measured new-limit-s))
                                       bench-result)
                        ;; Build full coordinate with impl
                        full-coord (assoc coord :impl impl-key)
                        ;; Accumulate into domain
                        new-domain (add-run domain full-coord bench-result)
                        ;; Track impl-specific runs for time estimation
                        new-impl-runs (conj impl-runs {:coord coord
                                                       :data bench-result})]

                    (when reporter
                      (report-run reporter impl-key coord idx))

                    {:domain new-domain
                     :impl-runs new-impl-runs}))
                {:domain domain :impl-runs []}
                (map-indexed vector sorted-coords))]

           (when reporter
             (report-end reporter impl-key))

           (:domain result))))
     initial-domain
     implementations)))
