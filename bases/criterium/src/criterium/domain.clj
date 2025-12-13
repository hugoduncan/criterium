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
   [criterium.bench.config :as bench-config]
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
  "Returns true if x is a domain extract result."
  [x]
  (and (map? x)
       (= :criterium/domain-extract (:type x))
       (contains? x :metric)
       (contains? x :data)))

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

  Examples:
  (domain)                                    ; empty domain
  (domain {:coord :baseline :data result1})  ; single run with keyword coord
  (domain {:coord {:n 100} :data result1}    ; runs with map coords
          {:coord {:n 1000} :data result2})"
  [& runs]
  {:type :criterium/domain
   :runs (vec runs)})

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

;;; Analysis

(defn extract
  "Extract metric values at a path from all runs in a domain.
  Returns a domain-extract result containing coordinate-value pairs.

  metric-path is a vector of keys specifying the path to the metric value,
  e.g., [:stats :elapsed-time :mean].

  For stats paths (where the first element is a stats key like :stats or
  :log-stats), transforms are applied to convert raw values to their
  display form.

  Returns nil for value if the metric is missing from a run.

  Example:
  (extract domain [:stats :elapsed-time :mean])
  ;; => {:type :criterium/domain-extract
  ;;     :metric [:stats :elapsed-time :mean]
  ;;     :data [[{:n 100} 1.23e-6] [{:n 1000} 1.45e-5] ...]}"
  [domain metric-path]
  (let [[stats-id metric-id value-key] metric-path]
    {:type :criterium/domain-extract
     :metric metric-path
     :data (mapv (fn [{:keys [coord data]}]
                   [coord (util/stats-value data stats-id metric-id value-key)])
                 (:runs domain))}))

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

;;; Analysis Pipeline
;;
;; These functions return transformers for use in composable analysis pipelines.
;; Each takes an options map and returns a function that transforms a data-map,
;; following the same pattern as criterium.analyse functions.

(defn domain-extract-fn
  "Returns a function that extracts metric values from a domain in a data-map.

  Parameters:
    opts - Map with keys:
      :id         - Key for result in output (default: :extract)
      :domain-id  - Key for source domain in input (default: :domain)
      :metric-path - Vector path to metric, e.g. [:stats :elapsed-time :mean]

  The returned function:
  - Takes a data-map containing a domain under :domain-id
  - Returns the data-map with a domain-extract result added under :id

  Example:
  (-> {:domain my-domain}
      ((domain-extract-fn {:id :mean
                           :metric-path [:stats :elapsed-time :mean]})))
  ;; => {:domain my-domain
  ;;     :mean {:type :criterium/domain-extract ...}}"
  ([] (domain-extract-fn {}))
  ([{:keys [id domain-id metric-path]}]
   (fn [data-map]
     (let [domain-id (or domain-id :domain)
           id (or id :extract)
           domain (data-map domain-id)
           result (extract domain metric-path)]
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
  Each model maps input size n to a transformed value for linear regression."
  {:logarithmic {:transform (fn [^double n] (Math/log n))
                 :label "O(log n)"}
   :linear {:transform identity
            :label "O(n)"}
   :n-log-n {:transform (fn [^double n] (* n (Math/log n)))
             :label "O(n log n)"}
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

(defn- fit-complexity-model
  "Fit a single complexity model to data points.
  Returns map with :id, :label, :coefficients, :r-squared, :residuals."
  [model-id {:keys [transform label]} xs ys]
  (let [transformed-xs (mapv (comp double transform) xs)
        {:keys [a b r-squared]} (linear-regression transformed-xs ys)
        residuals (mapv (fn [tx y]
                          (- y (+ (* a tx) b)))
                        transformed-xs ys)]
    {:id model-id
     :label label
     :coefficients {:a a :b b}
     :r-squared r-squared
     :residuals residuals}))

(defn domain-regression?
  "Returns true if x is a domain regression result."
  [x]
  (and (map? x)
       (= :criterium/domain-regression (:type x))
       (contains? x :axis)
       (contains? x :models)
       (contains? x :best-fit)))

(defn fit-complexity
  "Fit complexity models to domain extract data.
  Returns a domain-regression result identifying how metrics scale with input size.

  extract is a domain-extract result.
  axis is the coordinate key to use for x-values (e.g., :n for input size).
  models is a map of model-id to {:transform fn :label string}, or nil for defaults.

  Filters out data points with nil values before fitting.
  Selects best-fit model by highest R² value.

  Example:
  (fit-complexity extract :n)
  ;; => {:type :criterium/domain-regression
  ;;     :axis :n
  ;;     :metric [:stats :elapsed-time :mean]
  ;;     :models [{:id :linear :label \"O(n)\" :r-squared 0.98 ...} ...]
  ;;     :best-fit :linear}"
  ([extract axis] (fit-complexity extract axis nil))
  ([extract axis models]
   (let [models (or models default-complexity-models)
         metric-path (:metric extract)
         ;; Extract x (axis value) and y (metric value) from data
         ;; Filter out nil y values
         valid-data (filter (fn [[coord value]]
                              (and (some? value)
                                   (if (map? coord)
                                     (contains? coord axis)
                                     false)))
                            (:data extract))
         xs (mapv (fn [[coord _]] (double (get coord axis))) valid-data)
         ys (mapv (fn [[_ value]] (double value)) valid-data)
         ;; Fit each model
         fitted (when (>= (count xs) 2)
                  (mapv (fn [[model-id model-def]]
                          (fit-complexity-model model-id model-def xs ys))
                        models))
         ;; Find best fit by R²
         best-fit (when (seq fitted)
                    (:id (apply max-key :r-squared fitted)))]
     {:type :criterium/domain-regression
      :axis axis
      :metric metric-path
      :models (or fitted [])
      :best-fit best-fit})))

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

  Returns a function that takes a viewer keyword and data-map."
  [view-plan]
  (when-not (or (nil? view-plan) (sequential? view-plan))
    (throw
     (ex-info "view must be a sequence of specs" {:view view-plan})))
  (let [fns (mapv resolve-domain-view-fn view-plan)]
    (fn [viewer data-map]
      {:pre [(have? keyword? viewer)]}
      (run! #(% viewer data-map) fns)
      (view/flush-viewer viewer)
      data-map)))

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
    :analyse - Vector of analysis specs resolved from criterium.domain
    :view    - Vector of view specs resolved from criterium.view
    :viewer  - Keyword specifying output format (:print, :portal, :none)
               If not specified, uses the default viewer from
               criterium.bench/set-default-viewer!

  Returns the data-map with all analysis results.

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
        data-map (analyse-fn {:domain domain})]
    (view-fn viewer data-map)
    data-map))
