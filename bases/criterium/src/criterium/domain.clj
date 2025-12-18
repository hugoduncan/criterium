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
          {:coord {:n 100 :impl :foo} :data <bench-result>}]}

  See also:
  - criterium.domain.analysis for extract, compare-by, group-by-axis, fit-complexity
  - criterium.domain.builder for domain-builder and input generators"
  (:require
   [criterium.measured :as measured]))

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
  "Returns true if x is a domain comparison result.
  Supports both single-metric format (with :metric and :data keys)
  and multi-metric format (with :metrics key)."
  [x]
  (and (map? x)
       (= :criterium/domain-comparison (:type x))
       (contains? x :axis)
       (or (and (contains? x :metric) (contains? x :data)) ; single-metric
           (contains? x :metrics))))

(defn domain-regression?
  "Returns true if x is a domain regression result.
  Domain regressions contain a :regressions map with metric-id keys."
  [x]
  (and (map? x)
       (= :criterium/domain-regression (:type x))
       (contains? x :axis)
       (map? (:regressions x))))

;;; Construction

(defn domain
  "Create a domain from runs. Returns empty domain when called with no args.

  Each run is a map with :coord (keyword or map) and :data (bench result).

  Options (as final map argument):
  - :impl-axis - Coordinate axis for implementations
  - :implementations - Vector of impl keys to include"
  [& args]
  (let [[runs opts] (if (and (seq args)
                             (map? (last args))
                             (or (contains? (last args) :impl-axis)
                                 (contains? (last args) :implementations)))
                      [(butlast args) (last args)]
                      [args nil])]
    (cond-> {:type :criterium/domain
             :runs (vec runs)}
      (:implementations opts) (assoc :implementations (:implementations opts))
      (:impl-axis opts) (assoc :impl-axis (:impl-axis opts)))))

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
  "Return runs from a domain, optionally filtered by partial coordinate.
  For map coords, partial matching is supported (subset match)."
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
  Returns a set of keys. Keyword coordinates contribute no keys."
  [domain]
  (into #{}
        (comp (map :coord)
              (filter map?)
              (mapcat keys))
        (:runs domain)))

(defn implementations
  "Returns the implementation keys for a domain.
  Defaults to [:default] for domains without explicit implementations."
  [domain]
  (or (:implementations domain) [:default]))

(defn impl-axis
  "Returns the impl-axis key for a domain, or nil if not set.
  The impl-axis specifies which coordinate axis represents different
  implementations for comparison analysis."
  [domain]
  (:impl-axis domain))

(defn select
  "Filter domain to runs matching a partial coordinate. Returns a new domain.
  For map coords, partial matching is supported (subset match)."
  [domain partial-coord]
  (assoc domain :runs (filterv #(coord-matches? (:coord %) partial-coord)
                               (:runs domain))))

;;; Domain Expression Macro

(defn- parse-bindings
  "Parse binding vector [sym1 range1 sym2 range2 ...] into axes map."
  [bindings]
  (into {}
        (map (fn [[sym range-expr]]
               [(keyword sym) range-expr]))
        (partition 2 bindings)))

(defn- gen-arg-sym
  "Generate a placeholder symbol for the nth argument."
  [n]
  (symbol (str "arg" n)))

(defn- transform-impl-expr
  "Transform an implementation expression into {:measured ... :args-builder ...}.
  
  For a function call (f arg1 arg2 ...), generates:
  - measured: (measured/expr (f placeholder1 placeholder2 ...))
  - args-builder: (fn [{:keys [axis-syms]}] (fn [] [arg1 arg2 ...]))"
  [expr axis-syms]
  (if (and (seq? expr) (symbol? (first expr)))
    (let [f (first expr)
          args (rest expr)
          n-args (count args)
          placeholders (mapv gen-arg-sym (range n-args))
          measured-expr (cons f placeholders)
          axis-keys (mapv keyword axis-syms)]
      {:measured `(measured/expr ~measured-expr)
       :args-builder `(fn [{:keys ~(vec axis-syms)}]
                        (fn [] ~(vec args)))})
    ;; Non-function-call expression: wrap as nullary
    {:measured `(measured/expr ~expr)
     :args-builder `(constantly (fn [] []))}))

(defmacro domain-expr
  "Create a domain specification map from a concise expression syntax.
  
  Returns a map with :axes and :implementations suitable for use with
  domain-builder: (apply domain-builder (mapcat identity (domain-expr ...)))
  
  Syntax:
    ;; Single expression - uses :default impl key
    (domain-expr [n (log-range 10 1000 5)] (sort (random-seq n)))
    
    ;; Multiple implementations via map
    (domain-expr [n (log-range 10 1000 5)]
      {:sort (sort (random-seq n))
       :sort-by (sort-by identity (random-seq n))})
    
    ;; Multiple axes
    (domain-expr [n (log-range 10 1000 5)
                  m (linear-range 1 10 3)]
      (matrix-mult (random-matrix n m)))
  
  The binding vector defines axes as [sym range-expr ...] pairs.
  Axis symbols are available in implementation expressions.
  
  For each implementation expression (f arg1 arg2 ...):
  - Arguments become the args-builder body (evaluated per coordinate)
  - The function call structure becomes the measured expression"
  [bindings body]
  (let [axis-pairs (partition 2 bindings)
        axis-syms (mapv first axis-pairs)
        axes-map (into {}
                       (map (fn [[sym range-expr]]
                              [(keyword sym) range-expr]))
                       axis-pairs)
        impls (if (map? body) body {:default body})
        impl-entries (map (fn [[impl-key expr]]
                            [impl-key (transform-impl-expr expr axis-syms)])
                          impls)]
    `{:axes ~axes-map
      :implementations ~(into {} impl-entries)}))
