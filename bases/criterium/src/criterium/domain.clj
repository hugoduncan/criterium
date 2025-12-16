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
  - criterium.domain.builder for domain-builder and input generators")

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
  "Create a domain from runs.

  With no arguments, creates an empty domain.
  With arguments, creates a domain containing those runs.

  Each run must be a map with :coord and :data keys where:
  - :coord is either a keyword label or a map of dimension keys to values
  - :data is the full benchmark result from bench

  Options (as final map argument):
  - :impl-axis - Keyword specifying which coordinate axis represents
                 different implementations (e.g., :impl). When set,
                 analysis functions like fit-complexity will group
                 results by implementation.
  - :implementations - Vector of implementation keys. Defaults to [:default].

  Examples:
  (domain)                                    ; empty domain
  (domain {:coord :baseline :data result1})  ; single run with keyword coord
  (domain {:coord {:n 100} :data result1}    ; runs with map coords
          {:coord {:n 1000} :data result2})
  (domain {:coord {:n 100 :impl :vec} :data result1}
          {:coord {:n 100 :impl :list} :data result2}
          {:impl-axis :impl
           :implementations [:vec :list]})   ; with implementations"
  [& args]
  (let [[runs opts] (if (and (seq args)
                             (map? (last args))
                             (or (contains? (last args) :impl-axis)
                                 (contains? (last args) :implementations)))
                      [(butlast args) (last args)]
                      [args nil])]
    (cond-> {:type :criterium/domain
             :runs (vec runs)
             :implementations (or (:implementations opts) [:default])}
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
  "Returns the implementation keys for a domain."
  [domain]
  (:implementations domain))

(defn impl-axis
  "Returns the impl-axis key for a domain, or nil if not set.
  The impl-axis specifies which coordinate axis represents different
  implementations for comparison analysis."
  [domain]
  (:impl-axis domain))

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
