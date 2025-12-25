(ns criterium.domain.types
  "Core domain types and operations.

  A domain is an immutable collection of benchmark runs indexed by coordinates.
  This namespace provides the foundational data structures used by
  criterium.domain.builder and criterium.domain.analysis.")

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
