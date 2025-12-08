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
   [criterium.util.helpers :as util]))

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
        runs    (:runs domain)
        idx     (reduce-kv (fn [_ i run]
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
  Returns a vector of [coord value] pairs, preserving run order.

  metric-path is a vector of keys specifying the path to the metric value,
  e.g., [:stats :elapsed-time :mean].

  For stats paths (where the first element is a stats key like :stats or
  :log-stats), transforms are applied to convert raw values to their
  display form.

  Returns nil for value if the metric is missing from a run.

  Example:
  (extract domain [:stats :elapsed-time :mean])
  ;; => [[{:n 100} 1.23e-6] [{:n 1000} 1.45e-5] ...]"
  [domain metric-path]
  (let [[stats-id metric-id value-key] metric-path]
    (mapv (fn [{:keys [coord data]}]
            [coord (util/stats-value data stats-id metric-id value-key)])
          (:runs domain))))

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
