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
    {:type   :criterium/domain-extract
     :metric metric-path
     :data   (mapv (fn [{:keys [coord data]}]
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
    {:type   :criterium/domain-comparison
     :axis   axis-key
     :metric metric-path
     :data   (into {}
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
          log-end   (Math/log end)
          step      (/ (- log-end log-start) (dec n))]
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
           id        (or id :extract)
           domain    (data-map domain-id)
           result    (extract domain metric-path)]
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
           id        (or id :grouped)
           domain    (data-map domain-id)
           result    (group-by-axis domain axis-key)]
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
           id        (or id :comparison)
           domain    (data-map domain-id)
           result    (compare-by domain axis-key metric-path)]
       (assoc data-map id result)))))
