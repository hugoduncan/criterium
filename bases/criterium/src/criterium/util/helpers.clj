(ns criterium.util.helpers
  (:refer-clojure :exclude [update-vals])
  (:require
   [criterium.util.invariant :as invariant :refer [have have?]]))

(defn assoc-tag [sym t]
  (vary-meta sym assoc :tag t))

(defn spy
  [msg x]
  (prn msg x)
  x)

(defn- safe-keys
  [m]
  {:pre [(or (map? m) (nil? m))]}
  (dissoc m :state :expr-value))

(defn- merge-fn [op]
  (fn merge-fn-inner [a b]
    (if (or (map? a) (map? b))
      (merge-with
       merge-fn-inner
       (safe-keys a)
       (safe-keys b))
      (op a b))))

(defn sum
  ([] {})
  ([a b]
   (merge-with
    (merge-fn +)
    (safe-keys a)
    (safe-keys b))))

(defmacro sqr
  "Square of argument"
  [x] `(let [x# ~x] (* x# x#)))

(defn sqrd
  "Square of argument"
  ^double [^double x] (* x x))

(defn cubed
  "Cube of argument"
  ^double [^double x]
  (* x x x))

(defn trunc
  "Round towards zero to an integeral value."
  [^double x]
  (if (pos? x)
    (Math/floor x)
    (Math/ceil x)))

;; from clojure 1.11-aplha-2
(def update-vals-impl
  '(with-meta
     (persistent!
      (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                 (if (instance? clojure.lang.IEditableCollection m)
                   (transient m)
                   (transient {}))
                 m))
     (meta m)))

(defmacro provide-update-vals
  []
  (if (resolve 'clojure.core/update-vals)
    '(clojure.core/update-vals m f)
    update-vals-impl))

#_{:clj-kondo/ignore [:redefined-var :unused-binding]}
(defn update-vals
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where
  the keys of m are mapped to result of applying f to the corresponding
  values of m."
  [m f]
  (provide-update-vals))

(defn filter-map
  "Internal helper to filter map entries based on a predicate applied to values.

  Return a new map containing only the entries where (pred value)
  returns true.  Used internally for filtering metrics by their
  configuration values."
  [pred m]
  (into {} (filter (comp pred val)) m))

(defn reduce-double-vector
  "Reduce a double primitive value over a vector."
  ^double [^clojure.lang.IFn$DOD f
           ^double init
           ^clojure.lang.APersistentVector v]
  (let [n (.count v)]
    (loop [acc init
           i 0]
      (if (< i n)
        (recur (.invokePrim f acc (.nth v i)) (unchecked-inc i))
        acc))))

;; Modified version of clojure.walk to preserve metadata
(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (cond
    (list? form)
    (outer (with-meta
             (apply list (map inner form))
             (meta form)))

    (instance? clojure.lang.IMapEntry form)
    (outer
     (clojure.lang.MapEntry/create
      (inner (key form)) (inner (val form))))

    (seq? form)
    (outer (with-meta
             (doall (map inner form))
             (meta form)))

    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))

    (coll? form)
    (outer (with-meta
             (into (empty form) (map inner form))
             (meta form)))
    :else (outer form)))

(defn postwalk
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  {:added "1.1"}
  [f form]
  (walk (partial postwalk f) f form))

(defn deep-merge
  "Merge maps recursively."
  [& ms]
  (letfn [(merge* [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with merge* xs)
              (last xs)))]
    (reduce merge* ms)))

(defn report
  "Print format output"
  [format-string & values]
  (print (apply format format-string values)))

;;; Accessors

(defn data-entry-map
  [data type transform source-id]
  {:data data
   :type type
   :transform transform
   :source-id source-id})

(defn data
  [data-entry-map]
  {:pre [(have? :data data-entry-map)]
   :post [(have? map? %)]}
  (:data data-entry-map))

(defn metric->values
  [metrics-samples]
  {:post [(have? map? %)]}
  (:metric->values metrics-samples))

(defn metric->digest
  [digest-samples]
  (:metric->digest digest-samples))

(defn quantiles
  [quantiles-map]
  {:post [(have? map? %)]}
  (:quantiles quantiles-map))

(defn outliers
  [outliers-map]
  {:post [(have? map? %)]}
  (:outliers outliers-map))

(defn outlier-significance
  [outlier-significance-map]
  {:post [(have? map? %)]}
  (:outlier-significance outlier-significance-map))

(defn stats
  [stats-map]
  {:post [(have? map? %)]}
  (:stats stats-map))

(defn event-stats
  [event-stats-map]
  {:post [(have? map? %)]}
  (:event-stats event-stats-map))

(defn bootstrap
  [bootstrap-stats-map]
  {:post [(have? map? %)]}
  (:bootstrap bootstrap-stats-map))

;;; Value transforms

;; These allow sample values to be transformed to mesurements, and vice versa.
;; This enables, using a log-normal transform of the samples.

(defn add-transform-paths
  [v sample-> ->sample]
  (-> v
      (update :sample-> (fnil conj '()) sample->)
      (update :->sample (fnil conj '[]) ->sample)))

(defn get-transforms
  [result-map path]
  (loop [transforms (update-vals
                     (:transform
                      (have some? (result-map path) {:path path}))
                     vector)
         path (:source-id (result-map path))]
    (if path
      (let [t (:transform (result-map path))]
        (recur
         (add-transform-paths transforms (:sample-> t) (:->sample t))
         (:source-id (result-map path))))
      transforms)))

(defn transform-sample->
  ^double [value transforms]
  (reduce (fn [v f] (f v)) value (:sample-> transforms)))

(defn transform-vals->
  [m transforms]
  (let [tform #(reduce (fn [v f] (f v)) % (:sample-> transforms))]
    (update-vals m tform)))

(defn transform->sample [value transforms]
  (reduce (fn [v f] (f v)) value (reverse (:->sample transforms))))

(defn stats-value
  "Extract a transformed stats value from a benchmark data map.
  
  Takes a data map (from bench/last-bench :data), a stats-id
  (e.g. :stats or :log-stats), a metric-id (e.g. :elapsed-time),
  and a value-key (e.g. :mean, :std-dev).
  
  Returns the value with all transforms applied."
  [data-map stats-id metric-id value-key]
  {:pre [(have? keyword? stats-id)
         (have? keyword? value-key)]}
  (let [transforms (get-transforms data-map stats-id)
        raw-value (get-in data-map [stats-id :stats metric-id value-key])]
    (when raw-value
      (transform-sample-> raw-value transforms))))

;;; Thread
(defn valid-thread-priority
  [p]
  (when p
    (cond
      (= :max-priority p) Thread/MAX_PRIORITY
      (= :min-priority p) Thread/MIN_PRIORITY
      (and (integer? p)
           (<= Thread/MIN_PRIORITY
               p
               Thread/MAX_PRIORITY)) p
      :else
      (throw (ex-info "Invalid thread priority"
                      {:priority p
                       :min-priority Thread/MIN_PRIORITY
                       :max-priority Thread/MAX_PRIORITY})))))

(defmacro with-thread-priority
  [p & body]
  `(let [priority# (valid-thread-priority ~p)
         orig-priority# (.getPriority (Thread/currentThread))]
     (when priority#
       (.setPriority (Thread/currentThread) priority#))
     (try
       ~@body
       (finally
         (when priority#
           (.setPriority (Thread/currentThread) orig-priority#))))))

;;; Resolve

(defn maybe-ver-get-named
  "Resolve and deref a Named, or return the argument."
  [k {:keys [default-ns]}]
  {:pre [(or (keyword? k) (symbol? k))]}
  (if-let [n (namespace k)]
    (or (some-> (ns-resolve (symbol n) (symbol (name k))) deref) k)
    (or (some-> (ns-resolve default-ns (symbol (name k))) deref) k)))

(defn maybe-var-get
  "Resolve and deref a Named, or return the argument."
  [x options]
  (if (or (keyword? x) (symbol? x))
    (maybe-ver-get-named x options)
    x))

(defn lookup-data
  [data-map id]
  (or (data-map id)
      (throw (ex-info "Failed to find data set"
                      {:available-ids (keys data-map)
                       :id id}))))

;;; Typed entry accessors
;; These are simple lookup functions that can be instrumented via malli
;; for return type validation during development.

(defn get-generic-data-entry
  "Look up a generic data entry by id from a result map.
  Returns the entry or nil if not found. Type validation is
  handled by malli instrumentation during development."
  [result-map id]
  (result-map id))

(defn get-quantiles-entry
  "Look up a quantiles entry by id from a result map.
  Returns the entry or nil if not found. Type validation is
  handled by malli instrumentation during development."
  [result-map id]
  (result-map id))

;;; Type constructors
;; Identity functions that can be instrumented via malli to validate
;; constructed data types during development.

(defn ->collected-metrics-map
  "Identity wrapper for collected-metrics-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->quantiles-map
  "Identity wrapper for quantiles-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->outliers-map
  "Identity wrapper for outliers-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->stats-map
  "Identity wrapper for stats-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->event-stats-map
  "Identity wrapper for event-stats-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->histogram-map
  "Identity wrapper for histogram-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->kde-map
  "Identity wrapper for kde-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->modes-map
  "Identity wrapper for modes-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->outlier-significance-map
  "Identity wrapper for outlier-significance-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)
