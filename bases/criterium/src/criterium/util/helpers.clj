(ns criterium.util.helpers
  (:refer-clojure :exclude [update-vals])
  (:require
   [criterium.types :as types]
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
           i   0]
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
  {:data      data
   :type      type
   :transform transform
   :source-id source-id})

(defn data
  [data-entry-map]
  {:pre  [(have? :data data-entry-map)]
   :post [(have? map? %)]}
  (:data data-entry-map))

(defn metric->values
  [metrics-samples]
  {:pre  [(have? types/generic-metrics-samples-map? metrics-samples)]
   :post [(have? map? %)]}
  (:metric->values metrics-samples))

(defn metric->digest
  [digest-samples]
  (:metric->digest digest-samples))

(defn quantiles
  [quantiles-map]
  {:pre  [(have? types/quantiles-map? quantiles-map)]
   :post [(have? map? %)]}
  (:quantiles quantiles-map))

(defn outliers
  [outliers-map]
  {:pre  [(have? types/outliers-map? outliers-map)]
   :post [(have? map? %)]}
  (:outliers outliers-map))

(defn outlier-significance
  [outlier-significance-map]
  {:pre  [(have? types/outlier-significance-map? outlier-significance-map)]
   :post [(have? map? %)]}
  (:outlier-significance outlier-significance-map))

(defn stats
  [stats-map]
  {:pre  [(have? types/stats-map? stats-map)]
   :post [(have? map? %)]}
  (:stats stats-map))

(defn event-stats
  [event-stats-map]
  {:pre  [(have? types/event-stats-map? event-stats-map)]
   :post [(have? map? %)]}
  (:event-stats event-stats-map))

(defn bootstrap
  [bootstrap-stats-map]
  {:pre  [(have? types/bootstrap-map? bootstrap-stats-map)]
   :post [(have? map? %)]}
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
  {:pre [(have? types/result-map? result-map)]}
  (loop [transforms (update-vals
                     (:transform
                      (have some? (result-map path) {:path path}))
                     vector)
         path       (:source-id (result-map path))]
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

;;; Thread
(defn valid-thread-priority
  [p]
  (when p
    (cond
      (= :max-priority p)            Thread/MAX_PRIORITY
      (= :min-priority p)            Thread/MIN_PRIORITY
      (and (integer? p)
           (<= Thread/MIN_PRIORITY
               p
               Thread/MAX_PRIORITY)) p
      :else
      (throw (ex-info "Invalid thread priority"
                      {:priority     p
                       :min-priority Thread/MIN_PRIORITY
                       :max-priority Thread/MAX_PRIORITY})))))

(defmacro with-thread-priority
  [p & body]
  `(let [priority#      (valid-thread-priority ~p)
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
                       :id            id}))))
