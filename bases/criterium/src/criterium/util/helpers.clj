(ns criterium.util.helpers
  "Criterium domain helpers and backward-compatible re-exports from utils."
  (:refer-clojure :exclude [update-vals])
  (:require
   [utils.interface :as utils :refer [have have?]]))

;;; Re-exports from utils component for backward compatibility

(def assoc-tag utils/assoc-tag)
(def spy utils/spy)

(defmacro sqr
  "Square of argument"
  [x] `(utils/sqr ~x))

(def sqrd utils/sqrd)
(def cubed utils/cubed)
(def trunc utils/trunc)
(def update-vals-impl utils/update-vals-impl)

(defmacro provide-update-vals
  []
  `(utils/provide-update-vals))

(def update-vals utils/update-vals)
(def filter-map utils/filter-map)
(def reduce-double-vector utils/reduce-double-vector)
(def walk utils/walk)
(def postwalk utils/postwalk)
(def deep-merge utils/deep-merge)
(def report utils/report)

;;; Domain-specific helpers

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
