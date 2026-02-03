(ns criterium.util.helpers
  "Criterium domain helpers and backward-compatible re-exports from utils."
  (:refer-clojure :exclude [update-vals])
  (:require
   [criterium.array :as arr]
   [criterium.utils.interface :as utils :refer [have have?]]))

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

(defn autocorrelation
  [autocorrelation-map]
  {:post [(have? map? %)]}
  (:autocorrelation autocorrelation-map))

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

  Returns the value with all transforms applied, or nil if the
  stats-id entry is not present in the data map."
  [data-map stats-id metric-id value-key]
  {:pre [(have? keyword? stats-id)
         (have? keyword? value-key)]}
  (when (contains? data-map stats-id)
    (let [transforms (get-transforms data-map stats-id)
          raw-value (get-in data-map [stats-id :stats metric-id value-key])]
      (when raw-value
        (transform-sample-> raw-value transforms)))))

(defn bootstrap-quantile-value
  "Extract a transformed bootstrap quantile value from a benchmark data map.

  Takes a data map, metric-id (e.g. :elapsed-time), and quantile (e.g. 0.5).
  Returns the point estimate for that quantile with transforms applied,
  or nil if bootstrap stats are not available.

  Bootstrap stats structure:
     [:bootstrap-stats :bootstrap metric-id :quantiles q]
  where each quantile has :point-estimate and :estimate-quantiles."
  [data-map metric-id quantile]
  (when (contains? data-map :bootstrap-stats)
    (let [transforms (get-transforms data-map :bootstrap-stats)
          raw-value (get-in data-map [:bootstrap-stats :bootstrap metric-id
                                      :quantiles quantile :point-estimate])]
      (when raw-value
        (transform-sample-> raw-value transforms)))))

(defn bootstrap-quantile-ci
  "Extract confidence interval bounds for a bootstrap quantile.

  Returns {:ci-lower val :ci-upper val} map with transforms applied,
  or nil if CI bounds are not available or bootstrap stats are missing."
  [data-map metric-id quantile]
  (when (contains? data-map :bootstrap-stats)
    (let [transforms (get-transforms data-map :bootstrap-stats)
          estimate-quantiles (get-in
                              data-map
                              [:bootstrap-stats :bootstrap metric-id
                               :quantiles quantile
                               :estimate-quantiles])]
      (when (and estimate-quantiles (>= (count estimate-quantiles) 2))
        (let [lower-raw (-> estimate-quantiles first :value)
              upper-raw (-> estimate-quantiles second :value)]
          (when (and lower-raw upper-raw)
            {:ci-lower (transform-sample-> lower-raw transforms)
             :ci-upper (transform-sample-> upper-raw transforms)}))))))

(defn bootstrap-box-plot-stats
  "Extract all stats needed for box plot from bootstrap data.

  Returns a map with :median, :p10, :p90, and optionally :ci-lower, :ci-upper.
  Returns nil if required quantiles (0.1, 0.5, 0.9) are not available."
  [data-map metric-id]
  (let [p10 (bootstrap-quantile-value data-map metric-id 0.1)
        p50 (bootstrap-quantile-value data-map metric-id 0.5)
        p90 (bootstrap-quantile-value data-map metric-id 0.9)]
    (when (and p10 p50 p90)
      (merge {:median p50 :p10 p10 :p90 p90}
             (bootstrap-quantile-ci data-map metric-id 0.5)))))

(defn samples-single-value
  "Extract a metric value from raw samples when only a single sample exists.
  For use with :one-shot benchmarks that produce a single data point without
  statistical analysis.

  Returns the transformed metric value, or nil if samples unavailable,
  multi-sample, or the metric doesn't exist in the samples."
  [data-map metric-id]
  (let [samples (:samples data-map)]
    (when (and samples (= 1 (:num-samples samples)))
      (let [transforms (get-transforms data-map :samples)
            metric-values (get (:metric->values samples) [metric-id])]
        (when (and metric-values (pos? (arr/length metric-values)))
          (transform-sample-> (arr/get-double metric-values 0) transforms))))))

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

(defn ->distribution-fit-map
  "Identity wrapper for distribution-fit-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->tail-analysis-map
  "Identity wrapper for tail-analysis-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->autocorrelation-map
  "Identity wrapper for autocorrelation-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->effective-sample-size-map
  "Identity wrapper for effective-sample-size-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

(defn ->autocorrelation-classification-map
  "Identity wrapper for autocorrelation-classification-map construction.
  Validation handled by malli instrumentation during development."
  [x] x)

;;; Accessors for new analysis types

(defn effective-sample-size-data
  "Extract effective sample size data from an effective-sample-size-map."
  [ess-map]
  {:post [(have? map? %)]}
  (:effective-sample-size-data ess-map))

(defn autocorrelation-classification-data
  "Extract classification data from an autocorrelation-classification-map."
  [class-map]
  {:post [(have? map? %)]}
  (:classification-data class-map))
