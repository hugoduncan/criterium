(ns criterium.metric
  "Functions for working with metric configurations and definitions.

  A metric represents a measurable value that can be collected during
  benchmarking.  Each metric is described by a configuration map with
  the following structure:

  {:type    keyword    ; The type of metric (e.g., :timing, :memory)
   :name    string     ; Human readable name of the metric
   :values  [...]      ; Collection of metric value configurations}

  Metrics can be organized in groups using a metrics configuration map:
  {:group-name {:values [...]}                    ; Direct metric values
   :other-group {:groups {:subgroup {:values [...]}}} ; Nested metric groups}

  This namespace provides functions for querying and filtering metric
  configurations.  It supports both flat and hierarchical metric
  organization structures."
  (:require
   [clojure.set :as set]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :as invariant :refer [have?]]))

(def ^:private metric-keys
  #{:path :dimension :scale :label})

(defn metric-config?
  [x]
  (and (map? x)
       (or (set/subset? metric-keys (set (keys x)))
           (throw
            (invariant/assertion-error
             "Invalid keys"
             {:error-tupe ::invalid-map-keys
              :date       {:expected metric-keys
                           :actual   (keys x)
                           :missing  (set/difference
                                      metric-keys
                                      (set (keys x)))}})))))

(defn metric-configs
  "Returns a sequence of metric-config maps from a metrics configuration map.

  Takes a map of metric groups where each group contains a :values key
  with a sequence of metric configurations. Flattens all metric configs
  into a single sequence.

  Example input:
  {:timing {:values [{:type :timing, :name \"execution-time\"}]}
   :memory {:values [{:type :memory, :name \"heap-usage\"}]}}

  Returns:
   [{:type :timing, :name \"execution-time\"}
    {:type :memory, :name \"heap-usage\"}]"
  [metric-defs]
  {:post [(have? #(every? metric-config? %) %)]}
  (mapcat :values (vals metric-defs)))

(defn all-metric-configs
  "Return all metric configurations from both flat and nested structures.

  Similar to metric-configs but also handles nested metric
  groups. Processes both direct :values entries and nested :groups
  configurations recursively.

  Example input:
  {:timing {:values [{:type :timing, :name \"basic-timing\"}]}
  :memory  {:groups
  {:heap    {:values [{:type :memory, :name \"heap-used\"}]}
  :non-heap {:values [{:type :memory, :name \"metaspace\"}]}}}}

  Return flat sequence of all metric configurations regardless of
  nesting."
  [metric-defs]
  {:post [(have? #(every? metric-config? %) %)]}
  (reduce-kv
   (fn [res _k metric-group]
     (reduce
      conj
      res
      (or (:values metric-group)
          (mapcat :values (vals (:groups metric-group))))))
   []
   metric-defs))

(defn select-metrics
  [metrics-defs metric-ids]
  {:pre [(have? map? metrics-defs)]}
  (if metric-ids
    (select-keys metrics-defs metric-ids)
    metrics-defs))

(defn metrics-of-type
  "Returns a map of metric configurations filtered by type and optional IDs.

  Given a metrics configuration map, returns configurations matching the
  specified metric-type. If metric-ids is provided, only returns metrics
  with matching IDs.

  Parameters:
  metrics-config - Map of metric configurations

  metric-type    - Keyword identifying the type of metrics to
                   select (e.g., :timing)

  metric-ids     - Optional sequence of metric IDs to filter by. If nil,
                   returns all metrics of the specified type.

  Example:
  (metrics-of-type config :timing [:exec-time :wait-time])

  Return only :timing metrics with the specified IDs."
  [metrics-config metric-type metric-ids]
  (->>
   (select-metrics metrics-config metric-ids)
   (util/filter-map #(= metric-type (:type %)))))

(defn metric-configs-of-type
  "Return a sequence of metric configurations filtered by type and optional IDs.

  Similar to metrics-of-type but returns a flat sequence of
  configurations instead of a map. Useful when you need to process all
  matching configurations sequentially.

  Parameters:
    metrics-defs - Map of metric configurations
    metric-type     - Keyword identifying the type of metrics to select
    metric-ids      - Optional sequence of metric IDs to filter by

  Example:
    (metric-configs-of-type config :memory nil)
    ; Returns sequence of all memory metric configurations"
  [metrics-defs metric-type metric-ids]
  (->> (metrics-of-type metrics-defs metric-type metric-ids)
       metric-configs
       (filterv #(= metric-type (:type %)))))

;;; Sample Metric Accessors

(defn elapsed-time
  "Return the elapsed time in nanoseconds from a sample map.

  Parameters:
    sample - A map containing benchmark sample data with :elapsed-time key

  Return:
    long - The elapsed time in nanoseconds

  Note: This function assumes the sample contains an :elapsed-time value
  measured in nanoseconds."
  ^long [sample]
  (:elapsed-time sample))




;;;;



(defn filter-metric-values
  "Filter a sequence of metric value maps using predicate"
  [pred values]
  (filterv pred values))

(declare filter-metrics)

(defn filter-metrics*
  "Filter metrics tree keeping values matching predicate.
  Preserves structure while only keeping values that match the predicate.
  When filtering groups, removes empty groups after filtering."
  [pred metrics]
  (cond-> metrics
    (:values metrics)
    (update :values #(filterv pred %))

    (:groups metrics)
    (update :groups (fn [g] (filter-metrics g pred)))))

(defn filter-metrics
  [metrics pred]
  (->>
   (update-vals metrics (partial filter-metrics* pred))
   (util/filter-map #(or (seq (:values %)) (seq (:groups %))))))

(defn dimension-pred
  "Create predicate that matches metric values with given dimension"
  [dim]
  #(= dim (:dimension %)))

(defn type-pred
  "Create predicate that matches metric values with given type"
  [typ]
  #(= typ (:type %)))
