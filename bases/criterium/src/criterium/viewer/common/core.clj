(ns criterium.viewer.common.core
  "Core utility functions for viewer data preparation.

  This namespace provides foundational functions used across multiple viewer
  namespaces for formatting metrics, computing SI scaling, and preparing
  basic statistical data for display."
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]))

;;; Basic metric formatting

(defn metrics-map
  [sample metrics]
  (reduce
   (fn [res metric]
     (let [arr (sample (:path metric))
           v (when arr (arr/first-element arr))]
       (conj res
             {:metric (:label metric)
              :value (if (number? v)
                       (format/format-value
                        (:dimension metric)
                        (* (double v) (double (:scale metric))))
                       v)})))
   []
   metrics))

(defn stats-map
  [stats metric-configs transforms]
  (reduce
   (fn [res metric]
     (let [stat (util/transform-vals->
                 (get-in stats (:path metric))
                 transforms)
           min-val (double (:min-val stat))
           metric-scale (double (:scale metric))
           [scale label] (format/scale
                          (:dimension metric)
                          (* metric-scale min-val))
           scale (* (double scale) metric-scale)]
       (conj res
             (reduce
              (fn add-key-k [res k]
                (assoc res k
                       (format/round (* (double (get stat k)) scale) 4)))
              {:_metric (str (:label metric) " " label)} ; underscore so it sorts first
              [:mean :min-val :mean-minus-3sigma :mean-plus-3sigma :max-val]))))
   []
   (filterv (metric/type-pred :quantitative) metric-configs)))

(defn extremes-map
  "Prepare min/max extremes for display in tabular format.
  Returns a vector of maps with :metric, :min, and :max keys."
  [stats metric-configs transforms]
  (reduce
   (fn [res metric]
     (let [stat (util/transform-vals->
                 (get-in stats (:path metric))
                 transforms)
           min-val (double (:min-val stat))
           max-val (double (:max-val stat))
           metric-scale (double (:scale metric))
           [scale label] (format/scale
                          (:dimension metric)
                          (* metric-scale min-val))
           scale (* (double scale) metric-scale)]
       (conj res
             {:metric (str (:label metric) " " label)
              :min (format/round (* min-val scale) 4)
              :max (format/round (* max-val scale) 4)})))
   []
   (filterv (metric/type-pred :quantitative) metric-configs)))

(defn composite-key [path]
  (keyword (str/join "-" (mapv name path))))

(defn event-stats-metrics
  [event-stats _k metric ms]
  {:post [(have? (some-fn nil? map?) %)]}
  (let [sample-count-path (conj (pop (:path (first ms))) :sample-count)
        sample-count (event-stats sample-count-path)]
    (when (and sample-count (pos? (long sample-count)))
      (reduce
       (fn [res m]
         (assoc res
                (composite-key (rest (:path m)))
                (format/format-value
                 (:dimension (have :dimension m))
                 (* (double (get event-stats (:path m)))
                    (double (:scale m))))))
       {:metric (:label metric)}
       (into [{:path sample-count-path
               :dimension :count
               :scale 1}]
             ms)))))

(defn event-stats
  [metrics-defs ev-stats]
  {:pre [ev-stats]
   :post [(have? vector? %)]}
  (reduce-kv
   (fn [res k metric]
     (if-let [groups (:groups metric)]
       (into res (event-stats groups ev-stats))
       (if-let [m (event-stats-metrics ev-stats k metric (:values metric))]
         (conj res m)
         res)))
   []
   metrics-defs))

(defn quantiles
  [metric-configs all-quantiles transforms]
  {:pre [(have? all-quantiles)]}
  (reduce
   (fn [res metric-config]
     (let [quantiles (get-in all-quantiles (:path metric-config))
           median-val (util/transform-sample-> (quantiles 0.5) transforms)
           metric-scale (double (:scale metric-config))
           [scale unit] (format/scale
                         (:dimension metric-config)
                         (* metric-scale median-val))
           scale (* (double scale) metric-scale)]
       (conj res
             (reduce-kv
              (fn [res q v]
                (assoc
                 res
                 q
                 (format/round
                  (* (util/transform-sample-> (double v) transforms) scale)
                  4)))
              {:metric (str (:label metric-config) " " unit)}
              quantiles))))
   []
   metric-configs))

(defn outlier-counts
  [metrics outliers]
  (reduce
   (fn [res metric]
     (let [mcs (:outlier-counts (get-in outliers (:path metric)))]
       (if (some pos? (vals mcs))
         (conj res (assoc mcs :_metric (:label metric)))
         res)))
   []
   metrics))

(defn- sampled-scheme-data
  [sampled]
  (when sampled
    (assoc
     (select-keys sampled [:batch-size :num-samples])
     :num-evals
     (* (long (:num-samples sampled)) (long (:batch-size sampled))))))

(defn collect-plan-data
  [bench-map]
  (let [samples-schema (sampled-scheme-data
                        (-> bench-map :samples))
        warmup-scheme (sampled-scheme-data
                       (some-> bench-map :warmup))
        estimation-scheme (sampled-scheme-data
                           (some-> bench-map :estimation))]
    (cond-> [(merge {:phase :sample} samples-schema)]
      warmup-scheme
      (conj (merge {:phase :warmup} warmup-scheme))
      estimation-scheme
      (conj (merge {:phase :estimation} estimation-scheme)))))

(defn column-data->maps
  "Convert data where each column's values are stored in vectors."
  [column-data column-keys column-tforms]
  (let [make-row (fn [& vals]
                   (zipmap
                    column-keys
                    (mapv
                     (fn [k v] ((column-tforms k identity) v))
                     column-keys vals)))]
    (apply mapv make-row (map column-data column-keys))))

(defn histogram
  [histogram transforms metric-config]
  {:pre [(have? histogram)]}
  (let [transform #(util/transform-sample-> % transforms)
        ^double min-val (transform (:min histogram))
        ^double metric-scale (:scale metric-config)
        [scale unit] (format/scale
                      (:dimension metric-config)
                      (* metric-scale min-val))
        scale (* (double scale) metric-scale)
        round #(format/round % 4)
        t-center (comp round (partial * scale) transform)
        t-density #(format/round % 3)
        histogram (if (= :criterium/histogram-fixed-width (:type histogram))
                    histogram
                    (-> histogram
                        #_(assoc
                           :density
                           (mapv
                            (fn [^double d ^double w]
                              (* d w))
                            (:density histogram)
                            (:widths histogram)))
                        (update :widths #(mapv t-density %))))
        histogram (-> histogram
                      (update :centers #(mapv t-center %))
                      (update :min t-center)
                      (update :max t-center)
                      (update :density #(mapv t-density %))
                      (assoc
                       :metric-config metric-config
                       :unit unit))]
    histogram))

;;; Domain view helpers - coordinate formatting

(defn format-coord
  "Format a coordinate for display."
  [coord]
  (if (map? coord)
    (into {} (map (fn [[k v]] [(name k) v])) coord)
    (name coord)))

(defn metric-path->dimension
  "Return the dimension keyword for a metric-path.
  Used to determine appropriate scaling via format/scale."
  [metric-path]
  (case (first metric-path)
    (:stats :log-stats)
    (case (second metric-path)
      :elapsed-time :time
      :thread-allocation :memory
      nil)
    nil))

(defn metric-path->base-scale
  "Return base scale factor to convert raw metric values to base units.
  Elapsed-time is stored in nanoseconds, so convert to seconds for scaling."
  ^double [metric-path]
  (case (first metric-path)
    (:stats :log-stats)
    (case (second metric-path)
      :elapsed-time 1e-9 ; ns -> s
      1)
    1))

(defn compute-si-scaling
  "Compute SI scaling factors for a metric-path given sample values.
  Returns {:base-scale, :si-scale, :total-scale, :unit}.
  - base-scale: converts raw values to base units (e.g., ns -> s)
  - si-scale: SI prefix scaling factor
  - total-scale: base-scale * si-scale
  - unit: SI unit string (e.g., \"ms\", \"μs\")"
  [metric-path values]
  (let [base-scale (metric-path->base-scale metric-path)
        dimension (metric-path->dimension metric-path)
        base-values (when (seq values)
                      (map #(* (double %) base-scale) values))
        representative-value (when (seq base-values)
                               (/ (double (reduce + base-values))
                                  (count base-values)))
        [^double si-scale si-unit] (if (and dimension representative-value)
                                     (format/scale
                                      dimension
                                      representative-value)
                                     [1 ""])]
    {:base-scale base-scale
     :si-scale si-scale
     :total-scale (* base-scale si-scale)
     :unit si-unit}))

(defn format-value-with-unit
  "Format a value from domain-extract as a string with SI units."
  [value metric-path]
  (when (some? value)
    (let [base-value (* (double value) (metric-path->base-scale metric-path))
          dimension (metric-path->dimension metric-path)]
      (if dimension
        (format/format-value dimension base-value)
        (format "%g" (double base-value))))))

;;; Coordinate key helpers

(defn single-key-coord-info
  "Detect if all row-keys are single-key maps with the same key.
  Returns {:key k :values [v1 v2 ...]} if so, nil otherwise."
  [row-keys]
  (when (and (seq row-keys)
             (every? map? row-keys)
             (every? #(= 1 (count %)) row-keys))
    (let [keys-set (into #{} (mapcat keys) row-keys)]
      (when (= 1 (count keys-set))
        (let [k (first keys-set)]
          {:key k
           :values (mapv #(get % k) row-keys)})))))

(defn sort-row-keys
  "Sort row-keys, using numeric sort when all values are numbers."
  [row-keys single-key-info]
  (if single-key-info
    (let [{:keys [values]} single-key-info
          all-numeric? (every? number? values)]
      (if all-numeric?
        (sort-by #(get % (:key single-key-info)) row-keys)
        (sort-by #(str (get % (:key single-key-info))) row-keys)))
    (sort-by str row-keys)))

(defn format-row-key-value
  "Format a row-key for display, extracting the value for single-key maps."
  [row-key single-key-info]
  (if single-key-info
    (get row-key (:key single-key-info))
    (format-coord row-key)))

(defn coord-column-header
  "Return the appropriate column header for coordinates."
  [single-key-info]
  (if single-key-info
    (name (:key single-key-info))
    "coordinate"))

;;; Value extraction helpers

(defn get-numeric-value
  "Extract numeric value from plain value or error-bound format {:value v}."
  [v]
  (if (and (map? v) (contains? v :value))
    (:value v)
    v))

(defn error-bound-value?
  "Returns true if v is an error-bound value map {:value X :error E}."
  [v]
  (and (map? v) (contains? v :value)))

(defn values-have-error-bounds?
  "Returns true if any value in coll has error bounds.
  Checks for :lower/:upper keys (from with-error-bounds option) or
  :ci-lower/:ci-upper keys (from bootstrap stats)."
  [coll]
  (boolean
   (some (fn [v]
           (and (map? v)
                (or (and (contains? v :lower)
                         (contains? v :upper))
                    (and (contains? v :ci-lower)
                         (contains? v :ci-upper)))))
         coll)))

(defn has-box-plot-data?
  "Check if a value map contains all required box plot fields.
  Box plot data requires :median, :p10, and :p90 keys."
  [v]
  (and (map? v)
       (contains? v :median)
       (contains? v :p10)
       (contains? v :p90)))

(defn warn-missing-bootstrap-stats
  "Print warning to stdout when bootstrap stats are missing for a metric."
  [metric-id]
  (println (str "WARNING: Missing bootstrap stats for metric " metric-id
                ". Box plot will not be rendered.")))

(defn detect-uniform-axes
  "Find coordinate axes where all values are identical.
  Returns a set of keys that have uniform values across all coords."
  [coords]
  (when (seq coords)
    (let [first-coord (first coords)
          uniform-keys (filter (fn [k]
                                 (let [v (get first-coord k)]
                                   (every? #(= v (get % k)) coords)))
                               (keys first-coord))]
      (set uniform-keys))))

;;; Tail Analysis Table Helpers

(defn tail-summary-table
  "Prepare summary table data for tail analysis.

  Returns a vector of row maps with:
  - Threshold value (with SI units)
  - Exceedances count (k)
  - GPD shape (ξ) and scale (σ)
  - Hill stable estimate with k-range

  Takes tail-data map for a single metric and optional transforms."
  [tail-data transforms]
  (let [{:keys [gpd hill threshold]} tail-data
        {:keys [xi sigma exceedances-count]} gpd
        {:keys [stable-estimate k-range]} hill
        k-min (when (seq k-range) (apply min k-range))
        k-max (when (seq k-range) (apply max k-range))
        ;; Format threshold with SI units (time dimension)
        threshold-transformed (when threshold
                                (util/transform-sample-> threshold transforms))
        threshold-formatted (when threshold-transformed
                              (format/format-value :time threshold-transformed))]
    (cond-> []
      threshold-formatted
      (conj {:parameter "Threshold"
             :value threshold-formatted})
      exceedances-count
      (conj {:parameter "Exceedances (k)"
             :value exceedances-count})
      xi
      (conj {:parameter "GPD shape (ξ)"
             :value (clojure.core/format "%.4f" (double xi))})
      sigma
      (conj {:parameter "GPD scale (σ)"
             :value (clojure.core/format "%.4g" (double sigma))})
      stable-estimate
      (conj {:parameter "Hill estimate"
             :value (if (and k-min k-max)
                      (clojure.core/format "%.4f (k: %d-%d)"
                                           (double stable-estimate) k-min k-max)
                      (clojure.core/format "%.4f" (double stable-estimate)))}))))

(defn tail-ratios-table-data
  "Prepare tail ratios table data for display.

  Returns a vector of row maps with ratio name, value, and constituent percentiles.
  Takes tail-data map for a single metric."
  [tail-data]
  (let [{:keys [tail-ratios empirical-quantiles]} tail-data
        {:keys [p99-p95 p999-p99 p999-p95]} tail-ratios
        {:keys [p95 p99 p999]} empirical-quantiles]
    (cond-> []
      p99-p95
      (conj {:ratio "p99/p95"
             :value (clojure.core/format "%.3f" (double p99-p95))
             :p95 (when p95 (clojure.core/format "%.4g" (double p95)))
             :p99 (when p99 (clojure.core/format "%.4g" (double p99)))})
      p999-p99
      (conj {:ratio "p999/p99"
             :value (clojure.core/format "%.3f" (double p999-p99))
             :p99 (when p99 (clojure.core/format "%.4g" (double p99)))
             :p999 (when p999 (clojure.core/format "%.4g" (double p999)))})
      p999-p95
      (conj {:ratio "p999/p95"
             :value (clojure.core/format "%.3f" (double p999-p95))
             :p95 (when p95 (clojure.core/format "%.4g" (double p95)))
             :p999 (when p999 (clojure.core/format "%.4g" (double p999)))}))))

(defn tail-high-quantiles-table
  "Prepare high quantiles table data for display.

  Returns a vector of row maps with quantile label and estimated value (SI units).
  Takes tail-data map for a single metric and optional transforms."
  [tail-data transforms]
  (let [{:keys [high-quantiles]} tail-data]
    (when (seq high-quantiles)
      (->> high-quantiles
           (sort-by first)
           (mapv (fn [[q val]]
                   (let [tval (util/transform-sample-> val transforms)
                         formatted (format/format-value :time tval)]
                     {:quantile (clojure.core/format "p%.4g" (* (double q) 100))
                      :estimate formatted})))))))
