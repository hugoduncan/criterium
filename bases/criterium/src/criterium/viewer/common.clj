(ns criterium.viewer.common
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]))

(defn metrics-map
  [sample metrics]
  (reduce
   (fn [res metric]
     (let [v (first (sample (:path metric)))]
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
           median-val (double
                       (util/transform-sample-> (quantiles 0.5) transforms))
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
        min-val (double (transform (:min histogram)))
        metric-scale (double (:scale metric-config))
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

;;; Domain shape detection

(defn single-point-multi-impl?
  "Return true when extract has exactly one non-impl axis with one value
  and multiple implementations.

  This detects the 'single-point comparison' scenario where we're comparing
  multiple implementations at a single parameter point."
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        impls (:implementations extract)
        multi-impl? (and impls (> (count impls) 1))]
    (when multi-impl?
      (let [metrics (:metrics extract)]
        (when (seq metrics)
          (let [;; Get all coordinates from first metric
                first-metric-data (:data (val (first metrics)))
                all-coords (map first first-metric-data)
                ;; Get the non-impl axis keys from first coordinate
                first-coord (first all-coords)
                non-impl-keys (when (map? first-coord)
                                (disj (set (keys first-coord)) impl-axis-key))
                ;; Single axis with single unique value?
                single-axis? (= 1 (count non-impl-keys))]
            (when single-axis?
              (let [axis-key (first non-impl-keys)
                    axis-values (into #{} (map #(get % axis-key)) all-coords)]
                (= 1 (count axis-values))))))))))

(defn single-axis-multi-point?
  "Return true when extract has exactly one non-impl axis with multiple values
  and multiple implementations.

  This detects the 'line chart' scenario where we're comparing multiple
  implementations across a range of parameter values on a single axis."
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        impls (:implementations extract)
        multi-impl? (and impls (> (count impls) 1))]
    (when multi-impl?
      (let [metrics (:metrics extract)]
        (when (seq metrics)
          (let [;; Get all coordinates from first metric
                first-metric-data (:data (val (first metrics)))
                all-coords (map first first-metric-data)
                ;; Get the non-impl axis keys from first coordinate
                first-coord (first all-coords)
                non-impl-keys (when (map? first-coord)
                                (disj (set (keys first-coord)) impl-axis-key))
                ;; Single axis?
                single-axis? (= 1 (count non-impl-keys))]
            (when single-axis?
              (let [axis-key (first non-impl-keys)
                    axis-values (into #{} (map #(get % axis-key)) all-coords)]
                (> (count axis-values) 1)))))))))

(defn visualization-strategy
  "Determine the visualization strategy for domain extract data.

  Returns one of:
  - :single-point-bar  - single parameter point, multiple implementations (bar chart)
  - :multi-point-line  - multiple parameter points, single axis (line chart)
  - :default-table     - regular table format (no chart)"
  [extract]
  (cond
    (single-point-multi-impl? extract) :single-point-bar
    (single-axis-multi-point? extract) :multi-point-line
    :else :default-table))

(defn- comparison-all-entries
  "Iterate over all entries in a comparison, handling both single and multi-metric modes.
  Returns a lazy sequence of entry maps (each containing :coord, :value)."
  [{:keys [data metrics]}]
  (if metrics
    (->> metrics
         vals
         (mapcat (fn [{:keys [data]}]
                   (mapcat val data))))
    (mapcat val data)))

(defn- comparison-point-count
  "Count unique parameter points in a comparison.

  For implementation comparison (axis=:impl, coords have :n), counts unique
  non-axis coords (e.g., {:n 10}, {:n 100}).

  For parameter comparison (axis=:n, coords only have :n), counts unique
  axis values since non-axis would be empty.

  Returns {:count N :has-non-axis-key? bool :single-non-axis-key? bool}"
  [{:keys [axis] :as comparison}]
  (let [all-coords (into #{} (map :coord) (comparison-all-entries comparison))
        non-axis-coords (into #{} (map #(dissoc % axis)) all-coords)
        axis-values (into #{} (map #(get % axis)) all-coords)
        first-non-axis (first non-axis-coords)
        has-non-axis-key? (and (map? first-non-axis) (seq first-non-axis))
        single-non-axis-key? (and has-non-axis-key?
                                  (= 1 (count first-non-axis)))]
    {:count (if has-non-axis-key?
              (count non-axis-coords)
              (count axis-values))
     :has-non-axis-key? has-non-axis-key?
     :single-non-axis-key? single-non-axis-key?}))

(defn single-point-multi-impl-comparison?
  "Return true when comparison has multiple implementations at a single parameter point.

  Bar chart scenario: comparing implementations without varying parameters.
  - axis = :impl with no other params → true (bar chart)
  - axis = :impl with same n value across all → true (bar chart)
  - axis = :n with single n value → true (bar chart)
  - axis = :n with multiple n values → false (use line chart instead)"
  [comparison]
  (let [{:keys [implementations axis]} comparison
        multi-impl? (and implementations (> (count implementations) 1))]
    (when multi-impl?
      (let [{:keys [count has-non-axis-key?]} (comparison-point-count comparison)]
        (if (= axis :impl)
          ;; For axis = :impl, single point means no non-axis variation
          (or (not has-non-axis-key?) (= 1 count))
          ;; For axis != :impl, single point means only one axis value
          (= 1 count))))))

(defn single-axis-multi-point-comparison?
  "Return true when comparison has multiple implementations across multiple parameter points.

  Line chart scenario: comparing implementations with a varying parameter axis.
  - axis = :impl with different n values → true (line chart)
  - axis = :impl with no n or same n → false (use bar chart)
  - axis = :n with multiple n values → true (line chart)"
  [comparison]
  (let [{:keys [implementations axis]} comparison
        multi-impl? (and implementations (> (count implementations) 1))]
    (when multi-impl?
      (let [{:keys [count has-non-axis-key? single-non-axis-key?]}
            (comparison-point-count comparison)]
        ;; Multi-point if there are multiple parameter values
        (if (= axis :impl)
          ;; axis = :impl: need non-axis variation
          (and has-non-axis-key?
               (> (long count) 1)
               single-non-axis-key?)
          ;; axis != :impl (e.g., :n): axis itself is the varying parameter
          (> (long count) 1))))))

(defn comparison-visualization-strategy
  "Determine the visualization strategy for domain comparison data.

  Returns one of:
  - :single-point-bar  - single parameter point, multiple implementations (bar chart)
  - :multi-point-line  - multiple parameter points, single axis (line chart)
  - :default-table     - regular table format (no chart)"
  [comparison]
  (cond
    (single-point-multi-impl-comparison? comparison) :single-point-bar
    (single-axis-multi-point-comparison? comparison) :multi-point-line
    :else :default-table))

;;; Domain view helpers

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

(defn get-numeric-value
  "Extract numeric value from plain value or error-bound format {:value v}."
  [v]
  (if (and (map? v) (contains? v :value))
    (:value v)
    v))

(defn- error-bound-value?
  "Returns true if v is an error-bound value map {:value X :error E}."
  [v]
  (and (map? v) (contains? v :value)))

(defn values-have-error-bounds?
  "Returns true if any value in coll has :lower and :upper keys for error bounds."
  [coll]
  (boolean
   (some (fn [v]
           (and (map? v)
                (contains? v :lower)
                (contains? v :upper)))
         coll)))

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

(defn prepare-domain-extract-table
  "Prepare domain-extract data for table rendering.
  Returns {:heading string :coord-header string :col-headers [string...]
           :rows [{col-header value...}...]} or nil if extract is nil.
  Options:
    :header-sep - separator between impl and metric in multi-impl headers
                  (default \" \")"
  [extract {:keys [header-sep] :or {header-sep " "}}]
  (when extract
    (let [impl-axis-key (:impl-axis extract)
          multi-impl? (> (count (:implementations extract)) 1)
          metrics (:metrics extract)
          metric-ids (sort (keys metrics))

          ;; Collect all data points with their full coords
          all-data (for [[metric-id {:keys [metric data]}] metrics
                         [coord value] data]
                     {:metric-id metric-id
                      :metric metric
                      :coord coord
                      :value (get-numeric-value value)})

          ;; Collect all coordinates to detect uniform axes
          all-coords (map :coord all-data)

          ;; Find axes with uniform values across all coords (e.g., :impl
          ;; :default)
          ;; Only use uniform axes if stripping them leaves at least one key
          uniform-axes (detect-uniform-axes all-coords)
          first-coord (first all-coords)
          remaining-after-strip (when (and (map? first-coord)
                                           (seq uniform-axes))
                                  (count
                                   (apply dissoc first-coord uniform-axes)))
          use-uniform-axes? (and (seq uniform-axes)
                                 (some? remaining-after-strip)
                                 (pos? (long remaining-after-strip)))

          ;; Determine row key: strip impl axis for multi-impl, or uniform axes
          row-key-fn (cond
                       multi-impl?
                       (fn [coord] (dissoc coord impl-axis-key))

                       use-uniform-axes?
                       (fn [coord] (apply dissoc coord uniform-axes))

                       :else
                       identity)

          ;; Collect unique row keys
          raw-row-keys (->> all-data
                            (map (comp row-key-fn :coord))
                            distinct)

          ;; Detect single-key pattern and sort appropriately
          single-key-info (single-key-coord-info raw-row-keys)
          row-keys (sort-row-keys raw-row-keys single-key-info)
          coord-header (coord-column-header single-key-info)

          impl-vals (when multi-impl?
                      (->> all-data
                           (keep #(get (:coord %) impl-axis-key))
                           distinct
                           (sort-by str)))

          ;; Build column specs: [{:metric-id :impl (optional)}...]
          col-specs (if multi-impl?
                      (for [metric-id metric-ids
                            impl impl-vals]
                        {:metric-id metric-id :impl impl})
                      (for [metric-id metric-ids]
                        {:metric-id metric-id}))

          ;; Build lookup: {[row-key metric-id impl?] -> value}
          lookup (reduce (fn [acc {:keys [metric-id coord value]}]
                           (let [row-key (row-key-fn coord)
                                 impl-val (when
                                           multi-impl?
                                            (get coord impl-axis-key))
                                 lookup-key (if multi-impl?
                                              [row-key metric-id impl-val]
                                              [row-key metric-id])]
                             (assoc acc lookup-key value)))
                         {}
                         all-data)

          ;; Compute SI scale and unit per column
          col-scales
          (into
           {}
           (map (fn [col-spec]
                  (let [{:keys [metric-id impl]}
                        col-spec
                        metric-path (get-in metrics [metric-id :metric])
                        col-values (for [row-key row-keys
                                         :let [lk (if multi-impl?
                                                    [row-key metric-id impl]
                                                    [row-key metric-id])
                                               v (get lookup lk)]
                                         :when (some? v)]
                                     v)]
                    [col-spec (compute-si-scaling metric-path col-values)])))
           col-specs)

          ;; Build column headers
          col-headers
          (mapv (fn [col-spec]
                  (let [{:keys [metric-id impl]}
                        col-spec
                        {:keys [unit]} (get col-scales col-spec)
                        metric-name (name metric-id)
                        header-base (if (seq unit)
                                      (str metric-name " (" unit ")")
                                      metric-name)]
                    (if multi-impl?
                      (str (name impl) header-sep header-base)
                      header-base)))
                col-specs)

          ;; Build table rows
          table-rows
          (mapv (fn [row-key]
                  (into {coord-header
                         (format-row-key-value row-key single-key-info)}
                        (map-indexed
                         (fn [idx col-spec]
                           (let [{:keys [metric-id impl]}
                                 col-spec
                                 lk (if multi-impl?
                                      [row-key metric-id impl]
                                      [row-key metric-id])
                                 raw-value (get lookup lk)
                                 {:keys [^double total-scale]}
                                 (get col-scales col-spec)
                                 header (nth col-headers idx)]
                             [header (when raw-value
                                       (format
                                        "%.3g"
                                        (* (double raw-value) total-scale)))]))
                         col-specs)))
                row-keys)]

      {:heading "Domain Extract"
       :coord-header coord-header
       :col-headers col-headers
       :rows table-rows})))

(defn prepare-domain-extract-table-transposed
  "Prepare transposed domain-extract table for single-point multi-impl scenarios.
  Returns {:heading :col-headers :rows} where each row is one implementation.

  Columns include implementation name, then for each metric: value and factor.
  Factor is relative to baseline (first implementation)."
  [extract]
  (when extract
    (let [impl-axis-key (:impl-axis extract)
          implementations (:implementations extract)
          baseline-impl (first implementations)
          metrics (:metrics extract)
          metric-ids (sort (keys metrics))

          ;; Build lookup: {[impl metric-id] -> raw-value}
          lookup
          (reduce
           (fn [acc [metric-id {:keys [data]}]]
             (reduce
              (fn [acc2 [coord value]]
                (let [impl-val (get coord impl-axis-key)
                      raw-value (if (and (map? value) (contains? value :value))
                                  (:value value)
                                  value)]
                  (assoc acc2 [impl-val metric-id] raw-value)))
              acc
              data))
           {}
           metrics)

          ;; Compute SI scaling per metric (using all values for that metric)
          metric-scales
          (into {}
                (map (fn [metric-id]
                       (let [metric-path (get-in metrics [metric-id :metric])
                             all-values (keep (fn [impl]
                                                (get lookup [impl metric-id]))
                                              implementations)]
                         [metric-id (compute-si-scaling metric-path all-values)])))
                metric-ids)

          ;; Build column headers: Implementation, then for each metric: value and ×
          col-headers
          (into ["Implementation"]
                (mapcat (fn [metric-id]
                          (let [{:keys [unit]} (get metric-scales metric-id)
                                metric-name (name metric-id)
                                value-header (if (seq unit)
                                               (str metric-name " (" unit ")")
                                               metric-name)]
                            [value-header (str metric-name " ×")]))
                        metric-ids))

          ;; Build table rows: one per implementation
          table-rows
          (mapv
           (fn [impl]
             (into {"Implementation" (name impl)}
                   (mapcat
                    (fn [metric-id]
                      (let [{:keys [unit ^double total-scale]}
                            (get metric-scales metric-id)
                            metric-name (name metric-id)
                            value-header (if (seq unit)
                                           (str metric-name " (" unit ")")
                                           metric-name)
                            factor-header (str metric-name " ×")
                            raw-value (get lookup [impl metric-id])
                            baseline-value (get lookup [baseline-impl metric-id])
                            formatted-value (when raw-value
                                              (format "%.3g"
                                                      (* (double raw-value)
                                                         total-scale)))
                            factor (when (and raw-value baseline-value
                                              (not (zero? (double baseline-value))))
                                     (format "%.2f"
                                             (/ (double raw-value)
                                                (double baseline-value))))]
                        [[value-header formatted-value]
                         [factor-header factor]]))
                    metric-ids)))
           implementations)]

      {:heading "Domain Extract"
       :col-headers col-headers
       :rows table-rows})))

(defn prepare-comparison-bar-data
  "Prepare data for single-point bar chart from domain comparison.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword (or nil for single-metric mode)
    :metric-path - the metric path vector
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {:impl string :value number :valueLower number :valueUpper number} maps
            (valueLower/valueUpper only present when error bounds exist)"
  [comparison]
  (let [{:keys [metric metrics implementations data]} comparison]
    (if metrics
      ;; Multi-metric mode
      (mapv
       (fn [[metric-id {:keys [metric data]}]]
         (let [;; Build lookup: impl -> full value (may be map with :value/:lower/:upper)
               lookup (reduce
                       (fn [acc [impl-val entries]]
                         (reduce
                          (fn [acc2 {:keys [value]}]
                            (assoc acc2 impl-val value))
                          acc
                          entries))
                       {}
                       data)
               ;; Get all values from lookup for error bounds check and SI scaling
               all-raw-values (keep #(get lookup %) implementations)
               ;; Check if any values have error bounds
               has-error-bounds? (values-have-error-bounds? all-raw-values)
               ;; Get numeric values for SI scaling
               get-numeric (fn [v]
                             (if (and (map? v) (contains? v :value))
                               (:value v)
                               v))
               all-values (map get-numeric all-raw-values)
               {:keys [^double total-scale unit]}
               (compute-si-scaling metric all-values)
               ;; Build y-axis title with unit
               metric-name (name metric-id)
               base-title (if has-error-bounds?
                            (str "mean " metric-name)
                            metric-name)
               y-title (if (seq unit)
                         (str base-title " (" unit ")")
                         base-title)
               ;; Build chart data
               chart-data (mapv
                           (fn [impl]
                             (let [v (get lookup impl)
                                   raw-value (get-numeric v)]
                               (cond-> {"impl" (name impl)
                                        "value" (when raw-value
                                                  (* (double raw-value) total-scale))}
                                 (and has-error-bounds?
                                      (map? v)
                                      (contains? v :lower))
                                 (assoc "valueLower" (* (double (:lower v)) total-scale))
                                 (and has-error-bounds?
                                      (map? v)
                                      (contains? v :upper))
                                 (assoc "valueUpper" (* (double (:upper v)) total-scale)))))
                           implementations)]
           {:metric-id metric-id
            :metric-path metric
            :y-title y-title
            :has-error-bounds? has-error-bounds?
            :data chart-data}))
       (sort-by key metrics))
      ;; Single-metric mode
      (let [;; Build lookup: impl -> full value (may be map with :value/:lower/:upper)
            lookup (reduce
                    (fn [acc [impl-val entries]]
                      (reduce
                       (fn [acc2 {:keys [value]}]
                         (assoc acc2 impl-val value))
                       acc
                       entries))
                    {}
                    data)
            ;; Get all values from lookup for error bounds check and SI scaling
            all-raw-values (keep #(get lookup %) implementations)
            ;; Check if any values have error bounds
            has-error-bounds? (values-have-error-bounds? all-raw-values)
            ;; Get numeric values for SI scaling
            get-numeric (fn [v]
                          (if (and (map? v) (contains? v :value))
                            (:value v)
                            v))
            all-values (map get-numeric all-raw-values)
            {:keys [^double total-scale unit]}
            (compute-si-scaling metric all-values)
            ;; Build y-axis title with unit
            base-title (if has-error-bounds?
                         (str "mean " (pr-str metric))
                         (pr-str metric))
            y-title (if (seq unit)
                      (str base-title " (" unit ")")
                      base-title)
            ;; Build chart data
            chart-data (mapv
                        (fn [impl]
                          (let [v (get lookup impl)
                                raw-value (get-numeric v)]
                            (cond-> {"impl" (name impl)
                                     "value" (when raw-value
                                               (* (double raw-value) total-scale))}
                              (and has-error-bounds?
                                   (map? v)
                                   (contains? v :lower))
                              (assoc "valueLower" (* (double (:lower v)) total-scale))
                              (and has-error-bounds?
                                   (map? v)
                                   (contains? v :upper))
                              (assoc "valueUpper" (* (double (:upper v)) total-scale)))))
                        implementations)]
        [{:metric-id nil
          :metric-path metric
          :y-title y-title
          :has-error-bounds? has-error-bounds?
          :data chart-data}]))))

(defn prepare-line-chart-data
  "Prepare data for line chart from domain extract.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword
    :metric-path - the metric path vector
    :x-title - x-axis title (the axis name)
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {\"x\" number \"y\" number \"impl\" string} maps
            (yLower/yUpper only present when error bounds exist)"
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        metrics (:metrics extract)
        ;; Find the non-impl axis key
        first-metric-data (:data (val (first metrics)))
        first-coord (first (first first-metric-data))
        non-impl-keys (when (map? first-coord)
                        (disj (set (keys first-coord)) impl-axis-key))
        axis-key (first non-impl-keys)]
    (mapv
     (fn [[metric-id {:keys [metric data]}]]
       (let [;; Get all raw values for error detection and SI scaling
             all-raw-values (keep (fn [[_coord value]] value) data)
             ;; Check if any values have error bounds (with :lower/:upper)
             has-error-bounds? (values-have-error-bounds? all-raw-values)
             ;; Also check for error-bound-value? (with :value key) for y-title
             has-error-bound-format (some error-bound-value? all-raw-values)
             all-values (map get-numeric-value all-raw-values)
             {:keys [^double total-scale unit]}
             (compute-si-scaling metric all-values)
             ;; Build axis titles
             x-title (name axis-key)
             metric-name (name metric-id)
             base-title (if (or has-error-bounds? has-error-bound-format)
                          (str "mean " metric-name)
                          metric-name)
             y-title (if (seq unit)
                       (str base-title " (" unit ")")
                       base-title)
             ;; Build chart data points
             chart-data (mapv
                         (fn [[coord value]]
                           (let [raw-value (get-numeric-value value)
                                 x-val (get coord axis-key)
                                 impl-val (get coord impl-axis-key)]
                             (cond-> {"x" x-val
                                      "y" (when raw-value
                                            (* (double raw-value) total-scale))
                                      "impl" (name impl-val)}
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :lower))
                               (assoc "yLower" (* (double (:lower value)) total-scale))
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :upper))
                               (assoc "yUpper" (* (double (:upper value)) total-scale)))))
                         data)]
         {:metric-id metric-id
          :metric-path metric
          :x-title x-title
          :y-title y-title
          :has-error-bounds? has-error-bounds?
          :data chart-data}))
     (sort-by key metrics))))

(defn- find-non-axis-key
  "Find the non-axis coordinate key from comparison data.
  Returns the single non-axis key if coords have exactly one, else nil."
  [{:keys [axis metrics data]}]
  (let [first-entries (if metrics
                        (some-> metrics vals first :data vals first)
                        (some-> data vals first))
        first-coord (some-> first-entries first :coord)]
    (when (map? first-coord)
      (let [non-axis-keys (disj (set (keys first-coord)) axis)]
        (when (= 1 (count non-axis-keys))
          (first non-axis-keys))))))

(defn prepare-comparison-line-data
  "Prepare data for line chart from domain comparison.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword (or nil for single-metric mode)
    :metric-path - the metric path vector
    :x-title - x-axis title (the non-axis coordinate name)
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {\"x\" number \"y\" number \"impl\" string} maps
            (yLower/yUpper only present when error bounds exist)

  When the comparison axis is the implementation axis (i.e., implementations
  are present), uses the non-axis coordinate key for the x-axis."
  [comparison]
  (let [{:keys [axis metric metrics data implementations]} comparison
        ;; Use non-axis coord key for x when comparing implementations
        x-key (if implementations
                (or (find-non-axis-key comparison) axis)
                axis)
        x-title (name x-key)]
    (if metrics
      ;; Multi-metric mode
      (mapv
       (fn [[metric-id {:keys [metric data]}]]
         (let [;; Get all raw values for error detection and SI scaling
               all-raw-values (->> data
                                   vals
                                   (mapcat (fn [entries]
                                             (keep :value entries))))
               ;; Check if any values have error bounds (with :lower/:upper)
               has-error-bounds? (values-have-error-bounds? all-raw-values)
               ;; Also check for error-bound-value? (with :value key) for y-title
               has-error-bound-format (some error-bound-value? all-raw-values)
               all-values (map get-numeric-value all-raw-values)
               {:keys [^double total-scale unit]}
               (compute-si-scaling metric all-values)
               ;; Build y-axis title
               metric-name (name metric-id)
               base-title (if (or has-error-bounds? has-error-bound-format)
                            (str "mean " metric-name)
                            metric-name)
               y-title (if (seq unit)
                         (str base-title " (" unit ")")
                         base-title)
               ;; Build chart data points
               chart-data (vec
                           (for [[impl-val entries] data
                                 {:keys [coord value]} entries
                                 :let [raw-value (get-numeric-value value)
                                       x-val (get coord x-key)]
                                 :when (some? raw-value)]
                             (cond-> {"x" x-val
                                      "y" (* (double raw-value) total-scale)
                                      "impl" (name impl-val)}
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :lower))
                               (assoc "yLower" (* (double (:lower value)) total-scale))
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :upper))
                               (assoc "yUpper" (* (double (:upper value)) total-scale)))))]
           {:metric-id metric-id
            :metric-path metric
            :x-title x-title
            :y-title y-title
            :has-error-bounds? has-error-bounds?
            :data chart-data}))
       (sort-by key metrics))
      ;; Single-metric mode
      (let [;; Get all raw values for error detection and SI scaling
            all-raw-values (->> data
                                vals
                                (mapcat (fn [entries]
                                          (keep :value entries))))
            ;; Check if any values have error bounds (with :lower/:upper)
            has-error-bounds? (values-have-error-bounds? all-raw-values)
            ;; Also check for error-bound-value? (with :value key) for y-title
            has-error-bound-format (some error-bound-value? all-raw-values)
            all-values (map get-numeric-value all-raw-values)
            {:keys [^double total-scale unit]}
            (compute-si-scaling metric all-values)
            ;; Build y-axis title
            base-title (if (or has-error-bounds? has-error-bound-format)
                         (str "mean " (pr-str metric))
                         (pr-str metric))
            y-title (if (seq unit)
                      (str base-title " (" unit ")")
                      base-title)
            ;; Build chart data points
            chart-data (vec
                        (for [[impl-val entries] data
                              {:keys [coord value]} entries
                              :let [raw-value (get-numeric-value value)
                                    x-val (get coord x-key)]
                              :when (some? raw-value)]
                          (cond-> {"x" x-val
                                   "y" (* (double raw-value) total-scale)
                                   "impl" (name impl-val)}
                            (and has-error-bounds?
                                 (map? value)
                                 (contains? value :lower))
                            (assoc "yLower" (* (double (:lower value)) total-scale))
                            (and has-error-bounds?
                                 (map? value)
                                 (contains? value :upper))
                            (assoc "yUpper" (* (double (:upper value)) total-scale)))))]
        [{:metric-id nil
          :metric-path metric
          :x-title x-title
          :y-title y-title
          :has-error-bounds? has-error-bounds?
          :data chart-data}]))))

(defn- extract-row-key
  "Extract row key from coord, removing axis key for map coords."
  [coord axis]
  (if (map? coord)
    (dissoc coord axis)
    coord))

(defn- build-absolute-value-table
  "Build a table spec for absolute value display (no implementations)."
  [axis metric data]
  (let [axis-vals (sort-by str (keys data))]
    (when (and (seq data) (some #(seq (second %)) data))
      (let [all-entries (mapcat
                         (fn [[axis-val entries]]
                           (mapv #(assoc % :axis-val axis-val) entries))
                         data)
            raw-row-keys (->> all-entries
                              (mapv
                               #(extract-row-key (:coord %) axis)) distinct)
            single-key-info (single-key-coord-info raw-row-keys)
            row-keys (sort-row-keys
                      raw-row-keys
                      single-key-info)
            coord-header (coord-column-header single-key-info)
            lookup (reduce
                    (fn [acc {:keys [coord value axis-val]}]
                      (let [row-key (extract-row-key coord axis)]
                        (assoc-in acc [row-key axis-val] value)))
                    {}
                    all-entries)
            all-values (keep :value all-entries)
            {:keys [^double total-scale unit]}
            (compute-si-scaling metric all-values)
            heading (str
                     "Domain Comparison by "
                     (name axis) ": " (pr-str metric)
                     (when (seq unit) (str " (" unit ")")))
            col-headers (mapv str axis-vals)
            table-rows
            (mapv (fn [row-key]
                    (into {coord-header (format-row-key-value
                                         row-key
                                         single-key-info)}
                          (map (fn [av]
                                 (let [raw-value (double
                                                  (get-in lookup [row-key av]))]
                                   [(str av)
                                    (when raw-value
                                      (format
                                       "%.3g"
                                       (* raw-value total-scale)))]))
                               axis-vals)))
                  row-keys)]
        {:heading heading
         :coord-header coord-header
         :col-headers col-headers
         :rows table-rows}))))

(defn- build-factor-table-single-metric
  "Build a table spec for single-metric factor display (with implementations)."
  [axis metric implementations data]
  (let [data-keys (set (keys data))
        missing (remove data-keys implementations)]
    (when (seq missing)
      (throw (ex-info "Domain :implementations do not match comparison data keys"
                      {:implementations implementations
                       :data-keys (keys data)
                       :missing missing})))
    (let [baseline-impl (first implementations)
          other-impls (rest implementations)
          all-row-keys (->> (vals data)
                            (mapcat (fn [entries]
                                      (map #(extract-row-key (:coord %) axis) entries)))
                            distinct)
          single-key-info (single-key-coord-info all-row-keys)
          row-keys (sort-row-keys all-row-keys single-key-info)
          coord-header (coord-column-header single-key-info)
          lookup (reduce (fn [acc [impl-val entries]]
                           (reduce (fn [acc2 {:keys [coord value]}]
                                     (let [row-key (extract-row-key coord axis)]
                                       (assoc-in acc2 [impl-val row-key] value)))
                                   acc
                                   entries))
                         {}
                         data)
          ;; Build column specs: baseline shows value, others show value + factor
          col-specs (vec (cons {:type :baseline :impl baseline-impl}
                               (mapcat (fn [impl]
                                         [{:type :value :impl impl}
                                          {:type :factor :impl impl}])
                                       other-impls)))
          col-headers (mapv (fn [{:keys [type impl]}]
                              (case type
                                :baseline (str (name impl))
                                :value (str (name impl))
                                :factor (str (name impl) " ×")))
                            col-specs)
          table-rows
          (mapv
           (fn [row-key]
             (into {coord-header (format-row-key-value row-key single-key-info)}
                   (map (fn [{:keys [type impl]} header]
                          (let [raw-value (get-numeric-value
                                           (get-in lookup [impl row-key]))
                                value (when raw-value (double raw-value))
                                raw-baseline (get-numeric-value
                                              (get-in
                                               lookup
                                               [baseline-impl row-key]))
                                baseline-value (when raw-baseline
                                                 (double raw-baseline))]
                            [header
                             (case type
                               :baseline (format-value-with-unit value metric)
                               :value (format-value-with-unit value metric)
                               :factor (cond
                                         (nil? value) "-"
                                         (nil? baseline-value) "-"
                                         (zero? baseline-value) "-"
                                         :else
                                         (format
                                          "%.2f"
                                          (double (/ value baseline-value)))))]))
                        col-specs col-headers)))
           row-keys)]
      {:heading (str "Domain Comparison by " (name axis) ": " (pr-str metric))
       :coord-header coord-header
       :col-headers col-headers
       :rows table-rows})))

(defn- build-factor-table-multi-metric
  "Build a table spec for multi-metric factor display (with implementations)."
  [axis implementations metrics]
  (let [baseline-impl (first implementations)
        other-impls (rest implementations)
        metric-ids (sort (keys metrics))
        all-row-keys (->> (vals metrics)
                          (mapcat (fn [{:keys [data]}]
                                    (mapcat (fn [[_impl entries]]
                                              (map #(extract-row-key (:coord %) axis) entries))
                                            data)))
                          distinct)
        single-key-info (single-key-coord-info all-row-keys)
        row-keys (sort-row-keys all-row-keys single-key-info)
        coord-header (coord-column-header single-key-info)
        lookup (reduce (fn [acc [metric-id {:keys [data]}]]
                         (reduce (fn [acc2 [impl-val entries]]
                                   (reduce (fn [acc3 {:keys [coord value]}]
                                             (let [row-key (extract-row-key coord axis)]
                                               (assoc-in acc3 [metric-id impl-val row-key] value)))
                                           acc2
                                           entries))
                                 acc
                                 data))
                       {}
                       metrics)
        ;; Build column specs: baseline shows value, others show value + factor
        col-specs (vec (mapcat (fn [metric-id]
                                 (let [metric-path (get-in metrics [metric-id :metric])]
                                   (cons {:type :baseline
                                          :metric-id metric-id
                                          :metric-path metric-path
                                          :impl baseline-impl}
                                         (mapcat (fn [impl]
                                                   [{:type :value
                                                     :metric-id metric-id
                                                     :metric-path metric-path
                                                     :impl impl}
                                                    {:type :factor
                                                     :metric-id metric-id
                                                     :metric-path metric-path
                                                     :impl impl}])
                                                 other-impls))))
                               metric-ids))
        col-headers (mapv (fn [{:keys [type metric-id impl]}]
                            (case type
                              :baseline (str (name impl) " " (name metric-id))
                              :value (str (name impl) " " (name metric-id))
                              :factor (str (name impl) " ×")))
                          col-specs)
        table-rows
        (mapv
         (fn [row-key]
           (into
            {coord-header (format-row-key-value row-key single-key-info)}
            (map
             (fn [{:keys [type metric-id metric-path impl]} header]
               (let [raw-value (get-numeric-value
                                (get-in lookup [metric-id impl row-key]))
                     value (when raw-value (double raw-value))
                     raw-baseline (get-numeric-value
                                   (get-in
                                    lookup
                                    [metric-id baseline-impl row-key]))
                     baseline-value (when raw-baseline (double raw-baseline))]
                 [header
                  (case type
                    :baseline (format-value-with-unit value metric-path)
                    :value (format-value-with-unit value metric-path)
                    :factor (cond
                              (nil? value) "-"
                              (nil? baseline-value) "-"
                              (zero? baseline-value) "-"
                              :else (format "%.2f" (/ value baseline-value))))]))
             col-specs col-headers)))
         row-keys)]
    {:heading (str "Domain Comparison by " (name axis))
     :coord-header coord-header
     :col-headers col-headers
     :rows table-rows}))

(defn prepare-domain-comparison-tables
  "Prepare domain-comparison data for table rendering.
  Returns a vector of table specs, each with:
    {:heading string :coord-header string :col-headers [string...] :rows [{...}...]}
  Returns nil if comparison is nil.

  Handles 4 modes:
  - Multi-metric with implementations: single table with factor display
  - Multi-metric without implementations: multiple tables (one per metric)
  - Single-metric with implementations: single table with factor display
  - Single-metric without implementations: single table with absolute values"
  [comparison]
  (when comparison
    (let [{:keys [axis metric metrics implementations data]} comparison]
      (if metrics
        ;; Multi-metric mode
        (if implementations
          ;; Multi-metric with implementations - factor display
          [(build-factor-table-multi-metric axis implementations metrics)]
          ;; Multi-metric without implementations - one table per metric
          (vec (keep (fn [[_metric-id {:keys [metric data]}]]
                       (build-absolute-value-table axis metric data))
                     metrics)))
        ;; Single-metric mode
        (if implementations
          ;; Single-metric with implementations - factor display
          [(build-factor-table-single-metric axis metric implementations data)]
          ;; Single-metric without implementations - absolute values
          (when-let [table (build-absolute-value-table axis metric data)]
            [table]))))))

(defn prepare-domain-comparison-table-transposed
  "Prepare transposed domain-comparison table for single-point multi-impl scenarios.
  Returns {:heading :col-headers :rows} where each row is one implementation.

  Columns include implementation name, then for each metric: value and factor.
  Factor is relative to baseline (first implementation)."
  [comparison]
  (when comparison
    (let [{:keys [axis metric metrics implementations data]} comparison
          baseline-impl (first implementations)
          ;; Normalize to multi-metric structure
          ;; For single-metric mode, derive a meaningful key from metric path
          metrics-map (or metrics
                          (let [metric-id (or (second metric) :value)]
                            {metric-id {:metric metric :data data}}))
          metric-ids (sort (keys metrics-map))

          ;; Build lookup: {[impl metric-id] -> raw-value}
          lookup
          (reduce
           (fn [acc [metric-id {:keys [data]}]]
             (reduce
              (fn [acc2 [impl-val entries]]
                (reduce
                 (fn [acc3 {:keys [value]}]
                   (let [raw-value (if (and (map? value) (contains? value :value))
                                     (:value value)
                                     value)]
                     (assoc acc3 [impl-val metric-id] raw-value)))
                 acc2
                 entries))
              acc
              data))
           {}
           metrics-map)

          ;; Compute SI scaling per metric (using all values for that metric)
          metric-scales
          (into {}
                (map (fn [metric-id]
                       (let [metric-path (get-in metrics-map [metric-id :metric])
                             all-values (keep (fn [impl]
                                                (get lookup [impl metric-id]))
                                              implementations)]
                         [metric-id (compute-si-scaling metric-path all-values)])))
                metric-ids)

          ;; Build column headers: Implementation, then for each metric: value and ×
          col-headers
          (into ["Implementation"]
                (mapcat (fn [metric-id]
                          (let [{:keys [unit]} (get metric-scales metric-id)
                                metric-name (name metric-id)
                                value-header (if (seq unit)
                                               (str metric-name " (" unit ")")
                                               metric-name)]
                            [value-header (str metric-name " ×")]))
                        metric-ids))

          ;; Build table rows: one per implementation
          table-rows
          (mapv
           (fn [impl]
             (into {"Implementation" (name impl)}
                   (mapcat
                    (fn [metric-id]
                      (let [{:keys [unit ^double total-scale]}
                            (get metric-scales metric-id)
                            metric-name (name metric-id)
                            value-header (if (seq unit)
                                           (str metric-name " (" unit ")")
                                           metric-name)
                            factor-header (str metric-name " ×")
                            raw-value (get lookup [impl metric-id])
                            baseline-value (get lookup [baseline-impl metric-id])
                            formatted-value (when raw-value
                                              (format "%.3g"
                                                      (* (double raw-value)
                                                         total-scale)))
                            factor (when (and raw-value baseline-value
                                              (not (zero? (double baseline-value))))
                                     (format "%.2f"
                                             (/ (double raw-value)
                                                (double baseline-value))))]
                        [[value-header formatted-value]
                         [factor-header factor]]))
                    metric-ids)))
           implementations)]

      {:heading (str "Domain Comparison by " (name axis))
       :col-headers col-headers
       :rows table-rows})))

;;; Domain grouped view helpers

(defn prepare-domain-grouped-table
  "Prepare domain-grouped data for table rendering.
  Returns {:heading string :rows [{:axis-value string :run-count int}...]}
  or nil if grouped is nil."
  [grouped]
  (when grouped
    (let [{:keys [axis data]} grouped]
      {:heading (str "Domain Grouped by: " (name axis))
       :rows (mapv (fn [[axis-val sub-domain]]
                     {:axis-value (if (nil? axis-val)
                                    "<nil>"
                                    (str axis-val))
                      :run-count (count (:runs sub-domain))})
                   (sort-by (comp str key) data))})))

;;; Domain regression view helpers

(defn prepare-regression-model-table
  "Prepare model table rows for single-impl regression display.
  Returns vector of row maps with :model :r-squared :equation :best-fit keys.
  Options:
    :best-fit-marker - string to show for best fit (default \"✓\")
    :plotted-marker - string to show for plotted but not best (default \"\")
    :tolerance - fraction within best r-squared to mark as plotted (default 0.01)"
  [{:keys [models best-fit]}
   {:keys [best-fit-marker plotted-marker ^double tolerance]
    :or {best-fit-marker "✓" plotted-marker "" tolerance 0.01}}]
  (when (seq models)
    (let [best-r-squared (->> models
                              (filter #(= (:id %) best-fit))
                              first
                              :r-squared
                              double)
          plotted-ids (when best-r-squared
                        (->> models
                             (filter #(>= (double (:r-squared %))
                                          (* best-r-squared (- 1 tolerance))))
                             (map :id)
                             set))
          sorted-models (sort-by :r-squared > models)]
      (mapv (fn [{:keys [id label equation-str r-squared]}]
              (let [plotted? (and plotted-ids (plotted-ids id))]
                {:model label
                 :r-squared (format "%.4f" r-squared)
                 :equation (or equation-str "")
                 :best-fit (cond
                             (= id best-fit) best-fit-marker
                             plotted? plotted-marker
                             :else "")}))
            sorted-models))))

(defn prepare-regression-model-table-multi-impl
  "Prepare model table rows for multi-impl regression display.
  Returns vector of row maps with :implementation :model :r-squared :equation :best-fit.
  Options same as prepare-regression-model-table."
  [by-impl impl-keys options]
  (vec
   (mapcat
    (fn [impl-key]
      (let [impl-data (get by-impl impl-key)
            rows (prepare-regression-model-table impl-data options)]
        (mapv #(assoc % :implementation (name impl-key)) rows)))
    impl-keys)))

(defn prepare-regression-points
  "Prepare data points for regression scatter plot.
  Returns {:points [...] :total-scale number :unit string :x-vals [...]} or nil.
  Points have keys: x, y, and optionally yLower, yUpper for error bounds.
  For multi-impl mode, points also have :impl key."
  [extract-data {:keys [axis impl-axis has-error-bounds? metric]}]
  (when extract-data
    (let [{:keys [data]} extract-data
          get-value (if has-error-bounds?
                      (fn [[_ v]] (when v (:value v)))
                      (fn [[_ v]] v))
          multi-impl? (some? impl-axis)
          valid-data (filterv (fn [datum]
                                (let [[coord _] datum
                                      value (get-value datum)]
                                  (and (some? value)
                                       (map? coord)
                                       (contains? coord axis)
                                       (or (not multi-impl?)
                                           (contains? coord impl-axis)))))
                              data)]
      (when (seq valid-data)
        (let [raw-values (mapv get-value valid-data)
              {:keys [^double total-scale unit]}
              (compute-si-scaling metric raw-values)
              points (mapv (fn [[coord v]]
                             (let [y-val (double
                                          (if has-error-bounds?
                                            (:value v)
                                            v))
                                   x-val (double (get coord axis))]
                               (cond-> {"x" x-val
                                        "y" (* y-val total-scale)}
                                 has-error-bounds?
                                 (assoc "yLower" (* (double (:lower v))
                                                    total-scale)
                                        "yUpper" (* (double (:upper v))
                                                    total-scale))
                                 multi-impl?
                                 (assoc "impl" (name (get coord impl-axis))))))
                           valid-data)
              x-vals (mapv #(get % "x") points)]
          {:points points
           :total-scale total-scale
           :unit unit
           :x-vals x-vals
           :valid-data valid-data})))))

(defn prepare-regression-fit-lines
  "Generate fit line points for plotting.
  For single-impl mode, models is a seq of model maps.
  For multi-impl mode, by-impl is a map of impl-key -> {:models [...] :best-fit id}.
  Returns vector of point maps with x, y, and model or impl key."
  [{:keys [x-vals ^double total-scale]} {:keys [models by-impl impl-keys]}]
  (when (seq x-vals)
    (let [x-min (double (reduce min x-vals))
          x-max (double (reduce max x-vals))
          x-range (range x-min (+ x-max 1) (/ (- x-max x-min) 50))]
      (if by-impl
        ;; Multi-impl: one best-fit line per implementation
        (vec
         (mapcat
          (fn [impl-key]
            (let [{:keys [models best-fit]} (get by-impl impl-key)
                  best-model (first (filter #(= (:id %) best-fit) models))]
              (when best-model
                (let [mfn (:predict-fn best-model)]
                  (mapv (fn [x]
                          {"x" x
                           "y" (* (double (mfn x)) total-scale)
                           "impl" (name impl-key)})
                        x-range)))))
          impl-keys))
        ;; Single-impl: lines for all models to plot
        (vec
         (mapcat
          (fn [model]
            (let [mfn (:predict-fn model)]
              (mapv (fn [x]
                      {"x" x
                       "y" (* (double (mfn x)) total-scale)
                       "model" (:label model)})
                    x-range)))
          models))))))

(defn prepare-regression-residuals
  "Compute residual points for plotting.
  Returns vector of point maps with x, residual, and model or impl key."
  [{:keys [valid-data ^double total-scale]}
   {:keys [axis impl-axis has-error-bounds?
           models by-impl impl-keys]}]
  (let [get-value (if has-error-bounds?
                    (fn [[_ v]] (when v (:value v)))
                    (fn [[_ v]] v))]
    (if by-impl
      ;; Multi-impl mode
      (vec
       (mapcat
        (fn [impl-key]
          (let [{:keys [models best-fit]} (get by-impl impl-key)
                best-model (first (filter #(= (:id %) best-fit) models))]
            (when best-model
              (let [mfn (:predict-fn best-model)]
                (keep (fn [[coord v]]
                        (when (= (get coord impl-axis) impl-key)
                          (let [y-val (double (get-value [coord v]))
                                x-val (double (get coord axis))
                                predicted (double (mfn x-val))]
                            {"x" x-val
                             "residual" (* (- y-val predicted) total-scale)
                             "impl" (name impl-key)})))
                      valid-data)))))
        impl-keys))
      ;; Single-impl mode
      (vec
       (mapcat
        (fn [model]
          (let [mfn (:predict-fn model)]
            (mapv (fn [[coord v]]
                    (let [y-val (double (get-value [coord v]))
                          x-val (double (get coord axis))
                          predicted (double (mfn x-val))]
                      {"x" x-val
                       "residual" (* (- y-val predicted) total-scale)
                       "model" (:label model)}))
                  valid-data)))
        models)))))

;;; Log-Log Regression view helpers
;;
;; Functions for preparing log-log regression data for chart display.
;; Log-log plots show log(time) vs log(n), where the slope indicates
;; complexity class directly (slope ≈ 1 for O(n), slope ≈ 2 for O(n²)).

(defn prepare-log-log-points
  "Prepare log-log transformed data points for scatter plot.
  Returns {:points [...] :axis-name string} or nil.

  Points have keys: x (log(n)), y (log(time)), and optionally
  yLower, yUpper for log-transformed error bounds.
  For multi-impl mode, points also have :impl key.

  The log-log-data comes from the :regressions map of a
  :criterium/domain-log-log-regression result."
  [log-log-data {:keys [axis impl-axis]}]
  (when log-log-data
    (let [multi-impl? (contains? log-log-data :by-impl)]
      (if multi-impl?
        ;; Multi-implementation mode
        (let [by-impl (:by-impl log-log-data)
              impl-keys (keys by-impl)
              all-points
              (vec
               (mapcat
                (fn [impl-key]
                  (let [{:keys [log-xs log-ys log-lowers log-uppers]}
                        (get by-impl impl-key)]
                    (when (and log-xs log-ys)
                      (map-indexed
                       (fn [i log-x]
                         (let [log-y (nth log-ys i)]
                           (cond-> {"x" log-x
                                    "y" log-y
                                    "impl" (name impl-key)}
                             (and log-lowers (nth log-lowers i nil))
                             (assoc "yLower" (nth log-lowers i))
                             (and log-uppers (nth log-uppers i nil))
                             (assoc "yUpper" (nth log-uppers i)))))
                       log-xs))))
                impl-keys))]
          (when (seq all-points)
            {:points all-points
             :axis-name (name axis)
             :has-error-bounds? (boolean
                                 (some #(contains? % "yLower") all-points))}))
        ;; Single implementation mode
        (let [{:keys [log-xs log-ys log-lowers log-uppers]} log-log-data]
          (when (and log-xs log-ys)
            (let [points (vec
                          (map-indexed
                           (fn [i log-x]
                             (let [log-y (nth log-ys i)]
                               (cond-> {"x" log-x
                                        "y" log-y}
                                 (and log-lowers (nth log-lowers i nil))
                                 (assoc "yLower" (nth log-lowers i))
                                 (and log-uppers (nth log-uppers i nil))
                                 (assoc "yUpper" (nth log-uppers i)))))
                           log-xs))]
              {:points points
               :axis-name (name axis)
               :has-error-bounds? (boolean
                                   (some #(contains? % "yLower") points))})))))))

(defn prepare-log-log-fit-line
  "Generate fit line points for log-log plot.
  Returns vector of point maps with x, y, and optionally impl key.

  Uses the pre-computed slope and intercept: y = slope * x + intercept
  where x = log(n), y = log(time)."
  [log-log-data {:keys [axis impl-axis]}]
  (when log-log-data
    (let [multi-impl? (contains? log-log-data :by-impl)]
      (if multi-impl?
        ;; Multi-implementation mode: one line per impl
        (let [by-impl (:by-impl log-log-data)]
          (vec
           (mapcat
            (fn [[impl-key {:keys [slope intercept log-xs]}]]
              (when (and slope intercept log-xs (seq log-xs))
                (let [x-min (reduce min log-xs)
                      x-max (reduce max log-xs)
                      x-range (range x-min (+ x-max 0.1) (/ (- x-max x-min) 50))]
                  (mapv (fn [x]
                          {"x" x
                           "y" (+ (* (double slope) x) (double intercept))
                           "impl" (name impl-key)})
                        x-range))))
            by-impl)))
        ;; Single implementation mode
        (let [{:keys [slope intercept log-xs]} log-log-data]
          (when (and slope intercept log-xs (seq log-xs))
            (let [x-min (reduce min log-xs)
                  x-max (reduce max log-xs)
                  x-range (range x-min (+ x-max 0.1) (/ (- x-max x-min) 50))]
              (vec
               (mapv (fn [x]
                       {"x" x
                        "y" (+ (* (double slope) x) (double intercept))})
                     x-range)))))))))

(defn prepare-log-log-residuals
  "Compute residual points for log-log plot.
  Returns vector of point maps with x (log(n)), residual, and optionally impl.

  Residuals are in log space: residual = log(y) - (slope * log(x) + intercept)"
  [log-log-data {:keys [axis impl-axis]}]
  (when log-log-data
    (let [multi-impl? (contains? log-log-data :by-impl)]
      (if multi-impl?
        ;; Multi-implementation mode
        (let [by-impl (:by-impl log-log-data)]
          (vec
           (mapcat
            (fn [[impl-key {:keys [log-xs residuals]}]]
              (when (and log-xs residuals)
                (map-indexed
                 (fn [i log-x]
                   {"x" log-x
                    "residual" (nth residuals i)
                    "impl" (name impl-key)})
                 log-xs)))
            by-impl)))
        ;; Single implementation mode
        (let [{:keys [log-xs residuals]} log-log-data]
          (when (and log-xs residuals)
            (vec
             (map-indexed
              (fn [i log-x]
                {"x" log-x
                 "residual" (nth residuals i)})
              log-xs))))))))

(defn format-log-log-slope
  "Format log-log slope as complexity class estimate.
  Returns string like 'O(n^1.02)' or 'O(n)' for integer slopes."
  [^double slope]
  (let [rounded (Math/round slope)]
    (if (< (Math/abs (- slope rounded)) 0.05)
      ;; Close to integer - use simplified form
      (case rounded
        0 "O(1)"
        1 "O(n)"
        2 "O(n²)"
        3 "O(n³)"
        (format "O(n^%d)" rounded))
      ;; Show decimal
      (format "O(n^%.2f)" slope))))

;;; Allocation view helpers

(defn format-call-site
  "Format a call site map for display.
  Returns a string like 'class.method (file:line)'.
  When call-method is nil (object-type fallback), shows just the class.
  When call-site is not useful but object-types provided, shows those."
  ([call-site]
   (format-call-site call-site nil))
  ([{:keys [call-class call-method call-file call-line]} object-types]
   (cond
     ;; Full call-site info available
     (and (seq call-class) (seq call-method))
     (str call-class "." call-method " (" call-file ":" call-line ")")

     ;; Object-type fallback (call-method is nil)
     (and (seq call-class) (nil? call-method))
     call-class

     ;; No useful info, show object-types if available
     (seq object-types)
     (str "[" (clojure.string/join ", " (sort object-types)) "]")

     ;; Last resort
     :else
     (str call-class "." call-method " (" call-file ":" call-line ")"))))

(defn format-object-types
  "Format object types set for display.
  Returns a comma-separated string of simplified type names."
  [object-types]
  (when (seq object-types)
    (->> object-types
         sort
         (clojure.string/join ", "))))

;;; ASCII Treemap rendering

(defn ascii-bar
  "Generate a bar of █ characters proportional to value/max-value.
  Returns a string of at most `width` characters."
  ^String [^double value ^double max-value ^long width]
  (if (or (<= max-value 0) (<= value 0))
    ""
    (let [ratio (min 1.0 (/ value max-value))
          bar-len (max 0 (long (Math/round (* ratio width))))]
      (apply str (repeat bar-len \█)))))

;;; Treemap box-drawing constants

(def ^:private ^String tree-branch
  "Branch connector for non-last children: ├── "
  "\u251C\u2500\u2500 ")

(def ^:private ^String tree-last
  "Last child connector: └── "
  "\u2514\u2500\u2500 ")

(def ^:private ^String tree-vertical
  "Vertical continuation line: │   "
  "\u2502   ")

(def ^:private ^String tree-space
  "Space continuation (after last child): 4 spaces"
  "    ")

(def ^:private ^String ellipsis
  "Ellipsis for truncated names: …"
  "\u2026")

(defn- render-treemap-node
  "Recursively render a treemap node.
  Returns a vector of lines."
  [node prefix is-last? max-value opts depth]
  (let [{:keys [^long bar-width ^long name-width depth-limit ^double min-percent]}
        opts
        depth (long depth)
        {:keys [name value children]} node
        is-leaf? (empty? children)
        connector (if is-last? tree-last tree-branch)
        continuation (if is-last? tree-space tree-vertical)
        node-name (if is-leaf? name (str name "/"))
        size-str (str "[" (format/format-value :memory value) "]")
        bar-str (when is-leaf?
                  (ascii-bar (double value) max-value bar-width))
        ;; Keep prefix + connector intact, only truncate the name if needed
        prefix-connector (str prefix connector)
        prefix-len (long (count prefix-connector))
        available-for-name (- name-width prefix-len)
        node-name-len (long (count node-name))
        ;; Truncate name from left if it exceeds available space
        ;; Ensure at least 2 chars available (for ellipsis + 1 char)
        truncated-name (cond
                         (<= available-for-name 1)
                         ellipsis

                         (> node-name-len available-for-name)
                         (str ellipsis (subs node-name (- node-name-len (dec available-for-name))))

                         :else
                         node-name)
        truncated-name-len (long (count truncated-name))
        ;; Pad to fill remaining space
        padding-needed (max 0 (- available-for-name truncated-name-len))
        padded-line (str prefix-connector truncated-name
                         (when (pos? padding-needed)
                           (apply str (repeat padding-needed \space))))
        line (str padded-line " " size-str
                  (when (seq bar-str) (str " " bar-str)))
        current-line [line]
        ;; Recurse into children if not at depth limit
        child-prefix (str prefix continuation)
        at-depth-limit? (and depth-limit (>= depth (long depth-limit)))]
    (if (or is-leaf? at-depth-limit?)
      current-line
      (let [root-value (double (:root-value opts))
            filtered-children (->> children
                                   (filter (fn [child]
                                             (>= (* 100.0 (/ (double (:value child))
                                                             root-value))
                                                 min-percent)))
                                   (sort-by :value >))
            num-children (long (count filtered-children))]
        (into current-line
              (mapcat (fn [idx child]
                        (render-treemap-node
                         child
                         child-prefix
                         (= (long idx) (dec num-children))
                         max-value
                         opts
                         (inc depth)))
                      (range)
                      filtered-children))))))

;;; Modal Analysis View helpers

(defn format-mode-location
  "Format a mode location for display.
  Applies metric scale and transforms, then formats with appropriate dimension."
  [location metric-config transforms]
  (let [{:keys [dimension scale]} metric-config
        loc (util/transform-sample-> location transforms)]
    (format/format-value dimension (* (double scale) loc))))

(defn for-each-multimodal-metric
  "Iterate over metrics with multimodal distributions (n-modes > 1).
  Calls (f {:metric-config mc :n-modes n :modes modes :transforms transforms})
  for each metric where n-modes > 1.
  Returns nil."
  [data-map modes-id f]
  (let [modes-id (or modes-id :modes)
        modes-map (get data-map modes-id)]
    (when modes-map
      (let [transforms (util/get-transforms data-map modes-id)
            metrics-defs (:metrics-defs modes-map)
            metric-configs (when metrics-defs
                             (metric/all-metric-configs metrics-defs))
            all-modes (:modes modes-map)]
        (doseq [metric-config metric-configs]
          (when-let [modes-data (get all-modes (:path metric-config))]
            (let [n-modes (:n-modes modes-data)
                  modes (:modes modes-data)]
              (when (and n-modes (> (long n-modes) 1))
                (f {:metric-config metric-config
                    :n-modes n-modes
                    :modes modes
                    :transforms transforms})))))))))

(defn render-ascii-treemap
  "Render an allocation treemap as an ASCII tree string.

  treemap-data should be a :criterium/allocation-treemap map with :root, :group-by, :size-by.

  Options:
    :bar-width   - max bar characters (default 20)
    :depth-limit - max nesting depth to display, nil = unlimited (default nil)
    :min-percent - hide nodes below this % of root total (default 1)
    :name-width  - column width for names (default 40)"
  ([treemap-data] (render-ascii-treemap treemap-data {}))
  ([treemap-data opts]
   (let [{:keys [root group-by size-by]} treemap-data
         {:keys [bar-width depth-limit min-percent name-width]
          :or {bar-width 20 min-percent 1.0 name-width 40}} opts
         ^long name-width name-width]
     (if (nil? root)
       ""
       (let [root-value (double (:value root))
             max-leaf-value (if (empty? (:children root))
                              root-value
                              (->> (tree-seq :children :children root)
                                   (remove :children)
                                   (map :value)
                                   (reduce max 0.0)
                                   double))
             size-by-str (case size-by
                           :bytes "bytes"
                           :count "count"
                           :bytes-per-allocation "bytes/alloc"
                           (name (or size-by :bytes)))
             group-by-str (case group-by
                            :class→line→type "class→line→type"
                            :type→class→line "type→class→line"
                            (name (or group-by :class→line→type)))
             header (str "Allocation Treemap (by " size-by-str ", " group-by-str ")")
             root-name (str (:name root) "/")
             root-size (str "[" (format/format-value :memory root-value) "]")
             ;; Format root line with same fixed-width treatment as children
             root-name-len (long (count root-name))
             root-line (let [padded (cond
                                      (< root-name-len name-width)
                                      (str root-name
                                           (apply str (repeat (- name-width root-name-len) \space)))

                                      (> root-name-len name-width)
                                      (str ellipsis (subs root-name (- root-name-len (dec name-width))))

                                      :else
                                      root-name)]
                         (str padded " " root-size))
             render-opts {:bar-width bar-width
                          :depth-limit depth-limit
                          :min-percent (double min-percent)
                          :name-width name-width
                          :root-value root-value}
             children (:children root)
             filtered-children (->> children
                                    (filter (fn [child]
                                              (>= (* 100.0 (/ (double (:value child))
                                                              root-value))
                                                  (double min-percent))))
                                    (sort-by :value >))
             num-children (long (count filtered-children))
             child-lines (mapcat (fn [^long idx child]
                                   (render-treemap-node
                                    child
                                    ""
                                    (= idx (dec num-children))
                                    max-leaf-value
                                    render-opts
                                    1))
                                 (range)
                                 filtered-children)]
         (str/join "\n" (into [header root-line] child-lines)))))))

