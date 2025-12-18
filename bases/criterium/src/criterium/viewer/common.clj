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
  [metric-path]
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
        base-values (when (seq values) (map #(* % base-scale) values))
        representative-value (when (seq base-values)
                               (/ (reduce + base-values) (count base-values)))
        [si-scale si-unit] (if (and dimension representative-value)
                             (format/scale dimension representative-value)
                             [1 ""])]
    {:base-scale base-scale
     :si-scale si-scale
     :total-scale (* base-scale si-scale)
     :unit si-unit}))

(defn format-value-with-unit
  "Format a value from domain-extract as a string with SI units."
  [value metric-path]
  (when (some? value)
    (let [base-value (* value (metric-path->base-scale metric-path))
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

(defn- get-numeric-value
  "Extract numeric value from plain value or error-bound format {:value v}."
  [v]
  (if (and (map? v) (contains? v :value))
    (:value v)
    v))

(defn prepare-domain-extract-table
  "Prepare domain-extract data for table rendering.
  Returns {:heading string :coord-header string :col-headers [string...]
           :rows [{col-header value...}...]} or nil if extract is nil.
  Options:
    :header-sep - separator between impl and metric in multi-impl headers (default \" \")"
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

          ;; Determine row key (coord minus impl axis for multi-impl)
          row-key-fn (if multi-impl?
                       (fn [coord] (dissoc coord impl-axis-key))
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
                                 impl-val (when multi-impl? (get coord impl-axis-key))
                                 lookup-key (if multi-impl?
                                              [row-key metric-id impl-val]
                                              [row-key metric-id])]
                             (assoc acc lookup-key value)))
                         {}
                         all-data)

          ;; Compute SI scale and unit per column
          col-scales
          (into {}
                (map (fn [col-spec]
                       (let [{:keys [metric-id impl]} col-spec
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
                  (let [{:keys [metric-id impl]} col-spec
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
                  (into {(keyword coord-header)
                         (format-row-key-value row-key single-key-info)}
                        (map-indexed
                         (fn [idx col-spec]
                           (let [{:keys [metric-id impl]} col-spec
                                 lk (if multi-impl?
                                      [row-key metric-id impl]
                                      [row-key metric-id])
                                 raw-value (get lookup lk)
                                 {:keys [total-scale]} (get col-scales col-spec)
                                 header (nth col-headers idx)]
                             [header (when raw-value
                                       (format "%.3g" (double (* raw-value total-scale))))]))
                         col-specs)))
                row-keys)]

      {:heading "Domain Extract"
       :coord-header coord-header
       :col-headers col-headers
       :rows table-rows})))

