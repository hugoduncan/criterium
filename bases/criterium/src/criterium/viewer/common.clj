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
  (let [base-scale                 (metric-path->base-scale metric-path)
        dimension                  (metric-path->dimension metric-path)
        base-values                (when (seq values)
                                     (map #(* (double %) base-scale) values))
        representative-value       (when (seq base-values)
                                     (/ (double (reduce + base-values))
                                        (count base-values)))
        [^double si-scale si-unit] (if (and dimension representative-value)
                                     (format/scale
                                      dimension
                                      representative-value)
                                     [1 ""])]
    {:base-scale  base-scale
     :si-scale    si-scale
     :total-scale (* base-scale si-scale)
     :unit        si-unit}))

(defn format-value-with-unit
  "Format a value from domain-extract as a string with SI units."
  [value metric-path]
  (when (some? value)
    (let [base-value (* (double value) (metric-path->base-scale metric-path))
          dimension  (metric-path->dimension metric-path)]
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
          multi-impl?   (> (count (:implementations extract)) 1)
          metrics       (:metrics extract)
          metric-ids    (sort (keys metrics))

          ;; Collect all data points with their full coords
          all-data (for [[metric-id {:keys [metric data]}] metrics
                         [coord value]                     data]
                     {:metric-id metric-id
                      :metric    metric
                      :coord     coord
                      :value     (get-numeric-value value)})

          ;; Collect all coordinates to detect uniform axes
          all-coords (map :coord all-data)

          ;; Find axes with uniform values across all coords (e.g., :impl
          ;; :default)
          ;; Only use uniform axes if stripping them leaves at least one key
          uniform-axes          (detect-uniform-axes all-coords)
          first-coord           (first all-coords)
          remaining-after-strip (when (and (map? first-coord)
                                           (seq uniform-axes))
                                  (count
                                   (apply dissoc first-coord uniform-axes)))
          use-uniform-axes?     (and (seq uniform-axes)
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
          row-keys        (sort-row-keys raw-row-keys single-key-info)
          coord-header    (coord-column-header single-key-info)

          impl-vals (when multi-impl?
                      (->> all-data
                           (keep #(get (:coord %) impl-axis-key))
                           distinct
                           (sort-by str)))

          ;; Build column specs: [{:metric-id :impl (optional)}...]
          col-specs (if multi-impl?
                      (for [metric-id metric-ids
                            impl      impl-vals]
                        {:metric-id metric-id :impl impl})
                      (for [metric-id metric-ids]
                        {:metric-id metric-id}))

          ;; Build lookup: {[row-key metric-id impl?] -> value}
          lookup (reduce (fn [acc {:keys [metric-id coord value]}]
                           (let [row-key    (row-key-fn coord)
                                 impl-val   (when
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
                        col-values  (for [row-key row-keys
                                          :let    [lk (if multi-impl?
                                                        [row-key metric-id impl]
                                                        [row-key metric-id])
                                                   v (get lookup lk)]
                                          :when   (some? v)]
                                      v)]
                    [col-spec (compute-si-scaling metric-path col-values)])))
           col-specs)

          ;; Build column headers
          col-headers
          (mapv (fn [col-spec]
                  (let [{:keys [metric-id impl]}
                        col-spec
                        {:keys [unit]} (get col-scales col-spec)
                        metric-name    (name metric-id)
                        header-base    (if (seq unit)
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
                           (let [{:keys [metric-id impl]}
                                 col-spec
                                 lk        (if multi-impl?
                                             [row-key metric-id impl]
                                             [row-key metric-id])
                                 raw-value (get lookup lk)
                                 {:keys [^double total-scale]}
                                 (get col-scales col-spec)
                                 header    (nth col-headers idx)]
                             [header (when raw-value
                                       (format
                                        "%.3g"
                                        (* (double raw-value) total-scale)))]))
                         col-specs)))
                row-keys)]

      {:heading      "Domain Extract"
       :coord-header coord-header
       :col-headers  col-headers
       :rows         table-rows})))

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
      (let [all-entries     (mapcat
                             (fn [[axis-val entries]]
                               (mapv #(assoc % :axis-val axis-val) entries))
                             data)
            raw-row-keys    (->> all-entries
                                 (mapv
                                  #(extract-row-key (:coord %) axis)) distinct)
            single-key-info (single-key-coord-info raw-row-keys)
            row-keys        (sort-row-keys
                             raw-row-keys
                             single-key-info)
            coord-header    (coord-column-header single-key-info)
            lookup          (reduce
                             (fn [acc {:keys [coord value axis-val]}]
                               (let [row-key (extract-row-key coord axis)]
                                 (assoc-in acc [row-key axis-val] value)))
                             {}
                             all-entries)
            all-values      (keep :value all-entries)
            {:keys [^double total-scale unit]}
            (compute-si-scaling metric all-values)
            heading         (str
                             "Domain Comparison by "
                             (name axis) ": " (pr-str metric)
                             (when (seq unit) (str " (" unit ")")))
            col-headers     (mapv str axis-vals)
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
        {:heading      heading
         :coord-header coord-header
         :col-headers  col-headers
         :rows         table-rows}))))

(defn- build-factor-table-single-metric
  "Build a table spec for single-metric factor display (with implementations)."
  [axis metric implementations data]
  (let [data-keys (set (keys data))
        missing   (remove data-keys implementations)]
    (when (seq missing)
      (throw (ex-info "Domain :implementations do not match comparison data keys"
                      {:implementations implementations
                       :data-keys       (keys data)
                       :missing         missing})))
    (let [baseline-impl   (first implementations)
          other-impls     (rest implementations)
          all-row-keys    (->> (vals data)
                               (mapcat (fn [entries]
                                         (map #(extract-row-key (:coord %) axis) entries)))
                               distinct)
          single-key-info (single-key-coord-info all-row-keys)
          row-keys        (sort-row-keys all-row-keys single-key-info)
          coord-header    (coord-column-header single-key-info)
          lookup          (reduce (fn [acc [impl-val entries]]
                                    (reduce (fn [acc2 {:keys [coord value]}]
                                              (let [row-key (extract-row-key coord axis)]
                                                (assoc-in acc2 [impl-val row-key] value)))
                                            acc
                                            entries))
                                  {}
                                  data)
          col-specs       (vec (cons {:type :baseline :impl baseline-impl}
                                     (map (fn [impl] {:type :factor :impl impl})
                                          other-impls)))
          col-headers     (mapv (fn [{:keys [type impl]}]
                                  (if (= type :baseline)
                                    (str (name impl))
                                    (str (name impl) " ×")))
                                col-specs)
          table-rows
          (mapv
           (fn [row-key]
             (into {coord-header (format-row-key-value row-key single-key-info)}
                   (map (fn [{:keys [type impl]} header]
                          (let [value          (double
                                                (get-in lookup [impl row-key]))
                                baseline-value (double
                                                (get-in
                                                 lookup
                                                 [baseline-impl row-key]))]
                            [header
                             (if (= type :baseline)
                               (format-value-with-unit value metric)
                               (cond
                                 (nil? value)           "-"
                                 (nil? baseline-value)  "-"
                                 (zero? baseline-value) "-"
                                 :else
                                 (format
                                  "%.2f"
                                  (double (/ value baseline-value)))))]))
                        col-specs col-headers)))
           row-keys)]
      {:heading      (str "Domain Comparison by " (name axis) ": " (pr-str metric))
       :coord-header coord-header
       :col-headers  col-headers
       :rows         table-rows})))

(defn- build-factor-table-multi-metric
  "Build a table spec for multi-metric factor display (with implementations)."
  [axis implementations metrics]
  (let [baseline-impl   (first implementations)
        other-impls     (rest implementations)
        metric-ids      (sort (keys metrics))
        all-row-keys    (->> (vals metrics)
                             (mapcat (fn [{:keys [data]}]
                                       (mapcat (fn [[_impl entries]]
                                                 (map #(extract-row-key (:coord %) axis) entries))
                                               data)))
                             distinct)
        single-key-info (single-key-coord-info all-row-keys)
        row-keys        (sort-row-keys all-row-keys single-key-info)
        coord-header    (coord-column-header single-key-info)
        lookup          (reduce (fn [acc [metric-id {:keys [data]}]]
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
        col-specs       (vec (mapcat (fn [metric-id]
                                       (let [metric-path (get-in metrics [metric-id :metric])]
                                         (cons {:type        :baseline
                                                :metric-id   metric-id
                                                :metric-path metric-path
                                                :impl        baseline-impl}
                                               (map (fn [impl]
                                                      {:type        :factor
                                                       :metric-id   metric-id
                                                       :metric-path metric-path
                                                       :impl        impl})
                                                    other-impls))))
                                     metric-ids))
        col-headers     (mapv (fn [{:keys [type metric-id impl]}]
                                (if (= type :baseline)
                                  (str (name impl) " " (name metric-id))
                                  (str (name impl) " ×")))
                              col-specs)
        table-rows
        (mapv
         (fn [row-key]
           (into
            {coord-header (format-row-key-value row-key single-key-info)}
            (map
             (fn [{:keys [type metric-id metric-path impl]} header]
               (let [value          (double
                                     (get-in lookup [metric-id impl row-key]))
                     baseline-value (double
                                     (get-in
                                      lookup
                                      [metric-id baseline-impl row-key]))]
                 [header
                  (if (= type :baseline)
                    (format-value-with-unit value metric-path)
                    (cond
                      (nil? value)           "-"
                      (nil? baseline-value)  "-"
                      (zero? baseline-value) "-"
                      :else                  (format "%.2f" (/ value baseline-value))))]))
             col-specs col-headers)))
         row-keys)]
    {:heading      (str "Domain Comparison by " (name axis))
     :coord-header coord-header
     :col-headers  col-headers
     :rows         table-rows}))

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

;;; Domain grouped view helpers

(defn prepare-domain-grouped-table
  "Prepare domain-grouped data for table rendering.
  Returns {:heading string :rows [{:axis-value string :run-count int}...]}
  or nil if grouped is nil."
  [grouped]
  (when grouped
    (let [{:keys [axis data]} grouped]
      {:heading (str "Domain Grouped by: " (name axis))
       :rows    (mapv (fn [[axis-val sub-domain]]
                        {:axis-value (if (nil? axis-val)
                                       "<nil>"
                                       (str axis-val))
                         :run-count  (count (:runs sub-domain))})
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
    :or   {best-fit-marker "✓" plotted-marker "" tolerance 0.01}}]
  (when (seq models)
    (let [best-r-squared (->> models
                              (filter #(= (:id %) best-fit))
                              first
                              :r-squared
                              double)
          plotted-ids    (when best-r-squared
                           (->> models
                                (filter #(>= (double (:r-squared %))
                                             (* best-r-squared (- 1 tolerance))))
                                (map :id)
                                set))
          sorted-models  (sort-by :r-squared > models)]
      (mapv (fn [{:keys [id label equation-str r-squared]}]
              (let [plotted? (and plotted-ids (plotted-ids id))]
                {:model     label
                 :r-squared (format "%.4f" r-squared)
                 :equation  (or equation-str "")
                 :best-fit  (cond
                              (= id best-fit) best-fit-marker
                              plotted?        plotted-marker
                              :else           "")}))
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
          get-value      (if has-error-bounds?
                           (fn [[_ v]] (when v (:value v)))
                           (fn [[_ v]] v))
          multi-impl?    (some? impl-axis)
          valid-data     (filterv (fn [datum]
                                    (let [[coord _] datum
                                          value     (get-value datum)]
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
              points     (mapv (fn [[coord v]]
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
              x-vals     (mapv #(get % "x") points)]
          {:points      points
           :total-scale total-scale
           :unit        unit
           :x-vals      x-vals
           :valid-data  valid-data})))))

(defn prepare-regression-fit-lines
  "Generate fit line points for plotting.
  For single-impl mode, models is a seq of model maps.
  For multi-impl mode, by-impl is a map of impl-key -> {:models [...] :best-fit id}.
  Returns vector of point maps with x, y, and model or impl key."
  [{:keys [x-vals ^double total-scale]} {:keys [models by-impl impl-keys]}]
  (when (seq x-vals)
    (let [x-min   (double (reduce min x-vals))
          x-max   (double (reduce max x-vals))
          x-range (range x-min (+ x-max 1) (/ (- x-max x-min) 50))]
      (if by-impl
        ;; Multi-impl: one best-fit line per implementation
        (vec
         (mapcat
          (fn [impl-key]
            (let [{:keys [models best-fit]} (get by-impl impl-key)
                  best-model                (first (filter #(= (:id %) best-fit) models))]
              (when best-model
                (let [mfn (:predict-fn best-model)]
                  (mapv (fn [x]
                          {"x"    x
                           "y"    (* (double (mfn x)) total-scale)
                           "impl" (name impl-key)})
                        x-range)))))
          impl-keys))
        ;; Single-impl: lines for all models to plot
        (vec
         (mapcat
          (fn [model]
            (let [mfn (:predict-fn model)]
              (mapv (fn [x]
                      {"x"     x
                       "y"     (* (double (mfn x)) total-scale)
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
                best-model                (first (filter #(= (:id %) best-fit) models))]
            (when best-model
              (let [mfn (:predict-fn best-model)]
                (keep (fn [[coord v]]
                        (when (= (get coord impl-axis) impl-key)
                          (let [y-val     (double (get-value [coord v]))
                                x-val     (double (get coord axis))
                                predicted (double (mfn x-val))]
                            {"x"        x-val
                             "residual" (* (- y-val predicted) total-scale)
                             "impl"     (name impl-key)})))
                      valid-data)))))
        impl-keys))
      ;; Single-impl mode
      (vec
       (mapcat
        (fn [model]
          (let [mfn (:predict-fn model)]
            (mapv (fn [[coord v]]
                    (let [y-val     (double (get-value [coord v]))
                          x-val     (double (get coord axis))
                          predicted (double (mfn x-val))]
                      {"x"        x-val
                       "residual" (* (- y-val predicted) total-scale)
                       "model"    (:label model)}))
                  valid-data)))
        models)))))
