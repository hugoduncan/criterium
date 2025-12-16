(ns criterium.viewer.kindly
  "A viewer that outputs Kindly-annotated data structures for Clay notebooks.

  Uses an accumulator pattern where view functions append Kindly-annotated
  values to an atom. The `flush-viewer` multimethod returns a `kind/fragment`
  combining all accumulated values.

  No runtime dependency on scicloj/kindly - produces plain maps with
  appropriate `:kindly/kind` metadata."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]
   [criterium.viewer.common-charts :as charts]))

(defonce ^{:doc "Accumulator for Kindly-annotated values."}
  accumulated
  (atom []))

(defonce ^{:doc "Last flushed Kindly fragment for retrieval after bench completes."}
  last-fragment
  (atom nil))

(def ^:private chart-width
  "Width for Kindly vega-lite charts, sized for notebook display."
  700)

(def ^:private chart-height
  "Height for Kindly vega-lite charts, sized for notebook display."
  350)

(defn kindly-add
  "Add a value to the accumulator."
  [value]
  (swap! accumulated conj value)
  nil)

(defn kindly-heading
  "Add a markdown heading to the accumulator."
  [s]
  (kindly-add
   (with-meta
     [(str "**" s "**")]
     {:kindly/kind :kind/md})))

(defn kindly-table
  "Add a table to the accumulator."
  [data]
  (kindly-add
   (with-meta data {:kindly/kind :kind/table})))

(defn kindly-vega-lite
  "Add a Vega-Lite chart to the accumulator."
  [spec]
  (kindly-add
   (with-meta
     (assoc spec :$schema "https://vega.github.io/schema/vega-lite/v5.json")
     {:kindly/kind :kind/vega-lite})))

(defn flush
  "Return accumulated values as a kind/fragment and clear the accumulator.
  Also stores the fragment in `last-fragment` for retrieval after bench completes."
  []
  (let [[values _] (swap-vals! accumulated (constantly []))]
    (when (seq values)
      (let [fragment (with-meta values {:kindly/kind :kind/fragment})]
        (reset! last-fragment fragment)
        fragment))))

(defmethod view/flush-viewer :kindly [_]
  (flush))

(defmethod view/stats* :kindly
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map stats-id)]
    (when (seq metric-configs)
      (kindly-heading "Summary stats")
      (kindly-table
       (viewer-common/stats-map
        (util/stats stats-map)
        metric-configs
        transforms)))))

(defmethod view/quantiles* :kindly
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (kindly-heading "Quantiles")
    (kindly-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :kindly
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Outliers")
    (kindly-table
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/collect-plan* :kindly
  [_ _view data-map]
  (kindly-heading "Collect plan")
  (kindly-table
   (viewer-common/collect-plan-data data-map)))

(defmethod view/samples* :kindly
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        event-samples-id (:event-samples-id view quant-samples-id)
        outliers-analysis-id (:outliers-id view :outliers)

        quant-samples (data-map quant-samples-id)
        event-samples (data-map event-samples-id)
        outliers (data-map outliers-analysis-id)

        q-metrics-defs (-> (:metrics-defs quant-samples)
                           (metric/filter-metrics
                            (metric/type-pred :quantitative)))
        e-metrics-defs (-> (:metrics-defs event-samples)
                           (metric/filter-metrics
                            (metric/type-pred :event)))
        metric-configs (metric/all-metric-configs q-metrics-defs)
        event-metric->values (util/metric->values event-samples)
        e-metric-configs (->> (metric/all-metric-configs e-metrics-defs)
                              (filterv #(not-every? zero?
                                                    (get event-metric->values
                                                         (:path %)))))

        transforms (util/get-transforms data-map quant-samples-id)]
    (kindly-heading "Samples")
    (kindly-vega-lite
     {:data {:values [{}]}
      :encoding {:x {:field "index" :type "quantitative"}}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts/metric-layer
             (util/metric->values quant-samples)
             transforms
             (when outliers (util/outliers outliers))
             (have (first metric-configs)))]
           (mapcat
            #(charts/event-layer event-metric->values %)
            e-metrics-defs)))}]
       (mapv
        (fn [mc]
          {:width chart-width
           :height chart-height
           :layer [(charts/metric-layer
                    event-metric->values
                    transforms
                    nil mc)]})
        e-metric-configs))})))

(defmethod view/histogram* :kindly
  [_ {:keys [histogram-id samples-id stats-id]} data-map]
  (let [histogram-id (or histogram-id :histograms)
        stats-id (or stats-id :stats)
        quant-samples-id (or samples-id :samples)
        quant-samples (data-map quant-samples-id)
        stats (data-map stats-id)
        histograms-map (util/lookup-data data-map histogram-id)
        histograms (:histograms histograms-map)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        hist-transforms (util/get-transforms data-map histogram-id)
        stats-transforms (util/get-transforms data-map (:source-id stats))
        layer-num (volatile! 0)]
    (kindly-heading "Histogram")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:x "independent"
                        :y "independent"
                        :color "shared"}}
      :vconcat (mapv
                (fn [metric-config]
                  {:resolve {:scale {:x "shared" :y "independent"}}
                   :width chart-width
                   :height chart-height
                   :layer
                   (into
                    [(charts/metric-computed-histo-layer
                      hist-transforms
                      (histograms (:path metric-config))
                      metric-config
                      (vswap! layer-num unchecked-inc))]
                    (when stats
                      (->>
                       (charts/metric-sample-stats-layer
                        stats-transforms
                        (get-in (util/stats stats) (:path metric-config))
                        metric-config
                        (vswap! layer-num unchecked-inc)))))})
                metric-configs)})))

(defmethod view/sample-percentiles* :kindly
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quant-samples-id)]
    (kindly-heading "Percentiles")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defmethod view/metrics* :kindly
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Metrics")
    (kindly-table
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/event-stats* :kindly
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs (have (:metrics-defs event-stats-map))
        stats (viewer-common/event-stats
               metrics-defs
               (util/event-stats event-stats-map))]
    (when (seq stats)
      (kindly-heading "Event stats")
      (kindly-table stats))))

(defmethod view/outlier-significance* :kindly
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Outlier Significance")
    (kindly-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/sample-diffs* :kindly
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metric-configs (:metric-configs quant-samples)]
    (kindly-heading "Sample diffs")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

;;; Domain view implementations

(defn- format-coord
  "Format a coordinate for display."
  [coord]
  (if (map? coord)
    (into {} (map (fn [[k v]] [(name k) v])) coord)
    (name coord)))

(defn- metric-path->dimension
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

(defn- metric-path->base-scale
  "Return base scale factor to convert raw metric values to base units.
  Elapsed-time is stored in nanoseconds, so convert to seconds for scaling."
  [metric-path]
  (case (first metric-path)
    (:stats :log-stats)
    (case (second metric-path)
      :elapsed-time 1e-9 ; ns -> s
      1)
    1))

(defn- format-extract-value
  "Format a value from domain-extract for display.
  Applies base unit conversion (e.g., ns -> s for time)."
  [value metric-path]
  (when (some? value)
    (* value (metric-path->base-scale metric-path))))

(defn- format-extract-value-with-unit
  "Format a value from domain-extract as a string with SI units."
  [value metric-path]
  (when (some? value)
    (let [base-value (* value (metric-path->base-scale metric-path))
          dimension (metric-path->dimension metric-path)]
      (if dimension
        (format/format-value dimension base-value)
        (format "%g" (double base-value))))))

(defn- single-key-coord-info
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

(defn- sort-row-keys
  "Sort row-keys, using numeric sort when all values are numbers."
  [row-keys single-key-info]
  (if single-key-info
    (let [{:keys [values]} single-key-info
          all-numeric? (every? number? values)]
      (if all-numeric?
        (sort-by #(get % (:key single-key-info)) row-keys)
        (sort-by #(str (get % (:key single-key-info))) row-keys)))
    (sort-by str row-keys)))

(defn- format-row-key-value
  "Format a row-key for display, extracting the value for single-key maps."
  [row-key single-key-info]
  (if single-key-info
    (get row-key (:key single-key-info))
    (format-coord row-key)))

(defn- coord-column-header
  "Return the appropriate column header for coordinates."
  [single-key-info]
  (if single-key-info
    (name (:key single-key-info))
    "coordinate"))

(defn- compute-si-scaling
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

(defmethod view/domain-extract* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when extract
      (let [impl-axis-key (:impl-axis extract)
            multi-impl? (> (count (:implementations extract)) 1)
            metrics (:metrics extract)
            metric-ids (sort (keys metrics))

            ;; Helper to extract numeric value (handles both plain and error-bound formats)
            get-value (fn [v]
                        (if (and (map? v) (contains? v :value))
                          (:value v)
                          v))

            ;; Collect all data points with their full coords
            all-data (for [[metric-id {:keys [metric data]}] metrics
                           [coord value] data]
                       {:metric-id metric-id
                        :metric metric
                        :coord coord
                        :value (get-value value)})

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
                        (str (name impl) "\n" header-base)
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

        (kindly-heading "Domain Extract")
        (kindly-table table-rows)))))

(defmethod view/domain-grouped* :kindly
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when grouped
      (let [{:keys [axis data]} grouped]
        (kindly-heading (str "Domain Grouped by: " (name axis)))
        (kindly-table
         (mapv (fn [[axis-val sub-domain]]
                 {:axis-value (if (nil? axis-val) "<nil>" (str axis-val))
                  :run-count (count (:runs sub-domain))})
               (sort-by (comp str key) data)))))))

(defmethod view/domain-comparison* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when comparison
      (let [{:keys [axis metric metrics implementations data]} comparison]
        (if metrics
          ;; Multi-metric mode with factor display
          (if implementations
            ;; Multi-metric with implementations - show factors
            (let [baseline-impl (first implementations)
                  other-impls (rest implementations)
                  metric-ids (sort (keys metrics))
                  ;; Collect all row keys
                  all-row-keys (->> (vals metrics)
                                    (mapcat (fn [{:keys [data]}]
                                              (mapcat (fn [[_impl entries]]
                                                        (map (fn [{:keys [coord]}]
                                                               (if (map? coord)
                                                                 (dissoc coord axis)
                                                                 coord))
                                                             entries))
                                                      data)))
                                    distinct)
                  single-key-info (single-key-coord-info all-row-keys)
                  row-keys (sort-row-keys all-row-keys single-key-info)
                  coord-header (coord-column-header single-key-info)
                  ;; Build lookup: metric-id -> impl -> row-key -> value
                  lookup (reduce (fn [acc [metric-id {:keys [data]}]]
                                   (reduce (fn [acc2 [impl-val entries]]
                                             (reduce (fn [acc3 {:keys [coord value]}]
                                                       (let [row-key (if (map? coord)
                                                                       (dissoc coord axis)
                                                                       coord)]
                                                         (assoc-in acc3 [metric-id impl-val row-key] value)))
                                                     acc2
                                                     entries))
                                           acc
                                           data))
                                 {}
                                 metrics)
                  ;; Build columns
                  col-specs (vec (mapcat (fn [metric-id]
                                           (let [metric-path (get-in metrics [metric-id :metric])]
                                             (cons {:type :baseline
                                                    :metric-id metric-id
                                                    :metric-path metric-path
                                                    :impl baseline-impl}
                                                   (map (fn [impl]
                                                          {:type :factor
                                                           :metric-id metric-id
                                                           :metric-path metric-path
                                                           :impl impl})
                                                        other-impls))))
                                         metric-ids))
                  col-headers (mapv (fn [{:keys [type metric-id impl]}]
                                      (if (= type :baseline)
                                        (str (name impl) " " (name metric-id))
                                        (str (name impl) " ×")))
                                    col-specs)
                  ;; Build table rows
                  table-rows (mapv (fn [row-key]
                                     (into {coord-header (format-row-key-value row-key single-key-info)}
                                           (map (fn [{:keys [type metric-id metric-path impl] :as col-spec} header]
                                                  (let [value (get-in lookup [metric-id impl row-key])
                                                        baseline-value (get-in lookup [metric-id baseline-impl row-key])]
                                                    [header
                                                     (if (= type :baseline)
                                                       (format-extract-value-with-unit value metric-path)
                                                       (cond
                                                         (nil? value) "-"
                                                         (nil? baseline-value) "-"
                                                         (zero? baseline-value) "-"
                                                         :else (format "%.2f" (/ value baseline-value))))]))
                                                col-specs col-headers)))
                                   row-keys)]
              (kindly-heading (str "Domain Comparison by " (name axis)))
              (kindly-table table-rows))
            ;; Multi-metric mode without implementations - show all values
            (doseq [[metric-id {:keys [metric data]}] metrics]
              (let [axis-vals (sort-by str (keys data))]
                (when (and (seq data) (some #(seq (second %)) data))
                  (let [all-entries (mapcat (fn [[axis-val entries]]
                                              (map #(assoc % :axis-val axis-val) entries))
                                            data)
                        raw-row-keys (->> all-entries
                                          (map (fn [{:keys [coord]}]
                                                 (if (map? coord) (dissoc coord axis) coord)))
                                          distinct)
                        single-key-info (single-key-coord-info raw-row-keys)
                        row-keys (sort-row-keys raw-row-keys single-key-info)
                        coord-header (coord-column-header single-key-info)
                        lookup (reduce (fn [acc {:keys [coord value axis-val]}]
                                         (let [row-key (if (map? coord) (dissoc coord axis) coord)]
                                           (assoc-in acc [row-key axis-val] value)))
                                       {}
                                       all-entries)
                        all-values (keep :value all-entries)
                        {:keys [total-scale unit]} (compute-si-scaling metric all-values)
                        heading (str "Domain Comparison by " (name axis) ": "
                                     (pr-str metric)
                                     (when (seq unit) (str " (" unit ")")))]
                    (kindly-heading heading)
                    (kindly-table
                     (mapv (fn [row-key]
                             (into {coord-header (format-row-key-value row-key single-key-info)}
                                   (map (fn [av]
                                          (let [raw-value (get-in lookup [row-key av])]
                                            [(str av)
                                             (when raw-value
                                               (format "%.3g" (* raw-value total-scale)))]))
                                        axis-vals)))
                           row-keys)))))))
          ;; Single-metric mode
          (if implementations
            ;; Single-metric with implementations - show factors
            (let [data-keys (set (keys data))
                  missing (remove data-keys implementations)
                  _ (when (seq missing)
                      (throw (ex-info "Domain :implementations do not match comparison data keys"
                                      {:implementations implementations
                                       :data-keys (keys data)
                                       :missing missing})))
                  baseline-impl (first implementations)
                  other-impls (rest implementations)
                  ;; Collect all row keys
                  all-row-keys (->> (vals data)
                                    (mapcat (fn [entries]
                                              (map (fn [{:keys [coord]}]
                                                     (if (map? coord)
                                                       (dissoc coord axis)
                                                       coord))
                                                   entries)))
                                    distinct)
                  single-key-info (single-key-coord-info all-row-keys)
                  row-keys (sort-row-keys all-row-keys single-key-info)
                  coord-header (coord-column-header single-key-info)
                  ;; Build lookup: impl -> row-key -> value
                  lookup (reduce (fn [acc [impl-val entries]]
                                   (reduce (fn [acc2 {:keys [coord value]}]
                                             (let [row-key (if (map? coord)
                                                             (dissoc coord axis)
                                                             coord)]
                                               (assoc-in acc2 [impl-val row-key] value)))
                                           acc
                                           entries))
                                 {}
                                 data)
                  ;; Build columns
                  col-specs (vec (cons {:type :baseline :impl baseline-impl}
                                       (map (fn [impl] {:type :factor :impl impl})
                                            other-impls)))
                  col-headers (mapv (fn [{:keys [type impl]}]
                                      (if (= type :baseline)
                                        (str (name impl))
                                        (str (name impl) " ×")))
                                    col-specs)
                  ;; Build table rows
                  table-rows (mapv (fn [row-key]
                                     (into {coord-header (format-row-key-value row-key single-key-info)}
                                           (map (fn [{:keys [type impl]} header]
                                                  (let [value (get-in lookup [impl row-key])
                                                        baseline-value (get-in lookup [baseline-impl row-key])]
                                                    [header
                                                     (if (= type :baseline)
                                                       (format-extract-value-with-unit value metric)
                                                       (cond
                                                         (nil? value) "-"
                                                         (nil? baseline-value) "-"
                                                         (zero? baseline-value) "-"
                                                         :else (format "%.2f" (double (/ value baseline-value)))))]))
                                                col-specs col-headers)))
                                   row-keys)]
              (kindly-heading (str "Domain Comparison by " (name axis) ": " (pr-str metric)))
              (kindly-table table-rows))
            ;; Single-metric without implementations - absolute values
            (let [axis-vals (sort-by str (keys data))]
              (when (and (seq data) (some #(seq (second %)) data))
                (let [all-entries (mapcat (fn [[axis-val entries]]
                                            (map #(assoc % :axis-val axis-val) entries))
                                          data)
                      raw-row-keys (->> all-entries
                                        (map (fn [{:keys [coord]}]
                                               (if (map? coord)
                                                 (dissoc coord axis)
                                                 coord)))
                                        distinct)
                      single-key-info (single-key-coord-info raw-row-keys)
                      row-keys (sort-row-keys raw-row-keys single-key-info)
                      coord-header (coord-column-header single-key-info)
                      lookup (reduce (fn [acc {:keys [coord value axis-val]}]
                                       (let [row-key (if (map? coord)
                                                       (dissoc coord axis)
                                                       coord)]
                                         (assoc-in acc [row-key axis-val] value)))
                                     {}
                                     all-entries)
                      all-values (keep :value all-entries)
                      {:keys [total-scale unit]} (compute-si-scaling metric all-values)
                      heading (str "Domain Comparison by " (name axis) ": "
                                   (pr-str metric)
                                   (when (seq unit) (str " (" unit ")")))]
                  (kindly-heading heading)
                  (kindly-table
                   (mapv (fn [row-key]
                           (into {coord-header (format-row-key-value row-key single-key-info)}
                                 (map (fn [av]
                                        (let [raw-value (get-in lookup [row-key av])]
                                          [(str av)
                                           (when raw-value
                                             (format "%.3g" (* raw-value total-scale)))]))
                                      axis-vals)))
                         row-keys)))))))))))

(defn- regression-model-fn
  "Return a function that applies the model transform for plotting."
  [model-id {:keys [a b c]}]
  (case model-id
    :logarithmic (fn [x] (+ (* a (Math/log x)) b))
    :linear (fn [x] (+ (* a x) b))
    :n-log-n (fn [x] (+ (* a (* x (Math/log x))) b))
    :nlogn-linear (fn [x] (+ (* a (* x (Math/log x))) (* b x) c))
    :quadratic (fn [x] (+ (* a (* x x)) b))
    (fn [x] (+ (* a x) b))))

(defn- regression-equation-str
  "Format the fitted regression equation for a model."
  [model-id {:keys [a b c]}]
  (if c
    ;; Composite model: y = a*f1(x) + b*f2(x) + c
    (let [sign-b (if (neg? b) "-" "+")
          sign-c (if (neg? c) "-" "+")]
      (format "y = %.4g*n*log(n) %s %.4g*n %s %.4g"
              a sign-b (Math/abs ^double b) sign-c (Math/abs ^double c)))
    ;; Simple model: y = a*f(x) + b
    (when (and a b)
      (let [transform-str (case model-id
                            :logarithmic "log(n)"
                            :linear "n"
                            :n-log-n "n*log(n)"
                            :quadratic "n²"
                            "x")
            sign (if (neg? b) "-" "+")]
        (format "y = %.4g*%s %s %.4g" a transform-str sign (Math/abs ^double b))))))

(defmethod view/domain-regression* :kindly
  [_ {:keys [regression-id extract-id tolerance]} data-map]
  (let [regression-id (or regression-id :regression)
        regression (data-map regression-id)
        tolerance (or tolerance 0.01)]
    (when regression
      (let [{:keys [axis regressions impl-axis implementations]} regression
            extract-id (or extract-id :extract)
            extract (data-map extract-id)
            multi-impl? (> (count implementations) 1)
            ;; Color palette for models (single-impl) or implementations (multi-impl)
            model-colors ["orange" "green" "purple" "red" "brown"]
            impl-colors ["steelblue" "coral" "seagreen" "mediumpurple" "goldenrod"]]

        (if multi-impl?
          ;; Multi-implementation mode
          (doseq [[metric-id {:keys [metric by-impl with-error-bounds]}] regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  has-error-bounds? with-error-bounds
                  impl-keys (sort (keys by-impl))]
              (kindly-heading (str "Domain Regression (axis: " (name axis)
                                   ", metric: " (pr-str metric)
                                   ", by: " (name impl-axis) ")"))
;; Table of all models per implementation
              (when (seq by-impl)
                (kindly-table
                 (vec
                  (mapcat
                   (fn [impl-key]
                     (let [{:keys [models best-fit]} (get by-impl impl-key)
                           sorted-models (sort-by :r-squared > models)]
                       (mapv (fn [{:keys [id label coefficients r-squared]}]
                               {:implementation (name impl-key)
                                :model label
                                :r-squared (format "%.4f" r-squared)
                                :equation (or (regression-equation-str id coefficients) "")
                                :best-fit (if (= id best-fit) "✓" "")})
                             sorted-models)))
                   impl-keys))))
              ;; Vega-lite scatter plot with impl-colored points and fit curves
              (when metric-extract-data
                (let [{:keys [data]} metric-extract-data
                      get-value (if has-error-bounds?
                                  (fn [[_ v]] (when v (:value v)))
                                  (fn [[_ v]] v))
                      ;; Filter valid data
                      valid-data (filter (fn [datum]
                                           (let [[coord _] datum
                                                 value (get-value datum)]
                                             (and (some? value)
                                                  (map? coord)
                                                  (contains? coord axis)
                                                  (contains? coord impl-axis))))
                                         data)
;; Compute scaling
                      raw-values (mapv get-value valid-data)
                      {:keys [total-scale unit]} (compute-si-scaling metric raw-values)
                      ;; Create points with implementation info
                      points (mapv (fn [[coord v]]
                                     (let [y-val (if has-error-bounds? (:value v) v)
                                           x-val (double (get coord axis))
                                           impl-val (name (get coord impl-axis))]
                                       (cond-> {"x" x-val
                                                "y" (* y-val total-scale)
                                                "impl" impl-val}
                                         has-error-bounds?
                                         (assoc "yLower" (* (:lower v) total-scale)
                                                "yUpper" (* (:upper v) total-scale)))))
                                   valid-data)
                      ;; Generate fit lines per implementation
                      x-vals (mapv #(get % "x") points)
                      x-min (when (seq x-vals) (apply min x-vals))
                      x-max (when (seq x-vals) (apply max x-vals))
                      x-range (when (and x-min x-max)
                                (range x-min (+ x-max 1) (/ (- x-max x-min) 50)))
                      impl-line-pts
                      (when x-range
                        (mapcat
                         (fn [impl-key]
                           (let [{:keys [models best-fit]} (get by-impl impl-key)
                                 best-model (first (filter #(= (:id %) best-fit) models))]
                             (when best-model
                               (let [mfn (regression-model-fn best-fit (:coefficients best-model))]
                                 (mapv (fn [x]
                                         {"x" x
                                          "y" (* (mfn x) total-scale)
                                          "impl" (name impl-key)})
                                       x-range)))))
                         impl-keys))
                      y-title (if (seq unit)
                                (str (pr-str metric) " (" unit ")")
                                (pr-str metric))
                      residual-title (if (seq unit)
                                       (str "Residual (" unit ")")
                                       "Residual")
                      ;; Build chart layers
                      point-layer {:data {:values points}
                                   :mark {:type "point" :size 60}
                                   :encoding {:x {:field "x" :type "quantitative"
                                                  :title (name axis)}
                                              :y {:field "y" :type "quantitative"
                                                  :title y-title}
                                              :color {:field "impl" :type "nominal"
                                                      :legend {:title "Implementation"
                                                               :orient "none"
                                                               :legendX 10
                                                               :legendY 10}}}}
                      line-layer {:data {:values (vec impl-line-pts)}
                                  :mark {:type "line" :strokeWidth 2}
                                  :encoding {:x {:field "x" :type "quantitative"}
                                             :y {:field "y" :type "quantitative"}
                                             :color {:field "impl" :type "nominal"
                                                     :legend nil}}}
                      error-layer (when has-error-bounds?
                                    {:data {:values points}
                                     :mark {:type "rule" :strokeWidth 1.5}
                                     :encoding {:x {:field "x" :type "quantitative"}
                                                :y {:field "yLower" :type "quantitative"}
                                                :y2 {:field "yUpper"}
                                                :color {:field "impl" :type "nominal"
                                                        :legend nil}
                                                :opacity {:value 0.5}}})
                      layers (cond-> [point-layer line-layer]
                               has-error-bounds? (conj error-layer))]
                  (when (seq points)
                    (kindly-vega-lite
                     {:width chart-width
                      :height chart-height
                      :layer layers})
                    ;; Residual plot per implementation
                    (let [residual-pts
                          (mapcat
                           (fn [impl-key]
                             (let [{:keys [models best-fit]} (get by-impl impl-key)
                                   best-model (first (filter #(= (:id %) best-fit) models))]
                               (when best-model
                                 (let [mfn (regression-model-fn best-fit (:coefficients best-model))]
                                   (keep (fn [[coord v]]
                                           (when (= (get coord impl-axis) impl-key)
                                             (let [y-val (if has-error-bounds? (:value v) v)
                                                   x-val (double (get coord axis))
                                                   predicted (mfn x-val)]
                                               {"x" x-val
                                                "residual" (* (- y-val predicted) total-scale)
                                                "impl" (name impl-key)})))
                                         valid-data)))))
                           impl-keys)]
                      (kindly-heading "Residual Plot")
                      (kindly-vega-lite
                       {:width chart-width
                        :height (/ chart-height 2)
                        :layer [{:data {:values (vec residual-pts)}
                                 :mark {:type "point" :size 60}
                                 :encoding {:x {:field "x" :type "quantitative"
                                                :title (name axis)}
                                            :y {:field "residual" :type "quantitative"
                                                :title residual-title}
                                            :color {:field "impl" :type "nominal"
                                                    :legend {:title "Implementation"
                                                             :orient "none"
                                                             :legendX 10
                                                             :legendY 10}}}}
                                {:data {:values (vec residual-pts)}
                                 :transform [{:loess "residual"
                                              :on "x"
                                              :groupby ["impl"]
                                              :bandwidth 0.3}]
                                 :mark {:type "line" :strokeWidth 1}
                                 :encoding {:x {:field "x" :type "quantitative"}
                                            :y {:field "residual" :type "quantitative"}
                                            :color {:field "impl" :type "nominal"
                                                    :legend nil}
                                            :opacity {:value 0.4}}}
                                {:data {:values [{"y" 0}]}
                                 :mark {:type "rule" :strokeDash [4 4]}
                                 :encoding {:y {:field "y" :type "quantitative"}
                                            :color {:value "gray"}}}]})))))))

          ;; Single-implementation mode (original behavior)
          (doseq [[metric-id {:keys [metric models best-fit with-error-bounds]}] regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  has-error-bounds? with-error-bounds
                  best-r-squared (when best-fit
                                   (->> models
                                        (filter #(= (:id %) best-fit))
                                        first
                                        :r-squared))
                  models-to-plot (when best-r-squared
                                   (->> models
                                        (filter #(>= (:r-squared %)
                                                     (* best-r-squared (- 1 tolerance))))
                                        (sort-by :r-squared >)))]
              (kindly-heading (str "Domain Regression (axis: " (name axis)
                                   ", metric: " (pr-str metric) ")"))
              (when (seq models)
                (let [sorted-models (sort-by :r-squared > models)]
                  (kindly-table
                   (mapv (fn [{:keys [id label coefficients r-squared]}]
                           {:model label
                            :r-squared (format "%.4f" r-squared)
                            :equation (or (regression-equation-str id coefficients) "")
                            :best-fit (if (= id best-fit) "✓" "")})
                         sorted-models))))
              (when (and metric-extract-data (seq models-to-plot))
                (let [{:keys [data]} metric-extract-data
                      get-value (if has-error-bounds?
                                  (fn [[_ v]] (when v (:value v)))
                                  (fn [[_ v]] v))
                      valid-data (filter (fn [datum]
                                           (let [[coord _] datum
                                                 value (get-value datum)]
                                             (and (some? value)
                                                  (map? coord)
                                                  (contains? coord axis))))
                                         data)
                      raw-values (mapv get-value valid-data)
                      {:keys [total-scale unit]} (compute-si-scaling metric raw-values)
                      model-fns (into {}
                                      (map (fn [m]
                                             [(:id m) (regression-model-fn (:id m) (:coefficients m))])
                                           models-to-plot))
                      best-model-fn (get model-fns best-fit)
                      points (mapv (fn [[coord v]]
                                     (let [y-val (if has-error-bounds? (:value v) v)
                                           x-val (double (get coord axis))
                                           predicted (best-model-fn x-val)]
                                       (cond-> {"x" x-val
                                                "y" (* y-val total-scale)
                                                "predicted" (* predicted total-scale)
                                                "residual" (* (- y-val predicted) total-scale)
                                                "type" "actual"}
                                         has-error-bounds?
                                         (assoc "yLower" (* (:lower v) total-scale)
                                                "yUpper" (* (:upper v) total-scale)))))
                                   valid-data)
                      x-vals (mapv #(get % "x") points)
                      x-min (when (seq x-vals) (apply min x-vals))
                      x-max (when (seq x-vals) (apply max x-vals))
                      x-range (when (and x-min x-max)
                                (range x-min (+ x-max 1) (/ (- x-max x-min) 50)))
                      all-line-pts (when x-range
                                     (mapcat
                                      (fn [model color]
                                        (let [mfn (get model-fns (:id model))]
                                          (mapv (fn [x]
                                                  {"x" x
                                                   "y" (* (mfn x) total-scale)
                                                   "model" (:label model)})
                                                x-range)))
                                      models-to-plot
                                      (cycle model-colors)))
                      y-title (if (seq unit)
                                (str (pr-str metric) " (" unit ")")
                                (pr-str metric))
                      residual-title (if (seq unit)
                                       (str "Residual (" unit ")")
                                       "Residual")
                      point-layer {:data {:values points}
                                   :mark {:type "point" :size 60}
                                   :encoding {:x {:field "x" :type "quantitative"
                                                  :title (name axis)}
                                              :y {:field "y" :type "quantitative"
                                                  :title y-title}
                                              :color {:value "steelblue"}}}
                      line-layer {:data {:values (vec all-line-pts)}
                                  :mark {:type "line" :strokeWidth 2}
                                  :encoding {:x {:field "x" :type "quantitative"}
                                             :y {:field "y" :type "quantitative"}
                                             :color {:field "model" :type "nominal"
                                                     :legend {:title "Model"
                                                              :orient "none"
                                                              :legendX 10
                                                              :legendY 10}}}}
                      error-layer (when has-error-bounds?
                                    {:data {:values points}
                                     :mark {:type "rule" :strokeWidth 1.5}
                                     :encoding {:x {:field "x" :type "quantitative"}
                                                :y {:field "yLower" :type "quantitative"}
                                                :y2 {:field "yUpper"}
                                                :color {:value "steelblue"}
                                                :opacity {:value 0.5}}})
                      layers (cond-> [point-layer line-layer]
                               has-error-bounds? (conj error-layer))]
                  (when (seq points)
                    (kindly-vega-lite
                     {:width chart-width
                      :height chart-height
                      :layer layers})
                    (let [all-residual-pts (mapcat
                                            (fn [model]
                                              (let [mfn (get model-fns (:id model))]
                                                (mapv (fn [[coord v]]
                                                        (let [y-val (if has-error-bounds? (:value v) v)
                                                              x-val (double (get coord axis))
                                                              predicted (mfn x-val)]
                                                          {"x" x-val
                                                           "residual" (* (- y-val predicted) total-scale)
                                                           "model" (:label model)}))
                                                      valid-data)))
                                            models-to-plot)]
                      (kindly-heading "Residual Plot")
                      (kindly-vega-lite
                       {:width chart-width
                        :height (/ chart-height 2)
                        :layer [{:data {:values (vec all-residual-pts)}
                                 :mark {:type "point" :size 60}
                                 :encoding {:x {:field "x" :type "quantitative"
                                                :title (name axis)}
                                            :y {:field "residual" :type "quantitative"
                                                :title residual-title}
                                            :color {:field "model" :type "nominal"
                                                    :legend {:title "Model"
                                                             :orient "none"
                                                             :legendX 10
                                                             :legendY 10}}}}
                                {:data {:values (vec all-residual-pts)}
                                 :transform [{:loess "residual"
                                              :on "x"
                                              :groupby ["model"]
                                              :bandwidth 0.3}]
                                 :mark {:type "line" :strokeWidth 1}
                                 :encoding {:x {:field "x" :type "quantitative"}
                                            :y {:field "residual" :type "quantitative"}
                                            :color {:field "model" :type "nominal"
                                                    :legend nil}
                                            :opacity {:value 0.4}}}
                                {:data {:values [{"y" 0}]}
                                 :mark {:type "rule" :strokeDash [4 4]}
                                 :encoding {:y {:field "y" :type "quantitative"}
                                            :color {:value "gray"}}}]}))))))))))))

;;; Noop implementations for views not applicable to Kindly output

(defmethod view/bootstrap-stats* :kindly [_ _ _])
(defmethod view/final-gc-warnings* :kindly [_ _ _])
(defmethod view/os* :kindly [_ _ _])
(defmethod view/runtime* :kindly [_ _ _])
