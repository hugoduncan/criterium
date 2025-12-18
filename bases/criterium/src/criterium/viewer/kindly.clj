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
  [_ view data-map]
  (kindly-heading "Samples")
  (kindly-vega-lite
   (charts/samples-vega-spec data-map view {:width chart-width
                                            :height chart-height})))

(defmethod view/histogram* :kindly
  [_ view data-map]
  (kindly-heading "Histogram")
  (kindly-vega-lite
   (charts/histogram-vega-spec data-map view {:width chart-width
                                              :height chart-height})))

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

(defmethod view/domain-extract* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when-let [table-data (viewer-common/prepare-domain-extract-table
                           extract {:header-sep "\n"})]
      (kindly-heading (:heading table-data))
      (kindly-table (:rows table-data)))))

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
                  single-key-info (viewer-common/single-key-coord-info all-row-keys)
                  row-keys (viewer-common/sort-row-keys all-row-keys single-key-info)
                  coord-header (viewer-common/coord-column-header single-key-info)
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
                  table-rows
                  (mapv
                   (fn [row-key]
                     (into
                      {coord-header (viewer-common/format-row-key-value
                                     row-key
                                     single-key-info)}
                      (map
                       (fn [{:keys [type metric-id metric-path impl]
                             :as _col-spec}
                            header]
                         (let [value (get-in
                                      lookup
                                      [metric-id impl row-key])
                               baseline-value (get-in
                                               lookup
                                               [metric-id
                                                baseline-impl
                                                row-key])]
                           [header
                            (if (= type :baseline)
                              (viewer-common/format-value-with-unit value metric-path)
                              (cond
                                (nil? value) "-"
                                (nil? baseline-value) "-"
                                (zero? baseline-value) "-"
                                :else (format
                                       "%.2f"
                                       (/ value
                                          baseline-value))))]))
                       col-specs col-headers)))
                   row-keys)]
              (kindly-heading (str "Domain Comparison by " (name axis)))
              (kindly-table table-rows))
            ;; Multi-metric mode without implementations - show all values
            (doseq [[_metric-id {:keys [metric data]}] metrics]
              (let [axis-vals (sort-by str (keys data))]
                (when (and (seq data) (some #(seq (second %)) data))
                  (let [all-entries
                        (mapcat (fn [[axis-val entries]]
                                  (map #(assoc % :axis-val axis-val) entries))
                                data)
                        raw-row-keys
                        (->> all-entries
                             (map (fn [{:keys [coord]}]
                                    (if (map? coord)
                                      (dissoc coord axis)
                                      coord)))
                             distinct)
                        single-key-info (viewer-common/single-key-coord-info raw-row-keys)
                        row-keys (viewer-common/sort-row-keys
                                  raw-row-keys
                                  single-key-info)
                        coord-header (viewer-common/coord-column-header single-key-info)
                        lookup (reduce
                                (fn [acc
                                     {:keys [coord value axis-val]}]
                                  (let [row-key (if (map? coord)
                                                  (dissoc coord axis)
                                                  coord)]
                                    (assoc-in
                                     acc
                                     [row-key axis-val]
                                     value)))
                                {}
                                all-entries)
                        all-values (keep :value all-entries)
                        {:keys [total-scale unit]}
                        (viewer-common/compute-si-scaling metric all-values)
                        heading (str
                                 "Domain Comparison by "
                                 (name axis) ": "
                                 (pr-str metric)
                                 (when (seq unit) (str " (" unit ")")))]
                    (kindly-heading heading)
                    (kindly-table
                     (mapv (fn [row-key]
                             (into {coord-header (viewer-common/format-row-key-value
                                                  row-key
                                                  single-key-info)}
                                   (map (fn [av]
                                          (let [raw-value (get-in
                                                           lookup
                                                           [row-key av])]
                                            [(str av)
                                             (when raw-value
                                               (format
                                                "%.3g"
                                                (* raw-value total-scale)))]))
                                        axis-vals)))
                           row-keys)))))))
          ;; Single-metric mode
          (if implementations
            ;; Single-metric with implementations - show factors
            (let [data-keys (set (keys data))
                  missing (remove data-keys implementations)
                  _ (when (seq missing)
                      (throw
                       (ex-info
                        "Domain :implementations do not match comparison data keys"
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
                  single-key-info (viewer-common/single-key-coord-info all-row-keys)
                  row-keys (viewer-common/sort-row-keys all-row-keys single-key-info)
                  coord-header (viewer-common/coord-column-header single-key-info)
                  ;; Build lookup: impl -> row-key -> value
                  lookup (reduce (fn [acc [impl-val entries]]
                                   (reduce
                                    (fn [acc2 {:keys [coord value]}]
                                      (let [row-key (if (map? coord)
                                                      (dissoc
                                                       coord
                                                       axis)
                                                      coord)]
                                        (assoc-in
                                         acc2
                                         [impl-val row-key]
                                         value)))
                                    acc
                                    entries))
                                 {}
                                 data)
                  ;; Build columns
                  col-specs (vec
                             (cons {:type :baseline :impl baseline-impl}
                                   (map
                                    (fn [impl] {:type :factor :impl impl})
                                    other-impls)))
                  col-headers (mapv (fn [{:keys [type impl]}]
                                      (if (= type :baseline)
                                        (str (name impl))
                                        (str (name impl) " ×")))
                                    col-specs)
                  ;; Build table rows
                  table-rows
                  (mapv
                   (fn [row-key]
                     (into {coord-header (viewer-common/format-row-key-value
                                          row-key
                                          single-key-info)}
                           (map (fn [{:keys [type impl]} header]
                                  (let [value (get-in
                                               lookup
                                               [impl row-key])
                                        baseline-value (get-in
                                                        lookup
                                                        [baseline-impl
                                                         row-key])]
                                    [header
                                     (if (= type :baseline)
                                       (viewer-common/format-value-with-unit
                                        value
                                        metric)
                                       (cond
                                         (nil? value) "-"
                                         (nil? baseline-value) "-"
                                         (zero? baseline-value) "-"
                                         :else
                                         (format
                                          "%.2f"
                                          (double
                                           (/ value baseline-value)))))]))
                                col-specs col-headers)))
                   row-keys)]
              (kindly-heading
               (str "Domain Comparison by " (name axis) ": " (pr-str metric)))
              (kindly-table table-rows))
            ;; Single-metric without implementations - absolute values
            (let [axis-vals (sort-by str (keys data))]
              (when (and (seq data) (some #(seq (second %)) data))
                (let [all-entries
                      (mapcat
                       (fn [[axis-val entries]]
                         (map #(assoc % :axis-val axis-val) entries))
                       data)
                      raw-row-keys
                      (->> all-entries
                           (map (fn [{:keys [coord]}]
                                  (if (map? coord)
                                    (dissoc coord axis)
                                    coord)))
                           distinct)
                      single-key-info (viewer-common/single-key-coord-info
                                       raw-row-keys)
                      row-keys (viewer-common/sort-row-keys
                                raw-row-keys
                                single-key-info)
                      coord-header (viewer-common/coord-column-header
                                    single-key-info)
                      lookup
                      (reduce (fn [acc {:keys [coord value axis-val]}]
                                (let [row-key (if (map? coord)
                                                (dissoc coord axis)
                                                coord)]
                                  (assoc-in acc [row-key axis-val] value)))
                              {}
                              all-entries)
                      all-values (keep :value all-entries)
                      {:keys [total-scale unit]} (viewer-common/compute-si-scaling
                                                  metric
                                                  all-values)
                      heading (str
                               "Domain Comparison by "
                               (name axis) ": "
                               (pr-str metric)
                               (when (seq unit)
                                 (str " (" unit ")")))]
                  (kindly-heading heading)
                  (kindly-table
                   (mapv (fn [row-key]
                           (into {coord-header (viewer-common/format-row-key-value
                                                row-key
                                                single-key-info)}
                                 (map (fn [av]
                                        (let [raw-value (get-in
                                                         lookup
                                                         [row-key av])]
                                          [(str av)
                                           (when raw-value
                                             (format
                                              "%.3g"
                                              (* raw-value total-scale)))]))
                                      axis-vals)))
                         row-keys)))))))))))

(defmethod view/domain-regression* :kindly
  [_ {:keys [regression-id extract-id tolerance]} data-map]
  (let [regression-id (or regression-id :regression)
        regression (data-map regression-id)
        tolerance (or tolerance 0.01)]
    (when regression
      (let [{:keys [axis regressions impl-axis implementations]}
            regression
            extract-id (or extract-id :extract)
            extract (data-map extract-id)
            multi-impl? (> (count implementations) 1)
            ;; Color palette for models (single-impl) or
            ;; implementations (multi-impl)
            model-colors ["orange" "green" "purple" "red" "brown"]]

        (if multi-impl?
          ;; Multi-implementation mode
          (doseq [[metric-id {:keys [metric by-impl with-error-bounds]}]
                  regressions]
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
                           sorted-models (sort-by
                                          :r-squared
                                          >
                                          models)]
                       (mapv (fn [{:keys [id label coefficients r-squared]}]
                               {:implementation (name impl-key)
                                :model label
                                :r-squared (format "%.4f" r-squared)
                                :equation (or (charts/regression-equation-str
                                               id
                                               coefficients)
                                              "")
                                :best-fit (if (= id best-fit) "✓" "")})
                             sorted-models)))
                   impl-keys))))
              ;; Vega-lite scatter plot with impl-colored points and fit curves
              (when metric-extract-data
                (let [{:keys [data]} metric-extract-data
                      get-value (if has-error-bounds?
                                  (fn [[_ v]]
                                    (when v (:value v)))
                                  (fn [[_ v]] v))
                      ;; Filter valid data
                      valid-data
                      (filterv (fn [datum]
                                 (let [[coord _] datum
                                       value (get-value datum)]
                                   (and (some? value)
                                        (map? coord)
                                        (contains? coord axis)
                                        (contains? coord impl-axis))))
                               data)
                      ;; Compute scaling
                      raw-values (mapv get-value valid-data)
                      {:keys [total-scale unit]} (viewer-common/compute-si-scaling
                                                  metric
                                                  raw-values)
                      ;; Create points with implementation info
                      points
                      (mapv (fn [[coord v]]
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
                      x-min (when (seq x-vals)
                              (apply min x-vals))
                      x-max (when (seq x-vals)
                              (apply max x-vals))
                      x-range (when (and x-min x-max)
                                (range
                                 x-min
                                 (+ x-max 1)
                                 (/ (- x-max x-min) 50)))
                      impl-line-pts
                      (when x-range
                        (mapcat
                         (fn [impl-key]
                           (let [{:keys [models best-fit]} (get
                                                            by-impl
                                                            impl-key)
                                 best-model (first
                                             (filterv
                                              #(= (:id %)
                                                  best-fit)
                                              models))]
                             (when best-model
                               (let [mfn (charts/regression-model-fn
                                          best-fit
                                          (:coefficients best-model))]
                                 (mapv (fn [x]
                                         {"x" x
                                          "y" (* (mfn x) total-scale)
                                          "impl" (name impl-key)})
                                       x-range)))))
                         impl-keys))
                      y-title (if (seq unit)
                                (str
                                 (pr-str metric)
                                 " (" unit ")")
                                (pr-str metric))
                      residual-title (if (seq unit)
                                       (str "Residual (" unit ")")
                                       "Residual")
                      ;; Build chart layers
                      point-layer
                      {:data {:values points}
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
                      line-layer
                      {:data {:values (vec impl-line-pts)}
                       :mark {:type "line" :strokeWidth 2}
                       :encoding {:x {:field "x" :type "quantitative"}
                                  :y {:field "y" :type "quantitative"}
                                  :color {:field "impl" :type "nominal"
                                          :legend nil}}}
                      error-layer
                      (when has-error-bounds?
                        {:data {:values points}
                         :mark {:type "rule" :strokeWidth 1.5}
                         :encoding {:x {:field "x" :type "quantitative"}
                                    :y {:field "yLower"
                                        :type "quantitative"}
                                    :y2 {:field "yUpper"}
                                    :color {:field "impl" :type "nominal"
                                            :legend nil}
                                    :opacity {:value 0.5}}})
                      layers
                      (cond-> [point-layer line-layer]
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
                                 (let [mfn (charts/regression-model-fn best-fit (:coefficients best-model))]
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
                            :equation (or (charts/regression-equation-str id coefficients) "")
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
                      {:keys [total-scale unit]} (viewer-common/compute-si-scaling metric raw-values)
                      model-fns (into {}
                                      (map (fn [m]
                                             [(:id m) (charts/regression-model-fn (:id m) (:coefficients m))])
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
