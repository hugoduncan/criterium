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

(defmethod view/domain-extract* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when extract
      (let [{:keys [metric data]} extract
            has-error-bounds? (:with-error-bounds extract)
            ;; Helper to extract numeric value (handles both plain and error-bound formats)
            get-value (if has-error-bounds?
                        (fn [v] (when (map? v) (:value v)))
                        identity)
            ;; Convert to base units and compute SI scale
            base-scale (metric-path->base-scale metric)
            valid-values (keep (fn [[_ v]] (when-let [val (get-value v)]
                                             (* val base-scale)))
                               data)
            dimension (metric-path->dimension metric)
            representative-value (when (seq valid-values)
                                   (/ (reduce + valid-values) (count valid-values)))
            [si-scale si-unit] (if (and dimension representative-value)
                                 (format/scale dimension representative-value)
                                 [1 ""])
            total-scale (* base-scale si-scale)
            ;; Build header with unit
            value-header (if (seq si-unit)
                           (str "value (" si-unit ")")
                           "value")]
        (kindly-heading (str "Domain Extract: " (pr-str metric)))
        (kindly-table
         (mapv (fn [[coord v]]
                 (let [value (get-value v)]
                   {:coordinate (format-coord coord)
                    value-header (when value
                                   (format "%.3g" (* value total-scale)))}))
               data))))))

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
      (let [{:keys [axis metric data]} comparison
            axis-vals (sort-by str (keys data))]
        (when (and (seq data) (some #(seq (second %)) data))
          ;; Build table rows: one row per unique coord (minus axis)
          (let [all-entries (mapcat (fn [[axis-val entries]]
                                      (map #(assoc % :axis-val axis-val) entries))
                                    data)
                row-keys (->> all-entries
                              (map (fn [{:keys [coord]}]
                                     (if (map? coord)
                                       (dissoc coord axis)
                                       coord)))
                              distinct
                              (sort-by str))
                ;; Build lookup: row-key -> axis-val -> value
                lookup (reduce (fn [acc {:keys [coord value axis-val]}]
                                 (let [row-key (if (map? coord)
                                                 (dissoc coord axis)
                                                 coord)]
                                   (assoc-in acc [row-key axis-val] value)))
                               {}
                               all-entries)
                ;; Compute SI scale from all values
                base-scale (metric-path->base-scale metric)
                all-values (keep :value all-entries)
                valid-base-values (map #(* % base-scale) all-values)
                dimension (metric-path->dimension metric)
                representative-value (when (seq valid-base-values)
                                       (/ (reduce + valid-base-values)
                                          (count valid-base-values)))
                [si-scale si-unit] (if (and dimension representative-value)
                                     (format/scale dimension representative-value)
                                     [1 ""])
                ;; Build heading with unit
                heading (str "Domain Comparison by " (name axis) ": "
                             (pr-str metric)
                             (when (seq si-unit) (str " (" si-unit ")")))]
            (kindly-heading heading)
            (kindly-table
             (mapv (fn [row-key]
                     (into {:coordinate (format-coord row-key)}
                           (map (fn [av]
                                  (let [raw-value (get-in lookup [row-key av])]
                                    [(str av)
                                     (when raw-value
                                       (format "%.3g" (* (* raw-value base-scale)
                                                         si-scale)))]))
                                axis-vals)))
                   row-keys))))))))

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
  [_ {:keys [regression-id extract-id]} data-map]
  (let [regression-id (or regression-id :regression)
        regression (data-map regression-id)]
    (when regression
      (let [{:keys [axis metric models best-fit]} regression
            extract-id (or extract-id :extract)
            extract (data-map extract-id)
            has-error-bounds? (:with-error-bounds extract)]
        (kindly-heading (str "Domain Regression (axis: " (name axis)
                             ", metric: " (pr-str metric) ")"))
        ;; Table of models sorted by R²
        (when (seq models)
          (let [sorted-models (sort-by :r-squared > models)]
            (kindly-table
             (mapv (fn [{:keys [id label coefficients r-squared]}]
                     {:model label
                      :r-squared (format "%.4f" r-squared)
                      :equation (or (regression-equation-str id coefficients) "")
                      :best-fit (if (= id best-fit) "✓" "")})
                   sorted-models))))
        ;; Vega-lite scatter plot with best-fit curve and optional error bars
        (when (and extract (seq models) best-fit)
          (let [{:keys [data]} extract
                ;; Helper to extract value (handles both plain and error-bound formats)
                get-value (if has-error-bounds?
                            (fn [[_ v]] (when v (:value v)))
                            (fn [[_ v]] v))
                ;; Get data points (filtered for valid values)
                valid-data (filter (fn [datum]
                                     (let [[coord _] datum
                                           value (get-value datum)]
                                       (and (some? value)
                                            (map? coord)
                                            (contains? coord axis))))
                                   data)
                ;; Convert raw values to base units (e.g., ns -> s)
                base-scale (metric-path->base-scale metric)
                base-values (mapv (fn [datum] (* (get-value datum) base-scale)) valid-data)
                ;; Compute SI scale based on mean value
                dimension (metric-path->dimension metric)
                representative-value (/ (reduce + base-values) (count base-values))
                [si-scale si-unit] (if dimension
                                     (format/scale dimension representative-value)
                                     [1 ""])
                ;; Combined scale factor
                total-scale (* base-scale si-scale)
                ;; Get best-fit model function
                best-model (first (filter #(= (:id %) best-fit) models))
                model-fn (regression-model-fn best-fit (:coefficients best-model))
                ;; Create data points with SI scaling (including error bounds if available)
                points (mapv (fn [[coord v]]
                               (let [y-val (if has-error-bounds? (:value v) v)
                                     x-val (double (get coord axis))
                                     predicted (model-fn x-val)]
                                 (cond-> {"x" x-val
                                          "y" (* y-val total-scale)
                                          "predicted" (* predicted total-scale)
                                          "residual" (* (- y-val predicted) total-scale)
                                          "type" "actual"}
                                   has-error-bounds?
                                   (assoc "yLower" (* (:lower v) total-scale)
                                          "yUpper" (* (:upper v) total-scale)))))
                             valid-data)
                ;; Generate predicted line from best-fit model
                x-vals (mapv #(get % "x") points)
                x-min (apply min x-vals)
                x-max (apply max x-vals)
                ;; Generate points for curve (apply same scaling)
                x-range (range x-min (+ x-max 1) (/ (- x-max x-min) 50))
                line-pts (mapv (fn [x]
                                 {"x" x
                                  "y" (* (model-fn x) total-scale)
                                  "type" (:label best-model)})
                               x-range)
                ;; Build y-axis title with unit
                y-title (if (seq si-unit)
                          (str (pr-str metric) " (" si-unit ")")
                          (pr-str metric))
                residual-title (if (seq si-unit)
                                 (str "Residual (" si-unit ")")
                                 "Residual")
                ;; Build chart layers
                point-layer {:data {:values points}
                             :mark {:type "point" :size 60}
                             :encoding {:x {:field "x" :type "quantitative"
                                            :title (name axis)}
                                        :y {:field "y" :type "quantitative"
                                            :title y-title}
                                        :color {:value "steelblue"}}}
                line-layer {:data {:values line-pts}
                            :mark {:type "line" :strokeWidth 2}
                            :encoding {:x {:field "x" :type "quantitative"}
                                       :y {:field "y" :type "quantitative"}
                                       :color {:value "orange"}}}
                ;; Error bar layer (only if we have error bounds)
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
            ;; Main regression chart
            (kindly-vega-lite
             {:width chart-width
              :height chart-height
              :layer layers})
            ;; Residual plot - helps diagnose heteroscedasticity
            ;; Constant spread = homoscedastic (OLS optimal)
            ;; Fan-shaped spread = heteroscedastic (WLS may help)
            (kindly-heading "Residual Plot")
            (kindly-vega-lite
             {:width chart-width
              :height (/ chart-height 2)
              :layer [{:data {:values points}
                       :mark {:type "point" :size 60}
                       :encoding {:x {:field "predicted" :type "quantitative"
                                      :title "Fitted Value"}
                                  :y {:field "residual" :type "quantitative"
                                      :title residual-title}
                                  :color {:value "steelblue"}}}
                      ;; Zero reference line
                      {:data {:values [{"y" 0}]}
                       :mark {:type "rule" :strokeDash [4 4]}
                       :encoding {:y {:field "y" :type "quantitative"}
                                  :color {:value "gray"}}}]})))))))

;;; Noop implementations for views not applicable to Kindly output

(defmethod view/bootstrap-stats* :kindly [_ _ _])
(defmethod view/final-gc-warnings* :kindly [_ _ _])
(defmethod view/os* :kindly [_ _ _])
(defmethod view/runtime* :kindly [_ _ _])
