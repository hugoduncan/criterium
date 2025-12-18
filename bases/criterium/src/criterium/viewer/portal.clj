(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]
   [criterium.viewer.common-charts :as charts]))

(defonce tapped (atom {:values '()}))

(defn submit
  "Tap target function.

  This allows criterium to control the order of tapped output.

  ```clojure
  (def submit (criterium.viewer.portal/submit #'portal.api/submit))
  (add-tap #'submit)`
  (remove-tap #'submit)
  ``"
  [portal-submit]
  (swap! tapped assoc :portal-submit portal-submit)
  (fn
    [value]
    (swap! tapped update :values conj value)))

(defn flush
  "Flush tapped output"
  []
  (tap> ::_)
  (loop [i 0]
    (when (not= ::_ (first (:values @tapped)))
      (when (< i 1000)
        (Thread/yield)
        (recur (unchecked-inc i)))))

  (let [[{:keys [portal-submit values]}] (swap-vals! tapped assoc :values '())]
    (doseq [value values]
      (when (not= ::_ value)
        (portal-submit value)))))

(defmethod view/flush-viewer :portal [_]
  (flush))

(defn portal-heading [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/hiccup})))

(defn portal-table [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/table})))

(defn portal-vega-lite [s]
  (tap> (with-meta
          (assoc s :$schema "https://vega.github.io/schema/vega-lite/v5.json")
          {:portal.viewer/default :portal.viewer/vega-lite})))

(defn heading [s]
  (portal-heading [:b s]))

(defmethod view/metrics* :portal
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (portal-table
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map stats-id)]
    (when (seq metric-configs)
      (heading "Summary stats")
      (portal-table
       (viewer-common/stats-map
        (util/stats stats-map)
        metric-configs
        transforms)))))

(defmethod view/event-stats* :portal
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs (have (:metrics-defs event-stats-map))
        stats (viewer-common/event-stats
               metrics-defs
               (util/event-stats event-stats-map))]
    (when (seq stats)
      (heading "Event stats")
      (portal-table stats))))

(defmethod view/quantiles* :portal
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (heading "Quantiles")
    (portal-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :portal
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outliers")
    (portal-table
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/outlier-significance* :portal
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outlier Significance")
    (portal-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/collect-plan* :portal
  [_ _view data-map]
  (heading "Collect plan")
  (portal-table
   (viewer-common/collect-plan-data data-map)))

(defmethod view/samples* :portal
  [_ view data-map]
  (heading "Samples")
  (portal-vega-lite
   (charts/samples-vega-spec data-map view {:height 800})))

(defmethod view/histogram* :portal
  [_ view data-map]
  (heading "Histogram")
  (portal-vega-lite
   (charts/histogram-vega-spec data-map view {:height 800})))

(defmethod view/sample-percentiles* :portal
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quant-samples-id)]
    (heading "Percentiles")
    (portal-vega-lite
     {:data {:values [{}]} ; for portal
      :height 800
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defmethod view/sample-diffs* :portal
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metric-configs (:metric-configs quant-samples)]
    (heading "Sample diffs")
    (portal-vega-lite
     {:data {:values [{}]} ; for portal
      :height 800
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

(defmethod view/bootstrap-stats* :portal [_ _ _])

(defmethod view/final-gc-warnings* :portal [_ _ _])

(defmethod view/os* :portal [_ _ _])

(defmethod view/runtime* :portal [_ _ _])

;;; Domain Views

(defmethod view/domain-extract* :portal
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (when-let [table-data (viewer-common/prepare-domain-extract-table
                           extract {:header-sep " "})]
      (heading (:heading table-data))
      (portal-table (:rows table-data)))))

(defmethod view/domain-grouped* :portal
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when grouped
      (let [{:keys [axis data]} grouped
            table-data (mapv (fn [[axis-val sub-domain]]
                               {:axis-value (if (nil? axis-val)
                                              "<nil>"
                                              (str axis-val))
                                :run-count (count (:runs sub-domain))})
                             (sort-by (comp str key) data))]
        (heading (str "Domain Grouped by: " (name axis)))
        (portal-table table-data)))))

(defmethod view/domain-comparison* :portal
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (when-let [tables (viewer-common/prepare-domain-comparison-tables comparison)]
      (doseq [{:keys [heading rows]} tables]
        (heading heading)
        (portal-table rows)))))

(defmethod view/domain-regression* :portal
  [_ {:keys [regression-id extract-id tolerance]} data-map]
  (let [regression-id (or regression-id :regression)
        regression (data-map regression-id)
        tolerance (or tolerance 0.01)
        chart-width 600
        chart-height 400]
    (when regression
      (let [{:keys [axis regressions impl-axis implementations]}
            regression
            extract-id (or extract-id :extract)
            extract (data-map extract-id)
            multi-impl? (> (count implementations) 1)]

        (if multi-impl?
          ;; Multi-implementation mode
          (doseq [[metric-id {:keys [metric by-impl with-error-bounds]}]
                  regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  has-error-bounds? with-error-bounds
                  impl-keys (sort (keys by-impl))]
              (heading (str "Domain Regression (axis: " (name axis)
                            ", metric: " (pr-str metric)
                            ", by: " (name impl-axis) ")"))
              ;; Table of all models per implementation
              (when (seq by-impl)
                (portal-table
                 (vec
                  (mapcat
                   (fn [impl-key]
                     (let [{:keys [models best-fit]} (get by-impl impl-key)
                           sorted-models (sort-by :r-squared > models)]
                       (mapv (fn [{:keys [id label coefficients r-squared]}]
                               {:implementation (name impl-key)
                                :model label
                                :r-squared (format "%.4f" r-squared)
                                :equation (or (charts/regression-equation-str
                                               id coefficients)
                                              "")
                                :best-fit (if (= id best-fit) "✓" "")})
                             sorted-models)))
                   impl-keys))))
              ;; Vega-lite scatter plot with impl-colored points and fit curves
              (when metric-extract-data
                (let [{:keys [data]} metric-extract-data
                      get-value (if has-error-bounds?
                                  (fn [[_ v]] (when v (:value v)))
                                  (fn [[_ v]] v))
                      valid-data
                      (filterv (fn [datum]
                                 (let [[coord _] datum
                                       value (get-value datum)]
                                   (and (some? value)
                                        (map? coord)
                                        (contains? coord axis)
                                        (contains? coord impl-axis))))
                               data)
                      raw-values (mapv get-value valid-data)
                      {:keys [total-scale unit]} (viewer-common/compute-si-scaling
                                                  metric raw-values)
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
                                 best-model (first (filterv #(= (:id %) best-fit) models))]
                             (when best-model
                               (let [mfn (charts/regression-model-fn
                                          best-fit (:coefficients best-model))]
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
                      point-layer
                      {:data {:values points}
                       :mark {:type "point" :size 60}
                       :encoding {:x {:field "x" :type "quantitative"
                                      :title (name axis)}
                                  :y {:field "y" :type "quantitative"
                                      :title y-title}
                                  :color {:field "impl" :type "nominal"
                                          :legend {:title "Implementation"}}}}
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
                                    :y {:field "yLower" :type "quantitative"}
                                    :y2 {:field "yUpper"}
                                    :color {:field "impl" :type "nominal"
                                            :legend nil}
                                    :opacity {:value 0.5}}})
                      layers (cond-> [point-layer line-layer]
                               has-error-bounds? (conj error-layer))]
                  (when (seq points)
                    (portal-vega-lite
                     {:width chart-width
                      :height chart-height
                      :layer layers})
                    ;; Residual plot per implementation
                    (let [residual-pts
                          (vec
                           (mapcat
                            (fn [impl-key]
                              (let [{:keys [models best-fit]} (get by-impl impl-key)
                                    best-model (first (filter #(= (:id %) best-fit) models))]
                                (when best-model
                                  (let [mfn (charts/regression-model-fn
                                             best-fit (:coefficients best-model))]
                                    (keep (fn [[coord v]]
                                            (when (= (get coord impl-axis) impl-key)
                                              (let [y-val (if has-error-bounds? (:value v) v)
                                                    x-val (double (get coord axis))
                                                    predicted (mfn x-val)]
                                                {"x" x-val
                                                 "residual" (* (- y-val predicted) total-scale)
                                                 "impl" (name impl-key)})))
                                          valid-data)))))
                            impl-keys))]
                      (heading "Residual Plot")
                      (portal-vega-lite
                       {:width chart-width
                        :height (/ chart-height 2)
                        :layer [{:data {:values residual-pts}
                                 :mark {:type "point" :size 60}
                                 :encoding {:x {:field "x" :type "quantitative"
                                                :title (name axis)}
                                            :y {:field "residual" :type "quantitative"
                                                :title residual-title}
                                            :color {:field "impl" :type "nominal"
                                                    :legend {:title "Implementation"}}}}
                                {:data {:values residual-pts}
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

          ;; Single-implementation mode
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
              (heading (str "Domain Regression (axis: " (name axis)
                            ", metric: " (pr-str metric) ")"))
              (when (seq models)
                (let [sorted-models (sort-by :r-squared > models)]
                  (portal-table
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
                                             [(:id m) (charts/regression-model-fn
                                                       (:id m) (:coefficients m))])
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
                                     (vec
                                      (mapcat
                                       (fn [model]
                                         (let [mfn (get model-fns (:id model))]
                                           (mapv (fn [x]
                                                   {"x" x
                                                    "y" (* (mfn x) total-scale)
                                                    "model" (:label model)})
                                                 x-range)))
                                       models-to-plot)))
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
                      line-layer {:data {:values all-line-pts}
                                  :mark {:type "line" :strokeWidth 2}
                                  :encoding {:x {:field "x" :type "quantitative"}
                                             :y {:field "y" :type "quantitative"}
                                             :color {:field "model" :type "nominal"
                                                     :legend {:title "Model"}}}}
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
                    (portal-vega-lite
                     {:width chart-width
                      :height chart-height
                      :layer layers})
                    (let [all-residual-pts
                          (vec
                           (mapcat
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
                            models-to-plot))]
                      (heading "Residual Plot")
                      (portal-vega-lite
                       {:width chart-width
                        :height (/ chart-height 2)
                        :layer [{:data {:values all-residual-pts}
                                 :mark {:type "point" :size 60}
                                 :encoding {:x {:field "x" :type "quantitative"
                                                :title (name axis)}
                                            :y {:field "residual" :type "quantitative"
                                                :title residual-title}
                                            :color {:field "model" :type "nominal"
                                                    :legend {:title "Model"}}}}
                                {:data {:values all-residual-pts}
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

