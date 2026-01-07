(ns criterium.viewer.common-charts
  "Common Vega-Lite chart generation functions for viewers.

  Provides reusable chart-building functions extracted from the Portal viewer
  for generating histograms, scatter plots, percentile charts, and treemaps."
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.probability :as probability]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]))

;;; Scatter plots

(defn metric-layer
  "Build a scatter plot layer showing samples with outlier coloring.

  Returns a Vega-Lite layer spec."
  [metric->values transforms outliers metric]
  {:pre [(have? map? metric->values)]}
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        data (mapv
              #(let [outlier (get
                              (:outliers (get-in outliers path))
                              %2
                              "")]
                 (assoc
                  {k
                   (util/transform-sample->
                    %1
                    transforms)}
                  :index %2
                  :outlier outlier))
              (have some?
                    (metric->values path)
                    {:path path :available (keys metric->values)})
              (range))]
    {:data {:values data}
     :encoding {:x {:field "index" :type "quantitative"}
                :y {:field field-name
                    :type "quantitative"
                    :scale {:zero false}}
                :tooltip [{:field "index" :type "quantitative"}
                          {:field field-name :type "quantitative"}]
                :color {:field "outlier"
                        :legend {:orient "top-left" :offset 10}}}
     :mark "point"}))

;;; Histograms

(defn metric-computed-histo-layer
  "Build a histogram bar chart layer from pre-computed histogram data.

  Returns a Vega-Lite layer spec for displaying histogram bins."
  [transforms histogram metric _layer-num]
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        {:keys [counts centers widths width density]}
        histogram

        tform #(util/transform-sample-> % transforms)
        centers (mapv tform centers)
        widths (when widths (mapv tform widths))
        width (when width (tform width))
        data (if widths
                  ;; Variable width histogram
               (mapv (fn [_count ^double center ^double width density]
                       (let [half-w (* 0.95 (/ width 2.0))]
                         {field-name (- center half-w)
                          "end" (+ center half-w)
                          "density" density}))
                     counts centers widths density)
                  ;; Fixed width histogram
               (mapv (fn [_count ^double center ^double density]
                       (let [half-w (* 0.95 (/ (double width) 2.0))]
                         {field-name (- center half-w)
                          "end" (+ center half-w)
                          "density" density}))
                     counts centers density))]
    {:data {:values data}
     :transform [{:calculate (str "'" "Histogram " (:label metric) "'") :as "layer"}]
     :encoding {:y2 {:datum 0
                     :type "quantitative"}
                :y {:field "density"
                    :type "quantitative"}
                :x {:field field-name
                    :type "quantitative"
                    :scale {:zero false}}
                :x2 {:field "end"
                     :type "quantitative"}
                :color {:field "layer" :type "nominal"
                        :legend {:orient "top-left" :offset 10}}}
     :mark {:type "bar"
            :binSpacing 0}}))

;;; Normal distribution overlay

(defn normal-pdf-points
  "Generate points for a normal probability density function curve.

  Returns a vector of {:z value :p probability} maps."
  [min-val max-val mean variance transforms]
  (let [sigma (Math/sqrt variance)
        delta (/ (- (double max-val) (double min-val)) 120)
        pdf (probability/normal-pdf mean sigma)]
    (mapv
     (fn [z]
       {:z (util/transform-sample-> z transforms)
        :p (pdf z)})
     (range min-val max-val delta))))

(defn metric-sample-stats-layer
  "Build a normal distribution overlay layer for histogram charts.

  Returns a vector of Vega-Lite layer specs showing the fitted normal
  distribution and mean line."
  [transforms stats metric-config _layer-num]
  (let [{:keys [mean-minus-3sigma mean-plus-3sigma mean variance]}
        stats
        path (:path metric-config)
        k (first path)
        data (normal-pdf-points
              mean-minus-3sigma
              mean-plus-3sigma
              mean
              variance
              transforms)]
    [{:resolve {:scale {:y "shared"}}
      :layer
      [{:data {:values data}
        :transform [{:calculate
                     (str "'" "LogNormal fit " (:label metric-config) "'")
                     :as "layer"}]
        :encoding {:x {:field "z"
                       :type "quantitative"
                       :scale {:zero false}}
                   :y {:field "p"
                       :type "quantitative"}
                   :tooltip [{:field (name k)
                              :title "Normal"}]
                   :color {:field "layer" :type "nominal"
                           :legend {:orient "top-left" :offset 10}}}
        :mark {:type "line"}}
       {:data {:values [{k
                         (util/transform-sample-> mean transforms)
                         :title "mean"}]}
        :encoding {:x {:field (name k)
                       :type "quantitative"
                       :scale {:zero false}}
                   :tooltip [{:field (name k)
                              :title (str "Mean " (name k))}]}
        :mark "rule"}]}]))

(defn metric-bootstrap-boxplot-layer
  "Build a boxplot-style layer showing median CI and spread percentiles.

  Creates a rug-plot style annotation at the bottom of the histogram showing:
  - Box: median confidence interval bounds
  - Center line: median point estimate
  - Whiskers: 10th and 90th percentiles

  Transforms are applied to bootstrap values via the source-id chain.

  Returns a vector of Vega-Lite layer specs."
  [transforms bootstrap-stats metric-config]
  (let [quantiles (:quantiles bootstrap-stats)
        p10 (get quantiles 0.1)
        p50 (get quantiles 0.5)
        p90 (get quantiles 0.9)]
    (when (and p10 p50 p90)
      (let [path (:path metric-config)
            k (first path)
            field-name (name k)
            ;; Apply transforms to raw bootstrap values
            scale (fn [v] (util/transform-sample-> v transforms))
            p10-val (scale (:point-estimate p10))
            p50-val (scale (:point-estimate p50))
            p90-val (scale (:point-estimate p90))
            ;; Extract median CI bounds and alpha for label
            median-ci (:estimate-quantiles p50)
            ci-lower (when (seq median-ci)
                       (scale (:value (first median-ci))))
            ci-upper (when (seq median-ci)
                       (scale (:value (second median-ci))))
            ci-alpha (when (seq median-ci)
                       (:alpha (first median-ci)))
            ci-level (when ci-alpha
                       (long (* 100 (- 1.0 (* 2.0 (double ci-alpha))))))
            ;; Position boxplot at y=0 (histogram baseline)
            box-y 0]
        [{:layer
          (cond-> []
            ;; Whisker from p10 to p90
            true
            (conj {:data {:values [{field-name p10-val
                                    :end p90-val}]}
                   :transform [{:calculate "'Spread (10th-90th)'" :as "layer"}]
                   :encoding {:x {:field field-name
                                  :type "quantitative"
                                  :scale {:zero false}}
                              :x2 {:field "end"
                                   :type "quantitative"}
                              :y {:datum box-y}
                              :color {:field "layer" :type "nominal"
                                      :legend {:orient "top-left" :offset 10}}}
                   :mark {:type "rule"
                          :strokeWidth 1}})

            ;; Box for median CI - extends slightly below into negative density
            ;; space. The -0.02 height creates a thin overlay that doesn't
            ;; obscure histogram bars while remaining visible.
            (and ci-lower ci-upper)
            (conj (let [box-height -0.02
                        ci-label (if ci-level
                                   (str "Median CI (" ci-level "%)")
                                   "Median CI")]
                    {:data {:values [{field-name ci-lower
                                      :end ci-upper}]}
                     :transform [{:calculate (str "'" ci-label "'") :as "layer"}]
                     :encoding {:x {:field field-name
                                    :type "quantitative"
                                    :scale {:zero false}}
                                :x2 {:field "end"
                                     :type "quantitative"}
                                :y {:datum box-y}
                                :y2 {:datum box-height}
                                :color {:field "layer" :type "nominal"
                                        :legend {:orient "top-left" :offset 10}}}
                     :mark {:type "rect"
                            :opacity 0.6}}))

            ;; Median line
            true
            (conj {:data {:values [{field-name p50-val
                                    :title "median"}]}
                   :transform [{:calculate "'Median'" :as "layer"}]
                   :encoding {:x {:field field-name
                                  :type "quantitative"
                                  :scale {:zero false}}
                              :tooltip [{:field field-name
                                         :title (str "Median " (name k))}]
                              :color {:field "layer" :type "nominal"
                                      :legend {:orient "top-left" :offset 10}}}
                   :mark {:type "rule"
                          :strokeWidth 2}}))}]))))

;;; Event markers

(defn event-occurrence
  "Extract event occurrence data for a single sample index.

  Returns nil if no events occurred, otherwise a map of metric keys to values."
  [events metrics index]
  (reduce
   (fn [res metric-config]
     (let [path (:path metric-config)
           v (get (get events path) index)]
       (if (pos? (long v))
         (assoc res (core/composite-key path) v :index index)
         res)))
   nil
   metrics))

(defn event-layer
  "Build an event marker layer showing when events occurred during sampling.

  Returns a vector containing the Vega-Lite layer spec, or nil if no events."
  [events [_k metrics]]
  (let [data (->> (map
                   (partial event-occurrence events (:values metrics))
                   (range (-> events
                              (get (:path (first (:values metrics))))
                              count)))
                  (filterv some?))]
    (when (seq data)
      [{:data {:values data}
        :encoding {:x {:field "index"
                       :type "quantitative"}
                   :color {:vvalue "white"}
                   :size {:value 2},
                   :tooltip (conj
                             (mapv
                              #(hash-map
                                :field (name
                                        (core/composite-key (:path %)))
                                :type "quantitative"
                                :title (str (:label metrics) " " (:label %)))
                              (:values metrics))
                             {:field "index" :type "quantitative"})}
        :mark {:type "rule"
               :strokeDash [2 2]}}])))

;;; Samples chart

(defn samples-vega-spec
  "Build a complete Vega-Lite spec for sample visualization.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions. Returns the Vega-Lite spec without
  viewer-specific wrapping."
  [data-map view chart-options]
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
    {:data {:values [{}]}
     :encoding {:x {:field "index" :type "quantitative"}}
     :resolve {:scale {:y "independent"}}
     :vconcat
     (into
      [(merge
        chart-options
        {:layer
         (vec
          (into
           [(metric-layer
             (util/metric->values quant-samples)
             transforms
             (when outliers (util/outliers outliers))
             (have (first metric-configs)))]
           (mapcat
            #(event-layer event-metric->values %)
            e-metrics-defs)))})]
      (mapv
       (fn [mc]
         (merge
          chart-options
          {:layer [(metric-layer
                    event-metric->values
                    transforms
                    nil mc)]}))
       e-metric-configs))}))

(defn histogram-vega-spec
  "Build a complete Vega-Lite spec for histogram visualization.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions. Returns the Vega-Lite spec without
  viewer-specific wrapping."
  [data-map view chart-options]
  (let [histogram-id (or (:histogram-id view) :histograms)
        stats-id (or (:stats-id view) :stats)
        bootstrap-stats-id (or (:bootstrap-stats-id view) :bootstrap-stats)
        quant-samples-id (or (:samples-id view) :samples)
        quant-samples (data-map quant-samples-id)
        stats (data-map stats-id)
        bootstrap-stats-map (data-map bootstrap-stats-id)
        histograms-map (util/lookup-data data-map histogram-id)
        histograms (:histograms histograms-map)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        hist-transforms (util/get-transforms data-map histogram-id)
        stats-transforms (util/get-transforms data-map (:source-id stats))
        bootstrap-transforms (when bootstrap-stats-map
                               (util/get-transforms data-map bootstrap-stats-id))
        layer-num (volatile! 0)]
    {:data {:values []}
     :resolve {:scale {:x "independent"
                       :y "independent"
                       :color "shared"}}
     :vconcat (mapv
               (fn [metric-config]
                 (merge
                  chart-options
                  {:resolve {:scale {:x "shared" :y "independent"}}
                   :layer
                   (into
                    [(metric-computed-histo-layer
                      hist-transforms
                      (histograms (:path metric-config))
                      metric-config
                      (vswap!
                       layer-num
                       (fn [^long x] (unchecked-inc x))))]
                    (concat
                     ;; Bootstrap boxplot layers (median) first
                     (when bootstrap-stats-map
                       (->>
                        (metric-bootstrap-boxplot-layer
                         bootstrap-transforms
                         (get-in (util/bootstrap bootstrap-stats-map)
                                 (:path metric-config))
                         metric-config)))
                     ;; Stats layers (mean) last so mean line appears on top
                     (when stats
                       (->>
                        (metric-sample-stats-layer
                         stats-transforms
                         (get-in (util/stats stats) (:path metric-config))
                         metric-config
                         (vswap!
                          layer-num
                          (fn [^long x] (unchecked-inc x))))))))}))
               metric-configs)}))

;;; Percentile charts

(defn metric-percentile-layer
  "Build a percentile distribution chart layer.

  Uses a logarithmic scale on the x-axis to emphasize tail percentiles.
  Returns a Vega-Lite layer spec."
  [metric->values transforms metric]
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        vs (->> (metric->values path)
                (map #(util/transform-sample-> % transforms))
                sort
                vec)
        n (count vs)
        max-val (Math/log10 (double n))
        xs (mapv
            #(/ (- max-val (Math/log10 (- n (double %)))) max-val)
            (range 0 n))
        delta (/ 100.0 (dec n))
        percentiles (take n
                          (iterate
                           #(+ delta (double %)) 0))
        data (mapv
              #(hash-map k %1 :p %2 :x %3)
              vs
              percentiles
              xs)]
    {:data {:values data
            :name "vals"}

     :encoding
     {:x
      {:field "x" :type "quantitative"
       :scale {:domain [0 1.0]}
       :axis
       {:labelExpr
        ;; inverse of xs
        (format
         "format( (%d - pow(%d, 1 - min(datum.index, 1.0))) / %d,'.3%%')",
         n,n,(dec n))
        :tickCount 10}}
      :y {:field field-name
          :type "quantitative"
          :scale {:zero false
                  :type "log"}}
      :tooltip [{:field "p" :type "quantitative"}
                {:field field-name :type "quantitative"}]}
     :mark "point"}))

;;; Sample diffs

(defn metric-diff-layer
  "Build a sample differences chart layer.

  Shows sorted unique differences from minimum value.
  Returns a Vega-Lite layer spec."
  [samples metric]
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        vs (->> (get samples path)
                sort
                vec)
        min-v (double (first vs))
        diffs (-> (mapv
                   #(- (double %) min-v)
                   vs)
                  sort
                  distinct
                  vec)
        data (mapv
              #(hash-map k %1 :x %2)
              diffs
              (range))]
    {:data {:values data
            :name "vals"}

     :encoding
     {:x
      {:field "x" :type "quantitative"}
      :y {:field field-name
          :type "quantitative"
          :scale {:zero false}}
      :tooltip [{:field field-name :type "quantitative"}]}
     :mark "point"}))

;;; Regression helpers
;;
;; Note: regression-model-fn and regression-equation-str are no longer needed.
;; Model results now include :predict-fn and :equation-str directly from
;; fit-complexity-model in criterium.domain.analysis.

(defn regression-scatter-layer
  "Build scatter plot layer for regression points.
  Options:
    :color-field - field name for color encoding (e.g., \"impl\" or nil for static color)
    :color-value - static color when color-field is nil (default \"steelblue\")
    :legend-options - legend config map or nil for default"
  [points {:keys [axis-name y-title color-field color-value legend-options]
           :or {color-value "steelblue"}}]
  {:data {:values points}
   :mark {:type "point" :size 60}
   :encoding (cond-> {:x {:field "x" :type "quantitative" :title axis-name}
                      :y {:field "y" :type "quantitative" :title y-title}}
               color-field
               (assoc :color {:field color-field :type "nominal"
                              :legend (merge {:title (if (= color-field "impl")
                                                       "Implementation"
                                                       "Model")}
                                             legend-options)})
               (not color-field)
               (assoc :color {:value color-value}))})

(defn regression-error-layer
  "Build error bar layer for regression points with error bounds."
  [points {:keys [color-field color-value]
           :or {color-value "steelblue"}}]
  {:data {:values points}
   :mark {:type "rule" :strokeWidth 1.5}
   :encoding (cond-> {:x {:field "x" :type "quantitative"}
                      :y {:field "yLower" :type "quantitative"}
                      :y2 {:field "yUpper"}
                      :opacity {:value 0.5}}
               color-field
               (assoc :color {:field color-field :type "nominal" :legend nil})
               (not color-field)
               (assoc :color {:value color-value}))})

(defn regression-line-layer
  "Build fit line layer for regression models.
  Options:
    :color-field - field name for color encoding (\"impl\" or \"model\")
    :legend-options - legend config map or nil for default"
  [line-pts {:keys [color-field legend-options]}]
  {:data {:values (vec line-pts)}
   :mark {:type "line" :strokeWidth 2}
   :encoding {:x {:field "x" :type "quantitative"}
              :y {:field "y" :type "quantitative"}
              :color {:field color-field :type "nominal"
                      :legend (when legend-options
                                (merge {:title (if (= color-field "impl")
                                                 "Implementation"
                                                 "Model")}
                                       legend-options))}}})

(defn regression-chart-spec
  "Build complete regression scatter plot with fit lines.
  Options:
    :width - chart width
    :height - chart height
    :axis-name - x-axis title
    :y-title - y-axis title
    :color-field - field for color encoding (\"impl\" or \"model\" or nil)
    :color-value - static color when color-field is nil
    :legend-options - legend config map
    :has-error-bounds? - whether to include error bars"
  [points line-pts
   {:keys [width height color-field color-value
           legend-options has-error-bounds?]
    :or {width 600 height 400 color-value "steelblue"} :as opts}]
  (let [scatter-layer (regression-scatter-layer points opts)
        line-layer (regression-line-layer line-pts
                                          {:color-field color-field
                                           :legend-options legend-options})
        error-layer (when has-error-bounds?
                      (regression-error-layer points
                                              {:color-field color-field
                                               :color-value color-value}))
        layers (cond-> [scatter-layer line-layer]
                 has-error-bounds? (conj error-layer))]
    {:width width
     :height height
     :layer layers}))

(defn regression-residual-layer
  "Build scatter layer for residual plot.
  Options:
    :color-field - field name for color encoding
    :legend-options - legend config map or nil for default"
  [residual-pts {:keys [axis-name residual-title color-field legend-options]}]
  {:data {:values (vec residual-pts)}
   :mark {:type "point" :size 60}
   :encoding {:x {:field "x" :type "quantitative" :title axis-name}
              :y {:field "residual" :type "quantitative" :title residual-title}
              :color {:field color-field :type "nominal"
                      :legend (merge {:title (if (= color-field "impl")
                                               "Implementation"
                                               "Model")}
                                     legend-options)}}})

(defn regression-loess-layer
  "Build loess smoothing layer for residual plot.
  When color-field is nil, LOESS is applied to all data points as one series."
  [residual-pts {:keys [color-field]}]
  {:data {:values (vec residual-pts)}
   :transform [(cond-> {:loess "residual"
                        :on "x"
                        :bandwidth 0.3}
                 color-field (assoc :groupby [color-field]))]
   :mark {:type "line" :strokeWidth 1}
   :encoding (cond-> {:x {:field "x" :type "quantitative"}
                      :y {:field "residual" :type "quantitative"}
                      :opacity {:value 0.4}}
               color-field
               (assoc :color {:field color-field :type "nominal" :legend nil})
               (not color-field)
               (assoc :color {:value "steelblue"}))})

(defn regression-zero-line-layer
  "Build zero reference line layer for residual plot."
  []
  {:data {:values [{"y" 0}]}
   :mark {:type "rule" :strokeDash [4 4]}
   :encoding {:y {:field "y" :type "quantitative"}
              :color {:value "gray"}}})

(defn regression-residual-spec
  "Build complete residual plot spec.
  Options:
    :width - chart width
    :height - chart height (typically half of main chart)
    :axis-name - x-axis title
    :residual-title - y-axis title
    :color-field - field for color encoding
    :legend-options - legend config map"
  [residual-pts {:keys [width height color-field]
                 :or {width 600 height 200} :as opts}]
  {:width width
   :height height
   :layer [(regression-residual-layer residual-pts opts)
           (regression-loess-layer residual-pts {:color-field color-field})
           (regression-zero-line-layer)]})

;;; Log-Log Regression Charts
;;
;; Charts for visualizing log-log regression analysis.
;; In log-log space, power law relationships become linear:
;; log(y) = slope * log(x) + intercept
;; The slope directly indicates complexity class (1 for O(n), 2 for O(n²), etc.)

(defn log-log-scatter-layer
  "Build scatter layer for log-log plot.
  Points are in log space: x = log(n), y = log(time)."
  [points {:keys [axis-name color-field color-value legend-options]}]
  {:data {:values (vec points)}
   :mark {:type "point" :size 60 :filled true}
   :encoding (cond-> {:x {:field "x"
                          :type "quantitative"
                          :title (str "log(" axis-name ")")}
                      :y {:field "y"
                          :type "quantitative"
                          :title "log(time)"}}
               color-field
               (assoc :color {:field color-field
                              :type "nominal"
                              :legend (merge {:title (if (= color-field "impl")
                                                       "Implementation"
                                                       "Model")}
                                             legend-options)})
               (and (nil? color-field) color-value)
               (assoc :color {:value color-value}))})

(defn log-log-line-layer
  "Build fit line layer for log-log plot.
  Line represents: y = slope * x + intercept in log space."
  [line-pts {:keys [color-field _legend-options]}]
  {:data {:values (vec line-pts)}
   :mark {:type "line" :strokeWidth 2}
   :encoding (cond-> {:x {:field "x" :type "quantitative"}
                      :y {:field "y" :type "quantitative"}}
               color-field
               (assoc :color {:field color-field
                              :type "nominal"
                              :legend nil}))})

(defn log-log-error-layer
  "Build error bar layer for log-log plot.
  Shows confidence bounds in log space."
  [points {:keys [color-field color-value]}]
  {:data {:values (vec (filter #(and (contains? % "yLower")
                                     (contains? % "yUpper"))
                               points))}
   :mark {:type "rule" :strokeWidth 1}
   :encoding (cond-> {:x {:field "x" :type "quantitative"}
                      :y {:field "yLower" :type "quantitative"}
                      :y2 {:field "yUpper"}}
               color-field
               (assoc :color {:field color-field :type "nominal" :legend nil})
               (and (nil? color-field) color-value)
               (assoc :color {:value color-value}))})

(defn log-log-chart-spec
  "Build complete log-log scatter plot with fit line.
  Options:
    :width - chart width
    :height - chart height
    :axis-name - name of the axis variable (e.g., 'n')
    :color-field - field for color encoding ('impl' or nil)
    :color-value - static color when color-field is nil
    :legend-options - legend config map
    :has-error-bounds? - whether to include error bars
    :slope - slope value to show in title
    :r-squared - R² value to show in title"
  [points line-pts
   {:keys [width height color-field color-value
           legend-options has-error-bounds? slope r-squared]
    :or {width 600 height 400 color-value "steelblue"} :as opts}]
  (let [scatter-layer (log-log-scatter-layer points opts)
        line-layer (log-log-line-layer line-pts
                                       {:color-field color-field
                                        :legend-options legend-options})
        error-layer (when has-error-bounds?
                      (log-log-error-layer points
                                           {:color-field color-field
                                            :color-value color-value}))
        layers (cond-> [scatter-layer line-layer]
                 has-error-bounds? (conj error-layer))
        title (when (and slope r-squared (nil? color-field))
                (format "Log-Log Plot: slope=%.2f (R²=%.4f)"
                        (double slope) (double r-squared)))]
    (cond-> {:width width
             :height height
             :layer layers}
      title (assoc :title title))))

(defn log-log-residual-layer
  "Build scatter layer for log-log residual plot."
  [residual-pts {:keys [axis-name residual-title color-field legend-options]}]
  {:data {:values (vec residual-pts)}
   :mark {:type "point" :size 60}
   :encoding (cond-> {:x {:field "x"
                          :type "quantitative"
                          :title (str "log(" axis-name ")")}
                      :y {:field "residual"
                          :type "quantitative"
                          :title (or residual-title "Residual (log space)")}}
               color-field
               (assoc :color {:field color-field
                              :type "nominal"
                              :legend (merge {:title (if (= color-field "impl")
                                                       "Implementation"
                                                       "Model")}
                                             legend-options)}))})

(defn log-log-residual-spec
  "Build complete log-log residual plot spec.
  Options:
    :width - chart width
    :height - chart height (typically half of main chart)
    :axis-name - name of the axis variable (e.g., 'n')
    :residual-title - y-axis title
    :color-field - field for color encoding
    :legend-options - legend config map"
  [residual-pts {:keys [width height color-field axis-name]
                 :or {width 600 height 200} :as opts}]
  {:width width
   :height height
   :layer [(log-log-residual-layer residual-pts (assoc opts :axis-name axis-name))
           (regression-loess-layer residual-pts {:color-field color-field})
           (regression-zero-line-layer)]})

;;; Single-point comparison bar and box charts

(defn prepare-single-point-box-data
  "Prepare data for single-point box plot from domain extract.
  Extracts bootstrap statistics (median, CI, percentiles) from embedded data.

  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword
    :metric-path - the metric path vector
    :y-title - y-axis title with SI unit (uses 'median' prefix)
    :data - vector of maps with string keys:
            {\"impl\" string \"median\" number \"ciLower\" number \"ciUpper\" number
             \"p10\" number \"p90\" number}
            (ciLower/ciUpper omitted when CI bounds not available)

  If bootstrap stats are missing for a metric, warns to stdout and returns nil
  for that metric entry (filtered from result)."
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        implementations (:implementations extract)
        metrics (:metrics extract)]
    (->> metrics
         (sort-by key)
         (keep
          (fn [[metric-id {:keys [metric data]}]]
            (let [;; Build lookup: impl -> bootstrap stats value map
                  lookup (reduce
                          (fn [acc [coord value]]
                            (let [impl-val (get coord impl-axis-key)]
                              (assoc acc impl-val value)))
                          {}
                          data)
                  ;; Get all values from lookup
                  all-raw-values (keep #(get lookup %) implementations)
                  ;; Check if all values have required box plot fields
                  all-have-box-data? (every? core/has-box-plot-data?
                                             all-raw-values)]
              (if-not all-have-box-data?
                (do
                  (core/warn-missing-bootstrap-stats metric-id)
                  nil)
                (let [;; Get median values for SI scaling
                      all-medians (map :median all-raw-values)
                      {:keys [^double total-scale unit]}
                      (core/compute-si-scaling metric all-medians)
                      ;; Build y-axis title with unit
                      metric-name (name metric-id)
                      base-title (str "median " metric-name)
                      y-title (if (seq unit)
                                (str base-title " (" unit ")")
                                base-title)
                      ;; Build chart data
                      chart-data (mapv
                                  (fn [impl]
                                    (let [v (get lookup impl)]
                                      (cond-> {"impl" (name impl)
                                               "median" (* (double (:median v)) total-scale)
                                               "p10" (* (double (:p10 v)) total-scale)
                                               "p90" (* (double (:p90 v)) total-scale)}
                                        (contains? v :ci-lower)
                                        (assoc "ciLower" (* (double (:ci-lower v)) total-scale))
                                        (contains? v :ci-upper)
                                        (assoc "ciUpper" (* (double (:ci-upper v)) total-scale)))))
                                  implementations)]
                  {:metric-id metric-id
                   :metric-path metric
                   :y-title y-title
                   :data chart-data})))))
         vec)))

(defn prepare-single-point-bar-data
  "Prepare data for single-point bar chart from domain extract.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword
    :metric-path - the metric path vector
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {:impl string :value number :valueLower number :valueUpper number} maps
            (valueLower/valueUpper only present when error bounds exist)"
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        implementations (:implementations extract)
        metrics (:metrics extract)]
    (mapv
     (fn [[metric-id {:keys [metric data]}]]
       (let [;; Build lookup: impl -> full value (may be map with :value/:lower/:upper)
             lookup (reduce
                     (fn [acc [coord value]]
                       (let [impl-val (get coord impl-axis-key)]
                         (assoc acc impl-val value)))
                     {}
                     data)
             ;; Get all values from lookup for error bounds check and SI scaling
             all-raw-values (keep #(get lookup %) implementations)
             ;; Check if any values have error bounds
             has-error-bounds? (core/values-have-error-bounds?
                                all-raw-values)
             ;; Get numeric values for SI scaling
             all-values (map core/get-numeric-value all-raw-values)
             {:keys [^double total-scale unit]}
             (core/compute-si-scaling metric all-values)
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
                                 raw-value (core/get-numeric-value v)]
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
     (sort-by key metrics))))

(defn- chart-layer
  "Build a chart layer by merging chart-options with mark and encoding.
  Common structure for bar and line chart layers."
  [data chart-options mark encoding]
  (merge
   chart-options
   {:data {:values data}
    :mark mark
    :encoding encoding}))

(defn- bar-error-layer
  "Build error bar layer for bar charts with error bounds.
  Returns a layered spec with vertical rule and horizontal tick caps."
  [data]
  {:layer
   [;; Vertical rule (error bar stem)
    {:data {:values data}
     :mark {:type "rule" :strokeWidth 1.5}
     :encoding {:x {:field "impl" :type "nominal"}
                :y {:field "valueLower" :type "quantitative"}
                :y2 {:field "valueUpper"}
                :color {:value "#333"}}}
    ;; Lower tick cap
    {:data {:values data}
     :mark {:type "tick" :thickness 1.5 :size 8}
     :encoding {:x {:field "impl" :type "nominal"}
                :y {:field "valueLower" :type "quantitative"}
                :color {:value "#333"}}}
    ;; Upper tick cap
    {:data {:values data}
     :mark {:type "tick" :thickness 1.5 :size 8}
     :encoding {:x {:field "impl" :type "nominal"}
                :y {:field "valueUpper" :type "quantitative"}
                :color {:value "#333"}}}]})

(defn- box-plot-whisker-layer
  "Build whisker layer for box plot (rule from p10 to p90 with end caps).
  Returns a layer with sub-layers: the main whisker rule and tick caps at p10/p90."
  [data]
  {:layer
   [{:data {:values data}
     :mark {:type "rule" :strokeWidth 1.5}
     :encoding {:x {:field "impl" :type "nominal"}
                :y {:field "p10" :type "quantitative"}
                :y2 {:field "p90"}
                :color {:value "#333"}}}
    {:data {:values data}
     :mark {:type "tick" :thickness 1.5 :size 10}
     :encoding {:x {:field "impl" :type "nominal"}
                :y {:field "p10" :type "quantitative"}
                :color {:value "#333"}}}
    {:data {:values data}
     :mark {:type "tick" :thickness 1.5 :size 10}
     :encoding {:x {:field "impl" :type "nominal"}
                :y {:field "p90" :type "quantitative"}
                :color {:value "#333"}}}]})

(defn- box-plot-ci-layer
  "Build CI box layer for box plot (rect from ciLower to ciUpper)."
  [data]
  {:data {:values data}
   :mark {:type "bar" :width 20}
   :encoding {:x {:field "impl" :type "nominal"}
              :y {:field "ciLower" :type "quantitative"}
              :y2 {:field "ciUpper"}
              :color {:field "impl"
                      :type "nominal"
                      :legend {:title "Implementation"}}}})

(defn- box-plot-median-layer
  "Build median line layer for box plot (tick mark at median)."
  [data]
  {:data {:values data}
   :mark {:type "tick" :thickness 2 :size 20 :color "white"}
   :encoding {:x {:field "impl" :type "nominal"}
              :y {:field "median" :type "quantitative"}}})

(defn- box-plot-layer
  "Build a box plot from prepared box data.
  Returns a layered spec with:
  - Whisker rule from p10 to p90
  - CI box from ciLower to ciUpper (when present)
  - Median tick mark
  Used by both single-point-box-chart-spec and comparison-box-chart-spec."
  [{:keys [y-title data]} chart-options]
  (let [has-ci? (some #(contains? % "ciLower") data)
        base-tooltip [{:field "impl"
                       :type "nominal"
                       :title "Implementation"}
                      {:field "median"
                       :type "quantitative"
                       :title "Median"
                       :format ".3g"}
                      {:field "p10"
                       :type "quantitative"
                       :title "10th percentile"
                       :format ".3g"}
                      {:field "p90"
                       :type "quantitative"
                       :title "90th percentile"
                       :format ".3g"}]
        tooltip (if has-ci?
                  (into base-tooltip
                        [{:field "ciLower"
                          :type "quantitative"
                          :title "CI lower"
                          :format ".3g"}
                         {:field "ciUpper"
                          :type "quantitative"
                          :title "CI upper"
                          :format ".3g"}])
                  base-tooltip)
        ;; Build layers: whisker, optionally CI box, median
        layers (cond-> [(box-plot-whisker-layer data)]
                 has-ci? (conj (box-plot-ci-layer data))
                 true (conj (box-plot-median-layer data)))
        ;; Add invisible point layer for tooltip on hover
        tooltip-layer {:data {:values data}
                       :mark {:type "point" :opacity 0 :size 400}
                       :encoding {:x {:field "impl" :type "nominal"}
                                  :y {:field "median" :type "quantitative"}
                                  :tooltip tooltip}}]
    (merge chart-options
           {:layer (conj layers tooltip-layer)
            :encoding {:x {:field "impl"
                           :type "nominal"
                           :title "Implementation"
                           :sort nil
                           :axis {:labelAngle 0}}
                       :y {:type "quantitative"
                           :title y-title
                           :scale {:zero false}}}})))

(defn- bar-chart-layer
  "Build a bar chart from prepared bar data.
  Returns a layered spec with error bars when has-error-bounds? is true,
  otherwise returns a simple bar chart spec.
  Used by both single-point-bar-chart-spec and comparison-bar-chart-spec."
  [{:keys [y-title has-error-bounds? data]} chart-options]
  (let [base-tooltip [{:field "impl"
                       :type "nominal"
                       :title "Implementation"}
                      {:field "value"
                       :type "quantitative"
                       :title y-title
                       :format ".3g"}]
        tooltip (if has-error-bounds?
                  (into base-tooltip
                        [{:field "valueLower"
                          :type "quantitative"
                          :title "Lower bound"
                          :format ".3g"}
                         {:field "valueUpper"
                          :type "quantitative"
                          :title "Upper bound"
                          :format ".3g"}])
                  base-tooltip)
        bar-layer (chart-layer
                   data
                   {}
                   {:type "bar"}
                   {:x {:field "impl"
                        :type "nominal"
                        :title "Implementation"
                        :sort nil
                        :axis {:labelAngle 0}}
                    :y {:field "value"
                        :type "quantitative"
                        :title y-title}
                    :color {:field "impl"
                            :type "nominal"
                            :legend nil}
                    :tooltip tooltip})]
    (if has-error-bounds?
      (merge chart-options
             {:layer [bar-layer (bar-error-layer data)]})
      (merge chart-options bar-layer))))

(defn single-point-bar-chart-spec
  "Build a Vega-Lite bar chart spec for single-point multi-impl comparison.

  Shows implementations on x-axis and measured values on y-axis.
  One chart is generated per metric in the extract.

  Parameters:
    extract - Domain extract with single-point multi-impl data
    chart-options - Map with :width and/or :height for chart dimensions

  Returns a Vega-Lite spec with vconcat of bar charts (one per metric)."
  [extract chart-options]
  (let [bar-data (prepare-single-point-bar-data extract)]
    {:data {:values []}
     :vconcat (mapv #(bar-chart-layer % chart-options) bar-data)}))

(defn single-point-box-chart-spec
  "Build a Vega-Lite box plot spec for single-point multi-impl comparison.

  Shows implementations on x-axis with box plots showing:
  - Whiskers: 10th and 90th percentiles (p10, p90)
  - Box: Confidence interval on median (ciLower, ciUpper) when available
  - Center line: Median point estimate

  Requires bootstrap stats in the extract data. If bootstrap stats are missing,
  the chart will be empty (data prep warns and filters out metrics without stats).

  Parameters:
    extract - Domain extract with single-point multi-impl data containing bootstrap stats
    chart-options - Map with :width and/or :height for chart dimensions

  Returns a Vega-Lite spec with vconcat of box plots (one per metric)."
  [extract chart-options]
  (let [box-data (prepare-single-point-box-data extract)]
    {:data {:values []}
     :vconcat (mapv #(box-plot-layer % chart-options) box-data)}))

(defn comparison-bar-chart-spec
  "Build a Vega-Lite bar chart spec for single-point comparison data.

  Shows implementations on x-axis and measured values on y-axis.
  One chart is generated per metric in the comparison.

  Parameters:
    comparison - Domain comparison with single-point multi-impl data
    chart-options - Map with :width and/or :height for chart dimensions

  Returns a Vega-Lite spec with vconcat of bar charts (one per metric)."
  [comparison chart-options]
  (let [bar-data (comparison/prepare-comparison-bar-data comparison)]
    {:data {:values []}
     :vconcat (mapv #(bar-chart-layer % chart-options) bar-data)}))

(defn comparison-box-chart-spec
  "Build a Vega-Lite box plot spec for single-point comparison data.

  Shows implementations on x-axis with box plots showing:
  - Whiskers: 10th and 90th percentiles (p10, p90)
  - Box: Confidence interval on median (ciLower, ciUpper) when available
  - Center line: Median point estimate

  Requires bootstrap stats in the comparison data. If bootstrap stats are missing,
  the chart will be empty (data prep warns and filters out metrics without stats).

  Parameters:
    comparison - Domain comparison with single-point multi-impl data containing bootstrap stats
    chart-options - Map with :width and/or :height for chart dimensions

  Returns a Vega-Lite spec with vconcat of box plots (one per metric)."
  [comparison chart-options]
  (let [box-data (comparison/prepare-comparison-box-data comparison)]
    {:data {:values []}
     :vconcat (mapv #(box-plot-layer % chart-options) box-data)}))

;;; Multi-point line charts

(defn- line-confidence-band-layer
  "Build confidence band layer for line charts with error bounds.
  Uses area marks with y/y2 encoding for yLower/yUpper bounds.
  Color matches line color per implementation."
  [data]
  {:data {:values data}
   :mark {:type "area" :opacity 0.2}
   :encoding {:x {:field "x" :type "quantitative"}
              :y {:field "yLower" :type "quantitative"}
              :y2 {:field "yUpper"}
              :color {:field "impl" :type "nominal" :legend nil}}})

(defn- line-chart-layer
  "Build a single line chart layer from prepared line data.
  Returns a layered spec with confidence bands when has-error-bounds? is true,
  otherwise returns a simple line chart spec.
  Used by both domain-line-chart-spec and comparison-line-chart-spec."
  [{:keys [x-title y-title has-error-bounds? data]} chart-options]
  (let [line-layer (chart-layer
                    data
                    {}
                    {:type "line" :point true}
                    {:x {:field "x"
                         :type "quantitative"
                         :title x-title
                         :scale {:zero false}}
                     :y {:field "y"
                         :type "quantitative"
                         :title y-title}
                     :color {:field "impl"
                             :type "nominal"
                             :title "Implementation"}
                     :tooltip [{:field "impl"
                                :type "nominal"
                                :title "Implementation"}
                               {:field "x"
                                :type "quantitative"
                                :title x-title}
                               {:field "y"
                                :type "quantitative"
                                :title y-title
                                :format ".3g"}]})]
    (if has-error-bounds?
      (merge chart-options
             {:layer [(line-confidence-band-layer data) line-layer]})
      (merge chart-options line-layer))))

(defn domain-line-chart-spec
  "Build a Vega-Lite line chart spec for single-axis multi-point domain extract.

  Shows axis values on x-axis and measured values on y-axis, with one line
  per implementation differentiated by color.
  One chart is generated per metric in the extract.

  Parameters:
    extract - Domain extract with single-axis multi-point multi-impl data
    chart-options - Map with :width and/or :height for chart dimensions

  Returns a Vega-Lite spec with vconcat of line charts (one per metric)."
  [extract chart-options]
  (let [line-data (comparison/prepare-line-chart-data extract)]
    {:data {:values []}
     :vconcat (mapv #(line-chart-layer % chart-options) line-data)}))

(defn comparison-line-chart-spec
  "Build a Vega-Lite line chart spec for single-axis multi-point comparison data.

  Shows axis values on x-axis and measured values on y-axis, with one line
  per implementation differentiated by color.
  One chart is generated per metric in the comparison.

  Parameters:
    comparison - Domain comparison with single-axis multi-point multi-impl data
    chart-options - Map with :width and/or :height for chart dimensions

  Returns a Vega-Lite spec with vconcat of line charts (one per metric)."
  [comparison chart-options]
  (let [line-data (comparison/prepare-comparison-line-data comparison)]
    {:data {:values []}
     :vconcat (mapv #(line-chart-layer % chart-options) line-data)}))

;;; Treemap charts

(defn- flatten-treemap-node
  "Flatten a hierarchical treemap node into a sequence of flat records.
  Each record has :id, :parent, and :name keys for use with Vega stratify.
  Only leaf nodes get stats - parent sizes are computed by Vega's treemap transform."
  ([node] (flatten-treemap-node node nil []))
  ([node parent-id path]
   (let [node-name (:name node)
         node-id (if (empty? path)
                   "root"
                   (str/join "/" (conj path node-name)))
         current-path (conj path node-name)
         children (:children node)
         node-record (cond-> {:id node-id
                              :parent parent-id
                              :name node-name
                              :depth (count path)}
                       ;; Only set stats on leaf nodes
                       (not children) (assoc :value (:value node 0)
                                             :bytes (:bytes node)
                                             :count (:count node)
                                             :freed-bytes (:freed-bytes node 0)
                                             :freed-count (:freed-count node 0)))]
     (if children
       (cons node-record
             (mapcat #(flatten-treemap-node % node-id current-path) children))
       [node-record]))))

;;; KDE charts

(defn kde-density-layer
  "Build a density curve layer for KDE visualization.
  Uses 'kde-density' field for independent Y-scale from histogram.
  Returns a Vega-Lite layer spec."
  [kde-data metric-config transforms]
  (let [{:keys [grid density]} kde-data
        {:keys [label]} metric-config
        k (first (:path metric-config))
        field-name (name k)
        data (mapv (fn [log-x d]
                     {field-name (util/transform-sample-> log-x transforms)
                      "kde-density" d})
                   grid density)]
    {:data {:values data}
     :transform [{:calculate (str "'" "KDE " label "'") :as "layer"}]
     :mark {:type "line" :strokeWidth 2}
     :encoding {:x {:field field-name :type "quantitative"
                    :scale {:zero false}}
                :y {:field "kde-density" :type "quantitative"}
                :color {:field "layer" :type "nominal"
                        :legend {:orient "top-left" :offset 10}}}}))

(defn kde-confidence-band-layer
  "Build a confidence band area layer for KDE visualization.
  Uses 'kde-lower'/'kde-upper' fields to avoid Y-scale conflicts.
  Returns a Vega-Lite layer spec."
  [kde-data metric-config transforms]
  (let [{:keys [grid lower-band upper-band]} kde-data
        {:keys [_label]} metric-config
        k (first (:path metric-config))
        field-name (name k)
        data (mapv (fn [x lo hi]
                     {field-name (util/transform-sample-> x transforms)
                      "kde-lower" lo
                      "kde-upper" hi})
                   grid lower-band upper-band)]
    {:data {:values data}
     :mark {:type "area" :opacity 0.2}
     :encoding {:x {:field field-name :type "quantitative"
                    :scale {:zero false}}
                :y {:field "kde-lower" :type "quantitative"}
                :y2 {:field "kde-upper"}
                :color {:value "#ff7f0e"}}}))

(defn kde-modes-layer
  "Build mode marker layers for KDE visualization.
  Takes modes-data from separate modes analysis (not from kde-data).
  Uses 'kde-density' field to match KDE curve scale.
  Uses shape (not color) for significance to avoid color scale conflicts.
  Returns a vector of Vega-Lite layer specs (point markers and CI rules)."
  [modes-data metric-config transforms]
  (let [modes (:modes modes-data)
        k (first (:path metric-config))
        field-name (name k)
        data (mapv (fn [{:keys [location density ci-lower ci-upper significant?]}]
                     (cond-> {field-name (util/transform-sample-> location transforms)
                              "kde-density" density
                              "significant" (if significant? "yes" "no")}
                       ci-lower (assoc "ci-lower" (util/transform-sample-> ci-lower transforms))
                       ci-upper (assoc "ci-upper" (util/transform-sample-> ci-upper transforms))))
                   modes)]
    (when (seq data)
      ;; Return a vector of individual layers to avoid nested layer structure
      ;; Use shape instead of color for significance to avoid color scale conflicts
      [{:data {:values data}
        :mark {:type "point" :size 150 :filled true}
        :encoding {:x {:field field-name :type "quantitative"}
                   :y {:field "kde-density" :type "quantitative"}
                   :shape {:field "significant" :type "nominal"
                           :scale {:domain ["yes" "no"]
                                   :range ["circle" "triangle-up"]}
                           :legend {:title "Significant Mode"}}
                   :color {:value "red"}
                   :tooltip [{:field field-name :type "quantitative"
                              :title "Mode Location"}
                             {:field "kde-density" :type "quantitative"
                              :title "Density"}
                             {:field "ci-lower" :type "quantitative"
                              :title "CI Lower"}
                             {:field "ci-upper" :type "quantitative"
                              :title "CI Upper"}
                             {:field "significant" :type "nominal"
                              :title "Significant"}]}}
       {:data {:values data}
        :mark {:type "rule" :strokeWidth 2}
        :encoding {:x {:field "ci-lower" :type "quantitative"}
                   :x2 {:field "ci-upper"}
                   :y {:field "kde-density" :type "quantitative"}
                   :color {:value "red"}
                   :strokeDash {:field "significant" :type "nominal"
                                :scale {:domain ["yes" "no"]
                                        :range [[1 0] [4 4]]}
                                :legend nil}}}])))

(defn kde-vega-spec
  "Build a complete Vega-Lite spec for KDE visualization.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions.

  View options:
    :kde-id - Key for KDE data in data-map (default :kde)
    :histogram-id - Optional key for histogram data to overlay
    :modes-id - Optional key for modes data (default :modes)

  Returns the Vega-Lite spec without viewer-specific wrapping."
  [data-map view chart-options]
  (let [kde-id (or (:kde-id view) :kde)
        histogram-id (:histogram-id view)
        modes-id (or (:modes-id view) :modes)
        kde-map (util/lookup-data data-map kde-id)
        histograms-map (when histogram-id
                         (util/lookup-data data-map histogram-id))
        ;; Use get for optional modes lookup - don't throw if not present
        modes-map (get data-map modes-id)
        kdes (:kdes kde-map)
        all-modes (when modes-map (:modes modes-map))
        metrics-defs (-> (:metrics-defs kde-map)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        kde-transforms (util/get-transforms data-map kde-id)
        hist-transforms (when histogram-id
                          (util/get-transforms data-map histogram-id))]
    {:data {:values []}
     :resolve {:scale {:x "independent"
                       :y "independent"
                       :color "independent"}}
     :vconcat
     (mapv
      (fn [metric-config]
        (let [kde-data (get kdes (:path metric-config))
              histogram (when histograms-map
                          (get (:histograms histograms-map)
                               (:path metric-config)))
              modes-data (when all-modes
                           (get all-modes (:path metric-config)))]
          (when kde-data
            (merge
             chart-options
             {:resolve {:scale {:x "shared" :y "independent"}}
              :layer
              (cond-> []
                ;; Add histogram bars if available
                histogram
                (conj (metric-computed-histo-layer
                       hist-transforms
                       histogram
                       metric-config
                       0))
                ;; Wrap KDE layers in a nested group with shared Y-scale
                ;; This gives them independent Y-scale from histogram
                true
                (conj {:resolve {:scale {:y "shared"}}
                       :layer
                       (cond-> []
                         ;; Add confidence band
                         true
                         (conj (kde-confidence-band-layer
                                kde-data metric-config kde-transforms))
                         ;; Add density curve
                         true
                         (conj (kde-density-layer
                                kde-data metric-config kde-transforms))
                         ;; Add mode markers from separate modes analysis
                         (and modes-data (seq (:modes modes-data)))
                         (into (kde-modes-layer
                                modes-data metric-config kde-transforms)))}))}))))
      metric-configs)}))

(defn treemap-vega-spec
  "Build a complete Vega spec for treemap visualization.

  Takes treemap-data (a :criterium/allocation-treemap map from analysis)
  and opts map containing display options.

  Parameters:
    treemap-data - The allocation treemap map with :root containing hierarchy
    opts - Display options:
      :width (default 700)
      :height (default 400)
      :color-scheme (default \"tableau10\")

  Returns a full Vega spec (not Vega-Lite) using:
    - stratify transform to build hierarchy from flat data
    - treemap transform with squarify tiling
    - rect marks sized by x0/x1/y0/y1
    - color by first-level category
    - tooltip on hover showing name, value (formatted bytes), path"
  [treemap-data opts]
  (let [width (or (:width opts) 700)
        height (or (:height opts) 400)
        color-scheme (or (:color-scheme opts) "tableau10")
        root (:root treemap-data)
        flat-data (when root (vec (flatten-treemap-node root)))]
    {:$schema "https://vega.github.io/schema/vega/v5.json"
     :width width
     :height height

     :data [{:name "tree"
             :values (or flat-data [])
             :transform
             [{:type "stratify"
               :key "id"
               :parentKey "parent"}
              {:type "treemap"
               :field "value"
               :sort {:field "value" :order "descending"}
               :method "squarify"
               :ratio 1.6
               :size [{:signal "width"} {:signal "height"}]
               :as ["x0" "y0" "x1" "y1" "depth" "children"]}]}
            {:name "nodes"
             :source "tree"
             :transform [{:type "filter"
                          :expr "datum.children"}]}
            {:name "leaves"
             :source "tree"
             :transform [{:type "filter"
                          :expr "!datum.children"}]}]

     :scales [{:name "color"
               :type "ordinal"
               :domain {:data "nodes"
                        :field "name"
                        :sort true}
               :range {:scheme color-scheme}}]

     :marks [;; Parent category rectangles (colored background)
             {:type "rect"
              :from {:data "nodes"}
              :encode
              {:enter
               {:fill {:scale "color" :field "name"}}
               :update
               {:x {:field "x0"}
                :y {:field "y0"}
                :x2 {:field "x1"}
                :y2 {:field "y1"}}}}
             ;; Leaf rectangles (white stroke, interactive with tooltip)
             {:type "rect"
              :from {:data "leaves"}
              :encode
              {:enter
               {:stroke {:value "#fff"}
                :strokeWidth {:value 1}}
               :update
               {:x {:field "x0"}
                :y {:field "y0"}
                :x2 {:field "x1"}
                :y2 {:field "y1"}
                :fill {:value "transparent"}
                :tooltip
                {:signal
                 (str "{'Type': datum.name, "
                      "'Bytes': format(datum.bytes, '~s'), "
                      "'Count': datum.count, "
                      "'Bytes/Alloc': format(datum.bytes / datum.count, '.1f'), "
                      "'Freed %': format(datum['freed-count'] / datum.count, '.1%'), "
                      "'Path': replace(replace(datum.id, /^[^/]+\\//, ''), /\\/[^/]+$/, '')}")}}
               :hover
               {:fill {:value "rgba(0,0,0,0.1)"}}}}]}))
