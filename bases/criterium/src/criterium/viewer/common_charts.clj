(ns criterium.viewer.common-charts
  "Common Vega-Lite chart generation functions for viewers.

  Provides reusable chart-building functions extracted from the Portal viewer
  for generating histograms, scatter plots, percentile charts, and treemaps."
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.primitive-fn :as prim]
   [criterium.stats.interface :as si]
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
        samples (have some?
                      (metric->values path)
                      {:path path :available (keys metric->values)})
        outlier-map (:outliers (get-in outliers path))
        data (arr/indexed-dfold
              samples
              (fn [acc ^long idx ^double val]
                (conj acc
                      {:index idx
                       :outlier (get outlier-map idx "")
                       k (util/transform-sample-> val transforms)}))
              [])]
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
           v (arr/get-at (get events path) index)]
       (if (pos? (long v))
         (assoc res (core/composite-key path) v :index index)
         res)))
   nil
   metrics))

(defn event-layer
  "Build an event marker layer showing when events occurred during sampling.

  Returns a vector containing the Vega-Lite layer spec, or nil if no events."
  [events [_k metrics]]
  (when-let [arr (get events (:path (first (:values metrics))))]
    (let [data (->> (map
                     (partial event-occurrence events (:values metrics))
                     (range (arr/length arr)))
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
                 :strokeDash [2 2]}}]))))

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
                              (filterv #(arr/lany?
                                         (get event-metric->values (:path %))
                                         (fn [^long x] (pos? x)))))

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
        samples (metric->values path)
        ;; Transform and sort samples
        transformed (arr/dmap samples (fn ^double [^double x]
                                        (util/transform-sample-> x transforms)))
        sorted-arr (arr/sorted transformed)
        n (arr/length sorted-arr)
        max-val (Math/log10 n)
        delta (/ 100.0 (dec n))
        ;; Build data directly from typed array
        data (arr/indexed-dfold
              sorted-arr
              (fn [acc ^long i ^double v]
                (let [x (/ (- max-val (Math/log10 (- n i))) max-val)
                      p (* delta i)]
                  (conj acc {k v :p p :x x})))
              [])]
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
        sorted-arr (arr/sorted (get samples path))
        min-v (arr/first-double sorted-arr)
        ;; Build diffs directly from typed array
        diffs (-> (arr/dfold
                   sorted-arr
                   (fn [acc ^double v]
                     (conj acc (- v min-v)))
                   [])
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
  "Build CI box layer for box plot (rect from ciLower to ciUpper).
  Includes stroke styling for visibility when CI is tight."
  [data]
  {:data {:values data}
   :mark {:type "bar" :width 20 :stroke "#333" :strokeWidth 1}
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

;;; Distribution PDF overlay charts

(def ^:private distribution-order
  "Canonical ordering of distributions for consistent color assignment."
  [:gamma :lognormal :inverse-gaussian :weibull])

(def ^:private distribution-colors
  "Color palette for fitted distributions.
  Colors chosen to avoid conflict with histogram blue (#4682b4 steelblue)."
  {:gamma "#e41a1c"           ; red
   :lognormal "#ff7f00"       ; orange (was blue, conflicted with histogram)
   :inverse-gaussian "#4daf4a" ; green
   :weibull "#984ea3"})

(def ^:private distribution-labels
  "Human-readable labels for distributions."
  {:gamma "Gamma"
   :lognormal "Log-normal"
   :inverse-gaussian "Inverse Gaussian"
   :weibull "Weibull"})

(def ^:private distribution-color-scale
  "Vega-Lite color scale with domain and range in consistent order."
  {:domain (mapv #(get distribution-labels % (name %)) distribution-order)
   :range (mapv #(get distribution-colors % "#999999") distribution-order)})

(def ^:private pdf-color-scale
  "Vega-Lite color scale for PDF chart including KDE and fitted distributions.
  KDE uses black solid line; fitted distributions use colored dashed lines."
  {:domain (into ["KDE"]
                 (mapv #(get distribution-labels % (name %)) distribution-order))
   :range (into ["#333333"]
                (mapv #(get distribution-colors % "#999999") distribution-order))})

(defn- kde-pdf-layer
  "Build a KDE density curve layer for the distribution PDF chart.
  Uses 'distribution' field to share legend with fitted distributions.
  KDE shown as solid black line to distinguish from dashed fitted PDFs."
  [kde-data metric-config transforms]
  (let [{:keys [grid density]} kde-data
        k (first (:path metric-config))
        field-name (name k)
        data (mapv (fn [log-x d]
                     {field-name (util/transform-sample-> log-x transforms)
                      "pdf-density" d
                      "distribution" "KDE"})
                   grid density)]
    {:data {:values data}
     :mark {:type "line" :strokeWidth 2}
     :encoding {:x {:field field-name :type "quantitative"
                    :scale {:zero false}}
                :y {:field "pdf-density" :type "quantitative"}
                :color {:field "distribution" :type "nominal"
                        :scale pdf-color-scale
                        :legend {:title "Fitted Distributions"
                                 :orient "top-right"}}}}))

;;; Distribution quantile (inverse CDF) functions for Q-Q plots

(defn- weibull-quantile
  "Quantile function for Weibull distribution.
  Q(p) = λ * (-ln(1-p))^(1/k) where k=shape, λ=scale"
  [^double shape ^double scale]
  (fn ^double [^double p]
    (if (<= p 0.0)
      0.0
      (if (>= p 1.0)
        Double/POSITIVE_INFINITY
        (* scale (Math/pow (- (Math/log (- 1.0 p))) (/ 1.0 shape)))))))

(defn- lognormal-quantile
  "Quantile function for log-normal distribution.
  Q(p) = exp(μ + σ * Φ^(-1)(p))"
  [^double mu ^double sigma]
  (fn ^double [^double p]
    (if (<= p 0.0)
      0.0
      (if (>= p 1.0)
        Double/POSITIVE_INFINITY
        (Math/exp (+ mu (* sigma (si/normal-quantile p))))))))

(defn- gamma-quantile
  "Quantile function for gamma distribution using Newton-Raphson inversion.
  Finds x such that gamma-cdf(x) = p."
  [^double shape ^double scale]
  (let [cdf-fn        (si/gamma-cdf shape scale)
        pdf-fn        (si/gamma-pdf shape scale)
        ;; Initial guess using Wilson-Hilferty approximation for large shape
        initial-guess (fn ^double [^double p]
                        (let [z (si/normal-quantile p)]
                          (if (< shape 1.0)
                            ;; For small shape, use median approximation
                            (* scale shape (Math/pow (- 1.0 (/ 1.0 (* 9.0 (max shape 0.1)))) 3.0))
                            ;; Wilson-Hilferty approximation
                            (let [d      (/ 1.0 (* 9.0 shape))
                                  x-norm (- 1.0 d (- (* z (Math/sqrt d))))]
                              (* scale shape (Math/pow (max x-norm 0.01) 3.0))))))]
    (fn ^double [^double p]
      (cond
        (<= p 0.0) 0.0
        (>= p 1.0) Double/POSITIVE_INFINITY
        :else
        ;; Newton-Raphson: x_{n+1} = x_n - (F(x_n) - p) / f(x_n)
        (let [max-iter (long 50)
              tol      1e-10]
          (loop [x    (Math/max (prim/invoke-dd initial-guess p) 1e-10)
                 iter (long 0)]
            (if (>= iter max-iter)
              x
              (let [fx  (prim/invoke-dd cdf-fn x)
                    fpx (prim/invoke-dd pdf-fn x)]
                (if (< fpx 1e-100)
                  x
                  (let [x-new (Math/max (- x (/ (- fx p) fpx)) 1e-10)]
                    (if (< (Math/abs (- x-new x)) (* tol x))
                      x-new
                      (recur x-new (inc iter)))))))))))))

(defn- inverse-gaussian-quantile
  "Quantile function for inverse-gaussian distribution using Newton-Raphson inversion.
  Finds x such that inverse-gaussian-cdf(x) = p."
  [^double mu ^double lambda]
  (let [cdf-fn (si/inverse-gaussian-cdf mu lambda)
        pdf-fn (si/inverse-gaussian-pdf mu lambda)]
    (fn ^double [^double p]
      (cond
        (<= p 0.0) 0.0
        (>= p 1.0) Double/POSITIVE_INFINITY
        :else
        ;; Newton-Raphson with mu as initial guess
        (let [max-iter (long 50)
              tol 1e-10]
          (loop [x mu
                 iter (long 0)]
            (if (>= iter max-iter)
              x
              (let [fx (prim/invoke-dd cdf-fn x)
                    fpx (prim/invoke-dd pdf-fn x)]
                (if (< fpx 1e-100)
                  x
                  (let [x-new (Math/max (- x (/ (- fx p) fpx)) 1e-10)]
                    (if (< (Math/abs (- x-new x)) (* tol x))
                      x-new
                      (recur x-new (inc iter)))))))))))))

(defn- make-quantile-fn
  "Create a quantile (inverse CDF) function for the given distribution and parameters."
  [dist params]
  (case dist
    :gamma (gamma-quantile (:shape params) (:scale params))
    :lognormal (lognormal-quantile (:mu params) (:sigma params))
    :inverse-gaussian (inverse-gaussian-quantile (:mu params) (:lambda params))
    :weibull (weibull-quantile (:shape params) (:scale params))
    nil))

(defn- make-pdf-fn
  "Create a PDF function for the given distribution and parameters."
  [dist params]
  (case dist
    :gamma (si/gamma-pdf (:shape params) (:scale params))
    :lognormal (si/lognormal-pdf (:mu params) (:sigma params))
    :inverse-gaussian (si/inverse-gaussian-pdf (:mu params) (:lambda params))
    :weibull (si/weibull-pdf (:shape params) (:scale params))
    nil))

(defn distribution-pdf-layer
  "Build a PDF curve layer for a single fitted distribution.

  Takes the distribution keyword, fit result, grid (in original units), field-name,
  transforms, scale-by-jacobian? flag, and show-legend? flag.

  The grid should be in original sample units. The field-name should match the
  KDE layer's x-field for proper axis sharing. The transforms are applied to
  x-values for display alignment with the KDE (which may include batch normalization).

  When scale-by-jacobian? is true, the PDF is multiplied by x to convert from
  density-per-original-unit to density-per-log-unit (for overlay on log-transformed KDE).

  Returns a Vega-Lite layer spec or nil if the distribution couldn't be fitted."
  [dist fit-result grid field-name transforms scale-by-jacobian?]
  (when (and (:params fit-result)
             (not (:error fit-result))
             (not (:skipped fit-result)))
    (let [pdf-fn (make-pdf-fn dist (:params fit-result))
          label (get distribution-labels dist (name dist))
          is-best? (= dist (:best-model fit-result))
          data (->> grid
                    (mapv (fn [^double x]
                            (let [p (prim/invoke-dd pdf-fn x)
                                  ;; Scale by Jacobian if KDE is on log-transformed data
                                  ;; Converts density-per-original-unit to density-per-log-unit
                                  scaled-p (if scale-by-jacobian?
                                             (* p x)
                                             p)
                                  ;; Transform x for display to match KDE axis
                                  display-x (util/transform-sample-> x transforms)]
                              {field-name display-x "pdf-density" scaled-p})))
                    ;; Filter out non-finite values that can't be encoded in JSON
                    (filterv (fn [pt]
                               (let [p (get pt "pdf-density")
                                     x (get pt field-name)]
                                 (and (Double/isFinite p) (not (Double/isNaN p))
                                      (Double/isFinite x) (not (Double/isNaN x)))))))]
      {:data {:values data}
       :transform [{:calculate (str "'" label "'") :as "distribution"}]
       :mark {:type "line"
              :strokeWidth (if is-best? 2.5 1.5)
              :strokeDash (if is-best? [1 0] [4 4])}
       :encoding {:x {:field field-name :type "quantitative"
                      :scale {:zero false}}
                  :y {:field "pdf-density" :type "quantitative"}
                  :color {:field "distribution" :type "nominal"
                          :scale pdf-color-scale
                          ;; Legend shown on KDE layer, suppress here
                          :legend false}}})))

(defn distribution-pdf-overlay-layers
  "Build PDF overlay layers for all fitted distributions.

  Takes distribution-fit data for a metric, grid (in original units), field-name,
  transforms, and scale-by-jacobian? flag. When scale-by-jacobian? is true, PDFs
  are scaled by x to convert to density-per-log-unit for overlay on log-transformed KDE.
  The transforms are applied to x-values for display alignment with the KDE.

  Returns a vector of Vega-Lite layer specs for successfully fitted distributions."
  [fit-data grid field-name transforms scale-by-jacobian?]
  (let [distributions (:distributions fit-data)
        best-model (:best-model fit-data)
        dist-keys (vec (keys distributions))]
    (->> dist-keys
         (mapv
          (fn [dist]
            (distribution-pdf-layer
             dist
             (assoc (get distributions dist) :best-model best-model)
             grid
             field-name
             transforms
             scale-by-jacobian?)))
         (filterv some?))))

(defn distribution-pdf-vega-spec
  "Build a complete Vega-Lite spec for KDE with distribution PDF overlays.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions.

  View options:
    :kde-id - Key for KDE data in data-map (default :kde)
    :distribution-fit-id - Key for distribution fit data (default :distribution-fit)
    :histogram-id - Optional key for histogram data to overlay

  Note: The distribution fit is done on original samples (not log-transformed).
  The PDF grid is generated to match the KDE display range, constrained by
  the sample data range. PDF values are scaled by the Jacobian when the
  KDE is on log-transformed data.

  Returns the Vega-Lite spec without viewer-specific wrapping."
  [data-map view chart-options]
  (let [kde-id (or (:kde-id view) :kde)
        distribution-fit-id (or (:distribution-fit-id view) :distribution-fit)
        histogram-id (:histogram-id view)
        kde-map (util/lookup-data data-map kde-id)
        distribution-fit-map (get data-map distribution-fit-id)
        histograms-map (when histogram-id
                         (util/lookup-data data-map histogram-id))
        kdes (:kdes kde-map)
        fits (when distribution-fit-map (:fits distribution-fit-map))
        metrics-defs (-> (:metrics-defs kde-map)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        kde-transforms (util/get-transforms data-map kde-id)
        ;; Check if KDE is on log-transformed data - if so, PDF needs Jacobian scaling
        kde-source-id (:source-id kde-map)
        scale-by-jacobian? (= kde-source-id :log-samples)
        hist-transforms (when histogram-id
                          (util/get-transforms data-map histogram-id))
        ;; Distribution fit transforms differ from KDE transforms when KDE is on log-samples
        ;; KDE: log-space → exp → /batch-size (for log-samples)
        ;; Fit: original samples → /batch-size (no exp since fit uses original samples)
        fit-transforms (when distribution-fit-map
                         (util/get-transforms data-map distribution-fit-id))]
    {:data {:values []}
     :resolve {:scale {:x "independent"
                       :y "independent"
                       :color "independent"}}
     :vconcat
     (mapv
      (fn [metric-config]
        (let [path (:path metric-config)
              k (first path)
              field-name (name k)
              kde-data (get kdes path)
              fit-data (when fits (get fits path))
              histogram (when histograms-map
                          (get (:histograms histograms-map) path))
              ;; Use sample-range from distribution-fit analysis
              ;; The analysis layer filters outliers and provides the range
              [sample-min sample-max] (:sample-range fit-data)
              ;; Generate PDF grid spanning the sample range with slight margin
              pdf-grid (when (and sample-min sample-max
                                  (> (double sample-max) (double sample-min)))
                         (let [range-val (- (double sample-max) (double sample-min))
                               grid-min (max 1e-10 (- (double sample-min) (* 0.05 range-val)))
                               grid-max (+ (double sample-max) (* 0.05 range-val))
                               step (/ (- grid-max grid-min) 200.0)]
                           (vec (range grid-min grid-max step))))]
          (when kde-data
            (merge
             chart-options
             {:resolve {:scale {:x "shared" :y "independent" :color "independent"}}
              :layer
              (cond-> []
                ;; Add histogram bars if available
                histogram
                (conj (metric-computed-histo-layer
                       hist-transforms
                       histogram
                       metric-config
                       0))
                ;; Wrap KDE + distribution layers in a nested group with shared Y-scale
                ;; Note: KDE confidence band is intentionally omitted here.
                ;; It's shown in the plain KDE chart; including it here causes
                ;; scale mismatches with the fitted distribution PDFs.
                true
                (conj {:resolve {:scale {:y "shared"}}
                       :layer
                       (cond-> []
                         ;; Add KDE density curve (solid black line)
                         true
                         (conj (kde-pdf-layer
                                kde-data metric-config kde-transforms))
                         ;; Add distribution PDF overlays using original-unit grid
                         ;; Use same field name as KDE for shared x-axis
                         ;; Scale by Jacobian if KDE is on log-transformed data
                         ;; Apply fit-transforms (not kde-transforms) to x-values since
                         ;; distribution fit uses original samples, not log-samples
                         ;; Only add overlays when both fit-data and pdf-grid exist
                         (and fit-data (seq pdf-grid))
                         (into (distribution-pdf-overlay-layers
                                fit-data
                                pdf-grid
                                field-name
                                fit-transforms
                                scale-by-jacobian?)))}))}))))
      metric-configs)}))

;;; Distribution CDF overlay charts

(defn- make-cdf-fn
  "Create a CDF function for the given distribution and parameters."
  [dist params]
  (case dist
    :gamma (si/gamma-cdf (:shape params) (:scale params))
    :lognormal (si/lognormal-cdf (:mu params) (:sigma params))
    :inverse-gaussian (si/inverse-gaussian-cdf (:mu params) (:lambda params))
    :weibull (si/weibull-cdf (:shape params) (:scale params))
    nil))

(def ^:private cdf-color-scale
  "Vega-Lite color scale for CDF overlay including ECDF and fitted distributions."
  {:domain (into ["ECDF"]
                 (mapv #(get distribution-labels % (name %)) distribution-order))
   :range (into ["#333333"]
                (mapv #(get distribution-colors % "#999999") distribution-order))})

(defn ecdf-layer
  "Build an ECDF (empirical cumulative distribution function) step layer.

  Takes samples and transforms, returns a Vega-Lite layer spec showing
  the empirical CDF as a step function."
  [samples transforms]
  (let [sorted-samples (sort samples)
        n (count sorted-samples)
        ;; ECDF: F_n(x) = (number of samples <= x) / n
        ;; For step function, we need points at each sample value
        data (mapv (fn [i x]
                     {"x" (util/transform-sample-> x transforms)
                      "cdf" (/ (double (inc (long i))) n)})
                   (range n)
                   sorted-samples)]
    {:data {:values data}
     :transform [{:calculate "'ECDF'" :as "distribution"}]
     :mark {:type "line"
            :interpolate "step-after"
            :strokeWidth 2}
     :encoding {:x {:field "x" :type "quantitative"
                    :scale {:zero false}}
                :y {:field "cdf" :type "quantitative"
                    :title "Cumulative Probability"
                    :scale {:domain [0 1]}}
                :color {:field "distribution" :type "nominal"
                        :scale cdf-color-scale
                        :legend {:orient "bottom-right" :title "Distribution"}}}}))

(defn distribution-cdf-layer
  "Build a CDF curve layer for a single fitted distribution.

  Takes the distribution keyword, fit result, x-values grid, and transforms.
  Returns a Vega-Lite layer spec or nil if the distribution couldn't be fitted."
  [dist fit-result grid transforms]
  (when (and (:params fit-result)
             (not (:error fit-result))
             (not (:skipped fit-result)))
    (let [cdf-fn (make-cdf-fn dist (:params fit-result))
          label (get distribution-labels dist (name dist))
          is-best? (= dist (:best-model fit-result))
          data (->> grid
                    (mapv (fn [x]
                            (let [tx (util/transform-sample-> x transforms)
                                  ;; CDF needs to be evaluated at original x
                                  p (cdf-fn x)]
                              {"x" tx "cdf" p})))
                    ;; Filter out non-finite values that can't be encoded in JSON
                    (filterv (fn [pt]
                               (let [p (get pt "cdf")]
                                 (and (Double/isFinite p) (not (Double/isNaN p)))))))]
      {:data {:values data}
       :transform [{:calculate (str "'" label "'") :as "distribution"}]
       :mark {:type "line"
              :strokeWidth (if is-best? 2.5 1.5)
              :strokeDash (if is-best? [1 0] [4 4])}
       :encoding {:x {:field "x" :type "quantitative"
                      :scale {:zero false}}
                  :y {:field "cdf" :type "quantitative"}
                  ;; Use unified color scale that includes ECDF, omit legend
                  ;; since ECDF layer provides the combined legend
                  :color {:field "distribution" :type "nominal"
                          :scale cdf-color-scale
                          :legend nil}}})))

(defn distribution-cdf-overlay-layers
  "Build CDF overlay layers for all fitted distributions.

  Takes distribution-fit data for a metric, x-values grid, and transforms.
  Returns a vector of Vega-Lite layer specs for successfully fitted distributions."
  [fit-data grid transforms]
  (let [distributions (:distributions fit-data)
        best-model (:best-model fit-data)]
    (->> (keys distributions)
         (mapv (fn [dist]
                 (distribution-cdf-layer
                  dist
                  (assoc (get distributions dist) :best-model best-model)
                  grid
                  transforms)))
         (filterv some?))))

(defn distribution-cdf-vega-spec
  "Build a complete Vega-Lite spec for ECDF with distribution CDF overlays.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions.

  View options:
    :samples-id - Key for samples data in data-map (default :samples)
    :distribution-fit-id - Key for distribution fit data (default :distribution-fit)

  The x-axis range is derived from the sample data. Fitted CDFs are overlaid
  on the empirical CDF for visual comparison of goodness-of-fit.

  Returns the Vega-Lite spec without viewer-specific wrapping."
  [data-map view chart-options]
  (let [samples-id (or (:samples-id view) :samples)
        distribution-fit-id (or (:distribution-fit-id view) :distribution-fit)
        samples-map (util/lookup-data data-map samples-id)
        distribution-fit-map (get data-map distribution-fit-id)
        metrics-defs (-> (:metrics-defs samples-map)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        metric->values (util/metric->values samples-map)
        fits (when distribution-fit-map (:fits distribution-fit-map))
        samples-transforms (util/get-transforms data-map samples-id)]
    {:data {:values []}
     :resolve {:scale {:x "independent"
                       :y "shared"
                       :color "shared"}}
     :vconcat
     (mapv
      (fn [metric-config]
        (let [path (:path metric-config)
              samples (get metric->values path)
              fit-data (when fits (get fits path))
              ;; Generate grid from sample range for CDF curves
              sorted-samples (when (seq samples) (sort samples))
              min-val (when sorted-samples (first sorted-samples))
              max-val (when sorted-samples (last sorted-samples))
              ;; Extend range slightly for better visualization
              range-val (when (and min-val max-val)
                          (- (double max-val) (double min-val)))
              grid-min (when range-val (- (double min-val) (* 0.05 (double range-val))))
              grid-max (when range-val (+ (double max-val) (* 0.05 (double range-val))))
              grid (when (and grid-min grid-max)
                     (let [step (/ (- (double grid-max) (double grid-min)) 100.0)]
                       (vec (range grid-min grid-max step))))]
          (when (seq samples)
            (merge
             chart-options
             {:resolve {:scale {:y "shared" :color "shared"}}
              :layer
              (cond-> []
                ;; Add ECDF layer
                true
                (conj (ecdf-layer samples samples-transforms))
                ;; Add distribution CDF overlays
                (and fit-data grid)
                (into (distribution-cdf-overlay-layers
                       fit-data
                       grid
                       samples-transforms)))}))))
      metric-configs)}))

;;; Distribution Q-Q plot charts

(defn qq-points
  "Generate Q-Q plot points comparing sample quantiles to theoretical quantiles.

  For each sample value (ordered), computes:
  - theoretical: the theoretical quantile at probability (i-0.5)/n
  - observed: the actual sample value

  If data follows the theoretical distribution, points should lie along y=x.
  Points with non-finite theoretical values are filtered out.

  Returns a vector of {\"theoretical\" x \"observed\" y} maps."
  [samples quantile-fn transforms]
  (let [sorted-samples (vec (sort samples))
        n (count sorted-samples)]
    (->> (mapv (fn [i x]
                 (let [;; Hazen plotting position: (i - 0.5) / n
                       p (/ (- (double (inc (long i))) 0.5) (double n))
                       theoretical (quantile-fn p)]
                   {"theoretical" (util/transform-sample-> theoretical transforms)
                    "observed" (util/transform-sample-> x transforms)}))
               (range n)
               sorted-samples)
         ;; Filter out non-finite values that can't be encoded in JSON
         (filterv (fn [pt]
                    (let [t (get pt "theoretical")
                          o (get pt "observed")]
                      (and (Double/isFinite t) (not (Double/isNaN t))
                           (Double/isFinite o) (not (Double/isNaN o)))))))))

(defn distribution-qq-layer
  "Build a Q-Q scatter layer for a single fitted distribution.

  Takes the distribution keyword, fit result, samples, and transforms.
  Returns a Vega-Lite layer spec or nil if the distribution couldn't be fitted."
  [dist fit-result samples transforms]
  (when (and (:params fit-result)
             (not (:error fit-result))
             (not (:skipped fit-result)))
    (let [quantile-fn (make-quantile-fn dist (:params fit-result))
          label (get distribution-labels dist (name dist))
          is-best? (= dist (:best-model fit-result))
          data (qq-points samples quantile-fn transforms)]
      {:data {:values data}
       :transform [{:calculate (str "'" label "'") :as "distribution"}]
       :mark {:type "point"
              :size (if is-best? 60 40)
              :filled is-best?
              :opacity (if is-best? 0.8 0.5)}
       :encoding {:x {:field "theoretical" :type "quantitative"
                      :title "Theoretical Quantiles"
                      :scale {:zero false}}
                  :y {:field "observed" :type "quantitative"
                      :title "Sample Quantiles"
                      :scale {:zero false}}
                  :color {:field "distribution" :type "nominal"
                          :scale distribution-color-scale
                          :legend {:orient "top-right" :title "Fitted Distributions"}}
                  :tooltip [{:field "theoretical" :type "quantitative"
                             :title "Theoretical" :format ".4g"}
                            {:field "observed" :type "quantitative"
                             :title "Observed" :format ".4g"}]}})))

(defn qq-reference-line-layer
  "Build a y=x reference line layer for Q-Q plots.

  Takes the min and max values from the data range to draw the diagonal.
  Points lying on this line indicate perfect fit to the distribution."
  [^double min-val ^double max-val]
  (let [;; Extend range slightly for visual clarity
        margin (* 0.05 (- max-val min-val))
        start (- min-val margin)
        end (+ max-val margin)]
    {:data {:values [{"x" start "y" start}
                     {"x" end "y" end}]}
     :mark {:type "line"
            :strokeDash [4 4]
            :strokeWidth 1.5
            :color "#666666"}
     :encoding {:x {:field "x" :type "quantitative"}
                :y {:field "y" :type "quantitative"}}}))

(defn distribution-qq-overlay-layers
  "Build Q-Q overlay layers for all fitted distributions.

  Takes distribution-fit data for a metric, samples, and transforms.
  Returns a vector of Vega-Lite layer specs for successfully fitted distributions."
  [fit-data samples transforms]
  (let [distributions (:distributions fit-data)
        best-model (:best-model fit-data)]
    (->> (keys distributions)
         (mapv (fn [dist]
                 (distribution-qq-layer
                  dist
                  (assoc (get distributions dist) :best-model best-model)
                  samples
                  transforms)))
         (filterv some?))))

(defn- qq-subplot-spec
  "Build a single Q-Q subplot for one distribution.

  Returns a Vega-Lite spec with reference line and scatter points for the
  given distribution. Axes domain is computed from both theoretical and
  observed values to ensure all Q-Q points are visible."
  [dist fit-result samples transforms _observed-range subplot-options]
  (when (and (:params fit-result)
             (not (:error fit-result))
             (not (:skipped fit-result)))
    (let [quantile-fn (make-quantile-fn dist (:params fit-result))
          label (get distribution-labels dist (name dist))
          is-best? (= dist (:best-model fit-result))
          color (get distribution-colors dist "#999999")
          data (qq-points samples quantile-fn transforms)
          ;; Compute domain from both theoretical and observed values
          ;; to ensure all points are visible within the axes
          all-values (into (mapv #(get % "theoretical") data)
                           (mapv #(get % "observed") data))
          ^double min-val (apply min all-values)
          ^double max-val (apply max all-values)
          margin (* 0.05 (- max-val min-val))
          domain-min (- min-val margin)
          domain-max (+ max-val margin)]
      (merge
       subplot-options
       {:title {:text label
                :color (if is-best? color "#666666")
                :fontWeight (if is-best? "bold" "normal")}
        :layer
        [;; Reference line (y=x diagonal)
         {:data {:values [{"x" domain-min "y" domain-min}
                          {"x" domain-max "y" domain-max}]}
          :mark {:type "line"
                 :strokeDash [4 4]
                 :strokeWidth 1.5
                 :color "#999999"}
          :encoding {:x {:field "x" :type "quantitative"
                         :scale {:domain [domain-min domain-max]}}
                     :y {:field "y" :type "quantitative"
                         :scale {:domain [domain-min domain-max]}}}}
         ;; Q-Q scatter points
         {:data {:values data}
          :mark {:type "point"
                 :size 50
                 :filled true
                 :opacity 0.7
                 :color color}
          :encoding {:x {:field "theoretical" :type "quantitative"
                         :title "Theoretical Quantiles"
                         :scale {:domain [domain-min domain-max]}}
                     :y {:field "observed" :type "quantitative"
                         :title "Sample Quantiles"
                         :scale {:domain [domain-min domain-max]}}
                     :tooltip [{:field "theoretical" :type "quantitative"
                                :title "Theoretical" :format ".4g"}
                               {:field "observed" :type "quantitative"
                                :title "Observed" :format ".4g"}]}}]}))))

(defn distribution-qq-vega-spec
  "Build a complete Vega-Lite spec for Q-Q plots with separate subplots per distribution.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions.

  View options:
    :samples-id - Key for samples data in data-map (default :samples)
    :distribution-fit-id - Key for distribution fit data (default :distribution-fit)

  Each fitted distribution gets its own subplot. If data follows the distribution,
  points lie along the y=x reference line. The best-fit model has bold title.
  Subplots are arranged in a 2-column grid.

  Returns the Vega-Lite spec without viewer-specific wrapping."
  [data-map view chart-options]
  (let [samples-id (or (:samples-id view) :samples)
        distribution-fit-id (or (:distribution-fit-id view) :distribution-fit)
        samples-map (util/lookup-data data-map samples-id)
        distribution-fit-map (get data-map distribution-fit-id)
        metrics-defs (-> (:metrics-defs samples-map)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        metric->values (util/metric->values samples-map)
        fits (when distribution-fit-map (:fits distribution-fit-map))
        samples-transforms (util/get-transforms data-map samples-id)
        ;; Subplot dimensions - smaller since we have multiple
        subplot-width (or (:subplot-width chart-options)
                          (quot (long (or (:width chart-options) 400)) 2))
        subplot-height (or (:subplot-height chart-options)
                           (quot (long (or (:height chart-options) 300)) 2))
        subplot-options {:width subplot-width :height subplot-height}]
    {:data {:values []}
     :vconcat
     (mapv
      (fn [metric-config]
        (let [path (:path metric-config)
              samples (get metric->values path)
              fit-data (when fits (get fits path))]
          (when (and (seq samples) fit-data)
            ;; Compute observed range for consistent axes across subplots
            (let [sorted-samples (sort samples)
                  transformed-samples (mapv #(util/transform-sample-> % samples-transforms)
                                            sorted-samples)
                  min-val (apply min transformed-samples)
                  max-val (apply max transformed-samples)
                  observed-range [min-val max-val]
                  distributions (:distributions fit-data)
                  best-model (:best-model fit-data)
                  ;; Generate subplots for each distribution
                  subplots (->> distribution-order
                                (filter #(contains? distributions %))
                                (mapv (fn [dist]
                                        (qq-subplot-spec
                                         dist
                                         (assoc (get distributions dist)
                                                :best-model best-model)
                                         samples
                                         samples-transforms
                                         observed-range
                                         subplot-options)))
                                (filterv some?))]
              ;; Arrange in 2-column grid using vconcat of hconcat rows
              (when (seq subplots)
                {:vconcat
                 (->> subplots
                      (partition-all 2)
                      (mapv (fn [row] {:hconcat (vec row)})))})))))
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

;;; Call Tree Visualizations

(defn- clojure-invoke-method?
  "Check if a method name is a Clojure function invocation method."
  [method]
  (contains? #{"invoke" "invokeStatic" "invokePrim" "doInvoke"} method))

(defn- extract-clojure-fn-name
  "Extract Clojure function name from class name like 'myns.core$my_fn'.
  Returns the part after the last $ converted from underscores to hyphens."
  [class-name]
  (when class-name
    (when-let [idx (str/last-index-of class-name "$")]
      (-> (subs class-name (inc (long idx)))
          (str/replace "_" "-")
          (str/replace "BANG" "!")
          (str/replace "QMARK" "?")
          (str/replace "STAR" "*")
          (str/replace "PLUS" "+")
          (str/replace "GT" ">")
          (str/replace "LT" "<")
          (str/replace "EQ" "=")))))

(defn- extract-simple-class-name
  "Extract simple class name without package prefix.
  'java.util.HashMap' -> 'HashMap'"
  [class-name]
  (when class-name
    (if-let [idx (str/last-index-of class-name ".")]
      (subs class-name (inc (long idx)))
      class-name)))

(defn- call-tree-display-name
  "Get display name for a call tree node.
  For Clojure invoke methods, shows the function name extracted from the class.
  For Java methods, shows SimpleClassName.method (without package)."
  [class-name method]
  (if (and (clojure-invoke-method? method)
           (str/includes? (or class-name "") "$"))
    (or (extract-clojure-fn-name class-name)
        (str class-name "." method))
    (str (extract-simple-class-name (or class-name "<unknown>"))
         "."
         (or method "<unknown>"))))

(defn- flatten-call-tree-node
  "Flatten a hierarchical call tree node into a sequence of flat records.
  Each record has :id, :parent, :name, :call-count keys for use with Vega stratify.
  Unlike treemap, all nodes get call-count since we're showing the call hierarchy."
  ([node] (flatten-call-tree-node node nil []))
  ([node parent-id path]
   (when node
     (let [class-name (or (:class node) "<unknown>")
           method (or (:method node) "<unknown>")
           display-name (call-tree-display-name class-name method)
           node-name (str class-name "." method)
           node-id (if (empty? path)
                     "root"
                     (str/join "/" (conj path node-name)))
           current-path (conj path node-name)
           children (:children node)
           call-count (or (:call-count node) 0)
           node-record {:id node-id
                        :parent parent-id
                        :name display-name
                        :class class-name
                        :method method
                        :file (:file node)
                        :line (:line node)
                        :call-count call-count
                        :depth (count path)}]
       (if (seq children)
         (cons node-record
               (mapcat #(flatten-call-tree-node % node-id current-path) children))
         [node-record])))))

(defn call-tree-tree-vega-spec
  "Build a Vega spec for hierarchical tree visualization of call graph.

  Displays the call tree as a top-down tree diagram where node size represents
  call count. Uses Vega's tree layout for proper hierarchical positioning.

  Parameters:
    call-tree - The call tree map with :class, :method, :call-count, :children
    opts - Display options:
      :width (default 700)
      :height (default 500)

  Returns a full Vega spec with:
    - stratify transform to build hierarchy
    - tree layout for positioning
    - symbol marks sized by call count
    - link paths connecting parent to children
    - tooltip on hover showing method, call count, file:line"
  [call-tree opts]
  (let [width (or (:width opts) 700)
        height (or (:height opts) 500)
        flat-data (when call-tree (vec (flatten-call-tree-node call-tree)))
        max-calls (long (if (seq flat-data)
                          (apply max (map :call-count flat-data))
                          1))]
    {:$schema "https://vega.github.io/schema/vega/v5.json"
     :width width
     :height height
     :padding 5

     :data [{:name "tree"
             :values (or flat-data [])
             :transform
             [{:type "stratify"
               :key "id"
               :parentKey "parent"}
              {:type "tree"
               :method "tidy"
               :size [{:signal "width - 100"} {:signal "height - 100"}]
               :separation true
               :as ["x" "y" "depth" "children"]}
              ;; Offset to center in available space
              {:type "formula" :as "x" :expr "datum.x + 50"}
              {:type "formula" :as "y" :expr "datum.y + 50"}]}
            {:name "links"
             :source "tree"
             :transform
             [{:type "treelinks"}
              {:type "linkpath"
               :orient "vertical"
               :shape "diagonal"}]}]

     :scales [{:name "color"
               :type "ordinal"
               :domain {:data "tree" :field "class" :sort true}
               :range {:scheme "category20"}}
              {:name "size"
               :type "sqrt"
               :domain [1 max-calls]
               :range [100 2000]}]

     :marks [;; Links between nodes
             {:type "path"
              :from {:data "links"}
              :encode
              {:update
               {:path {:field "path"}
                :stroke {:value "#ccc"}
                :strokeWidth {:value 1.5}}}}
             ;; Nodes
             {:type "symbol"
              :from {:data "tree"}
              :encode
              {:enter
               {:size {:scale "size" :field "call-count"}
                :fill {:scale "color" :field "class"}
                :stroke {:value "#fff"}
                :strokeWidth {:value 1}}
               :update
               {:x {:field "x"}
                :y {:field "y"}
                :tooltip
                {:signal
                 (str "{"
                      "'Function': datum.name, "
                      "'Full': datum.class + '.' + datum.method, "
                      "'Calls': datum['call-count'], "
                      "'Location': datum.file ? (datum.file + ':' + datum.line) : 'unknown'"
                      "}")}}}}
             ;; Labels for nodes with high call counts
             {:type "text"
              :from {:data "tree"}
              :encode
              {:enter
               {:font {:value "sans-serif"}
                :fontSize {:value 10}
                :align {:value "center"}
                :baseline {:value "bottom"}}
               :update
               {:x {:field "x"}
                :y {:field "y" :offset -5}
                :text {:signal (str "datum['call-count'] > "
                                    (/ max-calls 10)
                                    " ? datum.name : ''")}
                :fillOpacity {:value 0.8}}}}]}))

(defn call-tree-flame-vega-spec
  "Build a Vega spec for flame chart visualization of call graph.

  Displays the call tree as a flame chart where horizontal width represents
  call count (not time). Each level shows methods called, with children
  stacked below their parent.

  Parameters:
    call-tree - The call tree map with :class, :method, :call-count, :children
    total-calls - Total call count for percentage calculation
    opts - Display options:
      :width (default 700)
      :height (default 400)

  Returns a full Vega spec with:
    - Custom flame layout computed in Clojure
    - rect marks with width proportional to call count
    - Color by class for visual grouping
    - tooltip showing method, call count, percentage"
  [call-tree total-calls opts]
  (let [width (long (or (:width opts) 700))
        height (long (or (:height opts) 400))
        width-d (double width)
        height-d (double height)
        row-height (double 24)
        total-calls-d (double (if (pos? (long total-calls)) total-calls 1))]
    ;; Type hints eliminate boxed math warnings in this tight loop
    (letfn [(compute-flame-data
              [node ^double x0 ^double node-width ^long depth]
              (when node
                (let [call-count (long (or (:call-count node) 0))
                      class-name (or (:class node) "<unknown>")
                      method (or (:method node) "<unknown>")
                      display-name (call-tree-display-name class-name method)
                      percentage (* 100.0 (/ (double call-count) total-calls-d))
                      x1 (+ x0 node-width)
                      node-record {:name display-name
                                   :class class-name
                                   :method method
                                   :file (:file node)
                                   :line (:line node)
                                   :call-count call-count
                                   :percentage percentage
                                   :depth depth
                                   :x0 x0
                                   :x1 x1
                                   :y0 (* (double depth) row-height)
                                   :y1 (* (double (inc depth)) row-height)}
                      children (:children node)
                      ;; Children are sized proportionally within parent's width
                      children-total (double
                                      (reduce + 0 (map #(or (:call-count %) 0) children)))]
                  (if (seq children)
                    (let [child-data
                          (loop [remaining children
                                 child-x x0
                                 acc []]
                            (if (empty? remaining)
                              acc
                              (let [child (first remaining)
                                    child-count (double (or (:call-count child) 0))
                                    child-width (if (pos? children-total)
                                                  (* node-width (/ child-count children-total))
                                                  0.0)
                                    child-results (compute-flame-data
                                                   child child-x child-width (inc depth))]
                                (recur (rest remaining)
                                       (+ child-x child-width)
                                       (into acc child-results)))))]
                      (cons node-record child-data))
                    [node-record]))))]
      (let [flame-data (when call-tree
                         (vec (compute-flame-data call-tree 0.0 width-d 0)))
            max-depth (long (if (seq flame-data)
                              (apply max (map :depth flame-data))
                              0))
            computed-height (long (Math/max height-d (* (double (inc max-depth)) row-height 1.2)))]
        {:$schema "https://vega.github.io/schema/vega/v5.json"
         :width width
         :height computed-height
         :padding 5

         :data [{:name "flame"
                 :values (or flame-data [])}]

         :scales [{:name "color"
                   :type "ordinal"
                   :domain {:data "flame" :field "class" :sort true}
                   :range {:scheme "category20"}}]

         :marks [{:type "rect"
                  :from {:data "flame"}
                  :encode
                  {:enter
                   {:stroke {:value "#fff"}
                    :strokeWidth {:value 0.5}}
                   :update
                   {:x {:field "x0"}
                    :x2 {:field "x1"}
                    :y {:field "y0"}
                    :y2 {:field "y1"}
                    :fill {:scale "color" :field "class"}
                    :tooltip
                    {:signal
                     (str "{"
                          "'Function': datum.name, "
                          "'Full': datum.class + '.' + datum.method, "
                          "'Calls': datum['call-count'], "
                          "'Percentage': format(datum.percentage, '.1f') + '%', "
                          "'Location': datum.file ? (datum.file + ':' + datum.line) : 'unknown'"
                          "}")}}
                   :hover
                   {:fill {:value "#ff6600"}}}}
                 ;; Labels for wider bars
                 {:type "text"
                  :from {:data "flame"}
                  :encode
                  {:enter
                   {:font {:value "sans-serif"}
                    :fontSize {:value 10}
                    :align {:value "left"}
                    :baseline {:value "middle"}
                    :fill {:value "#000"}}
                   :update
                   {:x {:signal "datum.x0 + 2"}
                    :y {:signal "(datum.y0 + datum.y1) / 2"}
                    ;; Only show text if bar is wide enough
                    :text {:signal "(datum.x1 - datum.x0) > 60 ? datum.name : ''"}
                    :limit {:signal "datum.x1 - datum.x0 - 4"}}}}]}))))

;;; Most-Called Bar Chart

(defn- most-called-display-name
  "Get display name for a most-called entry.
  For Clojure invoke methods, shows the function name.
  For Java methods, shows class.method."
  [class-name method]
  (if (and (clojure-invoke-method? method)
           (str/includes? (or class-name "") "$"))
    (or (extract-clojure-fn-name class-name)
        (str class-name "." method))
    (str (or class-name "<unknown>") "." (or method "<unknown>"))))

(defn most-called-vega-lite-spec
  "Build a Vega-Lite horizontal bar chart spec for most-called methods.

  Parameters:
    most-called-data - The :most-called analysis result map
    opts - Display options:
      :width (default 600)
      :height (default: computed from number of items)

  Returns a Vega-Lite spec with:
    - Horizontal bars showing call counts
    - Methods sorted by call count descending
    - Tooltip showing full class.method, calls, and location"
  [most-called-data opts]
  (let [width (or (:width opts) 600)
        methods (:most-called most-called-data)
        n (count methods)
        bar-height 20
        height (or (:height opts) (+ 50 (* n bar-height)))
        data (mapv (fn [{:keys [class method file line total-calls]}]
                     (let [display-name (most-called-display-name class method)
                           location (if (and file (pos? (long (or line 0))))
                                      (str file ":" line)
                                      "")]
                       {"method" display-name
                        "fullName" (str class "." method)
                        "calls" total-calls
                        "location" location}))
                   methods)]
    {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
     :width width
     :height height
     :data {:values data}
     :mark {:type "bar"}
     :encoding {:y {:field "method"
                    :type "nominal"
                    :title "Method"
                    :sort {:field "calls" :order "descending"}
                    :axis {:labelLimit 300}}
                :x {:field "calls"
                    :type "quantitative"
                    :title "Call Count"}
                :color {:field "calls"
                        :type "quantitative"
                        :scale {:scheme "blues"}
                        :legend nil}
                :tooltip [{:field "fullName" :type "nominal" :title "Full Name"}
                          {:field "calls" :type "quantitative" :title "Calls"}
                          {:field "location" :type "nominal" :title "Location"}]}}))
