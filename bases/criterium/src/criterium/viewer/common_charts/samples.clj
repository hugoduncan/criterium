(ns criterium.viewer.common-charts.samples
  "Sample visualization functions for Vega-Lite charts.

  Provides scatter plots, histograms, event markers, and sample difference
  visualizations for criterium benchmark data."
  (:require
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.probability :as probability]
   [criterium.viewer.common.core :as core]))

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
