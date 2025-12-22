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
   [criterium.viewer.common :as viewer-common]))

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
         (assoc res (viewer-common/composite-key path) v :index index)
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
                                        (viewer-common/composite-key (:path %)))
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
        quant-samples-id (or (:samples-id view) :samples)
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
                    (when stats
                      (->>
                       (metric-sample-stats-layer
                        stats-transforms
                        (get-in (util/stats stats) (:path metric-config))
                        metric-config
                        (vswap!
                         layer-num
                         (fn [^long x] (unchecked-inc x)))))))}))
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
  "Build loess smoothing layer for residual plot."
  [residual-pts {:keys [color-field]}]
  {:data {:values (vec residual-pts)}
   :transform [{:loess "residual"
                :on "x"
                :groupby [color-field]
                :bandwidth 0.3}]
   :mark {:type "line" :strokeWidth 1}
   :encoding {:x {:field "x" :type "quantitative"}
              :y {:field "residual" :type "quantitative"}
              :color {:field color-field :type "nominal" :legend nil}
              :opacity {:value 0.4}}})

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

;;; Treemap charts

(defn- flatten-treemap-node
  "Flatten a hierarchical treemap node into a sequence of flat records.
  Each record has :id, :parent, and :name keys for use with Vega stratify.
  Only leaf nodes get :value - parent sizes are computed by Vega's treemap transform."
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
                       ;; Only set value on leaf nodes
                       (not children) (assoc :value (:value node 0)))]
     (if children
       (cons node-record
             (mapcat #(flatten-treemap-node % node-id current-path) children))
       [node-record]))))

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
    - text labels for cells above size threshold
    - color by first-level category
    - tooltip showing name, value (formatted bytes), path"
  [treemap-data opts]
  (let [width (or (:width opts) 700)
        height (or (:height opts) 400)
        color-scheme (or (:color-scheme opts) "tableau10")
        root (:root treemap-data)
        flat-data (when root (flatten-treemap-node root))
        size-by (or (:size-by treemap-data) :bytes)
        value-format (if (= size-by :count) "d" "~s")]
    {:$schema "https://vega.github.io/schema/vega/v5.json"
     :width width
     :height height
     :padding 2
     :autosize "none"

     :data [{:name "tree"
             :values (or flat-data [])
             :transform
             [{:type "stratify"
               :key "id"
               :parentKey "parent"}
              {:type "treemap"
               :field "value"
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
             ;; Leaf rectangles (white stroke, interactive)
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
                 (str "{'Name': datum.name, "
                      "'Value': format(datum.value, '" value-format "'), "
                      "'Path': datum.id}")}}
               :hover
               {:fill {:value "rgba(0,0,0,0.1)"}}}}
             ;; Text labels for larger cells
             {:type "text"
              :from {:data "leaves"}
              :encode
              {:enter
               {:font {:value "Helvetica Neue, Arial"}
                :align {:value "center"}
                :baseline {:value "middle"}
                :fill {:value "#000"}
                :fontSize {:value 10}}
               :update
               {:x {:signal "(datum.x0 + datum.x1) / 2"}
                :y {:signal "(datum.y0 + datum.y1) / 2"}
                :text {:field "name"}
                :opacity
                {:signal
                 "(datum.x1 - datum.x0) > 40 && (datum.y1 - datum.y0) > 20 ? 1 : 0"}}}}]}))
