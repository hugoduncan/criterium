(ns criterium.viewer.common-charts
  "Common Vega-Lite chart generation functions for viewers.

  Provides reusable chart-building functions extracted from the Portal viewer
  for generating histograms, scatter plots, percentile charts, and treemaps."
  (:require
   [clojure.string :as str]
   [criterium.util.invariant :refer [have]]
   [criterium.viewer.common-charts.comparison :as comparison]
   [criterium.viewer.common-charts.distribution :as distribution]
   [criterium.viewer.common-charts.quantile :as quantile]
   [criterium.viewer.common-charts.samples :as samples]))

;;; Sample visualization re-exports (moved to common-charts.samples)

(def metric-layer
  "Build a scatter plot layer showing samples with outlier coloring.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-layer)

(def metric-computed-histo-layer
  "Build a histogram bar chart layer from pre-computed histogram data.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-computed-histo-layer)

(def normal-pdf-points
  "Generate points for a normal probability density function curve.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/normal-pdf-points)

(def metric-sample-stats-layer
  "Build a normal distribution overlay layer for histogram charts.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-sample-stats-layer)

(def metric-bootstrap-boxplot-layer
  "Build a boxplot-style layer showing median CI and spread percentiles.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-bootstrap-boxplot-layer)

(def event-occurrence
  "Extract event occurrence data for a single sample index.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/event-occurrence)

(def event-layer
  "Build an event marker layer showing when events occurred during sampling.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/event-layer)

(def samples-vega-spec
  "Build a complete Vega-Lite spec for sample visualization.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/samples-vega-spec)

(def histogram-vega-spec
  "Build a complete Vega-Lite spec for histogram visualization.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/histogram-vega-spec)

(def metric-percentile-layer
  "Build a percentile distribution chart layer.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-percentile-layer)

(def metric-diff-layer
  "Build a sample differences chart layer.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-diff-layer)

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
    :legend-options - legend config map or nil for default

  When color-field is \"model\", adds a \"series\" field with value \"data\" to
  each point since data points don't have model labels (only fit lines do)."
  [points {:keys [axis-name y-title color-field color-value legend-options]
           :or {color-value "steelblue"}}]
  (let [;; For single-impl mode (color-field="model"), data points need a series
        ;; label since they don't have individual model values like fit lines do
        single-impl-model-mode? (= color-field "model")
        labeled-points (if single-impl-model-mode?
                         (mapv #(assoc % "series" "data") points)
                         points)
        actual-color-field (if single-impl-model-mode? "series" color-field)
        base-tooltip [{:field "x"
                       :type "quantitative"
                       :title axis-name
                       :format ".4g"}
                      {:field "y"
                       :type "quantitative"
                       :title y-title
                       :format ".4g"}]
        tooltip (if actual-color-field
                  (into [{:field actual-color-field
                          :type "nominal"
                          :title (case actual-color-field
                                   "impl" "Implementation"
                                   "series" "Series"
                                   "Model")}]
                        base-tooltip)
                  base-tooltip)]
    {:data {:values labeled-points}
     :mark {:type "point" :size 60}
     :encoding (cond-> {:x {:field "x" :type "quantitative" :title axis-name}
                        :y {:field "y" :type "quantitative" :title y-title}
                        :tooltip tooltip}
                 actual-color-field
                 (assoc :color {:field actual-color-field :type "nominal"
                                :legend (merge {:title (case actual-color-field
                                                         "impl" "Implementation"
                                                         "series" "Series"
                                                         "Model")}
                                               legend-options)})
                 (not actual-color-field)
                 (assoc :color {:value color-value}))}))

(defn regression-error-layer
  "Build error bar layer for regression points with error bounds.
  When color-field is \"model\", adds a \"series\" field with value \"data\" to
  match the scatter layer behavior."
  [points {:keys [color-field color-value]
           :or {color-value "steelblue"}}]
  (let [single-impl-model-mode? (= color-field "model")
        labeled-points (if single-impl-model-mode?
                         (mapv #(assoc % "series" "data") points)
                         points)
        actual-color-field (if single-impl-model-mode? "series" color-field)]
    {:data {:values labeled-points}
     :mark {:type "rule" :strokeWidth 1.5}
     :encoding (cond-> {:x {:field "x" :type "quantitative"}
                        :y {:field "yLower" :type "quantitative"}
                        :y2 {:field "yUpper"}
                        :opacity {:value 0.5}}
                 actual-color-field
                 (assoc :color {:field actual-color-field :type "nominal" :legend nil})
                 (not actual-color-field)
                 (assoc :color {:value color-value}))}))

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
  Points are in log space: x = log(n), y = log(metric).
  Options:
    :axis-name - name of the x-axis variable (e.g., 'n')
    :metric-name - name of the metric being plotted (e.g., 'elapsed-time')"
  [points {:keys [axis-name metric-name color-field color-value legend-options]}]
  (have metric-name)
  {:data {:values (vec points)}
   :mark {:type "point" :size 60 :filled true}
   :encoding (cond-> {:x {:field "x"
                          :type "quantitative"
                          :title (str "log(" axis-name ")")}
                      :y {:field "y"
                          :type "quantitative"
                          :title (str "log(" metric-name ")")}}
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
    :metric-name - name of the metric being plotted (e.g., 'elapsed-time')
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

;;; Comparison chart re-exports (moved to common-charts.comparison)

(def prepare-single-point-box-data
  "Prepare data for single-point box plot from domain extract.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/prepare-single-point-box-data)

(def prepare-single-point-bar-data
  "Prepare data for single-point bar chart from domain extract.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/prepare-single-point-bar-data)

(def single-point-bar-chart-spec
  "Build a Vega-Lite bar chart spec for single-point multi-impl comparison.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/single-point-bar-chart-spec)

(def single-point-box-chart-spec
  "Build a Vega-Lite box plot spec for single-point multi-impl comparison.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/single-point-box-chart-spec)

(def comparison-bar-chart-spec
  "Build a Vega-Lite bar chart spec for single-point comparison data.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/comparison-bar-chart-spec)

(def comparison-box-chart-spec
  "Build a Vega-Lite box plot spec for single-point comparison data.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/comparison-box-chart-spec)

(def domain-line-chart-spec
  "Build a Vega-Lite line chart spec for single-axis multi-point domain extract.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/domain-line-chart-spec)

(def comparison-line-chart-spec
  "Build a Vega-Lite line chart spec for single-axis multi-point comparison data.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/comparison-line-chart-spec)

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

;;; KDE and Distribution re-exports (moved to common-charts.distribution)

(def kde-density-layer
  "Build a density curve layer for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-density-layer)

(def kde-confidence-band-layer
  "Build a confidence band area layer for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-confidence-band-layer)

(def kde-modes-layer
  "Build mode marker layers for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-modes-layer)

(def kde-vega-spec
  "Build a complete Vega-Lite spec for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-vega-spec)

;;; Distribution PDF overlay re-exports (moved to common-charts.distribution)

(def distribution-pdf-layer
  "Build a PDF curve layer for a single fitted distribution.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-pdf-layer)

(def distribution-pdf-overlay-layers
  "Build PDF overlay layers for all fitted distributions.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-pdf-overlay-layers)

(def distribution-pdf-vega-spec
  "Build a complete Vega-Lite spec for KDE with distribution PDF overlays.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-pdf-vega-spec)

;;; Distribution CDF overlay re-exports (moved to common-charts.distribution)

(def ecdf-layer
  "Build an ECDF (empirical cumulative distribution function) step layer.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/ecdf-layer)

(def distribution-cdf-layer
  "Build a CDF curve layer for a single fitted distribution.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-cdf-layer)

(def distribution-cdf-overlay-layers
  "Build CDF overlay layers for all fitted distributions.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-cdf-overlay-layers)

(def distribution-cdf-vega-spec
  "Build a complete Vega-Lite spec for ECDF with distribution CDF overlays.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-cdf-vega-spec)

;;; Q-Q plot re-exports (moved to common-charts.quantile)

(def qq-points
  "Generate Q-Q plot points comparing sample quantiles to theoretical quantiles.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/qq-points)

(def distribution-qq-layer
  "Build a Q-Q scatter layer for a single fitted distribution.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/distribution-qq-layer)

(def qq-reference-line-layer
  "Build a y=x reference line layer for Q-Q plots.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/qq-reference-line-layer)

(def distribution-qq-overlay-layers
  "Build Q-Q overlay layers for all fitted distributions.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/distribution-qq-overlay-layers)

(def distribution-qq-vega-spec
  "Build a complete Vega-Lite spec for Q-Q plots with separate subplots per distribution.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/distribution-qq-vega-spec)

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
