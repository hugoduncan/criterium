(ns criterium.viewer.common-charts
  "Common Vega-Lite chart generation functions for viewers.

  Provides reusable chart-building functions extracted from the Portal viewer
  for generating histograms, scatter plots, percentile charts, and treemaps."
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.viewer.common-charts.distribution :as distribution]
   [criterium.viewer.common-charts.samples :as samples]
   [criterium.viewer.common-charts.util :as charts-util]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]))

(def ^:private metric-type-prefix
  "Alias for charts-util/metric-type-prefix for backward compatibility."
  charts-util/metric-type-prefix)

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
             type-prefix (when has-error-bounds?
                           (or (metric-type-prefix metric) "mean"))
             base-title (if type-prefix
                          (str type-prefix " " metric-name)
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

(def ^:private chart-layer
  "Alias for charts-util/chart-layer for backward compatibility."
  charts-util/chart-layer)

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
              :color {:field "impl" :type "nominal"}}})

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
                             :legend {:title "Implementation"}}
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
     :resolve {:legend {:color "shared"}}
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
     :resolve {:legend {:color "shared"}}
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

;; Local aliases for Q-Q plot functions (to be extracted in Task #770)
(def ^:private distribution-order
  "Alias for distribution order constant used by Q-Q plots."
  distribution/distribution-order-for-qq)

(def ^:private distribution-colors
  "Alias for distribution colors constant used by Q-Q plots."
  distribution/distribution-colors-for-qq)

(def ^:private distribution-labels
  "Alias for distribution labels constant used by Q-Q plots."
  distribution/distribution-labels-for-qq)

(def ^:private distribution-color-scale
  "Alias for distribution color scale constant used by Q-Q plots."
  distribution/distribution-color-scale-for-qq)

(def ^:private make-quantile-fn
  "Alias for make-quantile-fn used by Q-Q plots."
  distribution/make-quantile-fn)

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
  (let [sorted-arr (arr/sorted samples)
        n (arr/length sorted-arr)
        sorted-samples (arr/fold sorted-arr conj [])]
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
              fit-data (when fits (get fits path))
              has-samples? (and samples (pos? (arr/length samples)))]
          (when (and has-samples? fit-data)
            ;; Compute observed range for consistent axes across subplots
            (let [sorted-arr (arr/sorted samples)
                  sorted-samples (arr/fold sorted-arr conj [])
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
