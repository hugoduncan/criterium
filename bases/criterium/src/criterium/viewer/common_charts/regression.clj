(ns criterium.viewer.common-charts.regression
  "Regression chart functions for Vega-Lite visualizations.

  Provides scatter plots with fit lines, residual plots, and log-log regression
  charts for complexity analysis. Supports both single-implementation and
  multi-implementation comparison modes."
  (:require
   [criterium.util.invariant :refer [have]]))

;;; Standard Regression Charts

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

;;; Residual Charts

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

;;; Log-Log Residual Charts

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
