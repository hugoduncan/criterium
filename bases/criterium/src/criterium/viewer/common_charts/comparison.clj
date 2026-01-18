(ns criterium.viewer.common-charts.comparison
  "Comparison chart functions for Vega-Lite charts.

  Provides single-point bar/box charts and multi-point line charts for
  comparing implementations across benchmarks. Used by both domain extracts
  and comparison data structures."
  (:require
   [criterium.viewer.common-charts.util :as charts-util]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as domain-comparison]))

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
                           (or (charts-util/metric-type-prefix metric) "mean"))
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

;;; Bar chart helpers

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

;;; Box plot helpers

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
        bar-layer (charts-util/chart-layer
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

;;; Public single-point chart specs

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
  (let [bar-data (domain-comparison/prepare-comparison-bar-data comparison)]
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
  (let [box-data (domain-comparison/prepare-comparison-box-data comparison)]
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
  (let [line-layer (charts-util/chart-layer
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
  (let [line-data (domain-comparison/prepare-line-chart-data extract)]
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
  (let [line-data (domain-comparison/prepare-comparison-line-data comparison)]
    {:data {:values []}
     :resolve {:legend {:color "shared"}}
     :vconcat (mapv #(line-chart-layer % chart-options) line-data)}))
