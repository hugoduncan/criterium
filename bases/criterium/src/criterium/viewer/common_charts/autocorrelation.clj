(ns criterium.viewer.common-charts.autocorrelation
  "Vega-Lite chart specification for ACF (Autocorrelation Function) plots.

  Provides point charts showing autocorrelation coefficients for all lags,
  with points colored by severity and horizontal threshold lines for
  lag-1 (blue) and other-lag (gray) thresholds."
  (:require
   [criterium.stats.interface :as stats]))

;;; Severity Colors

(def severity-colors
  "Color palette for ACF bar severity levels."
  {:none "#999999"      ; gray
   :minor "#f0ad4e"     ; yellow
   :moderate "#fd7e14"  ; orange
   :severe "#dc3545"})  ; red

(def severity-order
  "Canonical ordering of severity levels for consistent legend display."
  [:none :minor :moderate :severe])

(def severity-rank
  "Numeric ranking for severity comparison. Higher = more severe."
  {:none 0
   :minor 1
   :moderate 2
   :severe 3
   :alternating-none 0
   :alternating-minor 1
   :alternating-moderate 2
   :alternating-severe 3})

(defn has-severity-at-or-above?
  "Check if any lag has severity at or above the given threshold.

  Parameters:
    lag-severities - map of lag -> severity keyword
    min-severity - minimum severity to check for (:none, :minor, :moderate, :severe)

  Returns true if any lag meets or exceeds the threshold."
  [lag-severities min-severity]
  (let [min-rank (get severity-rank min-severity 0)]
    (some (fn [[_ sev]]
            (>= (get severity-rank sev 0) min-rank))
          lag-severities)))

(def severity-color-scale
  "Vega-Lite color scale for severity."
  {:domain (mapv name severity-order)
   :range (mapv #(get severity-colors % "#999999") severity-order)})

;;; Data Transformation

(defn acf->chart-data
  "Transform ACF analysis data to chart data format.

  Takes autocorrelation analysis result for a metric path.
  Returns vector of maps: [{:lag k :acf rₖ :severity \"none\"} ...]

  Parameters:
    acf-data - autocorrelation analysis result with :acf map and :effective-sample-size"
  [acf-data]
  (let [acf-map (:acf acf-data)
        n (get-in acf-data [:effective-sample-size :n-original])
        lag-severities (when (and acf-map n)
                         (stats/classify-lag-severities acf-map n))]
    (when (and acf-map lag-severities)
      (->> acf-map
           (sort-by first)
           (mapv (fn [[lag r]]
                   {:lag lag
                    :acf r
                    :severity (name (get lag-severities lag :none))}))))))

;;; Threshold Colors

(def lag-1-threshold-colors
  "Blue tones for lag-1 threshold lines."
  {:minor "#93c5fd"     ; light blue
   :moderate "#3b82f6"  ; medium blue
   :severe "#1d4ed8"})  ; dark blue

(def other-lag-threshold-colors
  "Gray tones for other-lag threshold lines."
  {:minor "#d1d5db"     ; light gray
   :moderate "#9ca3af"  ; medium gray
   :severe "#6b7280"})  ; dark gray

;;; Chart Layers

(defn- lag-1-threshold-crossed?
  "Check if lag-1 ACF value exceeds the given threshold."
  [chart-data ^double threshold]
  (when-let [lag-1-data (first (filter #(= 1 (:lag %)) chart-data))]
    (> (Math/abs (double (:acf lag-1-data))) threshold)))

(defn- other-lag-threshold-crossed?
  "Check if any non-lag-1 ACF value exceeds the given threshold."
  [chart-data ^double threshold]
  (some #(and (not= 1 (:lag %))
              (> (Math/abs (double (:acf %))) threshold))
        chart-data))

(defn threshold-rule-layers
  "Create horizontal rule layers for crossed threshold lines.

  Shows lag-1 thresholds in blue tones and other-lag thresholds in gray tones.
  Only shows thresholds that are actually crossed by at least one data point.

  Parameters:
    chart-data - vector of {:lag :acf :severity} maps
    thresholds - map with :lag-1 and :other threshold maps (from analysis)

  Returns vector of Vega-Lite layer specs."
  [chart-data thresholds]
  (let [lag-1-thresholds (:lag-1 thresholds)
        other-thresholds (:other thresholds)
        ;; Collect crossed thresholds
        lag-1-lines (for [[level threshold] lag-1-thresholds
                          :when (lag-1-threshold-crossed? chart-data threshold)]
                      {:threshold threshold
                       :neg-threshold (- threshold)
                       :color (get lag-1-threshold-colors level)
                       :label (str "lag-1 " (name level))})
        other-lines (for [[level threshold] other-thresholds
                          :when (other-lag-threshold-crossed? chart-data threshold)]
                      {:threshold threshold
                       :neg-threshold (- threshold)
                       :color (get other-lag-threshold-colors level)
                       :label (str "other " (name level))})]
    (->> (concat lag-1-lines other-lines)
         (mapcat (fn [{:keys [threshold neg-threshold color label]}]
                   [{:data {:values [{:y threshold :label (str "+" label)}]}
                     :mark {:type "rule"
                            :strokeDash [4 4]
                            :strokeWidth 1}
                     :encoding {:y {:field "y" :type "quantitative"}
                                :color {:value color}}}
                    {:data {:values [{:y neg-threshold :label (str "-" label)}]}
                     :mark {:type "rule"
                            :strokeDash [4 4]
                            :strokeWidth 1}
                     :encoding {:y {:field "y" :type "quantitative"}
                                :color {:value color}}}]))
         vec)))

(defn period-annotation-layer
  "Create text annotation layer for detected period.

  Returns Vega-Lite layer spec with text mark at the period lag, or nil if no period."
  [acf-data chart-data]
  (when-let [period (:detected-period acf-data)]
    (let [period-acf (some #(when (= (:lag %) period) (:acf %)) chart-data)]
      (when period-acf
        {:data {:values [{:lag period
                          :acf period-acf
                          :label (str "Period: " period)}]}
         :mark {:type "text"
                :dy -15
                :fontSize 11
                :fontWeight "bold"}
         :encoding {:x {:field "lag" :type "quantitative"}
                    :y {:field "acf" :type "quantitative"}
                    :text {:field "label"}
                    :color {:value "#dc3545"}}}))))

(defn acf-point-layer
  "Create point layer for ACF values.

  Returns Vega-Lite layer spec with points colored by severity."
  [chart-data]
  {:data {:values chart-data}
   :mark {:type "point"
          :filled true
          :size 60}
   :encoding {:x {:field "lag"
                  :type "quantitative"
                  :axis {:title "Lag"
                         :tickMinStep 1}
                  :scale {:nice false}}
              :y {:field "acf"
                  :type "quantitative"
                  :axis {:title "Autocorrelation"}
                  :scale {:domain [-1 1]}}
              :color {:field "severity"
                      :type "nominal"
                      :scale severity-color-scale
                      :legend {:title "Severity"
                               :orient "top-right"}}
              :tooltip [{:field "lag" :type "quantitative" :title "Lag"}
                        {:field "acf" :type "quantitative" :title "ACF" :format ".3f"}
                        {:field "severity" :type "nominal" :title "Severity"}]}})

(defn zero-line-layer
  "Create horizontal rule at y=0 for reference."
  []
  {:data {:values [{}]}
   :mark {:type "rule"
          :strokeWidth 1}
   :encoding {:y {:datum 0}
              :color {:value "#333333"}}})

;;; Main Chart Spec

(defn acf-plot-vega-spec
  "Build complete Vega-Lite spec for ACF plot.

  Takes autocorrelation analysis data for a single metric and chart options.

  Parameters:
    acf-data - autocorrelation analysis result with :acf, :effective-sample-size, etc.
    chart-options - map with :width, :height, :title, :min-severity (optional)
      :min-severity - if provided, returns nil unless at least one lag has
                      severity at or above this level (:none, :minor, :moderate, :severe)

  Returns Vega-Lite spec map, or nil if suppressed by min-severity threshold."
  [acf-data chart-options]
  (let [chart-data (acf->chart-data acf-data)
        n (get-in acf-data [:effective-sample-size :n-original])
        lag-severities (:lag-severities acf-data)
        min-severity (:min-severity chart-options)
        thresholds (or (:thresholds acf-data)
                       {:lag-1 {:minor 0.10 :moderate 0.20 :severe 0.35}
                        :other {:minor 0.15 :moderate 0.25 :severe 0.40}})
        title (or (:title chart-options)
                  (str "Autocorrelation Function (n=" n ")"))]
    (when (and chart-data n (seq chart-data)
               ;; Check min-severity threshold if specified
               (or (nil? min-severity)
                   (has-severity-at-or-above? lag-severities min-severity)))
      (let [threshold-layers (threshold-rule-layers chart-data thresholds)
            layers (-> [(zero-line-layer)
                        (acf-point-layer chart-data)]
                       (into threshold-layers)
                       (cond-> (:detected-period acf-data)
                         (conj (period-annotation-layer acf-data chart-data))))]
        (merge
         {:title title
          :layer (vec (remove nil? layers))}
         (select-keys chart-options [:width :height]))))))
