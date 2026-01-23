(ns criterium.viewer.common-charts.autocorrelation
  "Vega-Lite chart specification for ACF (Autocorrelation Function) plots.

  Provides bar charts showing autocorrelation coefficients for all lags,
  with bars colored by severity and horizontal threshold bands at ±2/√n."
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

;;; Chart Layers

(defn threshold-rule-layer
  "Create horizontal rule layer for threshold lines at ±2/√n.

  Returns Vega-Lite layer spec with dashed lines at positive and negative thresholds."
  [^long n]
  (let [threshold (stats/noise-floor n)]
    {:data {:values [{:threshold threshold :label "+2/√n"}
                     {:threshold (- threshold) :label "-2/√n"}]}
     :mark {:type "rule"
            :strokeDash [4 4]
            :strokeWidth 1}
     :encoding {:y {:field "threshold"
                    :type "quantitative"}
                :color {:value "#666666"}}}))

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

(defn acf-bar-layer
  "Create bar layer for ACF values.

  Returns Vega-Lite layer spec with bars colored by severity."
  [chart-data]
  {:data {:values chart-data}
   :mark {:type "bar"
          :width {:band 0.8}}
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
        title (or (:title chart-options)
                  (str "Autocorrelation Function (n=" n ")"))]
    (when (and chart-data n (seq chart-data)
               ;; Check min-severity threshold if specified
               (or (nil? min-severity)
                   (has-severity-at-or-above? lag-severities min-severity)))
      (let [layers (cond-> [(zero-line-layer)
                            (threshold-rule-layer n)
                            (acf-bar-layer chart-data)]
                     ;; Add period annotation if detected
                     (:detected-period acf-data)
                     (conj (period-annotation-layer acf-data chart-data)))]
        (merge
         {:title title
          :layer (vec (remove nil? layers))}
         (select-keys chart-options [:width :height]))))))
