(ns criterium.viewer.common-charts.quantile
  "Q-Q plot visualization functions for Vega-Lite charts.

  Provides Q-Q (quantile-quantile) plot generation for comparing sample
  distributions against fitted theoretical distributions. Q-Q plots show
  sample quantiles vs theoretical quantiles - points lying on the y=x
  diagonal indicate good fit."
  (:require
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.viewer.common-charts.distribution :as distribution]))

;;; Distribution constants (from distribution namespace)

(def ^:private distribution-order
  "Canonical ordering of distributions for consistent color assignment."
  distribution/distribution-order)

(def ^:private distribution-colors
  "Color palette for fitted distributions."
  distribution/distribution-colors)

(def ^:private distribution-labels
  "Human-readable labels for distributions."
  distribution/distribution-labels)

(def ^:private distribution-color-scale
  "Vega-Lite color scale with domain and range in consistent order."
  distribution/distribution-color-scale)

;;; Q-Q plot functions

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
    (let [quantile-fn (distribution/make-quantile-fn dist (:params fit-result))
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
    (let [quantile-fn (distribution/make-quantile-fn dist (:params fit-result))
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
