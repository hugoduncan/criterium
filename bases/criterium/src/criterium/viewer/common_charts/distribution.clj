(ns criterium.viewer.common-charts.distribution
  "Distribution overlay functions for Vega-Lite charts.

  Provides KDE, PDF, and CDF visualization layers and complete chart specs
  for displaying distribution fits and comparing empirical vs theoretical
  distributions."
  (:require
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.primitive-fn :as prim]
   [criterium.stats.probability :as si]
   [criterium.util.helpers :as util]
   [criterium.viewer.common-charts.samples :as samples]
   [criterium.viewer.common.distribution :as common.distribution]))

;;; Distribution constants

(def distribution-order
  "Canonical ordering of distributions for consistent color assignment."
  [:gamma :lognormal :inverse-gaussian :weibull])

(def distribution-colors
  "Color palette for fitted distributions.
  Colors chosen to avoid conflict with histogram blue (#4682b4 steelblue)."
  {:gamma "#e41a1c"           ; red
   :lognormal "#ff7f00"       ; orange (was blue, conflicted with histogram)
   :inverse-gaussian "#4daf4a" ; green
   :weibull "#984ea3"})

(def distribution-labels
  "Human-readable labels for distributions.
  Re-exported from common.distribution for backwards compatibility."
  common.distribution/distribution-labels)

(def distribution-color-scale
  "Vega-Lite color scale with domain and range in consistent order."
  {:domain (mapv #(get distribution-labels % (name %)) distribution-order)
   :range (mapv #(get distribution-colors % "#999999") distribution-order)})

(def ^:private pdf-color-scale
  "Vega-Lite color scale for PDF chart including KDE and fitted distributions.
  KDE uses black solid line; fitted distributions use colored dashed lines."
  {:domain (into ["KDE"]
                 (mapv
                  #(get distribution-labels % (name %))
                  distribution-order))
   :range (into ["#333333"]
                (mapv
                 #(get distribution-colors % "#999999")
                 distribution-order))})

(def ^:private cdf-color-scale
  "Vega-Lite color scale for CDF overlay including ECDF and fitted distributions."
  {:domain (into ["ECDF"]
                 (mapv
                  #(get distribution-labels % (name %))
                  distribution-order))
   :range (into ["#333333"]
                (mapv
                 #(get distribution-colors % "#999999")
                 distribution-order))})

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
                            (*
                             scale
                             shape
                             (Math/pow
                              (- 1.0 (/ 1.0 (* 9.0 (max shape 0.1))))
                              3.0))
                            ;; Wilson-Hilferty approximation
                            (let [d      (/ 1.0 (* 9.0 shape))
                                  x-norm (- 1.0 d (- (* z (Math/sqrt d))))]
                              (*
                               scale
                               shape
                               (Math/pow (max x-norm 0.01) 3.0))))))]
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

(defn make-quantile-fn
  "Create a quantile (inverse CDF) function for the given distribution and parameters.
  Used by Q-Q plot generation to compute theoretical quantiles."
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

(defn- make-cdf-fn
  "Create a CDF function for the given distribution and parameters."
  [dist params]
  (case dist
    :gamma (si/gamma-cdf (:shape params) (:scale params))
    :lognormal (si/lognormal-cdf (:mu params) (:sigma params))
    :inverse-gaussian (si/inverse-gaussian-cdf (:mu params) (:lambda params))
    :weibull (si/weibull-cdf (:shape params) (:scale params))
    nil))

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
        data (mapv
              (fn [{:keys [location density ci-lower ci-upper significant?]}]
                (cond-> {field-name
                         (util/transform-sample-> location transforms)
                         "kde-density"
                         density
                         "significant"
                         (if significant? "yes" "no")}
                  ci-lower (assoc
                            "ci-lower"
                            (util/transform-sample-> ci-lower transforms))
                  ci-upper (assoc
                            "ci-upper"
                            (util/transform-sample->
                             ci-upper
                             transforms))))
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
                (conj (samples/metric-computed-histo-layer
                       hist-transforms
                       histogram
                       metric-config))
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
                                modes-data
                                metric-config
                                kde-transforms)))}))}))))
      metric-configs)}))

;;; Distribution PDF overlay charts

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
                                  display-x (util/transform-sample->
                                             x
                                             transforms)]
                              {field-name display-x "pdf-density" scaled-p})))
                    ;; Filter out non-finite values that can't be encoded in JSON
                    (filterv (fn [pt]
                               (let [p (get pt "pdf-density")
                                     x (get pt field-name)]
                                 (and
                                  (Double/isFinite p)
                                  (not (Double/isNaN p))
                                  (Double/isFinite x)
                                  (not (Double/isNaN x)))))))]
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
                         (let [range-val
                               (- (double sample-max) (double sample-min))
                               grid-min
                               (max
                                1e-10
                                (- (double sample-min) (* 0.05 range-val)))
                               grid-max
                               (+ (double sample-max) (* 0.05 range-val))
                               step
                               (/ (- grid-max grid-min) 200.0)]
                           (vec (range grid-min grid-max step))))]
          (when kde-data
            (merge
             chart-options
             {:resolve
              {:scale {:x "shared" :y "independent" :color "independent"}}
              :layer
              (cond-> []
                ;; Add histogram bars if available
                histogram
                (conj (samples/metric-computed-histo-layer
                       hist-transforms
                       histogram
                       metric-config))
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

(defn ecdf-layer
  "Build an ECDF (empirical cumulative distribution function) step layer.

  Takes samples and transforms, returns a Vega-Lite layer spec showing
  the empirical CDF as a step function."
  [samples transforms]
  (let [sorted-samples (arr/sorted samples)
        n (arr/length sorted-samples)
        sorted-vec (arr/fold sorted-samples conj [])
        ;; ECDF: F_n(x) = (number of samples <= x) / n
        ;; For step function, we need points at each sample value
        data (mapv (fn [i x]
                     {"x" (util/transform-sample-> x transforms)
                      "cdf" (/ (double (inc (long i))) n)})
                   (range n)
                   sorted-vec)]
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
                :color {:field "distribution"
                        :type "nominal"
                        :scale cdf-color-scale
                        :legend {:orient "bottom-right"
                                 :title "Distribution"}}}}))

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
                                 (and
                                  (Double/isFinite p)
                                  (not (Double/isNaN p)))))))]
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
              has-samples? (and samples (pos? (arr/length samples)))
              ;; Generate grid from sample range for CDF curves
              sorted-samples (when has-samples? (arr/sorted samples))
              min-val (when sorted-samples (arr/first-double sorted-samples))
              max-val (when sorted-samples (arr/last-double sorted-samples))
              ;; Extend range slightly for better visualization
              range-val (when (and min-val max-val)
                          (- (double max-val) (double min-val)))
              grid-min (when range-val
                         (- (double min-val) (* 0.05 (double range-val))))
              grid-max (when range-val
                         (+ (double max-val) (* 0.05 (double range-val))))
              grid (when (and grid-min grid-max)
                     (let [step
                           (/ (- (double grid-max) (double grid-min)) 100.0)]
                       (vec (range grid-min grid-max step))))]
          (when has-samples?
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
