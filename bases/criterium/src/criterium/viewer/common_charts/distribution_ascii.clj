(ns criterium.viewer.common-charts.distribution-ascii
  "ASCII chart rendering for distribution analysis visualization.

  Provides ASCII chart functions for:
  - PDF (probability density function) line charts
  - CDF (cumulative distribution function) step charts
  - Q-Q (quantile-quantile) scatter plots

  These functions are used by :print and :pprint viewers for terminal-based
  visualization of distribution fit results."
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.primitive-fn :as prim]
   [criterium.stats.probability :as prob]
   [criterium.util.helpers :as util]
   [criterium.viewer.common-charts.distribution :as dist]
   [criterium.viewer.common.ascii-chart :as ascii-chart]
   [criterium.viewer.common.core :as common]))

(set! *unchecked-math* :warn-on-boxed)

;;; PDF Chart

(defn- make-pdf-fn
  "Create a PDF function for the given distribution and parameters."
  [dist params]
  (case dist
    :gamma (prob/gamma-pdf (:shape params) (:scale params))
    :lognormal (prob/lognormal-pdf (:mu params) (:sigma params))
    :inverse-gaussian (prob/inverse-gaussian-pdf (:mu params) (:lambda params))
    :weibull (prob/weibull-pdf (:shape params) (:scale params))
    nil))

(defn- pdf-points-for-distribution
  "Generate PDF points for a single fitted distribution.

  Returns vector of [x density] pairs, or nil if distribution not fitted."
  [dist fit-result grid]
  (when (and (:params fit-result)
             (not (:error fit-result))
             (not (:skipped fit-result)))
    (let [pdf-fn (make-pdf-fn dist (:params fit-result))]
      (when pdf-fn
        (->> grid
             (mapv (fn [^double x]
                     (let [p (prim/invoke-dd pdf-fn x)]
                       (when (and (Double/isFinite p) (not (Double/isNaN p)))
                         [x p]))))
             (filterv some?))))))

(defn render-ascii-pdf
  "Render ASCII PDF chart for the best-fit distribution.

  Parameters:
    fit-data    - distribution fit data for a metric (from :distribution-fit)
    transforms  - transforms to apply to x-values
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [dist-label n] -> header string
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if no valid fit."
  [fit-data transforms opts]
  (let [{:keys [best-model distributions sample-range n]} fit-data
        {:keys [width height header-fn indent]
         :or {width 60
              height 15
              header-fn (fn [label n] (format "PDF: %s (n=%d)" label n))
              indent ""}} opts
        [sample-min sample-max] sample-range]
    (when (and best-model sample-min sample-max
               (> (double sample-max) (double sample-min)))
      (let [fit-result (get distributions best-model)
            label (get dist/distribution-labels best-model (name best-model))
            ;; Generate grid spanning the sample range
            range-val (- (double sample-max) (double sample-min))
            grid-min (max 1e-10 (- (double sample-min) (* 0.05 range-val)))
            grid-max (+ (double sample-max) (* 0.05 range-val))
            step (/ (- grid-max grid-min) 100.0)
            grid (vec (range grid-min grid-max step))
            ;; Generate PDF points
            points (pdf-points-for-distribution best-model fit-result grid)]
        (when (seq points)
          ;; Transform x-values for display
          (let [display-points (mapv (fn [[x y]]
                                       [(util/transform-sample-> x transforms) y])
                                     points)
                header (header-fn label n)
                chart-lines (ascii-chart/render-chart
                             display-points
                             {:width (- (long width) (count indent))
                              :height height
                              :y-label "density"
                              :point-char \*
                              :line-char \.})]
            (when (seq chart-lines)
              (str header "\n"
                   (->> chart-lines
                        (map #(str indent %))
                        (str/join "\n"))))))))))

;;; CDF Chart

(defn- ecdf-points
  "Compute ECDF (empirical CDF) points from samples.

  Returns vector of [x cumulative-probability] pairs."
  [samples transforms]
  (let [sorted-arr (arr/sorted samples)
        n (arr/length sorted-arr)
        nd (double n)]
    (arr/indexed-dfold sorted-arr
                       (fn [acc ^long i ^double x]
                         (conj acc [(util/transform-sample-> x transforms)
                                    (/ (double (inc i)) nd)]))
                       [])))

(defn render-ascii-cdf
  "Render ASCII CDF chart showing ECDF as step function.

  Parameters:
    samples     - sample array for the metric
    transforms  - transforms to apply to sample values
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [n] -> header string
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if no samples."
  [samples transforms opts]
  (when (and samples (pos? (arr/length samples)))
    (let [{:keys [width height header-fn indent]
           :or {width 60
                height 15
                header-fn (fn [n] (format "CDF (n=%d)" n))
                indent ""}} opts
          n (arr/length samples)
          ;; Compute ECDF points
          ecdf (ecdf-points samples transforms)
          header (header-fn n)
          chart-lines (ascii-chart/render-chart
                       ecdf
                       {:width (- (long width) (count indent))
                        :height height
                        :y-label "cumulative"
                        :point-char \*
                        :line-char nil})] ; No lines, just points for step effect
      (when (seq chart-lines)
        (str header "\n"
             (->> chart-lines
                  (map #(str indent %))
                  (str/join "\n")))))))

;;; Q-Q Chart

(defn render-ascii-qq
  "Render ASCII Q-Q plot for the best-fit distribution.

  Shows theoretical quantiles (x-axis) vs observed quantiles (y-axis).
  Points near the diagonal indicate good fit.

  Parameters:
    samples     - sample array for the metric
    fit-data    - distribution fit data for a metric
    transforms  - transforms to apply to values
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [dist-label n] -> header string
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if no valid fit."
  [samples fit-data transforms opts]
  (let [{:keys [best-model distributions]} fit-data
        {:keys [width height header-fn indent]
         :or {width 60
              height 15
              header-fn (fn [label n] (format "Q-Q Plot: %s (n=%d)" label n))
              indent ""}} opts]
    (when (and best-model samples (pos? (arr/length samples)))
      (let [fit-result (get distributions best-model)
            label (get dist/distribution-labels best-model (name best-model))
            quantile-fn (dist/make-quantile-fn best-model (:params fit-result))
            n (arr/length samples)]
        (when quantile-fn
          ;; Generate Q-Q points using Hazen plotting positions
          (let [sorted-arr (arr/sorted samples)
                nd (double n)
                qq-points (arr/indexed-dfold
                           sorted-arr
                           (fn [acc ^long i ^double observed]
                             (let [p (/ (- (double (inc i)) 0.5) nd)
                                   theoretical (quantile-fn p)
                                   t-obs (util/transform-sample-> observed transforms)
                                   t-theo (util/transform-sample-> theoretical transforms)]
                               (if (and (Double/isFinite t-theo)
                                        (not (Double/isNaN t-theo)))
                                 (conj acc [t-theo t-obs])
                                 acc)))
                           [])]
            (when (seq qq-points)
              (let [header (header-fn label n)
                    chart-lines (ascii-chart/render-chart
                                 qq-points
                                 {:width (- (long width) (count indent))
                                  :height height
                                  :x-label "theoretical"
                                  :y-label "observed"
                                  :point-char \*
                                  :line-char nil})] ; Scatter plot, no lines
                (when (seq chart-lines)
                  (str header "\n"
                       (->> chart-lines
                            (map #(str indent %))
                            (str/join "\n"))))))))))))

;;; Data Extraction Helpers

(defn with-distribution-metrics
  "Iterate over metrics with distribution fit data, calling f for each.

  Parameters:
    view     - view options with :distribution-fit-id, :samples-id
    data-map - analysis data map
    f        - fn [fit-data samples transforms metric-config] -> any"
  [view data-map f]
  (let [distribution-fit-id (or (:distribution-fit-id view) :distribution-fit)
        samples-id (or (:samples-id view) :samples)
        distribution-fit-map (get data-map distribution-fit-id)
        samples-map (get data-map samples-id)]
    (when (and distribution-fit-map samples-map)
      (let [fits (:fits distribution-fit-map)
            metrics-defs (:metrics-defs samples-map)
            metric-configs (when metrics-defs
                             (metric/all-metric-configs
                              (metric/filter-metrics
                               metrics-defs
                               (metric/type-pred :quantitative))))
            metric->values (:metric->values samples-map)
            transforms (common/simple-transforms samples-map)]
        (doseq [mc metric-configs]
          (let [path (:path mc)
                fit-data (get fits path)
                samples (get metric->values path)]
            (when (and fit-data samples)
              (f fit-data samples transforms mc))))))))
