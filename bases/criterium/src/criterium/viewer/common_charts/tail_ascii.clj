(ns criterium.viewer.common-charts.tail-ascii
  "ASCII chart rendering for tail analysis visualization.

  Provides ASCII chart functions for:
  - Tail ratios bar chart
  - Hill plot (tail index estimates vs k)
  - MRL plot (mean residual life vs threshold)
  - Zipf plot (complementary CDF on log-log scale)
  - Exponential Q-Q plot
  - GPD Q-Q plot

  These functions are used by :print and :pprint viewers for terminal-based
  visualization of tail analysis results."
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.stats.tail :as stats]
   [criterium.util.helpers :as util]
   [criterium.viewer.common.ascii-chart :as ascii-chart]
   [criterium.viewer.common.core :as common]))

(set! *unchecked-math* :warn-on-boxed)

;;; Tail Ratios Chart

(defn render-ascii-tail-ratios
  "Render ASCII horizontal bar chart for tail ratios.

  Parameters:
    tail-data   - tail analysis data for a metric
    opts        - options map:
      :width      - chart width in characters (default 60)
      :header-fn  - fn [n] -> header string, where n is number of ratios
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if no ratios."
  [tail-data opts]
  (let [{:keys [tail-ratios empirical-quantiles]} tail-data
        {:keys [width header-fn indent]
         :or {width 60
              header-fn (fn [n] (format "Tail Ratios (n=%d)" n))
              indent ""}} opts
        {:keys [p99-p95 p999-p99 p999-p95]} tail-ratios
        {:keys [p95 p99 p999]} empirical-quantiles]
    (when (and tail-ratios (or p99-p95 p999-p99 p999-p95))
      (let [ratios (cond-> []
                     p99-p95 (conj {:label "p99/p95" :value p99-p95
                                    :detail (format "p99=%.3g p95=%.3g"
                                                    (double p99) (double p95))})
                     p999-p99 (conj {:label "p999/p99" :value p999-p99
                                     :detail (format
                                              "p999=%.3g p99=%.3g"
                                              (double p999)
                                              (double p99))})
                     p999-p95 (conj {:label "p999/p95" :value p999-p95
                                     :detail (format
                                              "p999=%.3g p95=%.3g"
                                              (double p999)
                                              (double p95))}))]
        (when (seq ratios)
          (let [n (count ratios)
                max-val (double (apply max (map :value ratios)))
                bar-width (-
                           (long width)
                           (count indent)
                           25) ; label + value + spacing
                header
                (header-fn n)
                lines
                (for [{:keys [label value detail]} ratios]
                  (let [bar-len (long (* (/ (double value) max-val)
                                         (double bar-width)))
                        bar (apply str (repeat bar-len \#))]
                    (format "%s%9s |%s %.3f  %s"
                            indent label bar value detail)))]
            (str header "\n" (str/join "\n" lines))))))))

;;; Hill Plot

(defn render-ascii-hill-plot
  "Render ASCII Hill plot showing tail index estimates vs k.

  Parameters:
    tail-data   - tail analysis data for a metric
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [n] -> header string, where n is number of k values
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if no Hill data."
  [tail-data opts]
  (let [{:keys [hill]} tail-data
        {:keys [k-range estimates stable-estimate]} hill
        {:keys [width height header-fn indent]
         :or {width 60
              height 15
              header-fn (fn [n] (format "Hill Plot (H_k vs k, n=%d)" n))
              indent ""}} opts]
    (when (and (seq k-range) (seq estimates))
      (let [n (count estimates)
            points (mapv vector k-range estimates)
            header (header-fn n)
            chart-lines (ascii-chart/render-chart
                         points
                         {:width (- (long width) (count indent))
                          :height height
                          :x-label "k"
                          :y-label "H_k"
                          :point-char \*
                          :line-char \.})]
        (when (seq chart-lines)
          (let [stable-line (when stable-estimate
                              (format
                               "%sStable estimate: %.4f"
                               indent
                               stable-estimate))]
            (str header "\n"
                 (->> chart-lines
                      (map #(str indent %))
                      (str/join "\n"))
                 (when stable-line (str "\n" stable-line)))))))))

;;; MRL Plot

(defn render-ascii-mrl-plot
  "Render ASCII mean residual life plot.

  Shows e(u) = E[X - u | X > u] vs threshold u. For GPD data, MRL is linear.

  Parameters:
    tail-data   - tail analysis data for a metric
    transforms  - transforms to apply to values
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [n] -> header string,
                    where n is number of threshold points
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if no MRL data."
  [tail-data transforms opts]
  (let [{:keys [mrl threshold]} tail-data
        {:keys [thresholds values]} mrl
        {:keys [width height header-fn indent]
         :or {width 60
              height 15
              header-fn (fn [n] (format "Mean Residual Life Plot (n=%d)" n))
              indent ""}} opts]
    (when (and (seq thresholds) (seq values))
      (let [n (count thresholds)
            points (mapv (fn [u mrl-val]
                           [(util/transform-sample-> u transforms)
                            (util/transform-sample-> mrl-val transforms)])
                         thresholds values)
            threshold-transformed (util/transform-sample-> threshold transforms)
            header (header-fn n)
            chart-lines (ascii-chart/render-chart
                         points
                         {:width (- (long width) (count indent))
                          :height height
                          :x-label "threshold"
                          :y-label "MRL"
                          :point-char \*
                          :line-char \.})]
        (when (seq chart-lines)
          (str header "\n"
               (->> chart-lines
                    (map #(str indent %))
                    (str/join "\n"))
               (format
                "\n%sSelected threshold: %.4g"
                indent
                threshold-transformed)))))))

;;; Zipf Plot

(defn render-ascii-zipf-plot
  "Render ASCII Zipf plot (complementary CDF on log-log scale).

  For Pareto-like tails, this appears as a straight line with slope -α.

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
                header-fn (fn [n] (format "Zipf Plot (log-log CCDF, n=%d)" n))
                indent ""}} opts
          sorted-arr (arr/sorted samples)
          n (arr/length sorted-arr)
          ;; Compute log10(x) vs log10(1-F(x)) points
          points (arr/indexed-dfold
                  sorted-arr
                  (fn [acc ^long i ^double x]
                    (let [ccdf (/ (double (- n i)) (double n))
                          tx (util/transform-sample-> x transforms)]
                      (if (and (pos? tx) (pos? ccdf))
                        (conj acc [(Math/log10 tx) (Math/log10 ccdf)])
                        acc)))
                  [])
          header (header-fn n)]
      (when (seq points)
        (let [chart-lines (ascii-chart/render-chart
                           points
                           {:width (- (long width) (count indent))
                            :height height
                            :x-label "log₁₀(x)"
                            :y-label "log₁₀(CCDF)"
                            :point-char \*
                            :line-char nil})] ; Scatter, no lines
          (when (seq chart-lines)
            (str header "\n"
                 (->> chart-lines
                      (map #(str indent %))
                      (str/join "\n")))))))))

;;; Exponential Q-Q Plot

(defn- exponential-quantile
  "Quantile function for standard exponential distribution.
  Q(p) = -ln(1-p)"
  ^double [^double p]
  (cond
    (<= p 0.0) 0.0
    (>= p 1.0) Double/POSITIVE_INFINITY
    :else (- (Math/log (- 1.0 p)))))

(defn render-ascii-exponential-qq
  "Render ASCII exponential Q-Q plot for tail exceedances.

  Compares exceedances (values above threshold) to exponential distribution.
  Points near diagonal indicate exponential tail (GPD with ξ=0).

  Parameters:
    samples     - sample array for the metric
    threshold   - threshold value for exceedances
    transforms  - transforms to apply to values
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [n-exceed] -> header string
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if insufficient exceedances."
  [samples ^double threshold transforms opts]
  (when (and samples (pos? (arr/length samples)))
    (let [{:keys [width height header-fn indent]
           :or {width 60
                height 15
                header-fn (fn [n]
                            (format
                             "Exponential Q-Q Plot (n=%d exceedances)"
                             n))
                indent ""}} opts
          exceedances (stats/exceedances-over-threshold samples threshold)
          n-exceed (arr/length exceedances)]
      (when (> n-exceed 2)
        (let [;; Compute mean for scaling
              mean-exceed (/ (arr/fold-double exceedances
                                              (fn ^double [^double acc
                                                           ^double y]
                                                (+ acc y))
                                              0.0)
                             n-exceed)
              sorted-exceed (arr/sorted exceedances)
              ;; Generate Q-Q points
              points (arr/indexed-dfold
                      sorted-exceed
                      (fn [acc ^long i ^double y]
                        (let [p (/ (- (double (inc i)) 0.5) (double n-exceed))
                              theoretical (*
                                           mean-exceed
                                           (exponential-quantile p))
                              t-theo (util/transform-sample->
                                      theoretical
                                      transforms)
                              t-obs (util/transform-sample-> y transforms)]
                          (if (and
                               (Double/isFinite t-theo)
                               (Double/isFinite t-obs))
                            (conj acc [t-theo t-obs])
                            acc)))
                      [])
              header (header-fn n-exceed)]
          (when (seq points)
            (let [chart-lines (ascii-chart/render-chart
                               points
                               {:width (- (long width) (count indent))
                                :height height
                                :x-label "theoretical"
                                :y-label "observed"
                                :point-char \*
                                :line-char nil})] ; Scatter, no lines
              (when (seq chart-lines)
                (str header "\n"
                     (->> chart-lines
                          (map #(str indent %))
                          (str/join "\n")))))))))))

;;; GPD Q-Q Plot

(defn render-ascii-gpd-qq
  "Render ASCII GPD Q-Q plot for tail exceedances.

  Compares exceedances to fitted Generalized Pareto Distribution.
  Points near diagonal indicate good GPD fit.

  Parameters:
    samples     - sample array for the metric
    threshold   - threshold value for exceedances
    gpd-fit     - GPD fit map {:xi :sigma}
    transforms  - transforms to apply to values
    opts        - options map:
      :width      - chart width in characters (default 60)
      :height     - chart height in lines (default 15)
      :header-fn  - fn [n-exceed] -> header string
      :indent     - string prefix for each line

  Returns string of ASCII chart, or nil if insufficient data."
  [samples threshold gpd-fit transforms opts]
  (when (and samples (pos? (arr/length samples)) gpd-fit)
    (let [{:keys [xi sigma]} gpd-fit
          threshold (double threshold)]
      (when (and xi sigma (pos? (double sigma)))
        (let [{:keys [width height header-fn indent]
               :or {width 60
                    height 15
                    header-fn (fn [n]
                                (format "GPD Q-Q Plot (n=%d exceedances)" n))
                    indent ""}} opts
              exceedances (stats/exceedances-over-threshold samples threshold)
              n-exceed (arr/length exceedances)]
          (when (> n-exceed 2)
            (let [gpd-quantile-fn
                  (stats/gpd-quantile (double xi) (double sigma))
                  sorted-exceed
                  (arr/sorted exceedances)
                  ;; Generate Q-Q points
                  points
                  (arr/indexed-dfold
                   sorted-exceed
                   (fn [acc ^long i ^double y]
                     (let [p
                           (/ (- (double (inc i)) 0.5) (double n-exceed))
                           theoretical
                           (gpd-quantile-fn p)
                           t-theo
                           (util/transform-sample->
                            theoretical
                            transforms)
                           t-obs
                           (util/transform-sample-> y transforms)]
                       (if (and
                            (Double/isFinite t-theo)
                            (Double/isFinite t-obs))
                         (conj acc [t-theo t-obs])
                         acc)))
                   [])
                  header
                  (header-fn n-exceed)]
              (when (seq points)
                (let [chart-lines (ascii-chart/render-chart
                                   points
                                   {:width (- (long width) (count indent))
                                    :height height
                                    :x-label "GPD theoretical"
                                    :y-label "observed"
                                    :point-char \*
                                    :line-char nil})] ; Scatter, no lines
                  (when (seq chart-lines)
                    (str header "\n"
                         (->> chart-lines
                              (map #(str indent %))
                              (str/join "\n")))))))))))))

;;; Data Extraction Helper

(defn with-tail-metrics
  "Iterate over metrics with tail analysis data, calling f for each.

  Parameters:
    view     - view options with :tail-analysis-id, :samples-id
    data-map - analysis data map
    f        - fn [tail-data samples transforms metric-config] -> any

  Samples may be nil if :samples-id data is not available."
  [view data-map f]
  (let [tail-analysis-id (or (:tail-analysis-id view) :tail-analysis)
        samples-id (or (:samples-id view) :samples)
        tail-analysis-map (get data-map tail-analysis-id)
        samples-map (get data-map samples-id)]
    (when tail-analysis-map
      (let [tail-results (:tail-analysis tail-analysis-map)
            metrics-defs (:metrics-defs tail-analysis-map)
            metric-configs (when metrics-defs
                             (metric/all-metric-configs
                              (metric/filter-metrics
                               metrics-defs
                               (metric/type-pred :quantitative))))
            metric->values (when samples-map (util/metric->values samples-map))
            transforms (common/simple-transforms tail-analysis-map)]
        (doseq [mc metric-configs]
          (let [path (:path mc)
                tail-data (get tail-results path)
                samples (when metric->values (get metric->values path))]
            (when tail-data
              (f tail-data samples transforms mc))))))))
