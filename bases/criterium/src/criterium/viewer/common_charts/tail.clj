(ns criterium.viewer.common-charts.tail
  "Tail analysis visualization functions for Vega-Lite charts.

  Provides chart generation for extreme value analysis including:
  - Tail ratios table showing percentile ratios
  - Hill plot showing tail index estimates across k values
  - Mean residual life plot for threshold selection
  - Zipf plot (complementary CDF on log-log scale)
  - Q-Q plots comparing exceedances to exponential and GPD distributions"
  (:require
   [criterium.array :as arr]
   [criterium.metric :as metric]
   [criterium.stats.interface :as stats]
   [criterium.util.helpers :as util]))

;;; Tail Ratios Table

(defn tail-ratios-table
  "Build a Vega-Lite table spec showing tail ratios.

  Tail ratios indicate how heavy the distribution tail is.
  Higher ratios suggest heavier tails.

  Takes tail-analysis data for a single metric.
  Returns a Vega-Lite spec for a horizontal bar chart styled as a table."
  [tail-data]
  (let [{:keys [tail-ratios empirical-quantiles]} tail-data
        {:keys [p99-p95 p999-p99 p999-p95]} tail-ratios
        {:keys [p95 p99 p999]} empirical-quantiles
        data (cond-> []
               p99-p95
               (conj {"Ratio" "p99/p95"
                      "Value" p99-p95
                      "Numerator" (str "p99=" (format "%.4g" (double p99)))
                      "Denominator" (str "p95=" (format "%.4g" (double p95)))})
               p999-p99
               (conj {"Ratio" "p999/p99"
                      "Value" p999-p99
                      "Numerator" (str "p999=" (format "%.4g" (double p999)))
                      "Denominator" (str "p99=" (format "%.4g" (double p99)))})
               p999-p95
               (conj {"Ratio" "p999/p95"
                      "Value" p999-p95
                      "Numerator" (str "p999=" (format "%.4g" (double p999)))
                      "Denominator" (str "p95=" (format "%.4g" (double p95)))}))]
    (when (seq data)
      {:data {:values data}
       :mark {:type "bar" :color "#4682b4"}
       :encoding {:y {:field "Ratio" :type "nominal"
                      :sort nil
                      :axis {:title nil}}
                  :x {:field "Value" :type "quantitative"
                      :axis {:title "Ratio Value"}}
                  :tooltip [{:field "Ratio" :type "nominal"}
                            {:field "Value" :type "quantitative" :format ".3f"}
                            {:field "Numerator" :type "nominal"}
                            {:field "Denominator" :type "nominal"}]}})))

;;; Hill Plot

(defn hill-plot
  "Build a Vega-Lite spec for the Hill plot.

  The Hill plot shows the Hill estimator H_k vs k (number of order statistics used).
  A stable region where estimates don't vary much with k indicates a reliable
  tail index estimate. The stable estimate is shown as a horizontal reference line.

  Takes tail-analysis data for a single metric.
  Returns a Vega-Lite layered spec with the Hill curve and stable estimate line."
  [tail-data]
  (let [{:keys [hill]} tail-data
        {:keys [k-range estimates stable-estimate]} hill]
    (when (and (seq k-range) (seq estimates))
      (let [data (mapv (fn [k est]
                         {"k" k "estimate" est})
                       k-range estimates)
            layers [{:data {:values data}
                     :mark {:type "line" :color "#4682b4" :strokeWidth 2}
                     :encoding {:x {:field "k" :type "quantitative"
                                    :title "k (order statistics)"}
                                :y {:field "estimate" :type "quantitative"
                                    :title "Hill Estimate (H_k)"
                                    :scale {:zero false}}
                                :tooltip [{:field "k" :type "quantitative"}
                                          {:field "estimate" :type "quantitative"
                                           :format ".4f"}]}}]]
        {:layer
         (if stable-estimate
           (conj layers
                 {:data {:values [{"stable" stable-estimate}]}
                  :mark {:type "rule"
                         :color "#e41a1c"
                         :strokeWidth 2
                         :strokeDash [4 4]}
                  :encoding {:y {:field "stable" :type "quantitative"}
                             :tooltip [{:field "stable" :type "quantitative"
                                        :title "Stable Estimate"
                                        :format ".4f"}]}})
           layers)}))))

;;; Mean Residual Life Plot

(defn mrl-plot
  "Build a Vega-Lite spec for the mean residual life plot.

  The MRL plot shows e(u) = E[X - u | X > u] vs threshold u.
  For GPD-distributed data, MRL is linear in u. A threshold where
  MRL becomes approximately linear suggests a good POT threshold.

  Takes tail-analysis data for a single metric.
  Returns a Vega-Lite spec with the MRL curve and the selected threshold."
  [tail-data transforms]
  (let [{:keys [mrl threshold]} tail-data
        {:keys [thresholds values]} mrl]
    (when (and (seq thresholds) (seq values))
      (let [data (mapv (fn [u mrl-val]
                         {"threshold" (util/transform-sample-> u transforms)
                          "mrl" (util/transform-sample-> mrl-val transforms)})
                       thresholds values)
            threshold-transformed (util/transform-sample-> threshold transforms)]
        {:layer
         [{:data {:values data}
           :mark {:type "line" :color "#4682b4" :strokeWidth 2}
           :encoding {:x {:field "threshold" :type "quantitative"
                          :title "Threshold (u)"
                          :scale {:zero false}}
                      :y {:field "mrl" :type "quantitative"
                          :title "Mean Residual Life e(u)"
                          :scale {:zero false}}
                      :tooltip [{:field "threshold" :type "quantitative"
                                 :format ".4g"}
                                {:field "mrl" :type "quantitative"
                                 :format ".4g"}]}}
          {:data {:values [{"selected" threshold-transformed}]}
           :mark {:type "rule"
                  :color "#e41a1c"
                  :strokeWidth 2
                  :strokeDash [4 4]}
           :encoding {:x {:field "selected" :type "quantitative"}
                      :tooltip [{:field "selected" :type "quantitative"
                                 :title "Selected Threshold"
                                 :format ".4g"}]}}]}))))

;;; Zipf Plot (Complementary CDF on log-log scale)

(defn zipf-plot
  "Build a Vega-Lite spec for the Zipf plot.

  The Zipf plot shows the complementary CDF (1 - F(x)) on a log-log scale.
  For Pareto-like tails, this appears as a straight line with slope -α
  where α is the tail index.

  Takes samples (typed array) and transforms.
  Returns a Vega-Lite spec with log-log scatter plot."
  [samples transforms]
  (when (and samples (pos? (arr/length samples)))
    (let [sorted-arr (arr/sorted samples)
          n (arr/length sorted-arr)
          ;; Compute complementary CDF: P(X > x) = (n - i) / n for i-th order statistic
          data (arr/indexed-dfold
                sorted-arr
                (fn [acc ^long i ^double x]
                  (let [ccdf (/ (double (- n i)) (double n))
                        tx (util/transform-sample-> x transforms)]
                    (if (and (pos? tx) (pos? ccdf))
                      (conj acc {"x" tx
                                 "ccdf" ccdf
                                 "log_x" (Math/log10 tx)
                                 "log_ccdf" (Math/log10 ccdf)})
                      acc)))
                [])]
      (when (seq data)
        {:data {:values data}
         :mark {:type "point" :size 30 :opacity 0.6 :color "#4682b4"}
         :encoding {:x {:field "log_x" :type "quantitative"
                        :title "log₁₀(x)"
                        :scale {:zero false}}
                    :y {:field "log_ccdf" :type "quantitative"
                        :title "log₁₀(1 - F(x))"
                        :scale {:zero false}}
                    :tooltip [{:field "x" :type "quantitative"
                               :title "Value" :format ".4g"}
                              {:field "ccdf" :type "quantitative"
                               :title "P(X > x)" :format ".4f"}]}}))))

;;; Exponential Q-Q Plot

(defn- exponential-quantile
  "Quantile function for standard exponential distribution.
  Q(p) = -ln(1-p)"
  ^double [^double p]
  (cond
    (<= p 0.0) 0.0
    (>= p 1.0) Double/POSITIVE_INFINITY
    :else (- (Math/log (- 1.0 p)))))

(defn exponential-qq-plot
  "Build a Vega-Lite spec for exponential Q-Q plot.

  Compares the tail exceedances (values above threshold) to an exponential
  distribution. If the tail is exponential (GPD with ξ=0), points lie
  on the y=x diagonal.

  Takes samples (typed array), threshold, and transforms.
  Returns a Vega-Lite layered spec with Q-Q scatter and reference line."
  [samples ^double threshold transforms]
  (when (and samples (pos? (arr/length samples)))
    (let [;; Extract exceedances (values above threshold, shifted to start at 0)
          exceedances (stats/exceedances-over-threshold samples threshold)
          n-exceed (arr/length exceedances)]
      (when (> n-exceed 2)
        (let [;; Compute mean of exceedances for scaling
              mean-exceed (/ (arr/fold-double exceedances
                                              (fn ^double [^double acc ^double y]
                                                (+ acc y))
                                              0.0)
                             n-exceed)
              ;; Sort exceedances for Q-Q plot
              sorted-exceed (arr/sorted exceedances)
              ;; Generate Q-Q points
              data (arr/indexed-dfold
                    sorted-exceed
                    (fn [acc ^long i ^double y]
                      (let [;; Hazen plotting position
                            p (/ (- (double (inc i)) 0.5) (double n-exceed))
                            ;; Theoretical quantile (scaled by mean)
                            theoretical (* mean-exceed (exponential-quantile p))
                            ;; Transform both for display
                            t-theoretical (util/transform-sample-> theoretical transforms)
                            t-observed (util/transform-sample-> y transforms)]
                        (if (and (Double/isFinite t-theoretical)
                                 (Double/isFinite t-observed))
                          (conj acc {"theoretical" t-theoretical
                                     "observed" t-observed})
                          acc)))
                    [])
              ;; Compute domain for reference line
              all-values (into (mapv #(get % "theoretical") data)
                               (mapv #(get % "observed") data))
              ^double min-val (if (seq all-values) (apply min all-values) 0.0)
              ^double max-val (if (seq all-values) (apply max all-values) 1.0)
              margin (* 0.05 (- max-val min-val))
              domain-min (- min-val margin)
              domain-max (+ max-val margin)]
          (when (seq data)
            {:layer
             [;; Reference line (y=x)
              {:data {:values [{"x" domain-min "y" domain-min}
                               {"x" domain-max "y" domain-max}]}
               :mark {:type "line"
                      :strokeDash [4 4]
                      :strokeWidth 1.5
                      :color "#666666"}
               :encoding {:x {:field "x" :type "quantitative"
                              :scale {:domain [domain-min domain-max]}}
                          :y {:field "y" :type "quantitative"
                              :scale {:domain [domain-min domain-max]}}}}
              ;; Q-Q scatter points
              {:data {:values data}
               :mark {:type "point" :size 50 :filled true
                      :opacity 0.7 :color "#4682b4"}
               :encoding {:x {:field "theoretical" :type "quantitative"
                              :title "Exponential Quantiles"
                              :scale {:domain [domain-min domain-max]}}
                          :y {:field "observed" :type "quantitative"
                              :title "Sample Exceedance Quantiles"
                              :scale {:domain [domain-min domain-max]}}
                          :tooltip [{:field "theoretical" :type "quantitative"
                                     :title "Theoretical" :format ".4g"}
                                    {:field "observed" :type "quantitative"
                                     :title "Observed" :format ".4g"}]}}]}))))))

;;; GPD Q-Q Plot

(defn gpd-qq-plot
  "Build a Vega-Lite spec for GPD Q-Q plot.

  Compares the tail exceedances to the fitted Generalized Pareto Distribution.
  If the GPD fit is good, points lie on the y=x diagonal.

  Takes samples (typed array), threshold, gpd-fit map {:xi :sigma}, and transforms.
  Returns a Vega-Lite layered spec with Q-Q scatter and reference line."
  [samples ^double threshold gpd-fit transforms]
  (when (and samples (pos? (arr/length samples)) gpd-fit)
    (let [{:keys [xi sigma]} gpd-fit]
      (when (and xi sigma (pos? (double sigma)))
        (let [;; Extract exceedances
              exceedances (stats/exceedances-over-threshold samples threshold)
              n-exceed (arr/length exceedances)]
          (when (> n-exceed 2)
            (let [;; GPD quantile function
                  gpd-quantile-fn (stats/gpd-quantile (double xi) (double sigma))
                  ;; Sort exceedances for Q-Q plot
                  sorted-exceed (arr/sorted exceedances)
                  ;; Generate Q-Q points
                  data (arr/indexed-dfold
                        sorted-exceed
                        (fn [acc ^long i ^double y]
                          (let [;; Hazen plotting position
                                p (/ (- (double (inc i)) 0.5) (double n-exceed))
                                ;; Theoretical GPD quantile
                                theoretical (gpd-quantile-fn p)
                                ;; Transform both for display
                                t-theoretical (util/transform-sample-> theoretical transforms)
                                t-observed (util/transform-sample-> y transforms)]
                            (if (and (Double/isFinite t-theoretical)
                                     (Double/isFinite t-observed))
                              (conj acc {"theoretical" t-theoretical
                                         "observed" t-observed})
                              acc)))
                        [])
                  ;; Compute domain for reference line
                  all-values (into (mapv #(get % "theoretical") data)
                                   (mapv #(get % "observed") data))
                  ^double min-val (if (seq all-values) (apply min all-values) 0.0)
                  ^double max-val (if (seq all-values) (apply max all-values) 1.0)
                  margin (* 0.05 (- max-val min-val))
                  domain-min (- min-val margin)
                  domain-max (+ max-val margin)]
              (when (seq data)
                {:layer
                 [;; Reference line (y=x)
                  {:data {:values [{"x" domain-min "y" domain-min}
                                   {"x" domain-max "y" domain-max}]}
                   :mark {:type "line"
                          :strokeDash [4 4]
                          :strokeWidth 1.5
                          :color "#666666"}
                   :encoding {:x {:field "x" :type "quantitative"
                                  :scale {:domain [domain-min domain-max]}}
                              :y {:field "y" :type "quantitative"
                                  :scale {:domain [domain-min domain-max]}}}}
                  ;; Q-Q scatter points
                  {:data {:values data}
                   :mark {:type "point" :size 50 :filled true
                          :opacity 0.7 :color "#4682b4"}
                   :encoding {:x {:field "theoretical" :type "quantitative"
                                  :title "GPD Quantiles"
                                  :scale {:domain [domain-min domain-max]}}
                              :y {:field "observed" :type "quantitative"
                                  :title "Sample Exceedance Quantiles"
                                  :scale {:domain [domain-min domain-max]}}
                              :tooltip [{:field "theoretical" :type "quantitative"
                                         :title "Theoretical" :format ".4g"}
                                        {:field "observed" :type "quantitative"
                                         :title "Observed" :format ".4g"}]}}]}))))))))

;;; Complete Vega Specs (combining all charts for a data-map)

(defn tail-analysis-vega-spec
  "Build a complete Vega-Lite spec for tail analysis visualization.

  Takes data-map, view options, and chart-options map containing :width and/or
  :height for chart dimensions.

  View options:
    :tail-analysis-id - Key for tail analysis data (default: :tail-analysis)
    :samples-id - Key for source samples (default: :samples)

  Returns the Vega-Lite spec with vertically concatenated tail analysis charts."
  [data-map view chart-options]
  (let [tail-analysis-id (or (:tail-analysis-id view) :tail-analysis)
        samples-id (or (:samples-id view) :samples)
        tail-analysis-map (get data-map tail-analysis-id)
        samples-map (get data-map samples-id)
        tail-results (when tail-analysis-map
                       (:tail-analysis tail-analysis-map))
        ;; Extract transforms directly - tail analysis uses simple identity transforms
        ;; that don't need the full get-transforms chain
        raw-transform (when tail-analysis-map
                        (:transform tail-analysis-map))
        transforms (when raw-transform
                     {:sample-> (let [s (:sample-> raw-transform)]
                                  (if (fn? s) (list s) s))
                      :->sample (let [s (:->sample raw-transform)]
                                  (if (fn? s) [s] s))})
        metrics-defs (when tail-analysis-map
                       (-> (:metrics-defs tail-analysis-map)
                           (metric/filter-metrics
                            (metric/type-pred :quantitative))))
        metric-configs (when metrics-defs
                         (metric/all-metric-configs metrics-defs))
        metric->values (when samples-map
                         (util/metric->values samples-map))]
    {:data {:values []}
     :resolve {:scale {:x "independent" :y "independent"}}
     :vconcat
     (vec
      (for [metric-config metric-configs
            :let [path (:path metric-config)
                  tail-data (get tail-results path)
                  samples (when metric->values (get metric->values path))]
            :when tail-data]
        (let [threshold (get tail-data :threshold)
              gpd (get tail-data :gpd)
              ratios-chart (tail-ratios-table tail-data)
              hill-chart (hill-plot tail-data)
              mrl-chart (mrl-plot tail-data transforms)
              zipf-chart (when samples (zipf-plot samples transforms))
              exp-qq-chart (when (and samples threshold)
                             (exponential-qq-plot samples threshold transforms))
              gpd-qq-chart (when (and samples threshold gpd)
                             (gpd-qq-plot samples threshold gpd transforms))
              charts (filterv some? [ratios-chart hill-chart mrl-chart
                                     zipf-chart exp-qq-chart gpd-qq-chart])]
          (when (seq charts)
            {:title {:text (:label metric-config)}
             :vconcat (mapv #(merge chart-options %) charts)}))))}))
