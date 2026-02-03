(ns criterium.viewer.print.tail
  "Tail analysis views for print viewer.

  Contains views for:
  - Tail summary (GPD/Hill parameters)
  - Tail ratios (p99/p95, p999/p99, p999/p95)
  - High quantile estimates (GPD extrapolation)
  - ASCII chart views (tail ratio charts, Hill/MRL/Zipf plots, Q-Q plots)"
  (:require
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common-charts.tail-ascii :as tail-ascii]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.print.core
    :as print-core
    :refer [get-analysis-context for-each-metric-keyed]]))

(set! *unchecked-math* false)

(defn- get-tail-context
  "Extract tail analysis context using common helper plus tail-specific data.
  Returns map with :tail-results, :metric-configs, :transforms, or nil
  if no data."
  [view data-map]
  (when-let [{:keys [analysis-map metric-configs transforms]}
             (get-analysis-context
              :tail-analysis-id
              :tail-analysis
              view
              data-map
              (metric/type-pred :quantitative))]
    (let [tail-results (:tail-analysis analysis-map)]
      (when (seq tail-results)
        {:tail-results tail-results
         :metric-configs metric-configs
         :transforms transforms}))))

(defn- print-tail-summary-metric
  "Print GPD/Hill summary for a single metric."
  [mc tail-data transforms]
  (let [summary-rows (core/tail-summary-table tail-data transforms)]
    (when (seq summary-rows)
      (println (print-core/format-label (:label mc)))
      (doseq [{:keys [parameter value]} summary-rows]
        (println
         (format "%s  %s: %s" (print-core/label-str "") parameter value))))))

(defn print-tail-summary
  "Print GPD/Hill summary statistics for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (println "Tail Summary:")
    (for-each-metric-keyed metric-configs tail-results
                           #(print-tail-summary-metric %1 %2 transforms))
    (println)))

(defmethod view/tail-summary* :print
  [_ view data-map]
  (print-tail-summary view data-map))

(defn- format-tail-ratio
  "Format a tail ratio value with reference percentile values."
  [[ratio-name ratio-val] {:keys [p95 p99 p999]}]
  (case ratio-name
    :p99-p95 (format "p99/p95 = %.3f (p99=%.4g, p95=%.4g)"
                     ratio-val (double p99) (double p95))
    :p999-p99 (format "p999/p99 = %.3f (p999=%.4g, p99=%.4g)"
                      ratio-val (double p999) (double p99))
    :p999-p95 (format "p999/p95 = %.3f (p999=%.4g, p95=%.4g)"
                      ratio-val (double p999) (double p95))
    (format "%s = %.3f" (name ratio-name) ratio-val)))

(defn- print-tail-ratios-metric
  "Print tail ratios for a single metric."
  [mc tail-data]
  (let [{:keys [tail-ratios empirical-quantiles]} tail-data]
    (when (and tail-ratios (seq tail-ratios))
      (let [sorted-ratios (sort-by first tail-ratios)]
        (doseq [[i [rname rval]] (map-indexed vector sorted-ratios)]
          (if (zero? i)
            (println (format "%s %s"
                             (print-core/format-label
                              (str (:label mc) " tail ratios"))
                             (format-tail-ratio
                              [rname rval]
                              empirical-quantiles)))
            (println (format "%s  %s"
                             (print-core/label-str "")
                             (format-tail-ratio
                              [rname rval]
                              empirical-quantiles)))))))))

(defn print-tail-ratios
  "Print tail ratio statistics for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (println "Tail Ratios:")
    (for-each-metric-keyed metric-configs tail-results print-tail-ratios-metric)
    (println)))

(defmethod view/tail-ratios* :print
  [_ view data-map]
  (print-tail-ratios view data-map))

(defn- print-tail-high-quantiles-metric
  "Print high quantile estimates for a single metric."
  [mc tail-data transforms]
  (let [{:keys [high-quantiles]} tail-data]
    (when (seq high-quantiles)
      (println (print-core/format-label (:label mc)))
      (doseq [[q val] (sort-by first high-quantiles)]
        (let [tval (util/transform-sample-> val transforms)
              [scale unit] (format/scale :time tval)
              scaled (* scale tval)]
          (println (format "%s  p%.4g = %s %s"
                           (print-core/label-str "")
                           (* q 100)
                           (format/format-scaled scaled 1.0)
                           unit)))))))

(defn print-tail-high-quantiles
  "Print high quantile estimates from GPD extrapolation for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (println "High Quantile Estimates (GPD):")
    (for-each-metric-keyed metric-configs tail-results
                           #(print-tail-high-quantiles-metric %1 %2 transforms))
    (println)))

(defmethod view/tail-high-quantiles* :print
  [_ view data-map]
  (print-tail-high-quantiles view data-map))

;;; Tail Ratios Chart View

(defn print-tail-ratios-chart
  "Print ASCII tail ratios bar chart for all metrics."
  [view data-map]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [n]
                    (format "%s Tail Ratios (n=%d)"
                            (print-core/format-sublabel "Chart") n))
        opts (assoc view
                    :header-fn header-fn
                    :indent indent
                    :width 70)]
    (tail-ascii/with-tail-metrics view data-map
      (fn [tail-data _samples _transforms _mc]
        (when-let [output (tail-ascii/render-ascii-tail-ratios tail-data opts)]
          (println output)
          (println))))))

(defmethod view/tail-ratios-chart* :print
  [_ view data-map]
  (print-tail-ratios-chart view data-map))

;;; Hill Plot View

(defn print-hill-plot
  "Print ASCII Hill plot for all metrics."
  [view data-map]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [n]
                    (format "%s Hill Plot (H_k vs k, n=%d)"
                            (print-core/format-sublabel "Chart") n))
        opts (assoc view
                    :header-fn header-fn
                    :indent indent
                    :width 70
                    :height 12)]
    (tail-ascii/with-tail-metrics view data-map
      (fn [tail-data _samples _transforms _mc]
        (when-let [output (tail-ascii/render-ascii-hill-plot tail-data opts)]
          (println output)
          (println))))))

(defmethod view/hill-plot* :print
  [_ view data-map]
  (print-hill-plot view data-map))

;;; MRL Plot View

(defn print-mrl-plot
  "Print ASCII mean residual life plot for all metrics."
  [view data-map]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [n]
                    (format "%s Mean Residual Life Plot (n=%d)"
                            (print-core/format-sublabel "Chart") n))
        opts (assoc view
                    :header-fn header-fn
                    :indent indent
                    :width 70
                    :height 12)]
    (tail-ascii/with-tail-metrics view data-map
      (fn [tail-data _samples transforms _mc]
        (when-let [output
                   (tail-ascii/render-ascii-mrl-plot tail-data transforms opts)]
          (println output)
          (println))))))

(defmethod view/mrl-plot* :print
  [_ view data-map]
  (print-mrl-plot view data-map))

;;; Zipf Plot View

(defn print-zipf-plot
  "Print ASCII Zipf plot (log-log CCDF) for all metrics."
  [view data-map]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [n]
                    (format "%s Zipf Plot (log-log CCDF, n=%d)"
                            (print-core/format-sublabel "Chart") n))
        opts (assoc view
                    :header-fn header-fn
                    :indent indent
                    :width 70
                    :height 12)]
    (tail-ascii/with-tail-metrics view data-map
      (fn [_tail-data samples transforms _mc]
        (when samples
          (when-let [output
                     (tail-ascii/render-ascii-zipf-plot
                      samples
                      transforms
                      opts)]
            (println output)
            (println)))))))

(defmethod view/zipf-plot* :print
  [_ view data-map]
  (print-zipf-plot view data-map))

;;; Exponential Q-Q Plot View

(defn print-exponential-qq-plot
  "Print ASCII exponential Q-Q plot for all metrics."
  [view data-map]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [n]
                    (format "%s Exponential Q-Q Plot (n=%d exceedances)"
                            (print-core/format-sublabel "Chart") n))
        opts (assoc view
                    :header-fn header-fn
                    :indent indent
                    :width 70
                    :height 12)]
    (tail-ascii/with-tail-metrics view data-map
      (fn [tail-data samples transforms _mc]
        (when (and samples (:threshold tail-data))
          (when-let [output (tail-ascii/render-ascii-exponential-qq
                             samples (:threshold tail-data) transforms opts)]
            (println output)
            (println)))))))

(defmethod view/exponential-qq-plot* :print
  [_ view data-map]
  (print-exponential-qq-plot view data-map))

;;; GPD Q-Q Plot View

(defn print-gpd-qq-plot
  "Print ASCII GPD Q-Q plot for all metrics."
  [view data-map]
  (let [indent (print-core/sublabel-indent-str)
        header-fn (fn [n]
                    (format "%s GPD Q-Q Plot (n=%d exceedances)"
                            (print-core/format-sublabel "Chart") n))
        opts (assoc view
                    :header-fn header-fn
                    :indent indent
                    :width 70
                    :height 12)]
    (tail-ascii/with-tail-metrics view data-map
      (fn [tail-data samples transforms _mc]
        (when (and samples (:threshold tail-data) (:gpd tail-data))
          (when-let [output (tail-ascii/render-ascii-gpd-qq
                             samples (:threshold tail-data) (:gpd tail-data)
                             transforms opts)]
            (println output)
            (println)))))))

(defmethod view/gpd-qq-plot* :print
  [_ view data-map]
  (print-gpd-qq-plot view data-map))
