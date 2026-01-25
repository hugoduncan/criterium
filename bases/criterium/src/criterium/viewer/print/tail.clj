(ns criterium.viewer.print.tail
  "Tail analysis views for print viewer.

  Contains views for:
  - Tail summary (GPD/Hill parameters)
  - Tail ratios (p99/p95, p999/p99, p999/p95)
  - High quantile estimates (GPD extrapolation)
  - No-op chart views (tail ratio charts, Hill/MRL/Zipf plots,
    Q-Q plots not supported in print)"
  (:require
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.print.core :as print-core]))

(set! *unchecked-math* false)

(defn- get-tail-analysis-context
  "Extract common context for tail analysis views.
  Returns map with :tail-results, :metric-configs, :transforms, or nil if no data."
  [{:keys [tail-analysis-id]} data-map]
  (let [tail-analysis-id (or tail-analysis-id :tail-analysis)
        tail-analysis-map (data-map tail-analysis-id)]
    (when tail-analysis-map
      (let [metrics-defs (-> (:metrics-defs tail-analysis-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            tail-results (:tail-analysis tail-analysis-map)
            transforms (util/get-transforms data-map tail-analysis-id)]
        (when (seq tail-results)
          {:tail-results tail-results
           :metric-configs metric-configs
           :transforms transforms})))))

(defn print-tail-summary
  "Print GPD/Hill summary statistics for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-analysis-context view data-map)]
    (println "Tail Summary:")
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (core/tail-summary-table tail-data transforms)]
          (when (seq summary-rows)
            (println (print-core/format-label (:label mc)))
            (doseq [{:keys [parameter value]} summary-rows]
              (println (format "%s  %s: %s" (print-core/label-str "") parameter value)))))))
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

(defn print-tail-ratios
  "Print tail ratio statistics for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-analysis-context view data-map)]
    (println "Tail Ratios:")
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [{:keys [tail-ratios empirical-quantiles]} tail-data]
          (when (and tail-ratios (seq tail-ratios))
            (let [sorted-ratios (sort-by first tail-ratios)]
              (doseq [[i [rname rval]] (map-indexed vector sorted-ratios)]
                (if (zero? i)
                  (println (format "%s %s"
                                   (print-core/format-label (str (:label mc) " tail ratios"))
                                   (format-tail-ratio [rname rval] empirical-quantiles)))
                  (println (format "%s  %s"
                                   (print-core/label-str "")
                                   (format-tail-ratio [rname rval] empirical-quantiles))))))))))
    (println)))

(defmethod view/tail-ratios* :print
  [_ view data-map]
  (print-tail-ratios view data-map))

(defn print-tail-high-quantiles
  "Print high quantile estimates from GPD extrapolation for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-analysis-context view data-map)]
    (println "High Quantile Estimates (GPD):")
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
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
                                 unit))))))))
    (println)))

(defmethod view/tail-high-quantiles* :print
  [_ view data-map]
  (print-tail-high-quantiles view data-map))

;;; Chart views are no-ops for print viewer
(defmethod view/tail-ratios-chart* :print [_ _ _])
(defmethod view/hill-plot* :print [_ _ _])
(defmethod view/mrl-plot* :print [_ _ _])
(defmethod view/zipf-plot* :print [_ _ _])
(defmethod view/exponential-qq-plot* :print [_ _ _])
(defmethod view/gpd-qq-plot* :print [_ _ _])
