(ns criterium.viewer.kindly.tail
  "Tail analysis views for kindly viewer.

  Contains views for:
  - Tail summary (GPD/Hill parameters)
  - Tail ratios (p99/p95, p999/p99, p999/p95)
  - High quantile estimates (GPD extrapolation)
  - Chart views (tail ratio charts, Hill/MRL/Zipf plots, Q-Q plots)"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.tail :as charts.tail]
   [criterium.viewer.common.core :as common.core]
   [criterium.viewer.common.tail :as common.tail]
   [criterium.viewer.kindly.core :as kindly.core]))

;;; Tail Analysis Views

(defmethod view/tail-summary* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (common.core/tail-summary-table
                            tail-data
                            transforms)]
          (when (seq summary-rows)
            (kindly.core/kindly-heading (str "Tail Summary: " (:label mc)))
            (kindly.core/kindly-table summary-rows
                                      {:column-names [:parameter :value]})))))))

(defmethod view/tail-ratios* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [ratios-rows (common.core/tail-ratios-table-data tail-data)]
          (when (seq ratios-rows)
            (kindly.core/kindly-heading (str "Tail Ratios: " (:label mc)))
            (kindly.core/kindly-table ratios-rows
                                      {:column-names [:ratio
                                                      :value
                                                      :p95
                                                      :p99
                                                      :p999]})))))))

(defmethod view/tail-high-quantiles* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [quantiles-rows (common.core/tail-high-quantiles-table
                              tail-data
                              transforms)]
          (when (seq quantiles-rows)
            (kindly.core/kindly-heading
             (str "High Quantile Estimates: " (:label mc)))
            (kindly.core/kindly-table quantiles-rows
                                      {:column-names [:quantile
                                                      :estimate]})))))))

(defmethod view/tail-ratios-chart* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/tail-ratios-table tail-data)]
          (kindly.core/kindly-heading (str "Tail Ratios Chart: " (:label mc)))
          (kindly.core/kindly-vega-lite
           (merge {:width kindly.core/chart-width :height 200} chart)))))))

(defmethod view/hill-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/hill-plot tail-data)]
          (kindly.core/kindly-heading (str "Hill Plot: " (:label mc)))
          (kindly.core/kindly-vega-lite
           (merge
            {:width kindly.core/chart-width :height kindly.core/chart-height}
            chart)))))))

(defmethod view/mrl-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/mrl-plot tail-data transforms)]
          (kindly.core/kindly-heading
           (str "Mean Residual Life Plot: " (:label mc)))
          (kindly.core/kindly-vega-lite
           (merge
            {:width kindly.core/chart-width :height kindly.core/chart-height}
            chart)))))))

(defmethod view/zipf-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [_tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values
                             (get metric->values (:path mc)))]
          (when-let [chart (charts.tail/zipf-plot samples transforms)]
            (kindly.core/kindly-heading (str "Zipf Plot: " (:label mc)))
            (kindly.core/kindly-vega-lite
             (merge
              {:width kindly.core/chart-width :height kindly.core/chart-height}
              chart))))))))

(defmethod view/exponential-qq-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values
                             (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [chart (charts.tail/exponential-qq-plot
                              samples
                              threshold
                              transforms)]
              (kindly.core/kindly-heading
               (str "Exponential Q-Q Plot: " (:label mc)))
              (kindly.core/kindly-vega-lite
               (merge
                {:width kindly.core/chart-width
                 :height kindly.core/chart-height}
                chart)))))))))

(defmethod view/gpd-qq-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values
                             (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [gpd (:gpd tail-data)]
              (when-let [chart (charts.tail/gpd-qq-plot
                                samples
                                threshold
                                gpd
                                transforms)]
                (kindly.core/kindly-heading (str "GPD Q-Q Plot: " (:label mc)))
                (kindly.core/kindly-vega-lite
                 (merge
                  {:width kindly.core/chart-width
                   :height kindly.core/chart-height}
                  chart))))))))))
