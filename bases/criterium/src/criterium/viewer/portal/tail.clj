(ns criterium.viewer.portal.tail
  "Tail analysis views for portal viewer.

  Contains views for:
  - Tail summary (GPD/Hill parameters)
  - Tail ratios (p99/p95, p999/p99, p999/p95)
  - High quantile estimates (GPD extrapolation)
  - Chart views (tail ratio charts, Hill/MRL/Zipf plots, Q-Q plots)"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.tail :as charts.tail]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.tail :as common.tail]
   [criterium.viewer.portal.core :as portal.core]))

(defmethod view/tail-summary* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (core/tail-summary-table tail-data transforms)]
          (when (seq summary-rows)
            (portal.core/heading (str "Tail Summary: " (:label mc)))
            (portal.core/portal-table summary-rows)))))))

(defmethod view/tail-ratios* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [ratios-rows (core/tail-ratios-table-data tail-data)]
          (when (seq ratios-rows)
            (portal.core/heading (str "Tail Ratios: " (:label mc)))
            (portal.core/portal-table ratios-rows)))))))

(defmethod view/tail-high-quantiles* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [quantiles-rows (core/tail-high-quantiles-table tail-data transforms)]
          (when (seq quantiles-rows)
            (portal.core/heading (str "High Quantile Estimates: " (:label mc)))
            (portal.core/portal-table quantiles-rows)))))))

(defmethod view/tail-ratios-chart* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/tail-ratios-table tail-data)]
          (portal.core/heading (str "Tail Ratios Chart: " (:label mc)))
          (portal.core/portal-vega-lite (merge {:height 200} chart)))))))

(defmethod view/hill-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/hill-plot tail-data)]
          (portal.core/heading (str "Hill Plot: " (:label mc)))
          (portal.core/portal-vega-lite (merge {:height 400} chart)))))))

(defmethod view/mrl-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/mrl-plot tail-data transforms)]
          (portal.core/heading (str "Mean Residual Life Plot: " (:label mc)))
          (portal.core/portal-vega-lite (merge {:height 400} chart)))))))

(defmethod view/zipf-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [_tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [chart (charts.tail/zipf-plot samples transforms)]
            (portal.core/heading (str "Zipf Plot: " (:label mc)))
            (portal.core/portal-vega-lite (merge {:height 400} chart))))))))

(defmethod view/exponential-qq-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [chart (charts.tail/exponential-qq-plot
                              samples threshold transforms)]
              (portal.core/heading (str "Exponential Q-Q Plot: " (:label mc)))
              (portal.core/portal-vega-lite (merge {:height 400} chart)))))))))

(defmethod view/gpd-qq-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (common.tail/get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [gpd (:gpd tail-data)]
              (when-let [chart (charts.tail/gpd-qq-plot
                                samples threshold gpd transforms)]
                (portal.core/heading (str "GPD Q-Q Plot: " (:label mc)))
                (portal.core/portal-vega-lite (merge {:height 400} chart))))))))))
