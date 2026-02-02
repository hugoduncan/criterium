(ns criterium.viewer.kindly.domain
  "Domain analysis views for Kindly viewer.

  Provides views for:
  - Domain extract tables and charts
  - Domain grouped tables
  - Domain comparison tables and charts
  - Domain regression analysis with tables and charts"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.common-charts.regression :as charts.regression]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.regression :as regression]
   [criterium.viewer.kindly.core :as core]))

;;; Domain view implementations

(defmethod view/domain-extract-table* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [{:keys [heading col-headers rows]}
                 (extract/prepare-domain-extract-table-transposed extract)]
        (core/kindly-heading heading)
        (core/kindly-table rows {:column-names col-headers}))

      :multi-point
      (when-let [{:keys [heading coord-header col-headers rows]}
                 (extract/prepare-domain-extract-table
                  extract {:header-sep "\n"})]
        (core/kindly-heading heading)
        (core/kindly-table
         rows
         {:column-names (into [coord-header] col-headers)}))

      :default-table
      (when-let [{:keys [heading coord-header col-headers rows]}
                 (extract/prepare-domain-extract-table
                  extract {:header-sep "\n"})]
        (core/kindly-heading heading)
        (core/kindly-table
         rows
         {:column-names (into [coord-header] col-headers)})))))

(defmethod view/domain-extract-chart* :kindly
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when extract
        (let [box-spec (charts.comparison/single-point-box-chart-spec
                        extract
                        {:width core/chart-width
                         :height core/chart-height})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (core/kindly-vega-lite box-spec)
            (core/kindly-vega-lite
             (charts.comparison/single-point-bar-chart-spec
              extract
              {:width core/chart-width
               :height core/chart-height})))))

      :multi-point
      (when extract
        (core/kindly-vega-lite
         (charts.comparison/domain-line-chart-spec
          extract
          {:width core/chart-width
           :height core/chart-height})))

      ;; :default-table - no chart output
      nil)))

(defmethod view/domain-grouped* :kindly
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (extract/prepare-domain-grouped-table
                                       grouped)]
      (core/kindly-heading heading)
      (core/kindly-table rows))))

(defmethod view/domain-comparison-table* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when-let [{:keys [heading col-headers rows]}
                 (comparison/prepare-domain-comparison-table-transposed
                  comparison)]
        (core/kindly-heading heading)
        (core/kindly-table rows {:column-names col-headers}))

      :multi-point
      (when-let [tables (comparison/prepare-domain-comparison-tables
                         comparison)]
        (doseq [{:keys [heading coord-header col-headers rows]} tables]
          (core/kindly-heading heading)
          (core/kindly-table
           rows
           {:column-names (into [coord-header] col-headers)})))

      :default-table
      (when-let [tables (comparison/prepare-domain-comparison-tables
                         comparison)]
        (doseq [{:keys [heading coord-header col-headers rows]} tables]
          (core/kindly-heading heading)
          (core/kindly-table
           rows
           {:column-names (into [coord-header] col-headers)}))))))

(defmethod view/domain-comparison-chart* :kindly
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when comparison
        (let [box-spec
              (charts.comparison/comparison-box-chart-spec
               comparison
               {:width core/chart-width
                :height core/chart-height})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (core/kindly-vega-lite box-spec)
            (core/kindly-vega-lite
             (charts.comparison/comparison-bar-chart-spec
              comparison
              {:width core/chart-width
               :height core/chart-height})))))

      :multi-point
      (when comparison
        (core/kindly-vega-lite
         (charts.comparison/comparison-line-chart-spec
          comparison
          {:width core/chart-width
           :height core/chart-height})))

      ;; :default-table - no chart output
      nil)))

(defmethod view/domain-regression* :kindly
  [_ {:keys [regression-id extract-id log-log-id tolerance]} data-map]
  (let [tolerance (double (or tolerance 0.01))
        legend-options {:orient "none"
                        :legendX 10
                        :legendY 10}]
    (regression/with-domain-regression-data
      data-map
      {:regression-id regression-id
       :extract-id extract-id
       :log-log-id log-log-id
       :tolerance tolerance
       :table-options {:best-fit-marker "✓"
                       :plotted-marker ""
                       :tolerance tolerance}}
      {:render-log-log-charts
       (fn [{:keys [title metric points line-pts residual-pts chart-opts]}]
         (core/kindly-heading title)
         (when (seq points)
           (core/kindly-vega-lite
            (charts.regression/log-log-chart-spec
             points line-pts
             (assoc chart-opts
                    :width core/chart-width
                    :height core/chart-height
                    :metric-name (name (second metric))
                    :legend-options legend-options)))
           (when (seq residual-pts)
             (core/kindly-heading "Log-Log Residual Plot")
             (core/kindly-vega-lite
              (charts.regression/log-log-residual-spec
               residual-pts
               (assoc chart-opts
                      :width core/chart-width
                      :height (long (/ (long core/chart-height) 2))
                      :legend-options legend-options))))))

       :render-model-heading
       (fn [{:keys [title]}]
         (core/kindly-heading title))

       :render-model-table
       (fn [{:keys [table-rows]}]
         (when (seq table-rows)
           (core/kindly-table table-rows)))

       :render-regression-charts
       (fn [{:keys
             [points line-pts residual-pts y-title residual-title chart-opts]}]
         (when (seq points)
           (core/kindly-vega-lite
            (charts.regression/regression-chart-spec
             points line-pts
             (assoc chart-opts
                    :width core/chart-width
                    :height core/chart-height
                    :y-title y-title
                    :legend-options legend-options)))
           (when (seq residual-pts)
             (core/kindly-heading "Residual Plot")
             (core/kindly-vega-lite
              (charts.regression/regression-residual-spec
               residual-pts
               (assoc chart-opts
                      :width core/chart-width
                      :height (long (/ (long core/chart-height) 2))
                      :residual-title residual-title
                      :legend-options legend-options))))))})))
