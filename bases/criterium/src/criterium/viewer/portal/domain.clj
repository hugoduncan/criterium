(ns criterium.viewer.portal.domain
  "Portal viewer domain analysis views.

  Provides Portal output for:
  - domain-grouped views
  - domain-extract tables and charts
  - domain-comparison tables and charts
  - domain-regression results
  - domain-apply iteration"
  (:require
   [criterium.benchmark :as benchmark]
   [criterium.domain.types :as domain.types]
   [criterium.view :as view]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.common-charts.regression :as charts.regression]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.regression :as regression]
   [criterium.viewer.portal.core :as portal.core]))

;;; Domain Extract Views

(defmethod view/domain-extract-table* :portal
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [{:keys [rows] heading-text :heading}
                 (extract/prepare-domain-extract-table-transposed extract)]
        (portal.core/heading heading-text)
        (portal.core/portal-table rows))

      :multi-point
      (when-let [table-data (extract/prepare-domain-extract-table
                             extract {:header-sep " "})]
        (portal.core/heading (:heading table-data))
        (portal.core/portal-table (:rows table-data)))

      :default-table
      (when-let [table-data (extract/prepare-domain-extract-table
                             extract {:header-sep " "})]
        (portal.core/heading (:heading table-data))
        (portal.core/portal-table (:rows table-data))))))

(defmethod view/domain-extract-chart* :portal
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when extract
        (let [box-spec (charts.comparison/single-point-box-chart-spec extract {:height 400})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (portal.core/portal-vega-lite box-spec)
            (portal.core/portal-vega-lite
             (charts.comparison/single-point-bar-chart-spec extract {:height 400})))))

      :multi-point
      (when extract
        (portal.core/portal-vega-lite
         (charts.comparison/domain-line-chart-spec extract {:height 400})))

      ;; :default-table - no chart output
      nil)))

;;; Domain Grouped Views

(defmethod view/domain-grouped* :portal
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [rows] heading-text :heading}
               (extract/prepare-domain-grouped-table grouped)]
      (portal.core/heading heading-text)
      (portal.core/portal-table rows))))

;;; Domain Comparison Views

(defmethod view/domain-comparison-table* :portal
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when-let [{:keys [rows] heading-text :heading}
                 (comparison/prepare-domain-comparison-table-transposed comparison)]
        (portal.core/heading heading-text)
        (portal.core/portal-table rows))

      :multi-point
      (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
        (doseq [{:keys [rows] heading-text :heading} tables]
          (portal.core/heading heading-text)
          (portal.core/portal-table rows)))

      :default-table
      (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
        (doseq [{:keys [rows] heading-text :heading} tables]
          (portal.core/heading heading-text)
          (portal.core/portal-table rows))))))

(defmethod view/domain-comparison-chart* :portal
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when comparison
        (let [box-spec (charts.comparison/comparison-box-chart-spec comparison {:height 400})]
          ;; Fall back to bar chart if box plot has no data (missing bootstrap stats)
          (if (seq (:vconcat box-spec))
            (portal.core/portal-vega-lite box-spec)
            (portal.core/portal-vega-lite
             (charts.comparison/comparison-bar-chart-spec comparison {:height 400})))))

      :multi-point
      (when comparison
        (portal.core/portal-vega-lite
         (charts.comparison/comparison-line-chart-spec comparison {:height 400})))

      ;; :default-table - no chart output
      nil)))

;;; Domain Regression Views

(defmethod view/domain-regression* :portal
  [_ {:keys [regression-id extract-id log-log-id tolerance]} data-map]
  (let [tolerance (double (or tolerance 0.01))
        chart-width 600
        chart-height 400]
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
         (portal.core/heading title)
         (when (seq points)
           (portal.core/portal-vega-lite
            (charts.regression/log-log-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :metric-name (name (second metric)))))
           (when (seq residual-pts)
             (portal.core/heading "Log-Log Residual Plot")
             (portal.core/portal-vega-lite
              (charts.regression/log-log-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (/ chart-height 2)))))))

       :render-model-heading
       (fn [{:keys [title]}]
         (portal.core/heading title))

       :render-model-table
       (fn [{:keys [table-rows]}]
         (when (seq table-rows)
           (portal.core/portal-table table-rows)))

       :render-regression-charts
       (fn [{:keys [points line-pts residual-pts y-title residual-title chart-opts]}]
         (when (seq points)
           (portal.core/portal-vega-lite
            (charts.regression/regression-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :y-title y-title)))
           (when (seq residual-pts)
             (portal.core/heading "Residual Plot")
             (portal.core/portal-vega-lite
              (charts.regression/regression-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (/ chart-height 2)
                      :residual-title residual-title))))))})))

;;; Domain Apply View

(defmethod view/domain-apply* :portal
  [viewer {:keys [domain-id view-spec]} data-map]
  (let [domain-id (or domain-id :domain)
        domain (get data-map domain-id)]
    (cond
      (nil? view-spec)
      (binding [*out* *err*]
        (println "WARNING: domain-apply requires :view-spec option"))

      (nil? domain)
      nil

      :else
      (let [view-fn (benchmark/->view [view-spec])]
        (doseq [{:keys [coord data]} (domain.types/runs domain)]
          (portal.core/heading (format "Run: %s" (pr-str coord)))
          (view-fn viewer data))))))
