(ns criterium.viewer.portal.distribution
  "Distribution fit views for portal viewer.

  Contains views for:
  - Distribution model comparison (AIC, BIC, goodness-of-fit tests)
  - Parameter confidence intervals for best-fit models
  - Distribution PDF, CDF, and Q-Q plot charts"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.common-charts.quantile :as charts.quantile]
   [criterium.viewer.common.distribution :as common.distribution]
   [criterium.viewer.portal.core :as portal.core]))

(defmethod view/distribution-models* :portal
  [_ {:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)]
        (when (seq fits)
          (doseq [[path fit-data] fits]
            (let [{:keys [n warning distributions best-model]} fit-data
                  metric-label (name (first path))]
              (portal.core/heading (str
                                    "Distribution Models: "
                                    metric-label
                                    " (n="
                                    n
                                    (when warning " - small sample")
                                    ")"))
              (portal.core/portal-table
               (mapv (fn [[dist result]]
                       (common.distribution/format-distribution-table-row
                        dist result best-model))
                     (sort-by (fn [[_ r]] (or (:delta-aic r) Double/MAX_VALUE))
                              distributions))))))))))

(defmethod view/distribution-parameter-cis* :portal
  [_ {:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)]
        (when (seq fits)
          (doseq [[path fit-data] fits]
            (let [{:keys [best-model parameter-cis]} fit-data
                  metric-label (name (first path))]
              (when-let [ci-rows (common.distribution/format-parameter-ci-rows
                                  best-model parameter-cis)]
                (portal.core/heading (str "Parameter CIs: " metric-label))
                (portal.core/portal-table ci-rows)))))))))

(defmethod view/distribution-pdf* :portal
  [_ {:keys [kde-id histogram-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (portal.core/heading "Distribution PDF")
      (portal.core/portal-vega-lite
       (charts.distribution/distribution-pdf-vega-spec
        data-map
        ;; Only pass histogram-id if histograms data exists
        (cond-> view
          (and (not histogram-id) (data-map :histograms))
          (assoc :histogram-id :histograms))
        {:height 400})))))

(defmethod view/distribution-cdf* :portal
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (portal.core/heading "Distribution CDF")
      (portal.core/portal-vega-lite
       (charts.distribution/distribution-cdf-vega-spec
        data-map
        view
        {:height 400})))))

(defmethod view/distribution-qq* :portal
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (portal.core/heading "Q-Q Plot")
      (portal.core/portal-vega-lite
       (charts.quantile/distribution-qq-vega-spec
        data-map
        view
        {:height 400})))))
