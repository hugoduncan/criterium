(ns criterium.viewer.kindly.distribution
  "Distribution fit views for kindly viewer.

  Contains views for:
  - Distribution model comparison (AIC, BIC, goodness-of-fit tests)
  - Parameter confidence intervals for best-fit models
  - Distribution PDF, CDF, and Q-Q plot charts"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.common-charts.quantile :as charts.quantile]
   [criterium.viewer.common.distribution :as common.distribution]
   [criterium.viewer.kindly.core :as kindly.core]))

;;; Distribution Models View

(defmethod view/distribution-models* :kindly
  [_ {:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)]
        (when (seq fits)
          (doseq [[path fit-data] fits]
            (let [{:keys [n warning distributions best-model]} fit-data
                  metric-label (name (first path))]
              (kindly.core/kindly-heading (str "Distribution Models: " metric-label
                                               " (n=" n (when warning " - small sample") ")"))
              (kindly.core/kindly-table
               (mapv (fn [[dist result]]
                       (common.distribution/format-distribution-table-row
                        dist result best-model))
                     (sort-by (fn [[_ r]] (or (:delta-aic r) Double/MAX_VALUE))
                              distributions))))))))))

;;; Distribution Parameter CIs View

(defmethod view/distribution-parameter-cis* :kindly
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
                (kindly.core/kindly-heading (str "Parameter CIs: " metric-label))
                (kindly.core/kindly-table ci-rows)))))))))

;;; Distribution PDF View

(defmethod view/distribution-pdf* :kindly
  [_ {:keys [kde-id histogram-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly.core/kindly-heading "Distribution PDF")
      (kindly.core/kindly-vega-lite
       (charts.distribution/distribution-pdf-vega-spec
        data-map
        ;; Only pass histogram-id if histograms data exists
        (cond-> view
          (and (not histogram-id) (data-map :histograms))
          (assoc :histogram-id :histograms))
        {:width kindly.core/chart-width
         :height kindly.core/chart-height})))))

;;; Distribution CDF View

(defmethod view/distribution-cdf* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly.core/kindly-heading "Distribution CDF")
      (kindly.core/kindly-vega-lite
       (charts.distribution/distribution-cdf-vega-spec data-map view {:width kindly.core/chart-width
                                                                      :height kindly.core/chart-height})))))

;;; Distribution Q-Q View

(defmethod view/distribution-qq* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly.core/kindly-heading "Q-Q Plot")
      (kindly.core/kindly-vega-lite
       (charts.quantile/distribution-qq-vega-spec data-map view {:width kindly.core/chart-width
                                                                 :height kindly.core/chart-height})))))
