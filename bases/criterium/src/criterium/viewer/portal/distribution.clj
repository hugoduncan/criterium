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
   [criterium.viewer.portal.core :as portal.core]))

(def ^:private distribution-labels
  "Human-readable labels for distributions."
  {:gamma "Gamma"
   :lognormal "Log-normal"
   :inverse-gaussian "Inverse Gaussian"
   :weibull "Weibull"})

(defn- format-distribution-table-row
  "Format a distribution fit result as a table row."
  [dist result best-model]
  (let [label (get distribution-labels dist (name dist))
        is-best? (= dist best-model)]
    (cond
      (:error result)
      {:distribution label
       :status "error"
       :aic "-"
       :delta-aic "-"
       :bic "-"
       :ks-stat "-"
       :ks-pvalue "-"
       :cvm-stat "-"
       :cvm-pvalue "-"
       :best? false}

      (:skipped result)
      {:distribution label
       :status (name (:skipped result))
       :aic "-"
       :delta-aic "-"
       :bic "-"
       :ks-stat "-"
       :ks-pvalue "-"
       :cvm-stat "-"
       :cvm-pvalue "-"
       :best? false}

      :else
      {:distribution label
       :status "fitted"
       :aic (format "%.1f" (:aic result))
       :delta-aic (format "%.1f" (or (:delta-aic result) 0.0))
       :bic (format "%.1f" (:bic result))
       :ks-stat (if-let [ks (:ks-test result)]
                  (format "%.4f" (:statistic ks)) "-")
       :ks-pvalue (if-let [ks (:ks-test result)]
                    (format "%.4f" (:p-value ks)) "-")
       :cvm-stat (if-let [cvm (:cvm-test result)]
                   (format "%.4f" (:statistic cvm)) "-")
       :cvm-pvalue (if-let [cvm (:cvm-test result)]
                     (format "%.4f" (:p-value cvm)) "-")
       :best? is-best?})))

(defn- format-parameter-ci-rows
  "Format parameter CIs as table rows."
  [best-model parameter-cis]
  (when (and best-model (get parameter-cis best-model))
    (let [label (get distribution-labels best-model (name best-model))
          cis (get parameter-cis best-model)]
      (mapv (fn [[param {:keys [point-estimate ci-lower ci-upper]}]]
              {:distribution label
               :parameter (name param)
               :estimate (format "%.4g" point-estimate)
               :ci-lower (format "%.4g" ci-lower)
               :ci-upper (format "%.4g" ci-upper)})
            cis))))

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
              (portal.core/heading (str "Distribution Models: " metric-label
                                        " (n=" n (when warning " - small sample") ")"))
              (portal.core/portal-table
               (mapv (fn [[dist result]]
                       (format-distribution-table-row dist result best-model))
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
              (when-let [ci-rows (format-parameter-ci-rows best-model parameter-cis)]
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
       (charts.distribution/distribution-cdf-vega-spec data-map view {:height 400})))))

(defmethod view/distribution-qq* :portal
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (portal.core/heading "Q-Q Plot")
      (portal.core/portal-vega-lite
       (charts.quantile/distribution-qq-vega-spec data-map view {:height 400})))))
