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
   [criterium.viewer.kindly.core :as kindly.core]))

;;; Chart Dimensions

(def ^:private chart-width
  "Width for Kindly vega-lite charts, sized for notebook display."
  700)

(def ^:private chart-height
  "Height for Kindly vega-lite charts, sized for notebook display."
  350)

;;; Helper Functions

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
                       (format-distribution-table-row dist result best-model))
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
              (when-let [ci-rows (format-parameter-ci-rows best-model parameter-cis)]
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
        {:width chart-width
         :height chart-height})))))

;;; Distribution CDF View

(defmethod view/distribution-cdf* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly.core/kindly-heading "Distribution CDF")
      (kindly.core/kindly-vega-lite
       (charts.distribution/distribution-cdf-vega-spec data-map view {:width chart-width
                                                                      :height chart-height})))))

;;; Distribution Q-Q View

(defmethod view/distribution-qq* :kindly
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (kindly.core/kindly-heading "Q-Q Plot")
      (kindly.core/kindly-vega-lite
       (charts.quantile/distribution-qq-vega-spec data-map view {:width chart-width
                                                                 :height chart-height})))))
