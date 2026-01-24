(ns criterium.viewer.common.distribution
  "Common distribution formatting utilities for viewer implementations.

  This namespace provides shared functions used by portal and kindly viewers
  for formatting distribution fit results in tables.")

;;; Constants

(def distribution-labels
  "Human-readable labels for distributions."
  {:gamma "Gamma"
   :lognormal "Log-normal"
   :inverse-gaussian "Inverse Gaussian"
   :weibull "Weibull"})

;;; Table Formatting

(defn format-distribution-table-row
  "Format a distribution fit result as a table row.

  Returns a map suitable for table display with keys:
  - :distribution - human-readable distribution name
  - :status - \"fitted\", \"error\", or skip reason
  - :aic, :delta-aic, :bic - information criteria
  - :ks-stat, :ks-pvalue - Kolmogorov-Smirnov test results
  - :cvm-stat, :cvm-pvalue - Cramér-von Mises test results
  - :best? - true if this is the best model"
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

(defn format-parameter-ci-rows
  "Format parameter CIs as table rows.

  Returns a vector of maps for table display, or nil if no CIs available.
  Each map has keys:
  - :distribution - distribution name
  - :parameter - parameter name
  - :estimate - point estimate
  - :ci-lower, :ci-upper - confidence interval bounds"
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
