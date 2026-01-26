(ns criterium.viewer.print.distribution
  "Distribution fit views for print viewer.

  Contains views for:
  - Distribution model comparison (AIC, BIC, goodness-of-fit tests)
  - Parameter confidence intervals for best-fit models
  - No-op chart views (PDF, CDF, Q-Q plots not supported in print)"
  (:require
   [criterium.metric :as metric]
   [criterium.view :as view]
   [criterium.viewer.common.distribution :as common.distribution]
   [criterium.viewer.print.core :as print-core :refer [for-each-metric-keyed]]))

(defn- format-gof-result
  "Format a goodness-of-fit test result."
  [{:keys [statistic p-value]}]
  (when (and statistic p-value)
    (format "D=%.4f p=%.4f" statistic p-value)))

(defn- format-param-ci
  "Format a parameter confidence interval."
  [{:keys [point-estimate ci-lower ci-upper]}]
  (when point-estimate
    (format "%.4g [%.4g, %.4g]" point-estimate ci-lower ci-upper)))

(defn- print-distribution-result
  "Print a single distribution's fit result."
  [dist result is-best?]
  (let [label (get common.distribution/distribution-labels dist (name dist))
        marker (if is-best? " <- BEST" "")]
    (cond
      (:error result)
      (println (format "%20s: error - %s" label (:error result)))

      (:skipped result)
      (println (format "%20s: skipped (%s)" label (name (:skipped result))))

      :else
      (let [{:keys [aic delta-aic bic ks cvm]} result]
        (println (format "%20s: AIC=%.1f (Δ%.1f) BIC=%.1f%s"
                         label aic (or delta-aic 0.0) bic marker))
        (when ks
          (println (format "%20s  K-S: %s"
                           "" (format-gof-result ks))))
        (when cvm
          (println (format "%20s  CvM: %s"
                           "" (format-gof-result cvm))))))))

(defn- print-parameter-cis
  "Print parameter confidence intervals for best model."
  [dist cis]
  (when (seq cis)
    (let [label (get common.distribution/distribution-labels dist (name dist))]
      (println (format "%20s  Parameter CIs:" label))
      (doseq [[param ci-data] cis]
        (println (format "%20s    %s: %s"
                         "" (name param) (format-param-ci ci-data)))))))

(defn- print-distribution-models-for-metric
  "Print distribution model comparison for a single metric."
  [metric-config fit-data]
  (let [{:keys [n warning distributions best-model]} fit-data
        {:keys [label]} metric-config]
    (println (format "%s Distribution Models (n=%d%s)"
                     (print-core/format-label label) n
                     (if warning " - WARNING: small sample" "")))
    ;; Print each distribution result, best model first
    (when best-model
      (print-distribution-result best-model (get distributions best-model) true))
    (doseq [[dist result] (sort-by (fn [[_ r]] (or (:delta-aic r) Double/MAX_VALUE))
                                   distributions)]
      (when (not= dist best-model)
        (print-distribution-result dist result false)))
    (println)))

(defn print-distribution-models
  "Print distribution model comparison for all metrics."
  [{:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)
            metrics-defs (:metrics-defs (data-map :samples))
            metric-configs (when metrics-defs
                             (metric/all-metric-configs
                              (metric/filter-metrics
                               metrics-defs
                               (metric/type-pred :quantitative))))]
        (when (seq fits)
          (println "Distribution Model Comparison:")
          (if metric-configs
            (for-each-metric-keyed metric-configs fits
                                   print-distribution-models-for-metric)
            ;; Fallback if no metric-configs available
            (doseq [[path fit-data] fits]
              (print-distribution-models-for-metric
               {:label (str path) :path path}
               fit-data))))))))

(defmethod view/distribution-models* :print
  [_ view data-map]
  (print-distribution-models view data-map))

(defn- print-distribution-parameter-cis-for-metric
  "Print parameter CIs for a single metric's best model."
  [metric-config fit-data]
  (let [{:keys [best-model parameter-cis]} fit-data
        {:keys [label]} metric-config]
    (when (and best-model (get parameter-cis best-model))
      (println (format "%s %s Parameter CIs"
                       (print-core/format-label label)
                       (get common.distribution/distribution-labels best-model (name best-model))))
      (print-parameter-cis best-model (get parameter-cis best-model))
      (println))))

(defn print-distribution-parameter-cis
  "Print parameter CIs for best models across all metrics."
  [{:keys [distribution-fit-id] :as _view} data-map]
  (let [distribution-fit-id (or distribution-fit-id :distribution-fit)
        distribution-fit-map (data-map distribution-fit-id)]
    (when distribution-fit-map
      (let [fits (:fits distribution-fit-map)
            metrics-defs (:metrics-defs (data-map :samples))
            metric-configs (when metrics-defs
                             (metric/all-metric-configs
                              (metric/filter-metrics
                               metrics-defs
                               (metric/type-pred :quantitative))))]
        (when (seq fits)
          (println "Distribution Parameter Confidence Intervals:")
          (if metric-configs
            (for-each-metric-keyed metric-configs fits
                                   print-distribution-parameter-cis-for-metric)
            ;; Fallback if no metric-configs available
            (doseq [[path fit-data] fits]
              (print-distribution-parameter-cis-for-metric
               {:label (str path) :path path}
               fit-data))))))))

(defmethod view/distribution-parameter-cis* :print
  [_ view data-map]
  (print-distribution-parameter-cis view data-map))

;; Chart views are no-ops for print viewer
(defmethod view/distribution-pdf* :print [_ _ _])
(defmethod view/distribution-cdf* :print [_ _ _])
(defmethod view/distribution-qq* :print [_ _ _])
