(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.benchmark :as benchmark]
   [criterium.domain.types :as domain.types]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts.autocorrelation :as charts.autocorrelation]
   [criterium.viewer.common-charts.comparison :as charts.comparison]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.common-charts.quantile :as charts.quantile]
   [criterium.viewer.common-charts.regression :as charts.regression]
   [criterium.viewer.common-charts.samples :as charts.samples]
   [criterium.viewer.common-charts.tail :as charts.tail]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.common.bootstrap :as bootstrap]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.regression :as regression]
   [criterium.viewer.common.shape :as shape]))

(defonce tapped (atom {:values '()}))

(defn submit
  "Tap target function.

  This allows criterium to control the order of tapped output.

  ```clojure
  (def submit (criterium.viewer.portal/submit #'portal.api/submit))
  (add-tap #'submit)`
  (remove-tap #'submit)
  ``"
  [portal-submit]
  (swap! tapped assoc :portal-submit portal-submit)
  (fn
    [value]
    (swap! tapped update :values conj value)))

(defn flush
  "Flush tapped output"
  []
  (tap> ::_)
  (loop [i 0]
    (when (not= ::_ (first (:values @tapped)))
      (when (< i 1000)
        (Thread/yield)
        (recur (unchecked-inc i)))))

  (let [[{:keys [portal-submit values]}] (swap-vals! tapped assoc :values '())]
    (doseq [value values]
      (when (not= ::_ value)
        (portal-submit value)))))

(defmethod view/flush-viewer :portal [_]
  (flush))

(defn portal-heading [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/hiccup})))

(defn portal-table [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/table})))

(defn portal-vega-lite [s]
  (tap> (with-meta
          (assoc s :$schema "https://vega.github.io/schema/vega-lite/v5.json")
          {:portal.viewer/default :portal.viewer/vega-lite})))

(defn portal-vega
  "Submit a full Vega spec to Portal."
  [s]
  (tap> (with-meta
          (assoc s :$schema "https://vega.github.io/schema/vega/v5.json")
          {:portal.viewer/default :portal.viewer/vega})))

(defn heading [s]
  (portal-heading [:b s]))

(defmethod view/metrics* :portal
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (portal-table
     (core/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (heading "Summary stats")
          (portal-table
           (core/stats-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

(defmethod view/extremes* :portal
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (heading "Extremes")
          (portal-table
           (core/extremes-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

(defmethod view/event-stats* :portal
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (have (:metrics-defs event-stats-map))
            stats (core/event-stats
                   metrics-defs
                   (util/event-stats event-stats-map))]
        (when (seq stats)
          (heading "Event stats")
          (portal-table stats))))))

(defmethod view/quantiles* :portal
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (heading "Quantiles")
    (portal-table
     (core/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :portal
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outliers")
    (portal-table
     (core/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/outlier-significance* :portal
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outlier Significance")
    (portal-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/collect-plan* :portal
  [_ _view data-map]
  (heading "Collect plan")
  (portal-table
   (core/collect-plan-data data-map)))

(defmethod view/samples* :portal
  [_ view data-map]
  (heading "Samples")
  (portal-vega-lite
   (charts.samples/samples-vega-spec data-map view {:height 800})))

(defmethod view/histogram* :portal
  [_ view data-map]
  (heading "Histogram")
  (portal-vega-lite
   (charts.samples/histogram-vega-spec data-map view {:height 800})))

(defmethod view/kde* :portal
  [_ view data-map]
  (let [kde-id (or (:kde-id view) :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (heading "Kernel Density Estimation")
      (portal-vega-lite
       (charts.distribution/kde-vega-spec data-map view {:height 400})))))

(defmethod view/sample-percentiles* :portal
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quant-samples-id)]
    (heading "Percentiles")
    (portal-vega-lite
     {:data {:values [{}]} ; for portal
      :height 800
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts.samples/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defmethod view/sample-diffs* :portal
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metric-configs (:metric-configs quant-samples)]
    (heading "Sample diffs")
    (portal-vega-lite
     {:data {:values [{}]} ; for portal
      :height 800
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(charts.samples/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

(defmethod view/bootstrap-stats* :portal
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)
        metrics-defs (:metrics-defs bootstrap-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        bootstrap (util/bootstrap bootstrap-map)
        transforms (util/get-transforms data-map bootstrap-stats-id)]
    (when (seq metric-configs)
      (heading "Bootstrap Statistics")
      (portal-table
       (vec
        (for [m metric-configs
              :let [stat (get-in bootstrap (:path m))]
              :when stat]
          (bootstrap/bootstrap-stat-row m stat transforms)))))))

(defmethod view/shape-stats* :portal
  [_ {:keys [bootstrap-stats-id] :as _view} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (-> (:metrics-defs bootstrap-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            shape-data (shape/shape-stats-data metric-configs bootstrap)]
        (when (seq shape-data)
          (heading "Shape Statistics")
          (portal-table
           (mapv (fn [{:keys [metric skewness skewness-class
                              kurtosis kurtosis-class cv cv-class]}]
                   {:metric metric
                    :skewness skewness
                    :skewness-interpretation (name skewness-class)
                    :kurtosis kurtosis
                    :kurtosis-interpretation (name kurtosis-class)
                    :cv cv
                    :cv-interpretation (name cv-class)})
                 shape-data)))))))

;;; Distribution Fit Views

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
              (heading (str "Distribution Models: " metric-label
                            " (n=" n (when warning " - small sample") ")"))
              (portal-table
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
                (heading (str "Parameter CIs: " metric-label))
                (portal-table ci-rows)))))))))

(defmethod view/distribution-pdf* :portal
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (heading "Distribution PDF")
      (portal-vega-lite
       (charts.distribution/distribution-pdf-vega-spec
        data-map
        (assoc view :histogram-id :histograms)
        {:height 400})))))

(defmethod view/distribution-cdf* :portal
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (heading "Distribution CDF")
      (portal-vega-lite
       (charts.distribution/distribution-cdf-vega-spec data-map view {:height 400})))))

(defmethod view/distribution-qq* :portal
  [_ {:keys [kde-id] :as view} data-map]
  (let [kde-id (or kde-id :kde)
        kde-map (data-map kde-id)]
    (when kde-map
      (heading "Q-Q Plot")
      (portal-vega-lite
       (charts.quantile/distribution-qq-vega-spec data-map view {:height 400})))))

;;; Tail Analysis Views

(defn- get-tail-context
  "Extract common context for tail analysis views.
  Returns map with :tail-analysis-map, :tail-results, :metric-configs, :transforms,
  :samples-map, :metric->values, or nil if no data."
  [{:keys [tail-analysis-id samples-id]} data-map]
  (let [tail-analysis-id (or tail-analysis-id :tail-analysis)
        samples-id (or samples-id :samples)
        tail-analysis-map (get data-map tail-analysis-id)
        samples-map (get data-map samples-id)]
    (when tail-analysis-map
      (let [tail-results (:tail-analysis tail-analysis-map)
            metrics-defs (-> (:metrics-defs tail-analysis-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            raw-transform (:transform tail-analysis-map)
            transforms (when raw-transform
                         {:sample-> (let [s (:sample-> raw-transform)]
                                      (if (fn? s) (list s) s))
                          :->sample (let [s (:->sample raw-transform)]
                                      (if (fn? s) [s] s))})
            metric->values (when samples-map (util/metric->values samples-map))]
        (when (seq tail-results)
          {:tail-analysis-map tail-analysis-map
           :tail-results tail-results
           :metric-configs metric-configs
           :transforms transforms
           :samples-map samples-map
           :metric->values metric->values})))))

(defmethod view/tail-summary* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (core/tail-summary-table tail-data transforms)]
          (when (seq summary-rows)
            (heading (str "Tail Summary: " (:label mc)))
            (portal-table summary-rows)))))))

(defmethod view/tail-ratios* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [ratios-rows (core/tail-ratios-table-data tail-data)]
          (when (seq ratios-rows)
            (heading (str "Tail Ratios: " (:label mc)))
            (portal-table ratios-rows)))))))

(defmethod view/tail-high-quantiles* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [quantiles-rows (core/tail-high-quantiles-table tail-data transforms)]
          (when (seq quantiles-rows)
            (heading (str "High Quantile Estimates: " (:label mc)))
            (portal-table quantiles-rows)))))))

(defmethod view/tail-ratios-chart* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/tail-ratios-table tail-data)]
          (heading (str "Tail Ratios Chart: " (:label mc)))
          (portal-vega-lite (merge {:height 200} chart)))))))

(defmethod view/hill-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/hill-plot tail-data)]
          (heading (str "Hill Plot: " (:label mc)))
          (portal-vega-lite (merge {:height 400} chart)))))))

(defmethod view/mrl-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/mrl-plot tail-data transforms)]
          (heading (str "Mean Residual Life Plot: " (:label mc)))
          (portal-vega-lite (merge {:height 400} chart)))))))

(defmethod view/zipf-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [_tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [chart (charts.tail/zipf-plot samples transforms)]
            (heading (str "Zipf Plot: " (:label mc)))
            (portal-vega-lite (merge {:height 400} chart))))))))

(defmethod view/exponential-qq-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [chart (charts.tail/exponential-qq-plot samples threshold transforms)]
              (heading (str "Exponential Q-Q Plot: " (:label mc)))
              (portal-vega-lite (merge {:height 400} chart)))))))))

(defmethod view/gpd-qq-plot* :portal
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [gpd (:gpd tail-data)]
              (when-let [chart (charts.tail/gpd-qq-plot samples threshold gpd transforms)]
                (heading (str "GPD Q-Q Plot: " (:label mc)))
                (portal-vega-lite (merge {:height 400} chart))))))))))

(defmethod view/final-gc-warnings* :portal [_ _ _])

(defmethod view/os* :portal [_ _ _])

(defmethod view/runtime* :portal [_ _ _])

;;; Autocorrelation Views

(defmethod view/autocorrelation* :portal
  [_ _view _data-map]
  ;; Text summary not needed for portal - use acf-plot for visual output
  nil)

(defmethod view/acf-plot* :portal
  [_ {:keys [autocorrelation-id]} data-map]
  (let [autocorrelation-id (or autocorrelation-id :autocorrelation)
        autocorr-map (data-map autocorrelation-id)]
    (when autocorr-map
      (let [autocorr (util/autocorrelation autocorr-map)
            metrics-defs (:metrics-defs autocorr-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (doseq [mc metric-configs]
          (when-let [acf-data (get autocorr (:path mc))]
            (when-let [spec (charts.autocorrelation/acf-plot-vega-spec
                             acf-data
                             {:height 400
                              :title (str "ACF: " (:label mc)
                                          " (n="
                                          (get-in acf-data [:effective-sample-size :n-original])
                                          ")")})]
              (heading (str "Autocorrelation: " (:label mc)))
              (portal-vega-lite spec))))))))

;; Classification and effective sample size views are no-ops for portal
;; Use acf-plot for visual output
(defmethod view/autocorrelation-classification* :portal [_ _ _])
(defmethod view/effective-sample-size* :portal [_ _ _])

;;; Domain Views

(defmethod view/domain-extract-table* :portal
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [{:keys [rows] heading-text :heading}
                 (extract/prepare-domain-extract-table-transposed extract)]
        (heading heading-text)
        (portal-table rows))

      :multi-point
      (when-let [table-data (extract/prepare-domain-extract-table
                             extract {:header-sep " "})]
        (heading (:heading table-data))
        (portal-table (:rows table-data)))

      :default-table
      (when-let [table-data (extract/prepare-domain-extract-table
                             extract {:header-sep " "})]
        (heading (:heading table-data))
        (portal-table (:rows table-data))))))

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
            (portal-vega-lite box-spec)
            (portal-vega-lite
             (charts.comparison/single-point-bar-chart-spec extract {:height 400})))))

      :multi-point
      (when extract
        (portal-vega-lite
         (charts.comparison/domain-line-chart-spec extract {:height 400})))

      ;; :default-table - no chart output
      nil)))

(defmethod view/domain-grouped* :portal
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [rows] heading-text :heading}
               (extract/prepare-domain-grouped-table grouped)]
      (heading heading-text)
      (portal-table rows))))

(defmethod view/domain-comparison-table* :portal
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when-let [{:keys [rows] heading-text :heading}
                 (comparison/prepare-domain-comparison-table-transposed comparison)]
        (heading heading-text)
        (portal-table rows))

      :multi-point
      (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
        (doseq [{:keys [rows] heading-text :heading} tables]
          (heading heading-text)
          (portal-table rows)))

      :default-table
      (when-let [tables (comparison/prepare-domain-comparison-tables comparison)]
        (doseq [{:keys [rows] heading-text :heading} tables]
          (heading heading-text)
          (portal-table rows))))))

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
            (portal-vega-lite box-spec)
            (portal-vega-lite
             (charts.comparison/comparison-bar-chart-spec comparison {:height 400})))))

      :multi-point
      (when comparison
        (portal-vega-lite
         (charts.comparison/comparison-line-chart-spec comparison {:height 400})))

      ;; :default-table - no chart output
      nil)))

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
         (heading title)
         (when (seq points)
           (portal-vega-lite
            (charts.regression/log-log-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :metric-name (name (second metric)))))
           (when (seq residual-pts)
             (heading "Log-Log Residual Plot")
             (portal-vega-lite
              (charts.regression/log-log-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (/ chart-height 2)))))))

       :render-model-heading
       (fn [{:keys [title]}]
         (heading title))

       :render-model-table
       (fn [{:keys [table-rows]}]
         (when (seq table-rows)
           (portal-table table-rows)))

       :render-regression-charts
       (fn [{:keys [points line-pts residual-pts y-title residual-title chart-opts]}]
         (when (seq points)
           (portal-vega-lite
            (charts.regression/regression-chart-spec
             points line-pts
             (assoc chart-opts
                    :width chart-width
                    :height chart-height
                    :y-title y-title)))
           (when (seq residual-pts)
             (heading "Residual Plot")
             (portal-vega-lite
              (charts.regression/regression-residual-spec
               residual-pts
               (assoc chart-opts
                      :width chart-width
                      :height (/ chart-height 2)
                      :residual-title residual-title))))))})))

;;; Allocation Views

(defmethod view/allocation-summary* :portal
  [_ {:keys [summary-id]} data-map]
  (let [summary-id (or summary-id :allocation-summary)
        summary (data-map summary-id)]
    (when summary
      (let [{:keys [total-allocated total-freed num-allocations num-freed
                    freed-ratio]}
            summary
            total-allocated (long total-allocated)
            total-freed (long total-freed)
            retained (- total-allocated total-freed)]
        (heading "Allocation Summary")
        (portal-table
         [{:metric "Total allocated" :value total-allocated}
          {:metric "Total freed" :value total-freed}
          {:metric "Retained" :value retained}
          {:metric "Allocation count" :value num-allocations}
          {:metric "Freed count" :value num-freed}
          {:metric "Freed ratio"
           :value (format "%.1f%%" (* 100.0 (double freed-ratio)))}])))))

(defmethod view/allocation-hotspots* :portal
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)]
        (when (seq hotspots)
          (heading "Allocation Hotspots")
          (portal-table
           (mapv (fn [{:keys [call-site object-type count bytes freed-count freed-bytes]}]
                   {:call-site (allocation/format-call-site call-site nil)
                    :object-type (or object-type "")
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 hotspots)))))))

(defmethod view/allocation-by-type* :portal
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (heading "Allocations by Type")
          (portal-table
           (mapv (fn [[type-name {:keys [count bytes freed-count freed-bytes]}]]
                   {:type type-name
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 sorted)))))))

(defmethod view/allocation-treemap* :portal
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (heading "Allocation Treemap")
      (portal-vega (charts.profile/treemap-vega-spec treemap-data {})))))

;;; Call Tree Views

(defmethod view/call-tree* :portal
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (heading (format "Call Tree (%d total calls)" total-calls))
        (portal-vega (charts.profile/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :portal
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (heading "Call Flame Chart")
        (portal-vega (charts.profile/call-tree-flame-vega-spec call-tree total-calls {}))))))

(defmethod view/most-called* :portal
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (heading (format "Most Called Methods (top %d, %d total calls)"
                         (count methods) total-in-list))
        (portal-vega-lite (charts.profile/most-called-vega-lite-spec most-called-data {}))))))

;;; Modal Analysis Views

(defmethod view/multimodal-warning* :portal
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (heading (str "WARNING: Multimodal distribution - "
                   (:label metric-config)))
     (portal-table
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (portal-heading [:em "Mode locations:"])
       (portal-table
        (mapv (fn [{:keys [location density]}]
                {:location (modal/format-mode-location
                            location metric-config transforms)
                 :density (format "%.4g" density)})
              modes))))))

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
          (heading (format "Run: %s" (pr-str coord)))
          (view-fn viewer data))))))
