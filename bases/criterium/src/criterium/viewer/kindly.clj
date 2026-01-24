(ns criterium.viewer.kindly
  "A viewer that outputs Kindly-annotated data structures for Clay notebooks.

  Uses an accumulator pattern where view functions append Kindly-annotated
  values to an atom. The `flush-viewer` multimethod returns a `kind/fragment`
  combining all accumulated values.

  No runtime dependency on scicloj/kindly - produces plain maps with
  appropriate `:kindly/kind` metadata."
  (:refer-clojure :exclude [flush])
  (:require
   [clojure.string :as str]
   [criterium.domain.types :as domain.types]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts.autocorrelation :as charts.autocorrelation]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.common-charts.tail :as charts.tail]
   [criterium.viewer.common.core :as common.core]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.shape :as shape]
   ;; Sub-namespaces - provide specialized views
   [criterium.viewer.kindly.allocation]
   [criterium.viewer.kindly.core :as core]
   [criterium.viewer.kindly.distribution]
   [criterium.viewer.kindly.domain]))

;;; Re-export from core for backwards compatibility

(def accumulated
  "Accumulator for Kindly-annotated values."
  core/accumulated)

(def last-fragment
  "Last flushed Kindly fragment for retrieval after bench completes."
  core/last-fragment)

(def kindly-add
  "Add a value to the accumulator."
  core/kindly-add)

(def kindly-heading
  "Add a markdown heading to the accumulator."
  core/kindly-heading)

(def kindly-table
  "Add a table to the accumulator.
  Optionally accepts :column-names in opts for explicit column ordering."
  core/kindly-table)

(def kindly-vega-lite
  "Add a Vega-Lite chart to the accumulator."
  core/kindly-vega-lite)

(def kindly-vega
  "Add a full Vega chart to the accumulator."
  core/kindly-vega)

(def flush
  "Return accumulated values as a kind/fragment and clear the accumulator.
  Also stores the fragment in `last-fragment` for retrieval after bench completes."
  core/flush)

;;; Chart Dimensions

(def ^:private chart-width
  "Width for Kindly vega-lite charts, sized for notebook display."
  700)

(def ^:private chart-height
  "Height for Kindly vega-lite charts, sized for notebook display."
  350)

;;; Shape statistics view

(defmethod view/shape-stats* :kindly
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
          (kindly-heading "Shape Statistics")
          (kindly-table
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

;;; Call Tree Views

(defmethod view/call-tree* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading (clojure.core/format "Call Tree (%d total calls)" total-calls))
        (kindly-vega (charts.profile/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading "Call Flame Chart")
        (kindly-vega (charts.profile/call-tree-flame-vega-spec call-tree total-calls {}))))))

(defmethod view/most-called* :kindly
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (kindly-heading (clojure.core/format "Most Called Methods (top %d, %d total calls)"
                                             (count methods) total-in-list))
        (kindly-vega-lite (charts.profile/most-called-vega-lite-spec most-called-data {}))))))

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

(defmethod view/tail-summary* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (common.core/tail-summary-table tail-data transforms)]
          (when (seq summary-rows)
            (kindly-heading (str "Tail Summary: " (:label mc)))
            (kindly-table summary-rows
                          {:column-names [:parameter :value]})))))))

(defmethod view/tail-ratios* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [ratios-rows (common.core/tail-ratios-table-data tail-data)]
          (when (seq ratios-rows)
            (kindly-heading (str "Tail Ratios: " (:label mc)))
            (kindly-table ratios-rows
                          {:column-names [:ratio :value :p95 :p99 :p999]})))))))

(defmethod view/tail-high-quantiles* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [quantiles-rows (common.core/tail-high-quantiles-table tail-data transforms)]
          (when (seq quantiles-rows)
            (kindly-heading (str "High Quantile Estimates: " (:label mc)))
            (kindly-table quantiles-rows
                          {:column-names [:quantile :estimate]})))))))

(defmethod view/tail-ratios-chart* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/tail-ratios-table tail-data)]
          (kindly-heading (str "Tail Ratios Chart: " (:label mc)))
          (kindly-vega-lite (merge {:width chart-width :height 200} chart)))))))

(defmethod view/hill-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/hill-plot tail-data)]
          (kindly-heading (str "Hill Plot: " (:label mc)))
          (kindly-vega-lite (merge {:width chart-width :height chart-height} chart)))))))

(defmethod view/mrl-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [chart (charts.tail/mrl-plot tail-data transforms)]
          (kindly-heading (str "Mean Residual Life Plot: " (:label mc)))
          (kindly-vega-lite (merge {:width chart-width :height chart-height} chart)))))))

(defmethod view/zipf-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [_tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [chart (charts.tail/zipf-plot samples transforms)]
            (kindly-heading (str "Zipf Plot: " (:label mc)))
            (kindly-vega-lite (merge {:width chart-width :height chart-height} chart))))))))

(defmethod view/exponential-qq-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [chart (charts.tail/exponential-qq-plot samples threshold transforms)]
              (kindly-heading (str "Exponential Q-Q Plot: " (:label mc)))
              (kindly-vega-lite (merge {:width chart-width :height chart-height} chart)))))))))

(defmethod view/gpd-qq-plot* :kindly
  [_ view data-map]
  (when-let [{:keys [tail-results metric-configs transforms metric->values]}
             (get-tail-context view data-map)]
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (when-let [samples (when metric->values (get metric->values (:path mc)))]
          (when-let [threshold (:threshold tail-data)]
            (when-let [gpd (:gpd tail-data)]
              (when-let [chart (charts.tail/gpd-qq-plot samples threshold gpd transforms)]
                (kindly-heading (str "GPD Q-Q Plot: " (:label mc)))
                (kindly-vega-lite (merge {:width chart-width :height chart-height} chart))))))))))

;;; Autocorrelation Views

(defmethod view/autocorrelation* :kindly
  [_ _view _data-map]
  ;; Text summary not needed for kindly - use acf-plot for visual output
  nil)

(defmethod view/acf-plot* :kindly
  [_ {:keys [autocorrelation-id min-severity]} data-map]
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
                             (cond-> {:width chart-width
                                      :height chart-height
                                      :title (str "ACF: " (:label mc)
                                                  " (n="
                                                  (get-in acf-data [:effective-sample-size :n-original])
                                                  ")")}
                               min-severity (assoc :min-severity min-severity)))]
              (kindly-heading (str "Autocorrelation: " (:label mc)))
              (kindly-vega-lite spec))))))))

(def ^:private severity-labels
  {:none "none"
   :minor "minor"
   :moderate "moderate"
   :severe "severe"
   :alternating-none "alternating"
   :alternating-minor "alternating"
   :alternating-moderate "alternating"
   :alternating-severe "alternating"})

(def ^:private displayable-patterns
  "Patterns that warrant display of pattern label and recommendations.
  Excludes :clean, :alternating-none, and :alternating-minor."
  #{:transient-effects :drift :periodic :severe
    :alternating-moderate :alternating-severe})

(defn- collect-classification-metrics
  "Collect classification data for all metrics. Returns seq of [class-data acf-data mc].
  Uses :classification-id to get classification analysis results, and looks up
  the source autocorrelation for lag-1 data."
  [{:keys [classification-id autocorrelation-id]} data-map]
  (let [classification-id (or classification-id :autocorrelation-classification)
        class-map (data-map classification-id)]
    (when class-map
      (let [class-data (util/autocorrelation-classification-data class-map)
            ;; Get autocorrelation data for lag-1
            autocorr-id (or autocorrelation-id (:source-id class-map))
            autocorr-map (when autocorr-id (data-map autocorr-id))
            autocorr (when autocorr-map (util/autocorrelation autocorr-map))
            metrics-defs (:metrics-defs class-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (for [mc metric-configs
              :let [p (:path mc)
                    cd (get class-data p)
                    acf (when autocorr (get autocorr p))]
              :when cd]
          [cd acf mc])))))

(defn- collect-ess-metrics
  "Collect ESS data for all metrics. Returns seq of [ess-data acf-data mc].
  Uses :ess-id to get ESS analysis results, and looks up
  the source autocorrelation for lag-1 data."
  [{:keys [ess-id autocorrelation-id]} data-map]
  (let [ess-id (or ess-id :effective-sample-size)
        ess-map (data-map ess-id)]
    (when ess-map
      (let [ess-data (util/effective-sample-size-data ess-map)
            ;; Get autocorrelation data for lag-1
            autocorr-id (or autocorrelation-id (:source-id ess-map))
            autocorr-map (when autocorr-id (data-map autocorr-id))
            autocorr (when autocorr-map (util/autocorrelation autocorr-map))
            metrics-defs (:metrics-defs ess-map)
            metric-configs (metric/all-metric-configs metrics-defs)]
        (for [mc metric-configs
              :let [p (:path mc)
                    ed (get ess-data p)
                    acf (when autocorr (get autocorr p))]
              :when ed]
          [ed acf mc])))))

(defn- format-anomalous-lags
  "Format anomalous lags for display.
  Returns a string like '1 (minor), 12 (moderate)' or nil if empty."
  [anomalous-lags lag-severities]
  (when (seq anomalous-lags)
    (str/join ", "
              (map (fn [lag]
                     (format "%d (%s)"
                             lag
                             (get severity-labels (get lag-severities lag :none) "unknown")))
                   anomalous-lags))))

(defn- format-detected-period
  "Format detected period with ACF value and severity.
  Returns a string like '60 samples (r=0.35, moderate)' or nil if period is nil."
  [detected-period acf-map lag-severities]
  (when detected-period
    (let [acf-val (get acf-map detected-period)
          severity (get lag-severities detected-period :none)]
      (if acf-val
        (format "%d samples (r=%.2f, %s)"
                detected-period
                (double acf-val)
                (get severity-labels severity "unknown"))
        (format "%d samples" detected-period)))))

(defmethod view/autocorrelation-classification* :kindly
  [_ view data-map]
  (let [metrics (collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (doseq [[class-data acf-data mc] metrics]
        (let [{:keys [ljung-box classification pattern detected-period]} class-data
              lag-1 (:lag-1 acf-data)
              acf-map (:acf acf-data)
              anomalous-lags (:anomalous-lags acf-data)
              lag-severities (:lag-severities acf-data)]
          (kindly-heading "Sample Independence Classification")
          (kindly-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format "%.2f (%s)"
                                      (:value lag-1)
                                      (get severity-labels (:severity lag-1) "unknown"))})
                    {:metric (str (:label mc) " Ljung-Box p-value")
                     :value (format "%.2f" (:p-value ljung-box))}
                    {:metric "Assessment"
                     :value (str/capitalize (name classification))}]
             (seq anomalous-lags)
             (conj {:metric "Anomalous lags"
                    :value (format-anomalous-lags anomalous-lags lag-severities)})

             (and (#{:warning :fail} classification)
                  (displayable-patterns pattern))
             (conj {:metric "Pattern" :value (name pattern)})

             (format-detected-period detected-period acf-map lag-severities)
             (conj {:metric "Suspected period"
                    :value (format-detected-period detected-period acf-map lag-severities)})

             true
             (->> (remove nil?) vec))))))))

(defmethod view/effective-sample-size* :kindly
  [_ view data-map]
  (let [metrics (collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (doseq [[ess-data acf-data mc] metrics]
        (let [{:keys [effective-sample-size ci-inflation-factor]} ess-data
              lag-1 (:lag-1 acf-data)]
          (kindly-heading "Effective Sample Size")
          (kindly-table
           (cond-> [(when lag-1
                      {:metric (str (:label mc) " Lag-1 autocorrelation")
                       :value (format
                               "%.2f (%s)"
                               (:value lag-1)
                               (get
                                severity-labels
                                (:severity lag-1)
                                "unknown"))})
                    {:metric "Effective sample size"
                     :value (format "%d of %d (%.0f%%)"
                                    (:n-effective effective-sample-size)
                                    (:n-original effective-sample-size)
                                    (* 100.0
                                       (double
                                        (:ratio effective-sample-size))))}
                    {:metric "CI inflation factor"
                     :value (format "%.2f×" ci-inflation-factor)}]
             true
             (->> (remove nil?) vec))))))))

;;; Modal Analysis Views

(defmethod view/multimodal-warning* :kindly
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (kindly-heading (str "WARNING: Multimodal distribution - "
                          (:label metric-config)))
     (kindly-table
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (kindly-add
        (with-meta
          ["*Mode locations:*"]
          {:kindly/kind :kind/md}))
       (kindly-table
        (mapv (fn [{:keys [location density]}]
                {:location (modal/format-mode-location
                            location metric-config transforms)
                 :density (format "%.4g" density)})
              modes))))))

;;; Domain Apply View

(defn- resolve-view-fn-without-flush
  "Resolve a view spec to a function without automatic flushing.
  Returns nil and logs a warning if the view-spec cannot be resolved.
  Used by domain-apply to accumulate all run outputs before flushing."
  [view-spec]
  (let [[view-kw opts] (if (sequential? view-spec)
                         [(first view-spec) (second view-spec)]
                         [view-spec {}])
        view-fn-var (ns-resolve 'criterium.view (symbol (name view-kw)))]
    (if view-fn-var
      (view-fn-var (or opts {}))
      (binding [*out* *err*]
        (println (format "WARNING: Unknown view-spec '%s' - no such view function in criterium.view"
                         view-kw))))))

(defmethod view/domain-apply* :kindly
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
      ;; Use direct view function resolution to avoid automatic flush
      ;; that benchmark/->view performs after each call.
      ;; For kindly, we want to accumulate all runs' output first.
      (when-let [view-fn (resolve-view-fn-without-flush view-spec)]
        (doseq [{:keys [coord data]} (domain.types/runs domain)]
          (kindly-heading (format "Run: %s" (pr-str coord)))
          (view-fn viewer data))))))
