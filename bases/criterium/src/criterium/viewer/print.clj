(ns criterium.viewer.print
  "A print viewer"
  (:require
   [clojure.string :as str]
   [criterium.jvm :as jvm]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.regression :as regression]))

(set! *unchecked-math* false)

(defn print-metrics
  [metrics metrics->values]
  (doseq [m metrics]
    (when-let [v (first (metrics->values (:path m)))]
      (println
       (format
        "%36s: %s"
        (:label m)
        (if (number? v)
          (format/format-value (:dimension m) (* v (:scale m)))
          v))))))

(defmethod view/metrics* :print
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (print-metrics metric-configs (util/metric->values metrics-samples))))

(defn print-stat
  [metric stat transforms]
  (when (:mean stat)
    (let [stat (util/transform-vals-> stat transforms)
          [scale unit] (format/scale
                        (:dimension metric)
                        (* (:scale metric) (:mean stat)))
          scale (* scale (:scale metric))]
      (println
       (format
        "%32s: %s %s  3σ [%s %s]  min %s"
        (:label metric)
        (format/format-scaled (:mean stat) scale)
        unit
        (format/format-scaled (:mean-minus-3sigma stat) scale)
        (format/format-scaled (:mean-plus-3sigma stat) scale)
        (format/format-scaled (:min-val stat) scale))))))

(defn print-stats
  [metrics stats transforms]
  (doseq [metric metrics]
    (print-stat metric (get-in stats (:path metric)) transforms)))

(defmethod view/stats* :print
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)
        metrics-defs (-> (:metrics-defs stats-map)
                         (metric/select-metrics metric-ids))
        metric-configs (metric/all-metric-configs metrics-defs)]
    (print-stats
     metric-configs
     (util/stats stats-map)
     (util/get-transforms data-map stats-id))))

(defn print-event-stats-metrics
  [event-stats metric ms]
  (let [sample-count-path
        (conj (vec (butlast (:path (first ms)))) :sample-count)]
    (when (and sample-count-path
               (pos? (event-stats sample-count-path)))
      (let [vals (mapv
                  (fn [m]
                    (format/format-value
                     (:dimension m)
                     (* (get event-stats (:path m))
                        (:scale m))))
                  (conj ms {:path sample-count-path
                            :dimension :count
                            :scale 1}))]
        (println (apply format (:summary metric) (:label metric) vals))))))

(defn print-event-stats
  [metrics-defs event-stats]
  {:pre [event-stats]}
  (doseq [[_k metric] metrics-defs]
    (if-let [groups (:groups metric)]
      (print-event-stats groups event-stats)
      (print-event-stats-metrics event-stats metric (:values metric)))))

(defmethod view/event-stats* :print
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (-> (:metrics-defs event-stats-map)
                             (metric/filter-metrics
                              (metric/type-pred :event)))
            event-stats (util/event-stats event-stats-map)]
        (print-event-stats metrics-defs event-stats)))))

(defn print-bootstrap-stat
  "Print bootstrap statistics for a metric.

  Output format:
  1. Median with 95% CI
  2. Mean with 95% CI
  3. p10-p90 percentile spread"
  [metric
   {:keys [mean quantiles]}]
  (let [{:keys [dimension label]} metric
        [scale units] (format/scale
                       dimension
                       (* (:scale metric) (:point-estimate mean)))
        mean-ci (:estimate-quantiles mean)
        median-est (get quantiles 0.5)
        median-ci (:estimate-quantiles median-est)
        p10-est (get quantiles 0.1)
        p90-est (get quantiles 0.9)
        scale (* (:scale metric) scale)]
    (when (and median-est (seq median-ci))
      (println
       (format "%36s: %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
               (str label " median")
               (* scale (:point-estimate median-est))
               units
               (* scale (-> median-ci first :value))
               (* scale (-> median-ci second :value))
               (-> median-ci first :alpha)
               (-> median-ci second :alpha))))
    (println
     (format "%36s: %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
             (str label " mean")
             (* scale (:point-estimate mean))
             units
             (* scale (-> mean-ci first :value))
             (* scale (-> mean-ci second :value))
             (-> mean-ci first :alpha)
             (-> mean-ci second :alpha)))
    (when (and p10-est p90-est)
      (println
       (format "%36s: [%.3g %.3g] %s (10th-90th percentile)"
               (str label " spread")
               (* scale (:point-estimate p10-est))
               (* scale (:point-estimate p90-est))
               units)))))

(defn print-bootstrap-stats
  [{:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)
        metrics-defs (:metrics-defs bootstrap-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        bootstrap (util/bootstrap bootstrap-map)]
    (doseq [metric metric-configs]
      (when-let [stat (get-in bootstrap (:path metric))]
        (print-bootstrap-stat metric stat)))))

(defmethod view/bootstrap-stats* :print
  [_ view data-map]
  (print-bootstrap-stats view data-map))

(defn print-final-gc-warnings
  [{:keys [final-gc-id samples-id warn-threshold]} data-map]
  {:pre [(number? warn-threshold)]}
  (let [final-gc-id (or final-gc-id :final-gc)
        samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-deps (:metrics-deps metrics-samples)
        gc-metric-configs (metric/all-metric-configs
                           (select-keys
                            metrics-deps
                            [:elapsed-time :garbage-collector]))
        metric (first gc-metric-configs)
        gc-time-metrics (->> (next gc-metric-configs)
                             (filterv #(= :time (:dimension %))))
        metric->values (util/metric->values metrics-samples)
        total (* (:scale metric)
                 (reduce + (metric->values [:elapsed-time])))
        gc-samples (-> data-map final-gc-id util/metric->values)
        total-gc (reduce
                  +
                  (mapv
                   (fn [m]
                     (* (:scale m) (reduce + (gc-samples (:path m)))))
                   gc-time-metrics))
        frac (/ total-gc total)]
    (when (and total-gc (> frac warn-threshold))
      (println (format "Final GC ran for %s, %.1f%% of total sampling time (%s)"
                       (format/format-value :time total-gc)
                       (* frac 100)
                       (format/format-value :time total))))))

(defmethod view/final-gc-warnings* :print
  [_ view data-map]
  (print-final-gc-warnings view data-map))

(defn- skewness-classification
  "Classify skewness based on medcouple value.
  Returns a keyword indicating the type and degree of skewness."
  [^double mc]
  (cond
    (< mc -0.6) :strongly-left-skewed
    (< mc -0.2) :moderately-left-skewed
    (< mc -0.05) :slightly-left-skewed
    (<= mc 0.05) :symmetric
    (<= mc 0.2) :slightly-right-skewed
    (<= mc 0.6) :moderately-right-skewed
    :else :strongly-right-skewed))

(defn- format-skewness
  "Format skewness classification for display."
  [classification]
  (case classification
    :strongly-left-skewed "strongly left-skewed"
    :moderately-left-skewed "moderately left-skewed"
    :slightly-left-skewed "slightly left-skewed"
    :symmetric "symmetric"
    :slightly-right-skewed "slightly right-skewed"
    :moderately-right-skewed "moderately right-skewed"
    :strongly-right-skewed "strongly right-skewed"))

(defn print-outlier-count
  "Print outlier counts for a metric.
  When show-medcouple? is true, also displays the medcouple value and skewness
  classification if available."
  [metric-config num-samples outliers show-medcouple?]
  (let [outlier-counts (:outlier-counts outliers)
        sum (reduce + (vals outlier-counts))
        mc (:medcouple outliers)]
    (when (pos? sum)
      (util/report "%32s: Found %d outliers in %d samples (%.3g %%)\n"
                   (:label metric-config)
                   sum
                   num-samples
                   (* 100.0 (/ sum num-samples)))
      (doseq [[c v] (->> outlier-counts
                         (filter #(pos? (val %))))]
        (util/report
         "                                 %12s\t %d (%2.4f %%)\n"
         (name c) v (* 100.0 (/ v num-samples)))))
    (when (and show-medcouple? mc)
      (let [classification (skewness-classification mc)]
        (util/report "%32s: medcouple %.4f (%s)\n"
                     (:label metric-config)
                     mc
                     (format-skewness classification))))))

(defn print-outlier-counts
  "Print outlier counts for all metrics.
  Options:
    :outliers-id - key for outliers in data-map (default :outliers)
    :show-medcouple - if true, display medcouple and skewness classification"
  [{:keys [outliers-id show-medcouple] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        num-samples (have (:num-samples outliers-map))
        outliers (util/outliers outliers-map)]
    (doseq [m metric-configs]
      (print-outlier-count m num-samples (get-in outliers (:path m)) show-medcouple))))

(defmethod view/outlier-counts* :print
  [_ view data-map]
  (print-outlier-counts view data-map))

(defn print-outlier-significance
  [metric-config outlier-significance]
  {:pre [(have? outlier-significance)]}
  (let [labels {:unaffected "unaffected"
                :slight "slightly inflated"
                :moderate "moderately inflated"
                :severe "severely inflated"}]
    (util/report "%s Variance contribution from outliers : %.3g %%"
                 (:label metric-config)
                 (* (:significance outlier-significance) 100.0))
    (util/report "%s Variance is %s by outliers\n"
                 (:label metric-config)
                 (-> outlier-significance :effect labels))))

(defn print-outlier-significances
  [{:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        metrics-defs (-> (:metrics-defs outlier-sig-map)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        outlier-sig (util/outlier-significance outlier-sig-map)]
    (doseq [m metric-configs]
      (print-outlier-significance
       m
       (have seq (get-in outlier-sig (:path m))
             {:metric m :outlier-sig outlier-sig})))))

(defmethod view/outlier-significance* :print
  [_ view data-map]
  (print-outlier-significances view data-map))

(defn- print-samples-with-outliers
  [metric->values transforms outliers metric]
  (let [path (:path metric)
        values (metric->values path)
        outlier-data (get-in outliers path)]
    (doseq [[i v] (sort-by first (:outliers outlier-data))]
      (println
       (format "%36s[%5d] %s %s"
               ""
               i
               (format/format-value
                (:dimension metric)
                (* (:scale metric)
                   (util/transform-sample-> (values i) transforms)))
               (name v))))))

(defmethod view/samples* :print
  [_ {:keys [samples-id outliers-id] :as _view} data-map]
  (let [samples-id (or samples-id :samples)
        outliers-id (or outliers-id :outliers)
        metrics-samples (data-map samples-id)
        outliers (data-map outliers-id)
        metrics-defs (-> (:metrics-defs outliers)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map samples-id)]

    (println
     (format "%32s: %d samples with batch-size %d"
             "Samples"
             (:num-samples metrics-samples) (:batch-size metrics-samples)))
    (when outliers
      (doseq [metric metric-configs]
        (println (format "%36s%s" "" (:label metric)))
        (print-samples-with-outliers
         (util/metric->values metrics-samples)
         transforms
         (util/outliers outliers)
         metric))
      (println))))

(defmethod view/collect-plan* :print
  [_ _view data-map]
  (let [warmup (some-> data-map :warmup)
        est (some-> data-map :estimation)
        samples (-> data-map :samples)
        fmt "%32s: %d samples with batch-size %d (%d evaluations)"]
    (println
     (format fmt
             "Sample Scheme"
             (:num-samples samples)
             (:batch-size samples)
             (:eval-count samples)))
    (when warmup
      (println
       (format fmt
               "Warmup"
               (:num-samples warmup) (:batch-size warmup)
               (* (:num-samples warmup) (:batch-size warmup)))))
    (when est
      (println
       (format fmt
               "Estimation"
               (:num-samples est) (:batch-size est)
               (* (:num-samples est) (:batch-size est)))))))

(defmethod view/histogram* :print
  [_ {:keys [histogram-id] :as _view} data-map]
  (let [histogram-id (or histogram-id :histograms)
        histograms (util/lookup-data data-map histogram-id)
        metrics-defs (-> (:metrics-defs histograms)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map histogram-id)
        histograms (->> metric-configs
                        (mapv
                         #(core/histogram
                           (have
                            some?
                            ((:histograms histograms) (:path %))
                            {:keys (keys (:histograms histograms))
                             :path %})
                           transforms
                           %)))]
    (doseq [h histograms]
      (println
       (format "%32s: %s Histogram"
               (-> h :metric-config :label)
               (-> h :unit)))
      (run!
       (fn [[x bin-count density]]
         (println
          (format "%34s %-7.3f %5d  %-7.3g" "" x (long bin-count) density)))
       (mapv vector (:centers h) (:counts h) (:density h)))
      (println))))

(defn- format-kde-mode
  "Format a single mode for display with significance indicator."
  [mode metric-config transforms]
  (let [{:keys [location density ci-lower ci-upper significant?]} mode
        {:keys [dimension scale]} metric-config
        loc (util/transform-sample-> location transforms)
        ci-lo (when ci-lower (util/transform-sample-> ci-lower transforms))
        ci-hi (when ci-upper (util/transform-sample-> ci-upper transforms))
        sig-marker (if significant? "*" "")]
    [(str (format/format-value dimension (* scale loc)) sig-marker)
     (format "%.4g" density)
     (if ci-lo (format/format-value dimension (* scale ci-lo)) "-")
     (if ci-hi (format/format-value dimension (* scale ci-hi)) "-")]))

(defn- print-kde-metric
  "Print KDE summary for a single metric with optional modes from separate analysis."
  [metric-config kde-data modes-data transforms]
  (let [{:keys [bandwidth n]} kde-data
        {:keys [label dimension scale]} metric-config
        bw (util/transform-sample-> bandwidth transforms)
        modes (when modes-data (:modes modes-data))
        n-modes (when modes-data (:n-modes modes-data))
        test-results (when modes-data (:test-results modes-data))]
    (println (format "%32s: KDE (n=%d)" label n))
    (println (format "%34s bandwidth: %s"
                     ""
                     (format/format-value dimension (* scale bw))))
    (when (seq modes)
      (println (format "%34s modes: %d (validated: %s)"
                       "" (count modes) (or n-modes "?")))
      (println (format "%34s %12s %12s %12s %12s"
                       "" "Location" "Density" "CI Lower" "CI Upper"))
      (doseq [mode modes]
        (let [[loc dens ci-lo ci-hi] (format-kde-mode mode metric-config transforms)]
          (println (format "%34s %12s %12s %12s %12s"
                           "" loc dens ci-lo ci-hi))))
      (when test-results
        (let [{:keys [method p-values]} test-results
              method-name (case method
                            :acr "ACR"
                            :silverman "Silverman"
                            (name method))]
          (println (format "%34s %s test p-values:" "" method-name))
          (doseq [k (sort (keys p-values))]
            (println (format "%36s k=%d: p=%.4f" "" k (get p-values k)))))))
    (println)))

(defmethod view/kde* :print
  [_ {:keys [kde-id modes-id] :as _view} data-map]
  (let [kde-id (or kde-id :kde)
        modes-id (or modes-id :modes)
        kde-map (get data-map kde-id)
        modes-map (get data-map modes-id)]
    (when kde-map
      (let [metrics-defs (-> (:metrics-defs kde-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map kde-id)
            kdes (:kdes kde-map)
            all-modes (when modes-map (:modes modes-map))]
        (doseq [metric-config metric-configs]
          (when-let [kde-data (get kdes (:path metric-config))]
            (let [modes-data (when all-modes (get all-modes (:path metric-config)))]
              (print-kde-metric metric-config kde-data modes-data transforms))))))))

(defmethod view/quantiles* :print
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (util/get-quantiles-entry data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)
        table (core/quantiles
               metric-configs
               (util/quantiles quantiles-map)
               transforms)]
    (doseq [vs table]
      (let [ks (sort (keys (dissoc vs :metric)))
            pks (filterv #{0.25 0.5 0.75} ks)
            oks (into [] (remove #{0.25 0.5 0.75}) ks)]
        (println
         (format "%22s Quantiles: %s"
                 (:metric vs)
                 (str/join ", " (mapv #(str % " " (vs %)) pks))))
        (doseq [ok oks]
          (println (format "%32s  %3.3g %s" "" ok (vs ok))))))))

(defmethod view/os* :print
  [_ _ _sampled]
  (let [ks [:arch :name :version :available-processors]]
    (apply println
           (-> (map
                #(%1 (jvm/os-details))
                ks)
               vec (conj "cpu(s)")))))

(defmethod view/runtime* :print
  [_ _ _sampled]
  (let [runtime-details (jvm/runtime-details)]
    (apply println (map #(%1 runtime-details) [:vm-name :vm-version]))
    (apply println "Runtime arguments:"
           (:input-arguments runtime-details))))

(defmethod view/sample-percentiles* :print
  [_ _view _sampled])
  ;; TODO

;;; Shape Statistics Views

(defn- format-skewness-class
  "Format skewness classification for display."
  [classification]
  (case classification
    :highly-left-skewed "highly left-skewed"
    :moderately-left-skewed "moderately left-skewed"
    :slightly-left-skewed "slightly left-skewed"
    :symmetric "symmetric"
    :slightly-right-skewed "slightly right-skewed"
    :moderately-right-skewed "moderately right-skewed"
    :highly-right-skewed "highly right-skewed"
    (name classification)))

(defn- format-kurtosis-class
  "Format kurtosis classification for display."
  [classification]
  (case classification
    :heavy-tails "heavy tails (leptokurtic)"
    :light-tails "light tails (platykurtic)"
    :normal-tails "normal tails (mesokurtic)"
    (name classification)))

(defn- format-cv-class
  "Format CV classification for display."
  [classification]
  (case classification
    :low-variability "low variability"
    :moderate-variability "moderate variability"
    :high-variability "high variability"
    (name classification)))

(defn print-shape-stats
  "Print shape statistics (skewness, kurtosis, CV) for bootstrap results."
  [{:keys [bootstrap-stats-id] :as _view} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (-> (:metrics-defs bootstrap-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            shape-data (viewer-common/shape-stats-data metric-configs bootstrap)]
        (when (seq shape-data)
          (println "Shape Statistics:")
          (doseq [{:keys [metric skewness skewness-class
                          kurtosis kurtosis-class
                          cv cv-class]} shape-data]
            (println
             (format "%32s: skewness %s (%s)"
                     metric skewness (format-skewness-class skewness-class)))
            (println
             (format "%32s  kurtosis %s (%s)"
                     "" kurtosis (format-kurtosis-class kurtosis-class)))
            (println
             (format "%32s  CV %s (%s)"
                     "" cv (format-cv-class cv-class)))))))))

(defmethod view/shape-stats* :print
  [_ view data-map]
  (print-shape-stats view data-map))

;;; Distribution Fit Views

(def ^:private distribution-labels
  "Human-readable labels for distributions."
  {:gamma "Gamma"
   :lognormal "Log-normal"
   :inverse-gaussian "Inverse Gaussian"
   :weibull "Weibull"})

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
  (let [label (get distribution-labels dist (name dist))
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
    (let [label (get distribution-labels dist (name dist))]
      (println (format "%20s  Parameter CIs:" label))
      (doseq [[param ci-data] cis]
        (println (format "%20s    %s: %s"
                         "" (name param) (format-param-ci ci-data)))))))

(defn- print-distribution-fit-for-metric
  "Print distribution fit results for a single metric."
  [metric-config fit-data]
  (let [{:keys [n warning distributions best-model parameter-cis]} fit-data
        {:keys [label]} metric-config]
    (println (format "%32s: Distribution Fit (n=%d%s)"
                     label n
                     (if warning " - WARNING: small sample" "")))
    ;; Print each distribution result, best model first
    (when best-model
      (print-distribution-result best-model (get distributions best-model) true))
    (doseq [[dist result] (sort-by (fn [[_ r]] (or (:delta-aic r) Double/MAX_VALUE))
                                   distributions)]
      (when (not= dist best-model)
        (print-distribution-result dist result false)))
    ;; Print parameter CIs for best model
    (when (and best-model (get parameter-cis best-model))
      (print-parameter-cis best-model (get parameter-cis best-model)))
    (println)))

(defn print-distribution-fit
  "Print distribution fit results for all metrics."
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
          (println "Distribution Fitting Results:")
          (if metric-configs
            (doseq [mc metric-configs]
              (when-let [fit-data (get fits (:path mc))]
                (print-distribution-fit-for-metric mc fit-data)))
            ;; Fallback if no metric-configs available
            (doseq [[path fit-data] fits]
              (print-distribution-fit-for-metric
               {:label (str path) :path path}
               fit-data))))))))

(defmethod view/distribution-fit* :print
  [_ view data-map]
  (print-distribution-fit view data-map))

;;; Domain Views

(defn- format-coord
  "Format a coordinate for display."
  [coord]
  (if (map? coord)
    (str/join " " (map (fn [[k v]] (str (name k) "=" v)) (sort-by key coord)))
    (name coord)))

(defn- format-extract-value
  "Format a value from domain-extract for display.
  Applies metric scale and formats with appropriate dimension.
  Handles both plain values and error-bound maps {:value :lower :upper}."
  [value metric-path]
  (let [raw-value (if (map? value) (:value value) value)]
    (if (nil? raw-value)
      "nil"
      (let [[dimension scale]
            (case (first metric-path)
              (:stats :log-stats)
              (case (second metric-path)
                :elapsed-time [:time 1e-9]
                :thread-allocation [:memory 1]
                [:count 1])
              [:count 1])]
        (format/format-value dimension (* raw-value scale))))))

(defn- format-extract-value-with-unit
  "Format a value with SI units for display."
  [value metric-path]
  (when (some? value)
    (let [base-value (* (double value)
                        (core/metric-path->base-scale metric-path))
          dimension (core/metric-path->dimension metric-path)]
      (if dimension
        (format/format-value dimension base-value)
        (format "%g" base-value)))))

(defn- sort-coords
  "Sort coordinate-value pairs, using numeric sort when coord values are numbers."
  [data single-key-info]
  (if single-key-info
    (let [k (:key single-key-info)
          all-numeric? (every? #(number? (get (first %) k)) data)]
      (if all-numeric?
        (sort-by #(get (first %) k) data)
        (sort-by #(str (get (first %) k)) data)))
    (sort-by #(str (first %)) data)))

(defn- format-coord-value
  "Format a coordinate for display, extracting the value for single-key maps."
  [coord single-key-info]
  (if single-key-info
    (str (get coord (:key single-key-info)))
    (format-coord coord)))

(defn- strip-uniform-axes
  "Strip uniform-value axes from coordinates.
  Only strips axes if doing so leaves at least one key in each coord.
  Returns coords unchanged if any coord is not a map."
  [coords]
  (if-not (every? map? coords)
    coords
    (let [uniform-axes (core/detect-uniform-axes coords)
          first-coord (first coords)
          remaining-keys (count (apply dissoc first-coord uniform-axes))]
      (if (and (seq uniform-axes) (pos? remaining-keys))
        (mapv #(apply dissoc % uniform-axes) coords)
        coords))))

(defn- print-transposed-table
  "Print a transposed table with implementation rows and metric columns.
  Takes {:heading :col-headers :rows} from prepare-*-table-transposed functions."
  [{:keys [heading col-headers rows]}]
  (println heading)
  (let [;; Calculate column widths
        col-widths (mapv
                    (fn [col-idx]
                      (let [header (nth col-headers col-idx)
                            values (map #(str (get % header "")) rows)]
                        (apply max (count header) (map count values))))
                    (range (count col-headers)))]
    ;; Print header row
    (print "  ")
    (doseq [[i header] (map-indexed vector col-headers)]
      (when (pos? i) (print " │ "))
      (print (format (str "%" (nth col-widths i) "s") header)))
    (println)
    ;; Print separator
    (print "  ")
    (doseq [[i w] (map-indexed vector col-widths)]
      (when (pos? i) (print "─┼─"))
      (print (apply str (repeat w "─"))))
    (println)
    ;; Print data rows
    (doseq [row rows]
      (print "  ")
      (doseq [[i header] (map-indexed vector col-headers)]
        (when (pos? i) (print " │ "))
        (print (format (str "%" (nth col-widths i) "s") (or (get row header) "-"))))
      (println))))

(defmethod view/domain-extract* :print
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [table (extract/prepare-domain-extract-table-transposed
                        extract)]
        (print-transposed-table table))

      ;; :multi-point and :default-table both use the standard format
      (when extract
        (doseq [[_metric-id {:keys [metric data]}] (:metrics extract)]
          (let [raw-coords (map first data)
                ;; Strip uniform axes (e.g., :impl :default for single-impl scenarios)
                stripped-coords (strip-uniform-axes raw-coords)
                coord-map (zipmap raw-coords stripped-coords)
                single-key-info (core/single-key-coord-info stripped-coords)
                sorted-data (sort-coords data single-key-info)]
            (println (format "Domain Extract: %s" (pr-str metric)))
            (doseq [[coord value] sorted-data]
              (let [display-coord (get coord-map coord coord)]
                (println (format "  %24s: %s"
                                 (format-coord-value display-coord single-key-info)
                                 (format-extract-value value metric)))))
            (println)))))))

(defmethod view/domain-grouped* :print
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped (data-map grouped-id)]
    (when-let [{:keys [heading rows]} (extract/prepare-domain-grouped-table
                                       grouped)]
      (println heading)
      (doseq [{:keys [axis-value run-count]} rows]
        (println (format "  %24s: %d run%s"
                         axis-value
                         run-count
                         (if (= 1 run-count) "" "s")))))))

(defn- coord-without-axis
  "Remove the axis key from a map coordinate, or return the coord unchanged
  if it's a keyword."
  [coord axis-key]
  (if (map? coord)
    (dissoc coord axis-key)
    coord))

(defn- format-row-key
  "Format a row key (coordinate without axis) for display."
  [row-key]
  (cond
    (keyword? row-key) (name row-key)
    (and (map? row-key) (empty? row-key)) "<all>"
    (map? row-key) (format-coord row-key)
    :else (str row-key)))

(defn- build-comparison-table
  "Build a table structure from comparison data for tabular display.
  Returns {:columns [col-headers] :rows [{:key row-key :values [vals]}]}."
  [axis data]
  (let [axis-vals (sort-by (comp str identity) (keys data))
        all-entries (mapcat (fn [[axis-val entries]]
                              (map (fn [{:keys [coord value]}]
                                     {:axis-val axis-val
                                      :row-key (coord-without-axis coord axis)
                                      :value value})
                                   entries))
                            data)
        row-keys (distinct (map :row-key all-entries))
        val-lookup (reduce (fn [m {:keys [axis-val row-key value]}]
                             (assoc-in m [row-key axis-val] value))
                           {}
                           all-entries)]
    {:columns axis-vals
     :rows (mapv (fn [row-key]
                   {:key row-key
                    :values (mapv #(get-in val-lookup [row-key %]) axis-vals)})
                 row-keys)}))

(defn- format-axis-val
  "Format an axis value for column header."
  [axis-val]
  (if (nil? axis-val) "<nil>" (str axis-val)))

(defn- print-comparison-table
  "Print comparison data as a formatted table."
  [axis metric data]
  (let [{:keys [columns rows]} (build-comparison-table axis data)
        formatted-vals (mapv (fn [{:keys [values]}]
                               (mapv #(format-extract-value % metric) values))
                             rows)
        col-headers (mapv format-axis-val columns)
        row-keys (mapv #(format-row-key (:key %)) rows)
        col-widths (mapv (fn [col-idx]
                           (apply max
                                  (count (nth col-headers col-idx))
                                  (map #(count (nth % col-idx)) formatted-vals)))
                         (range (count columns)))
        row-key-width (apply max 8 (map count row-keys))]
    (println (format "Domain Comparison by %s: %s" (name axis) (pr-str metric)))
    (print (format "  %s" (format (str "%" row-key-width "s") "")))
    (doseq [[i header] (map-indexed vector col-headers)]
      (print (format " │ %s" (format (str "%" (nth col-widths i) "s") header))))
    (println)
    (print (format "  %s" (apply str (repeat row-key-width "─"))))
    (doseq [w col-widths]
      (print (format "─┼─%s" (apply str (repeat w "─")))))
    (println)
    (doseq [[row-key vals] (map vector row-keys formatted-vals)]
      (print (format "  %s" (format (str "%" row-key-width "s") row-key)))
      (doseq [[i v] (map-indexed vector vals)]
        (print (format " │ %s" (format (str "%" (nth col-widths i) "s") v))))
      (println))))

(defn- print-single-metric-factor-table
  "Print single-metric comparison with factor display.
  Baseline impl shows absolute value with SI unit, others show factors."
  [axis metric implementations data]
  (let [baseline-impl (first implementations)
        other-impls (rest implementations)
        ;; Collect all row keys (coords without axis)
        all-row-keys (->> (vals data)
                          (mapcat (fn [entries]
                                    (map (fn [{:keys [coord]}]
                                           (coord-without-axis coord axis))
                                         entries)))
                          distinct
                          (sort-by str))
        ;; Build lookup: impl -> row-key -> value
        lookup (reduce (fn [acc [impl-val entries]]
                         (reduce (fn [acc2 {:keys [coord value]}]
                                   (let [row-key (coord-without-axis coord axis)]
                                     (assoc-in acc2 [impl-val row-key] value)))
                                 acc
                                 entries))
                       {}
                       data)
        ;; Build columns: baseline (with unit), other impls (value + factor)
        col-specs (vec (cons {:type :baseline :impl baseline-impl}
                             (mapcat (fn [impl]
                                       [{:type :value :impl impl}
                                        {:type :factor :impl impl}])
                                     other-impls)))
        col-headers (mapv (fn [{:keys [type impl]}]
                            (case type
                              :baseline (str (name impl))
                              :value (str (name impl))
                              :factor (str (name impl) " ×")))
                          col-specs)
        ;; Format cell values
        format-cell (fn [{:keys [type impl]} row-key]
                      (let [value (get-in lookup [impl row-key])
                            baseline-value (get-in lookup [baseline-impl row-key])]
                        (case type
                          :baseline (or (format-extract-value-with-unit value metric) "-")
                          :value (or (format-extract-value-with-unit value metric) "-")
                          :factor (cond
                                    (nil? value) "-"
                                    (nil? baseline-value) "-"
                                    (zero? baseline-value) "-"
                                    :else (format "%.2f" (double (/ value baseline-value)))))))
        formatted-rows (mapv (fn [row-key]
                               (mapv #(format-cell % row-key) col-specs))
                             all-row-keys)
        row-keys-formatted (mapv format-row-key all-row-keys)
        col-widths (mapv (fn [col-idx]
                           (apply max
                                  (count (nth col-headers col-idx))
                                  (map #(count (nth % col-idx)) formatted-rows)))
                         (range (count col-specs)))
        row-key-width (apply max 8 (map count row-keys-formatted))]
    (println (format "Domain Comparison by %s: %s" (name axis) (pr-str metric)))
    (print (format "  %s" (format (str "%" row-key-width "s") "")))
    (doseq [[i header] (map-indexed vector col-headers)]
      (print (format " │ %s" (format (str "%" (nth col-widths i) "s") header))))
    (println)
    (print (format "  %s" (apply str (repeat row-key-width "─"))))
    (doseq [w col-widths]
      (print (format "─┼─%s" (apply str (repeat w "─")))))
    (println)
    (doseq [[row-key vals] (map vector row-keys-formatted formatted-rows)]
      (print (format "  %s" (format (str "%" row-key-width "s") row-key)))
      (doseq [[i v] (map-indexed vector vals)]
        (print (format " │ %s" (format (str "%" (nth col-widths i) "s") v))))
      (println))))

(defn- print-multi-metric-comparison-table
  "Print multi-metric comparison with factor display.
  Baseline impl shows absolute values with SI units, others show factors."
  [axis implementations metrics]
  (let [baseline-impl (first implementations)
        other-impls (rest implementations)
        metric-ids (keys metrics)
        ;; Collect all row keys from all metrics and implementations
        all-row-keys (->> (vals metrics)
                          (mapcat (fn [{:keys [data]}]
                                    (mapcat (fn [[_impl-val entries]]
                                              (map (fn [{:keys [coord]}]
                                                     (coord-without-axis coord axis))
                                                   entries))
                                            data)))
                          distinct
                          (sort-by str))
        ;; Build lookup: metric-id -> impl -> row-key -> value
        lookup (reduce (fn [acc [metric-id {:keys [data]}]]
                         (reduce (fn [acc2 [impl-val entries]]
                                   (reduce (fn [acc3 {:keys [coord value]}]
                                             (let [row-key (coord-without-axis coord axis)]
                                               (assoc-in acc3 [metric-id impl-val row-key] value)))
                                           acc2
                                           entries))
                                 acc
                                 data))
                       {}
                       metrics)
        ;; Build columns: baseline metric (unit), other impl × for each metric
        col-specs (mapcat (fn [metric-id]
                            (let [metric-path (get-in metrics [metric-id :metric])]
                              (cons {:type :baseline
                                     :metric-id metric-id
                                     :metric-path metric-path
                                     :impl baseline-impl}
                                    (map (fn [impl]
                                           {:type :factor
                                            :metric-id metric-id
                                            :metric-path metric-path
                                            :impl impl})
                                         other-impls))))
                          metric-ids)
        ;; Format column headers
        col-headers (mapv (fn [{:keys [type metric-id impl]}]
                            (if (= type :baseline)
                              (str (name impl) " " (name metric-id))
                              (str (name impl) " ×")))
                          col-specs)
        ;; Format cell values
        format-cell (fn [{:keys [type metric-id metric-path impl]} row-key]
                      (let [value (get-in lookup [metric-id impl row-key])
                            baseline-value (get-in lookup [metric-id baseline-impl row-key])]
                        (if (= type :baseline)
                          (or (format-extract-value-with-unit value metric-path) "-")
                          ;; Factor relative to baseline
                          (cond
                            (nil? value) "-"
                            (nil? baseline-value) "-"
                            (zero? baseline-value) "-"
                            :else (format "%.2f" (/ value baseline-value))))))
        formatted-rows (mapv (fn [row-key]
                               (mapv #(format-cell % row-key) col-specs))
                             all-row-keys)
        row-keys-formatted (mapv format-row-key all-row-keys)
        col-widths (mapv (fn [col-idx]
                           (apply max
                                  (count (nth col-headers col-idx))
                                  (map #(count (nth % col-idx)) formatted-rows)))
                         (range (count col-specs)))
        row-key-width (apply max 8 (map count row-keys-formatted))]
    (println (format "Domain Comparison by %s" (name axis)))
    (print (format "  %s" (format (str "%" row-key-width "s") "")))
    (doseq [[i header] (map-indexed vector col-headers)]
      (print (format " │ %s" (format (str "%" (nth col-widths i) "s") header))))
    (println)
    (print (format "  %s" (apply str (repeat row-key-width "─"))))
    (doseq [w col-widths]
      (print (format "─┼─%s" (apply str (repeat w "─")))))
    (println)
    (doseq [[row-key vals] (map vector row-keys-formatted formatted-rows)]
      (print (format "  %s" (format (str "%" row-key-width "s") row-key)))
      (doseq [[i v] (map-indexed vector vals)]
        (print (format " │ %s" (format (str "%" (nth col-widths i) "s") v))))
      (println))))

(defmethod view/domain-comparison* :print
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when-let [table (comparison/prepare-domain-comparison-table-transposed
                        comparison)]
        (print-transposed-table table))

      ;; :multi-point and :default-table both use the standard format
      (when comparison
        (let [{:keys [axis metric metrics implementations data]} comparison]
          (if metrics
            ;; Multi-metric mode with factor display
            (if implementations
              (print-multi-metric-comparison-table axis implementations metrics)
              ;; Multi-metric mode without implementations - show all values
              (doseq [[_metric-id {:keys [metric data]}] metrics]
                (when (and (seq data) (some #(seq (second %)) data))
                  (print-comparison-table axis metric data))))
            ;; Single-metric mode
            (if (and (seq data) (some #(seq (second %)) data))
              (if implementations
                (let [data-keys (set (keys data))
                      missing (remove data-keys implementations)]
                  (when (seq missing)
                    (throw
                     (ex-info
                      "Domain :implementations do not match comparison data keys"
                      {:implementations implementations
                       :data-keys (keys data)
                       :missing missing})))
                  (print-single-metric-factor-table
                   axis
                   metric
                   implementations data))
                (print-comparison-table axis metric data))
              (println (format "Domain Comparison by %s: %s (no data)"
                               (name axis) (pr-str metric))))))))))

(defn- print-log-log-info
  "Print log-log regression summary."
  [slope r-squared multi-impl?]
  (when (and slope r-squared)
    (let [complexity (regression/format-log-log-slope slope)]
      (if multi-impl?
        (println (format "    Log-log: slope=%.3f ≈ %s (R²=%.4f)"
                         slope complexity r-squared))
        (println (format "  Log-log diagnostic: slope=%.3f ≈ %s (R²=%.4f)"
                         slope complexity r-squared))))))

(defn- print-model-rows
  "Print regression model rows with consistent formatting."
  [table-rows multi-impl? _tolerance]
  (when (seq table-rows)
    (let [label-width (reduce max (map #(count (:model %)) table-rows))]
      (doseq [{:keys [model r-squared aic bic equation best-fit implementation]}
              table-rows]
        (let [indent (if multi-impl? "    " "  ")
              impl-prefix (when (and multi-impl? implementation)
                            (str "[" implementation "] "))]
          (println
           (format "%s%s%s  R²=%s%s%s%s%s"
                   indent
                   (or impl-prefix "")
                   (format (str "%-" label-width "s") model)
                   r-squared
                   (if (and aic bic) (format "  AIC=%s BIC=%s" aic bic) "")
                   (if (seq equation) (str "  " equation) "")
                   (if (= best-fit "✓") "  <- best fit" "")
                   (if (and (not (= best-fit "✓")) (seq best-fit))
                     "  [plotted]" ""))))))))

(defmethod view/domain-regression* :print
  [_ {:keys [regression-id extract-id log-log-id tolerance]} data-map]
  (let [tolerance (double (or tolerance 0.01))]
    (regression/with-domain-regression-data
      data-map
      {:regression-id regression-id
       :extract-id extract-id
       :log-log-id log-log-id
       :tolerance tolerance
       :table-options {:best-fit-marker "✓"
                       :plotted-marker "*"
                       :tolerance tolerance}}
      {:render-log-log-charts
       (fn [{:keys [slope r-squared chart-opts]}]
         ;; Print viewer shows log-log info as text
         (let [multi-impl? (some? (:color-field chart-opts))]
           (print-log-log-info slope r-squared multi-impl?)))

       :render-model-heading
       (fn [{:keys [title]}]
         (println title))

       :render-model-table
       (fn [{:keys [table-rows multi-impl?]}]
         (if (seq table-rows)
           (print-model-rows table-rows multi-impl? tolerance)
           (println (if multi-impl?
                      "  (no implementations)"
                      "  (insufficient data for regression)")))
         (println))

       :render-regression-charts
       (fn [_]
         ;; Print viewer doesn't render charts
         nil)})))

;;; Allocation Views

(defmethod view/allocation-summary* :print
  [_ {:keys [summary-id]} data-map]
  (let [summary-id (or summary-id :allocation-summary)
        summary (data-map summary-id)]
    (when summary
      (let [{:keys [total-allocated total-freed num-allocations num-freed
                    freed-ratio]} summary
            total-allocated (long total-allocated)
            total-freed (long total-freed)
            retained (- total-allocated total-freed)]
        (println)
        (println "Allocation Summary:")
        (println (format "  %16s: %12d bytes"
                         "Total allocated"
                         total-allocated))
        (println (format "  %16s: %12d bytes"
                         "Total freed"
                         total-freed))
        (println (format "  %16s: %12d bytes"
                         "Retained"
                         retained))
        (println (format "  %16s: %12d"
                         "Allocation count"
                         num-allocations))
        (println (format "  %16s: %12d"
                         "Freed count"
                         num-freed))
        (println (format "  %16s: %12.1f%%"
                         "Freed ratio"
                         (* 100.0 (double freed-ratio))))))))

(defmethod view/allocation-hotspots* :print
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)
            type-col-width 30
            truncate-type (fn [s]
                            (if (and s (> (count s) type-col-width))
                              (str "…" (subs s (- (count s) (- type-col-width 1))))
                              (or s "")))]
        (when (seq hotspots)
          (println)
          (println "Allocation Hotspots:")
          (println (format "%8s %12s %8s %12s  %-30s  %s"
                           "Count" "Bytes" "Freed" "Freed Bytes" "Object Type" "Call Site"))
          (println (apply str (repeat 110 "-")))
          (doseq [{:keys [call-site object-type count bytes freed-count freed-bytes]} hotspots]
            (println (format "%8d %12d %8d %12d  %-30s  %s"
                             count
                             bytes
                             freed-count
                             freed-bytes
                             (truncate-type object-type)
                             (allocation/format-call-site call-site nil)))))))))

(defmethod view/allocation-by-type* :print
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (println)
          (println "Allocations by Type:")
          (println (format "%8s %12s %8s %12s  %s"
                           "Count" "Bytes" "Freed" "Freed Bytes" "Type"))
          (println (apply str (repeat 80 "-")))
          (doseq [[type-name {:keys [count bytes freed-count freed-bytes]}] sorted]
            (println (format "%8d %12d %8d %12d  %s"
                             count
                             bytes
                             freed-count
                             freed-bytes
                             type-name))))))))

(defmethod view/allocation-treemap* :print
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (println)
      (println (allocation/render-ascii-treemap treemap-data)))))

;;; Modal Analysis Views

(defmethod view/multimodal-warning* :print
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config modes transforms]}]
     (println
      (format "%32s: Multimodal distribution detected"
              (:label metric-config)))
     (when (seq modes)
       (let [locations (map #(modal/format-mode-location
                              (:location %)
                              metric-config
                              transforms)
                            modes)]
         (println (format "%32s  Mode locations: %s" ""
                          (str/join ", " locations))))))))
