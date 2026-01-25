(ns criterium.viewer.print.core
  "Print viewer core functions for basic metrics display.

  Provides text output for:
  - metrics, stats, extremes
  - bootstrap statistics
  - samples with outliers
  - outlier counts and significance
  - event stats (class loader, JIT, GC)
  - final GC warnings
  - OS and runtime info
  - histograms, KDE, quantiles"
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.jvm :as jvm]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]
   [criterium.viewer.common.core :as core]))

(set! *unchecked-math* false)

;;; Label Formatting

(def ^:const label-width
  "Primary label width for print viewer output."
  32)

(def ^:const sublabel-width
  "Secondary/detail label width for print viewer output."
  36)

(defn label-str
  "Format a label padded to primary width (32 chars).
  Returns a format-ready string without trailing colon."
  [label]
  (format (str "%" label-width "s") label))

(defn sublabel-str
  "Format a label padded to secondary width (36 chars).
  Returns a format-ready string without trailing colon."
  [label]
  (format (str "%" sublabel-width "s") label))

(defn format-label
  "Format a label with colon at primary width.
  Returns string like '           My Label:'"
  [label]
  (str (label-str label) ":"))

(defn format-sublabel
  "Format a label with colon at secondary width.
  Returns string like '               My Sublabel:'"
  [label]
  (str (sublabel-str label) ":"))

;;; Metrics

(defn print-metrics
  [metrics metrics->values]
  (doseq [m metrics]
    (when-let [a (metrics->values (:path m))]
      (when-let [v (arr/first-element a)]
        (println
         (str
          (format-sublabel (:label m))
          " "
          (if (number? v)
            (format/format-value (:dimension m) (* v (:scale m)))
            v)))))))

(defmethod view/metrics* :print
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (print-metrics metric-configs (util/metric->values metrics-samples))))

;;; Stats

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
        "%s %s %s  3σ [%s %s]  min %s"
        (format-label (:label metric))
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
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)]
        (print-stats
         metric-configs
         (util/stats stats-map)
         (util/get-transforms data-map stats-id))))))

;;; Extremes

(defn print-extreme
  "Print min and max values for a metric."
  [metric stat transforms]
  (when (and (:min-val stat) (:max-val stat))
    (let [stat (util/transform-vals-> stat transforms)
          [scale unit] (format/scale
                        (:dimension metric)
                        (* (:scale metric) (:min-val stat)))
          scale (* scale (:scale metric))]
      (println
       (format
        "%s %s %s - %s %s"
        (format-label (:label metric))
        (format/format-scaled (:min-val stat) scale)
        unit
        (format/format-scaled (:max-val stat) scale)
        unit)))))

(defn print-extremes
  "Print min/max extremes for all metrics."
  [metrics stats transforms]
  (println (format-sublabel "Extremes"))
  (doseq [metric metrics]
    (print-extreme metric (get-in stats (:path metric)) transforms)))

(defmethod view/extremes* :print
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)]
        (print-extremes
         metric-configs
         (util/stats stats-map)
         (util/get-transforms data-map stats-id))))))

;;; Event Stats

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

;;; Bootstrap Stats

(defn print-bootstrap-stat
  "Print bootstrap statistics for a metric.

  Output format:
  1. Median with 95% CI
  2. Mean with 95% CI
  3. p10-p90 percentile spread"
  [metric
   {:keys [mean quantiles]}
   transforms]
  (let [{:keys [dimension label]} metric
        tform #(util/transform-sample-> % transforms)
        mean-val (tform (:point-estimate mean))
        [scale units] (format/scale dimension (* (:scale metric) mean-val))
        mean-ci (:estimate-quantiles mean)
        median-est (get quantiles 0.5)
        median-ci (:estimate-quantiles median-est)
        p10-est (get quantiles 0.1)
        p90-est (get quantiles 0.9)
        scale (* (:scale metric) scale)]
    (when (and median-est (seq median-ci))
      (println
       (format "%s %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
               (format-sublabel (str label " median"))
               (* scale (tform (:point-estimate median-est)))
               units
               (* scale (tform (-> median-ci first :value)))
               (* scale (tform (-> median-ci second :value)))
               (-> median-ci first :alpha)
               (-> median-ci second :alpha))))
    (println
     (format "%s %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
             (format-sublabel (str label " mean"))
             (* scale mean-val)
             units
             (* scale (tform (-> mean-ci first :value)))
             (* scale (tform (-> mean-ci second :value)))
             (-> mean-ci first :alpha)
             (-> mean-ci second :alpha)))
    (when (and p10-est p90-est)
      (println
       (format "%s [%.3g %.3g] %s (10th-90th percentile)"
               (format-sublabel (str label " spread"))
               (* scale (tform (:point-estimate p10-est)))
               (* scale (tform (:point-estimate p90-est)))
               units)))))

(defn print-bootstrap-stats
  [{:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (:metrics-defs bootstrap-map)
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            transforms (util/get-transforms data-map bootstrap-stats-id)]
        (doseq [metric metric-configs]
          (when-let [stat (get-in bootstrap (:path metric))]
            (print-bootstrap-stat metric stat transforms)))))))

(defmethod view/bootstrap-stats* :print
  [_ view data-map]
  (print-bootstrap-stats view data-map))

;;; Final GC Warnings

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
                 (arr/sum (metric->values [:elapsed-time])))
        gc-samples (-> data-map final-gc-id util/metric->values)
        total-gc (reduce
                  +
                  (mapv
                   (fn [m]
                     (* (:scale m) (arr/sum (gc-samples (:path m)))))
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

;;; Outliers

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

(defn- format-outlier-method
  "Format outlier detection method for display."
  [method]
  (case method
    :medcouple "medcouple-adjusted boxplot"
    :tukey "Tukey boxplot"
    (str method)))

(defn print-outlier-count
  "Print outlier counts for a metric.
  When show-medcouple? is true, also displays the medcouple value and skewness
  classification if available."
  [metric-config num-samples outliers show-medcouple?]
  (let [outlier-counts (:outlier-counts outliers)
        sum (reduce + (vals outlier-counts))
        mc (:medcouple outliers)
        method (:outlier-method outliers)]
    (when (pos? sum)
      (util/report "%s Found %d outliers in %d samples (%.3g %%)%s\n"
                   (format-label (:label metric-config))
                   sum
                   num-samples
                   (* 100.0 (/ sum num-samples))
                   (if method (str " [" (format-outlier-method method) "]") ""))
      (doseq [[c v] (->> outlier-counts
                         (filter #(pos? (val %))))]
        (util/report
         "                                 %12s\t %d (%2.4f %%)\n"
         (name c) v (* 100.0 (/ v num-samples)))))
    (when (and show-medcouple? mc)
      (let [classification (skewness-classification mc)]
        (util/report "%s medcouple %.4f (%s)\n"
                     (format-label (:label metric-config))
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

;;; Samples

(defn- print-samples-with-outliers
  [metric->values transforms outliers metric]
  (let [path (:path metric)
        values (metric->values path)
        outlier-data (get-in outliers path)]
    (doseq [[i v] (sort-by first (:outliers outlier-data))]
      (println
       (format "%s[%5d] %s %s"
               (sublabel-str "")
               i
               (format/format-value
                (:dimension metric)
                (* (:scale metric)
                   (util/transform-sample-> (arr/get-at values i) transforms)))
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
     (format "%s %d samples with batch-size %d"
             (format-label "Samples")
             (:num-samples metrics-samples) (:batch-size metrics-samples)))
    (when outliers
      (doseq [metric metric-configs]
        (println (str (sublabel-str "") (:label metric)))
        (print-samples-with-outliers
         (util/metric->values metrics-samples)
         transforms
         (util/outliers outliers)
         metric))
      (println))))

;;; Collect Plan

(defmethod view/collect-plan* :print
  [_ _view data-map]
  (let [warmup (some-> data-map :warmup)
        est (some-> data-map :estimation)
        samples (-> data-map :samples)
        fmt-val (fn [label n bs evals]
                  (format "%s %d samples with batch-size %d (%d evaluations)"
                          (format-label label) n bs evals))]
    (println
     (fmt-val "Sample Scheme"
              (:num-samples samples)
              (:batch-size samples)
              (:eval-count samples)))
    (when warmup
      (println
       (fmt-val "Warmup"
                (:num-samples warmup) (:batch-size warmup)
                (* (:num-samples warmup) (:batch-size warmup)))))
    (when est
      (println
       (fmt-val "Estimation"
                (:num-samples est) (:batch-size est)
                (* (:num-samples est) (:batch-size est)))))))

;;; Histogram

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
       (format "%s %s Histogram"
               (format-label (-> h :metric-config :label))
               (-> h :unit)))
      (run!
       (fn [[x bin-count density]]
         (println
          (format "%34s %-7.3f %5d  %-7.3g" "" x (long bin-count) density)))
       (mapv vector (:centers h) (:counts h) (:density h)))
      (println))))

;;; KDE

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
    (println (format "%s KDE (n=%d)" (format-label label) n))
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

;;; Quantiles

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

;;; OS

(defmethod view/os* :print
  [_ _ _sampled]
  (let [ks [:arch :name :version :available-processors]]
    (apply println
           (-> (map
                #(%1 (jvm/os-details))
                ks)
               vec (conj "cpu(s)")))))

;;; Runtime

(defmethod view/runtime* :print
  [_ _ _sampled]
  (let [runtime-details (jvm/runtime-details)]
    (apply println (map #(%1 runtime-details) [:vm-name :vm-version]))
    (apply println "Runtime arguments:"
           (:input-arguments runtime-details))))

;;; Sample Percentiles

(defmethod view/sample-percentiles* :print
  [_ _view _sampled])
  ;; TODO
