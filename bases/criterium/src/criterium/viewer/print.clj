(ns criterium.viewer.print
  "A print viewer"
  (:require
   [clojure.string :as str]
   [criterium.jvm :as jvm]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

(defn print-metrics
  [metrics sample]
  (doseq [m metrics]
    (when-let [v (first (sample (:path m)))]
      (println
       (format
        "%36s: %s"
        (:label m)
        (format/format-value (:dimension m) (* v (:scale m))))))))

(defmethod view/metrics* :print
  [{:keys [samples-id metric-ids]} sampled]
  (let [samples-id      (or samples-id :samples)
        metrics-configs (metric/metric-configs-of-type
                         (:metrics-configs sampled)
                         :quantitative metric-ids)
        samples         (get sampled samples-id)]
    (print-metrics metrics-configs samples)))

(defn print-stat
  [metric stat]
  (when-let [mean (:mean stat)]
    (let [[scale unit] (format/scale
                        (:dimension metric)
                        (* (:scale metric) mean))
          scale        (* scale (:scale metric))]
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
  [metrics stats]
  (doseq [metric metrics]
    (print-stat metric (get-in stats (:path metric)))))

(defmethod view/stats* :print
  [{:keys [stats-id metric-ids]} sampled]
  (let [stats-id       (or stats-id :stats)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (print-stats
     metric-configs
     (get sampled stats-id))))

(defn print-event-stats-metrics
  [event-stats metric ms]
  (let [sample-count-path
        (conj (vec (butlast (:path (first ms)))) :sample-count)]
    (when (and sample-count-path
               (pos? (get event-stats sample-count-path)))
      (let [vals (mapv
                  (fn [m]
                    (format/format-value
                     (:dimension m)
                     (* (get event-stats (:path m))
                        (:scale m))))
                  (conj ms {:path      sample-count-path
                            :dimension :count
                            :scale     1}))]
        (println (apply format (:summary metric) (:label metric) vals))))))

(defn print-event-stats
  [metrics-configs event-stats]
  {:pre [event-stats]}
  (doseq [[_k metric] metrics-configs]
    (if-let [gs (:groups metric)]
      (print-event-stats gs event-stats)
      (print-event-stats-metrics event-stats metric (:values metric)))))

(defmethod view/event-stats* :print
  [{:keys [event-stats-id metric-ids]} sampled]
  (let [event-stats-id  (or event-stats-id :event-stats)
        metrics-configs (metric/metrics-of-type
                         (:metrics-configs sampled)
                         :event metric-ids)
        event-stats     (get sampled event-stats-id)]
    (print-event-stats metrics-configs event-stats)))

(defn print-bootstrap-stat
  [metric
   {:keys  [mean
            mean-minus-3sigma
            mean-plus-3sigma]
    minval :min-val
    :as    stat}]
  (assert minval stat)
  (let [{:keys [dimension label]} metric
        [scale units]             (format/scale
                                   dimension
                                   (* (:scale metric) (:point-estimate mean)))
        min-quantiles             (:estimate-quantiles minval)
        quantiles                 (:estimate-quantiles mean)
        scale                     (* (:scale metric) scale)]
    (println
     (format "%36s: %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
             (str label " min")
             (* scale (:point-estimate minval))
             units
             (* scale (-> min-quantiles first :value))
             (* scale (-> min-quantiles second :value))
             (-> min-quantiles first :alpha)
             (-> min-quantiles second :alpha)))
    (println
     (format "%36s: %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
             (str label " mean")
             (* scale (:point-estimate mean))
             units
             (* scale (-> quantiles first :value))
             (* scale (-> quantiles second :value))
             (-> quantiles first :alpha)
             (-> quantiles second :alpha)))
    (println
     (format "%36s: [%.3g %.3g] %s "
             (str label " 3σ")
             (* scale (:point-estimate mean-minus-3sigma))
             (* scale (:point-estimate mean-plus-3sigma))
             units))))

(defn print-bootstrap-stats
  [{:keys [bootstrap-stats-id metric-ids]} sampled]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        stats              (get sampled bootstrap-stats-id)
        metric-configs     (metric/metric-configs-of-type
                            (:metrics-configs sampled)
                            :quantitative metric-ids)]
    (doseq [metric metric-configs]
      (when-let [stat (get-in stats (:path metric))]
        (print-bootstrap-stat metric stat)))))

(defmethod view/bootstrap-stats* :print
  [view sampled]
  (print-bootstrap-stats view sampled))

(defn print-final-gc-warnings
  [{:keys [final-gc-id samples-id warn-threshold]} sampled]
  {:pre [(number? warn-threshold)]}
  (let [final-gc-id       (or final-gc-id :final-gc)
        samples-id        (or samples-id :samples)
        metrics-configs   (:metrics-configs sampled)
        gc-metric-configs (metric/all-metric-configs
                           (select-keys metrics-configs
                                        [:elapsed-time :garbage-collector]))
        metric            (first gc-metric-configs)
        gc-time-metrics   (->> (next gc-metric-configs)
                               (filterv #(= :time (:dimension %))))
        samples           (get sampled samples-id)
        total             (* (:scale metric)
                             (reduce + (samples [:elapsed-time])))
        gc-samples        (:samples (get sampled final-gc-id))
        total-gc          (reduce
                           +
                           (mapv
                            (fn [m]
                              (* (:scale m) (reduce + (gc-samples (:path m)))))
                            gc-time-metrics))
        frac              (/ total-gc total)]
    (when (and total-gc (> frac  warn-threshold))
      (println (format "Final GC ran for %s, %.1f%% of total sampling time (%s)"
                       (format/format-value :time total-gc)
                       (* frac 100)
                       (format/format-value :time total))))))

(defmethod view/final-gc-warnings* :print
  [view sampled]
  (print-final-gc-warnings view sampled))

(defn print-outlier-count
  [metric-config num-samples outliers]
  (let [outlier-counts (:outlier-counts outliers)
        sum            (reduce + (vals outlier-counts))]
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
         (name c) v (* 100.0 (/ v num-samples)))))))

(defn print-outlier-counts
  [{:keys [metric-ids outliers-id] :as _view} sampled]
  (let [outliers-id    (or outliers-id :outliers)
        outliers       (get sampled outliers-id)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)
        num-samples    (:num-samples sampled)]
    (doseq [m metric-configs]
      (print-outlier-count m num-samples (get-in outliers (:path m))))))

(defmethod view/outlier-counts* :print
  [view sampled]
  (print-outlier-counts view sampled))

(defn print-outlier-significance
  [metric-config outlier-significance]
  {:pre [outlier-significance]}
  (let [labels {:unaffected "unaffected"
                :slight     "slightly inflated"
                :moderate   "moderately inflated"
                :severe     "severely inflated"}]
    (util/report "%s Variance contribution from outliers : %.3g %%"
                 (:label metric-config)
                 (* (:significance outlier-significance) 100.0))
    (util/report "%s Variance is %s by outliers\n"
                 (:label metric-config)
                 (-> outlier-significance :effect labels))))

(defn print-outlier-significances
  [{:keys [metric-ids outlier-significance-id] :as _view} sampled]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig    (get sampled outlier-sig-id)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (doseq [m metric-configs]
      (print-outlier-significance m (get-in outlier-sig (:path m))))))

(defmethod view/outlier-significance* :print
  [view sampled]
  (print-outlier-significances view sampled))

(defmethod view/samples* :print
  [{:keys [samples-id outliers-id] :as _view} sampled]
  (let [samples-id  (or samples-id :samples)
        outliers-id (or outliers-id :outliers)
        samples     (get sampled samples-id)
        _outliers   (get sampled outliers-id)]
    (println
     (format "%32s: %d samples with batch-size %d"
             "Samples"
             (count samples) (:batch-size sampled)))))

(defmethod view/collect-plan* :print
  [_view sampled]
  (let [warmup (sampled :warmup)
        est    (sampled :estimation)
        fmt    "%32s: %d samples with batch-size %d (%d evaluations)"]
    (println
     (format fmt
             "Sample Scheme"
             (:num-samples sampled)
             (:batch-size sampled)
             (* (:num-samples sampled) (:batch-size sampled))))
    (println
     (format fmt
             "Warmup"
             (:num-samples warmup) (:batch-size warmup)
             (* (:num-samples warmup) (:batch-size warmup))))
    (println
     (format fmt
             "Estimation"
             (:num-samples est) (:batch-size est)
             (* (:num-samples est) (:batch-size est))))))

(defmethod view/histogram* :print
  [_view _sampled])

(defmethod view/quantiles* :print
  [{:keys [quantiles-id metric-ids]} sampled]
  (let [quantiles-id   (or quantiles-id :quantiles)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)
        table          (viewer-common/quantiles
                        metric-configs
                        (get sampled quantiles-id))]
    (doseq [vs table]
      (let [ks (sort (keys (dissoc vs :metric)))]
        (println
         (format "%22s Quantiles: %s"
                 (:metric vs)
                 (str/join ", " (mapv #(str % " " (vs %)) ks))))))))

(defmethod view/os* :print
  [_ _sampled]
  (let [ks [:arch :name :version :available-processors]]
    (apply println
           (->  (map
                 #(%1 (jvm/os-details))
                 ks)
                vec (conj "cpu(s)")))))

(defmethod view/runtime* :print
  [_ _sampled]
  (let [runtime-details (jvm/runtime-details)]
    (apply println (map #(%1 runtime-details) [:vm-name :vm-version]))
    (apply println "Runtime arguments:"
           (:input-arguments runtime-details))))

(defmethod view/sample-percentiles* :print
  [_view _sampled]
  ;; TODO
  )
