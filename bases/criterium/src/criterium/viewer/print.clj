(ns criterium.viewer.print
  "A print viewer"
  (:require
   [clojure.string :as str]
   [criterium.jvm :as jvm]
   [criterium.metric :as metric]
   [criterium.types :as types]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

(set! *unchecked-math*  false)

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
  (let [samples-id      (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs    (:metrics-defs metrics-samples)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (print-metrics metric-configs (util/metric->values metrics-samples))))

(defn print-stat
  [metric stat transforms]
  (when (:mean stat)
    (let [stat         (util/transform-vals-> stat transforms)
          [scale unit] (format/scale
                        (:dimension metric)
                        (* (:scale metric) (:mean stat)))
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
  [metrics stats transforms]
  (doseq [metric metrics]
    (print-stat metric (get-in stats (:path metric)) transforms)))

(defmethod view/stats* :print
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id       (or stats-id :stats)
        stats-map      (data-map stats-id)
        metrics-defs   (-> (:metrics-defs stats-map)
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
                  (conj ms {:path      sample-count-path
                            :dimension :count
                            :scale     1}))]
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
  (let [event-stats-id  (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)
        metrics-defs    (-> (:metrics-defs event-stats-map)
                            (metric/filter-metrics
                             (metric/type-pred :event)))
        event-stats     (util/event-stats event-stats-map)]
    (print-event-stats metrics-defs event-stats)))

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
  [{:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map      (data-map bootstrap-stats-id)
        metrics-defs       (:metrics-defs bootstrap-map)
        metric-configs     (metric/all-metric-configs metrics-defs)
        bootstrap          (util/bootstrap bootstrap-map)]
    (doseq [metric metric-configs]
      (when-let [stat (get-in bootstrap (:path metric))]
        (print-bootstrap-stat metric stat)))))

(defmethod view/bootstrap-stats* :print
  [_ view data-map]
  (print-bootstrap-stats view data-map))

(defn print-final-gc-warnings
  [{:keys [final-gc-id samples-id warn-threshold]} data-map]
  {:pre [(number? warn-threshold)]}
  (let [final-gc-id       (or final-gc-id :final-gc)
        samples-id        (or samples-id :samples)
        metrics-samples   (data-map samples-id)
        metrics-deps      (:metrics-deps metrics-samples)
        gc-metric-configs (metric/all-metric-configs
                           (select-keys
                            metrics-deps
                            [:elapsed-time :garbage-collector]))
        metric            (first gc-metric-configs)
        gc-time-metrics   (->> (next gc-metric-configs)
                               (filterv #(= :time (:dimension %))))
        metric->values    (util/metric->values metrics-samples)
        total             (* (:scale metric)
                             (reduce + (metric->values [:elapsed-time])))
        gc-samples        (-> data-map final-gc-id util/metric->values)
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
  [_ view data-map]
  (print-final-gc-warnings view data-map))

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
  [{:keys [outliers-id] :as _view} data-map]
  (let [outliers-id    (or outliers-id :outliers)
        outliers-map   (data-map outliers-id)
        metrics-defs   (:metrics-defs  outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        num-samples    (have (:num-samples outliers-map))
        outliers       (util/outliers outliers-map)]
    (doseq [m metric-configs]
      (print-outlier-count m num-samples (get-in outliers (:path m))))))

(defmethod view/outlier-counts* :print
  [_ view data-map]
  (print-outlier-counts view data-map))

(defn print-outlier-significance
  [metric-config outlier-significance]
  {:pre [(have? outlier-significance)]}
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
  [{:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id  (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        metrics-defs    (-> (:metrics-defs outlier-sig-map)
                            (metric/filter-metrics
                             (metric/type-pred :quantitative)))
        metric-configs  (metric/all-metric-configs metrics-defs)
        outlier-sig     (util/outlier-significance outlier-sig-map)]
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
  (let [path         (:path metric)
        values       (metric->values path)
        outlier-data (get-in outliers path)]
    (doseq [[i v] (sort-by  first (:outliers  outlier-data))]
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
  (let [samples-id      (or samples-id :samples)
        outliers-id     (or outliers-id :outliers)
        metrics-samples (data-map samples-id)
        outliers        (data-map outliers-id)
        metrics-defs    (-> (:metrics-defs outliers)
                            (metric/filter-metrics
                             (metric/type-pred :quantitative)))
        metric-configs  (metric/all-metric-configs metrics-defs)
        transforms      (util/get-transforms data-map samples-id)]

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
  (let [warmup  (some-> data-map :warmup)
        est     (some-> data-map :estimation)
        samples (-> data-map :samples)
        fmt     "%32s: %d samples with batch-size %d (%d evaluations)"]
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
  (let [histogram-id   (or histogram-id :histograms)
        histograms     (util/lookup-data data-map histogram-id)
        metrics-defs   (-> (:metrics-defs histograms)
                           (metric/filter-metrics
                            (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map histogram-id)
        histograms     (->> metric-configs
                            (mapv
                             #(viewer-common/histogram
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

(defmethod view/quantiles* :print
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id   (or quantiles-id :quantiles)
        quantiles-map  (have types/quantiles-map?
                             (data-map quantiles-id))
        metrics-defs   (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms data-map quantiles-id)
        table          (viewer-common/quantiles
                        metric-configs
                        (util/quantiles quantiles-map)
                        transforms)]
    (doseq [vs table]
      (let [ks  (sort (keys (dissoc vs :metric)))
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
           (->  (map
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
  [_ _view _sampled]
  ;; TODO
  )

;;; Domain Views

(defn- format-coord
  "Format a coordinate for display."
  [coord]
  (if (map? coord)
    (str/join " " (map (fn [[k v]] (str (name k) "=" v)) (sort-by key coord)))
    (name coord)))

(defn- format-extract-value
  "Format a value from domain-extract for display.
  Applies metric scale and formats with appropriate dimension."
  [value metric-path]
  (if (nil? value)
    "nil"
    (let [[dimension scale]
          (case (first metric-path)
            (:stats :log-stats)
            (case (second metric-path)
              :elapsed-time [:time 1e-9]
              :thread-allocation [:memory 1]
              [:count 1])
            [:count 1])]
      (format/format-value dimension (* value scale)))))

(defmethod view/domain-extract* :print
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract    (data-map extract-id)]
    (when extract
      (let [{:keys [metric data]} extract]
        (println (format "Domain Extract: %s" (pr-str metric)))
        (doseq [[coord value] data]
          (println (format "  %24s: %s"
                           (format-coord coord)
                           (format-extract-value value metric))))))))

(defmethod view/domain-grouped* :print
  [_ {:keys [grouped-id]} data-map]
  (let [grouped-id (or grouped-id :grouped)
        grouped    (data-map grouped-id)]
    (when grouped
      (let [{:keys [axis data]} grouped]
        (println (format "Domain Grouped by: %s" (name axis)))
        (doseq [[axis-val sub-domain] (sort-by (comp str key) data)]
          (let [run-count (count (:runs sub-domain))]
            (println (format "  %24s: %d run%s"
                             (if (nil? axis-val) "<nil>" (str axis-val))
                             run-count
                             (if (= 1 run-count) "" "s")))))))))

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
  (let [axis-vals   (sort-by (comp str identity) (keys data))
        all-entries (mapcat (fn [[axis-val entries]]
                              (map (fn [{:keys [coord value]}]
                                     {:axis-val axis-val
                                      :row-key  (coord-without-axis coord axis)
                                      :value    value})
                                   entries))
                            data)
        row-keys    (distinct (map :row-key all-entries))
        val-lookup  (reduce (fn [m {:keys [axis-val row-key value]}]
                              (assoc-in m [row-key axis-val] value))
                            {}
                            all-entries)]
    {:columns axis-vals
     :rows    (mapv (fn [row-key]
                      {:key    row-key
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
        col-headers    (mapv format-axis-val columns)
        row-keys       (mapv #(format-row-key (:key %)) rows)
        col-widths     (mapv (fn [col-idx]
                               (apply max
                                      (count (nth col-headers col-idx))
                                      (map #(count (nth % col-idx)) formatted-vals)))
                             (range (count columns)))
        row-key-width  (apply max 8 (map count row-keys))]
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

(defmethod view/domain-comparison* :print
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison    (data-map comparison-id)]
    (when comparison
      (let [{:keys [axis metric data]} comparison]
        (if (and (seq data)
                 (some #(seq (second %)) data))
          (print-comparison-table axis metric data)
          (println (format "Domain Comparison by %s: %s (no data)"
                           (name axis) (pr-str metric))))))))

(defn- regression-equation-str
  "Format the fitted regression equation for a model.
  The model fits y = a*transform(x) + b where transform depends on model id."
  [model-id {:keys [a b]}]
  (when (and a b)
    (let [transform-str (case model-id
                          :logarithmic "log(n)"
                          :linear      "n"
                          :n-log-n     "n*log(n)"
                          :quadratic   "n²"
                          "x")
          sign          (if (neg? b) "-" "+")]
      (format "y = %.4g*%s %s %.4g" a transform-str sign (Math/abs ^double b)))))

(defmethod view/domain-regression* :print
  [_ {:keys [regression-id]} data-map]
  (let [regression-id (or regression-id :regression)
        regression    (data-map regression-id)]
    (when regression
      (let [{:keys [axis metric models best-fit]} regression]
        (println (format "Domain Regression (axis: %s, metric: %s)"
                         (name axis) (pr-str metric)))
        (if (seq models)
          (let [sorted-models (sort-by :r-squared > models)
                label-width   (apply max (map #(count (:label %)) models))]
            (doseq [{:keys [id label coefficients r-squared]} sorted-models]
              (let [eq-str (regression-equation-str id coefficients)]
                (println (format "  %s  R²=%.4f%s%s"
                                 (format (str "%-" label-width "s") label)
                                 r-squared
                                 (if eq-str (str "  " eq-str) "")
                                 (if (= id best-fit) "  <- best fit" ""))))))
          (println "  (insufficient data for regression)"))))))
