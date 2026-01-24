(ns criterium.viewer.print
  "A print viewer

  Core functionality (metrics, stats, extremes, bootstrap, samples, outliers,
  events, GC, OS, runtime) is in criterium.viewer.print.core."
  (:require
   [clojure.string :as str]
   [criterium.benchmark :as benchmark]
   [criterium.domain.types :as domain.types]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.view :as view]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.common.autocorrelation :as acf-common]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.common.regression :as regression]
   [criterium.viewer.common.shape :as shape]
   [criterium.viewer.print.core :as print.core]))

;;; Re-exported functions from print.core for backwards compatibility
(def print-stat
  "Print a single metric's stats. Delegates to criterium.viewer.print.core."
  print.core/print-stat)

(def print-extreme
  "Print a single metric's min/max values. Delegates to criterium.viewer.print.core."
  print.core/print-extreme)

(def print-bootstrap-stat
  "Print bootstrap statistics for a metric. Delegates to criterium.viewer.print.core."
  print.core/print-bootstrap-stat)

(def print-outlier-count
  "Print outlier counts for a metric. Delegates to criterium.viewer.print.core."
  print.core/print-outlier-count)

(set! *unchecked-math* false)

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
            shape-data (shape/shape-stats-data metric-configs bootstrap)]
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

;;; Tail Analysis Views

(defn- get-tail-analysis-context
  "Extract common context for tail analysis views.
  Returns map with :tail-results, :metric-configs, :transforms, or nil if no data."
  [{:keys [tail-analysis-id]} data-map]
  (let [tail-analysis-id (or tail-analysis-id :tail-analysis)
        tail-analysis-map (data-map tail-analysis-id)]
    (when tail-analysis-map
      (let [metrics-defs (-> (:metrics-defs tail-analysis-map)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
            metric-configs (metric/all-metric-configs metrics-defs)
            tail-results (:tail-analysis tail-analysis-map)
            transforms (util/get-transforms data-map tail-analysis-id)]
        (when (seq tail-results)
          {:tail-results tail-results
           :metric-configs metric-configs
           :transforms transforms})))))

(defn print-tail-summary
  "Print GPD/Hill summary statistics for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-analysis-context view data-map)]
    (println "Tail Summary:")
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [summary-rows (core/tail-summary-table tail-data transforms)]
          (when (seq summary-rows)
            (println (format "%32s:" (:label mc)))
            (doseq [{:keys [parameter value]} summary-rows]
              (println (format "%32s  %s: %s" "" parameter value)))))))
    (println)))

(defmethod view/tail-summary* :print
  [_ view data-map]
  (print-tail-summary view data-map))

(defn- format-tail-ratio
  "Format a tail ratio value with reference percentile values."
  [[ratio-name ratio-val] {:keys [p95 p99 p999]}]
  (case ratio-name
    :p99-p95 (format "p99/p95 = %.3f (p99=%.4g, p95=%.4g)"
                     ratio-val (double p99) (double p95))
    :p999-p99 (format "p999/p99 = %.3f (p999=%.4g, p99=%.4g)"
                      ratio-val (double p999) (double p99))
    :p999-p95 (format "p999/p95 = %.3f (p999=%.4g, p95=%.4g)"
                      ratio-val (double p999) (double p95))
    (format "%s = %.3f" (name ratio-name) ratio-val)))

(defn print-tail-ratios
  "Print tail ratio statistics for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs]}
             (get-tail-analysis-context view data-map)]
    (println "Tail Ratios:")
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [{:keys [tail-ratios empirical-quantiles]} tail-data]
          (when (and tail-ratios (seq tail-ratios))
            (let [sorted-ratios (sort-by first tail-ratios)]
              (doseq [[i [rname rval]] (map-indexed vector sorted-ratios)]
                (if (zero? i)
                  (println (format "%32s: %s"
                                   (str (:label mc) " tail ratios")
                                   (format-tail-ratio [rname rval] empirical-quantiles)))
                  (println (format "%32s  %s"
                                   ""
                                   (format-tail-ratio [rname rval] empirical-quantiles))))))))))
    (println)))

(defmethod view/tail-ratios* :print
  [_ view data-map]
  (print-tail-ratios view data-map))

(defn print-tail-high-quantiles
  "Print high quantile estimates from GPD extrapolation for all metrics."
  [view data-map]
  (when-let [{:keys [tail-results metric-configs transforms]}
             (get-tail-analysis-context view data-map)]
    (println "High Quantile Estimates (GPD):")
    (doseq [mc metric-configs]
      (when-let [tail-data (get tail-results (:path mc))]
        (let [{:keys [high-quantiles]} tail-data]
          (when (seq high-quantiles)
            (println (format "%32s:" (:label mc)))
            (doseq [[q val] (sort-by first high-quantiles)]
              (let [tval (util/transform-sample-> val transforms)
                    [scale unit] (format/scale :time tval)
                    scaled (* scale tval)]
                (println (format "%32s  p%.4g = %s %s"
                                 ""
                                 (* q 100)
                                 (format/format-scaled scaled 1.0)
                                 unit))))))))
    (println)))

(defmethod view/tail-high-quantiles* :print
  [_ view data-map]
  (print-tail-high-quantiles view data-map))

;; Chart views are no-ops for print viewer
(defmethod view/tail-ratios-chart* :print [_ _ _])
(defmethod view/hill-plot* :print [_ _ _])
(defmethod view/mrl-plot* :print [_ _ _])
(defmethod view/zipf-plot* :print [_ _ _])
(defmethod view/exponential-qq-plot* :print [_ _ _])
(defmethod view/gpd-qq-plot* :print [_ _ _])

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

(defn- print-distribution-models-for-metric
  "Print distribution model comparison for a single metric."
  [metric-config fit-data]
  (let [{:keys [n warning distributions best-model]} fit-data
        {:keys [label]} metric-config]
    (println (format "%32s: Distribution Models (n=%d%s)"
                     label n
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
            (doseq [mc metric-configs]
              (when-let [fit-data (get fits (:path mc))]
                (print-distribution-models-for-metric mc fit-data)))
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
      (println (format "%32s: %s Parameter CIs"
                       label
                       (get distribution-labels best-model (name best-model))))
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
            (doseq [mc metric-configs]
              (when-let [fit-data (get fits (:path mc))]
                (print-distribution-parameter-cis-for-metric mc fit-data)))
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
  "Format a value with SI units for display.
  When sub-key is provided and value is a map, extracts that key first."
  ([value metric-path]
   (format-extract-value-with-unit value metric-path nil))
  ([value metric-path sub-key]
   (let [raw-value (if (and sub-key (map? value))
                     (get value sub-key)
                     value)]
     (when (some? raw-value)
       (let [base-value (* (double raw-value)
                           (core/metric-path->base-scale metric-path))
             dimension (core/metric-path->dimension metric-path)]
         (if dimension
           (format/format-value dimension base-value)
           (format "%g" base-value)))))))

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

(defmethod view/domain-extract-table* :print
  [_ {:keys [extract-id]} data-map]
  (let [extract-id (or extract-id :extract)
        extract (data-map extract-id)]
    (case (detection/visualization-strategy extract)
      :single-point
      (when-let [table (extract/prepare-domain-extract-table-transposed
                        extract)]
        (print-transposed-table table))

      (:multi-point :default-table)
      (when extract
        (doseq [[_metric-id {:keys [metric data]}] (:metrics extract)]
          (let [raw-coords (map first data)
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

(defmethod view/domain-extract-chart* :print
  [_ _ _]
  ;; Print viewer doesn't render charts
  nil)

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
                            baseline-value (get-in lookup [metric-id baseline-impl row-key])
                            ;; Extract :value from error-bound maps
                            rv (if (map? value) (:value value) value)
                            bv (if (map? baseline-value) (:value baseline-value) baseline-value)]
                        (if (= type :baseline)
                          (or (format-extract-value-with-unit value metric-path :value) "-")
                          ;; Factor relative to baseline
                          (cond
                            (nil? rv) "-"
                            (nil? bv) "-"
                            (zero? bv) "-"
                            :else (format "%.2f" (/ rv bv))))))
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

(defmethod view/domain-comparison-table* :print
  [_ {:keys [comparison-id]} data-map]
  (let [comparison-id (or comparison-id :comparison)
        comparison (data-map comparison-id)]
    (case (detection/comparison-visualization-strategy comparison)
      :single-point
      (when-let [table (comparison/prepare-domain-comparison-table-transposed
                        comparison)]
        (print-transposed-table table))

      (:multi-point :default-table)
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

(defmethod view/domain-comparison-chart* :print
  [_ _ _]
  ;; Print viewer doesn't render charts
  nil)

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

;;; Autocorrelation Views

(defn print-autocorrelation
  "Print autocorrelation analysis summary for a metric.

  Output format:
  Sample Independence:
                      Lag-1 autocorrelation: 0.12 (minor)
                    Effective sample size: 157 of 200 (78%)
                       CI inflation factor: 1.13×
                        Ljung-Box p-value: 0.08
                           Assessment: Acceptable

  For warning/fail, adds pattern and recommendation lines."
  [{:keys [acf lag-1 effective-sample-size ci-inflation-factor
           ljung-box pattern classification detected-period
           anomalous-lags lag-severities]}
   metric-label]
  (println (format "%36s:" "Sample Independence"))
  (println (format "%36s: %.2f (%s)"
                   (str metric-label " Lag-1 autocorrelation")
                   (:value lag-1)
                   (acf-common/format-severity (:severity lag-1))))
  (println (format "%36s: %d of %d (%.0f%%)"
                   "Effective sample size"
                   (:n-effective effective-sample-size)
                   (:n-original effective-sample-size)
                   (* 100.0 (:ratio effective-sample-size))))
  (println (format "%36s: %.2f×"
                   "CI inflation factor"
                   ci-inflation-factor))
  (println (format "%36s: %.2f"
                   "Ljung-Box p-value"
                   (:p-value ljung-box)))
  (println (format "%36s: %s"
                   "Assessment"
                   (str/capitalize (name classification))))
  (when-let [formatted (acf-common/format-anomalous-lags anomalous-lags lag-severities)]
    (println (format "%36s: %s"
                     "Anomalous lags"
                     formatted)))
  (when (and (#{:warning :fail} classification)
             (acf-common/displayable-patterns pattern))
    (println (format "%36s: %s"
                     "Pattern"
                     (get acf-common/pattern-labels pattern (name pattern))))
    (when-let [period-str (acf-common/format-detected-period
                           detected-period acf lag-severities)]
      (println (format "%36s: %s"
                       "Suspected period"
                       period-str)))
    (when-let [rec (get acf-common/pattern-recommendations pattern)]
      (println (format "%36s: %s"
                       "Recommendation"
                       rec)))))

(defn print-autocorrelations
  "Print autocorrelation analysis for all metrics."
  [view data-map]
  (acf-common/with-autocorrelation-metrics view data-map
    (fn [acf-data mc]
      (print-autocorrelation acf-data (:label mc)))))

(defmethod view/autocorrelation* :print
  [_ view data-map]
  (print-autocorrelations view data-map))

;; ACF plot is a no-op for print viewer (charts not supported)
(defmethod view/acf-plot* :print [_ _ _])

(defn- print-classification-for-metric
  "Print classification analysis for a single metric."
  [{:keys [acf lag-1 lag-severities ljung-box pattern classification detected-period]}
   metric-label]
  (println (format "%36s:" "Sample Independence Classification"))
  (println (format "%36s: %.2f (%s)"
                   (str metric-label " Lag-1 autocorrelation")
                   (:value lag-1)
                   (acf-common/format-severity (:severity lag-1))))
  (println (format "%36s: %.2f"
                   (str metric-label " Ljung-Box p-value")
                   (:p-value ljung-box)))
  (println (format "%36s: %s"
                   "Assessment"
                   (str/capitalize (name classification))))
  (when (and (#{:warning :fail} classification)
             (acf-common/displayable-patterns pattern))
    (println (format "%36s: %s"
                     "Pattern"
                     (get acf-common/pattern-labels pattern (name pattern))))
    (when-let [period-str (acf-common/format-detected-period
                           detected-period acf lag-severities)]
      (println (format "%36s: %s"
                       "Suspected period"
                       period-str)))
    (when-let [rec (get acf-common/pattern-recommendations pattern)]
      (println (format "%36s: %s"
                       "Recommendation"
                       rec)))))

(defn print-autocorrelation-classifications
  "Print autocorrelation classification for all metrics.
  Only outputs if at least one metric is not classified as :pass."
  [view data-map]
  (let [metrics (acf-common/collect-classification-metrics view data-map)]
    (when (some #(not= :pass (:classification (first %))) metrics)
      (doseq [[class-data acf-data mc] metrics]
        (let [combined (merge class-data
                              (select-keys acf-data [:lag-1 :acf :lag-severities]))]
          (print-classification-for-metric combined (:label mc)))))))

(defmethod view/autocorrelation-classification* :print
  [_ view data-map]
  (print-autocorrelation-classifications view data-map))

(defn- print-effective-sample-size-for-metric
  "Print effective sample size analysis for a single metric."
  [{:keys [lag-1 effective-sample-size ci-inflation-factor]} metric-label]
  (println (format "%36s:" "Effective Sample Size"))
  (when lag-1
    (println (format "%36s: %.2f (%s)"
                     (str metric-label " Lag-1 autocorrelation")
                     (:value lag-1)
                     (acf-common/format-severity (:severity lag-1)))))
  (println (format "%36s: %d of %d (%.0f%%)"
                   "Effective sample size"
                   (:n-effective effective-sample-size)
                   (:n-original effective-sample-size)
                   (* 100.0 (:ratio effective-sample-size))))
  (println (format "%36s: %.2f×"
                   "CI inflation factor"
                   ci-inflation-factor)))

(defn print-effective-sample-sizes
  "Print effective sample size analysis for all metrics.
  Only outputs if at least one metric has CI inflation factor other than 1.0."
  [view data-map]
  (let [metrics (acf-common/collect-ess-metrics view data-map)]
    (when (some #(not= 1.0 (:ci-inflation-factor (first %))) metrics)
      (doseq [[ess-data acf-data mc] metrics]
        (let [combined (merge ess-data (select-keys acf-data [:lag-1]))]
          (print-effective-sample-size-for-metric combined (:label mc)))))))

(defmethod view/effective-sample-size* :print
  [_ view data-map]
  (print-effective-sample-sizes view data-map))

;;; Domain Apply View

(defmethod view/domain-apply* :print
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
          (println (format "Run: %s" (pr-str coord)))
          (view-fn viewer data))))))
