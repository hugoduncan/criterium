(ns criterium.viewer.print.domain
  "Print viewer domain analysis views.

  Provides text output for:
  - domain-grouped views
  - domain-extract tables
  - domain-comparison tables
  - domain-regression results
  - domain-apply iteration"
  (:require
   [clojure.string :as str]
   [criterium.benchmark :as benchmark]
   [criterium.domain.types :as domain.types]
   [criterium.util.format :as format]
   [criterium.view :as view]
   [criterium.viewer.common.core :as core]
   [criterium.viewer.common.domain.comparison :as comparison]
   [criterium.viewer.common.domain.detection :as detection]
   [criterium.viewer.common.domain.extract :as extract]
   [criterium.viewer.common.regression :as regression]
   [criterium.viewer.print.core :as print.core]))

(set! *unchecked-math* false)

;;; Helper functions

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

(defn- sort-data-by-coords
  "Sort coordinate-value pairs, using numeric sort when coord values are numbers."
  [data single-key-info]
  (let [coords (map first data)
        sorted-coords (core/sort-row-keys coords single-key-info)
        coord-order (zipmap sorted-coords (range))]
    (sort-by #(get coord-order (first %)) data)))

(defn- format-coord-value
  "Format a coordinate for display, extracting the value for single-key maps."
  [coord single-key-info]
  (let [v (core/format-row-key-value coord single-key-info)]
    (if (string? v) v (str v))))

(defn- print-transposed-table
  "Print a transposed table with implementation rows and metric columns.
  Takes {:heading :col-headers :rows} from prepare-*-table-transposed functions."
  [{:keys [heading col-headers rows]}]
  (let [;; Convert rows from maps to vectors based on col-headers order
        row-vectors (mapv (fn [row]
                            (mapv #(or (get row %) "-") col-headers))
                          rows)]
    (print.core/print-table
     {:heading heading
      :columns (mapv (fn [h] {:header h}) col-headers)
      :rows row-vectors})))

;;; Domain Extract Views

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
                stripped-coords (core/strip-uniform-axes raw-coords)
                coord-map (zipmap raw-coords stripped-coords)
                single-key-info (core/single-key-coord-info stripped-coords)
                sorted-data (sort-data-by-coords data single-key-info)]
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

;;; Domain Grouped Views

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

;;; Domain Comparison Views

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
        table-rows (mapv (fn [row-key vals]
                           (into [row-key] vals))
                         row-keys formatted-vals)]
    (print.core/print-table
     {:heading (format "Domain Comparison by %s: %s" (name axis) (pr-str metric))
      :row-key-col {:header ""}
      :columns (mapv (fn [h] {:header h}) col-headers)
      :rows table-rows})))

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
        table-rows (mapv (fn [row-key vals]
                           (into [row-key] vals))
                         row-keys-formatted formatted-rows)]
    (print.core/print-table
     {:heading (format "Domain Comparison by %s: %s" (name axis) (pr-str metric))
      :row-key-col {:header ""}
      :columns (mapv (fn [h] {:header h}) col-headers)
      :rows table-rows})))

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
        col-specs (vec (mapcat (fn [metric-id]
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
                               metric-ids))
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
        table-rows (mapv (fn [row-key vals]
                           (into [row-key] vals))
                         row-keys-formatted formatted-rows)]
    (print.core/print-table
     {:heading (format "Domain Comparison by %s" (name axis))
      :row-key-col {:header ""}
      :columns (mapv (fn [h] {:header h}) col-headers)
      :rows table-rows})))

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

;;; Domain Regression Views

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
