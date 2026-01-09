(ns criterium.viewer.common.domain.comparison
  "Domain comparison data preparation functions.

  Provides functions to prepare domain-comparison data for various chart types
  including box plots, bar charts, and line charts, as well as table rendering."
  (:require
   [criterium.viewer.common.core :as core]))

;;; Box plot data preparation

(defn- prepare-comparison-box-data-multi-metric
  "Prepare box data for multi-metric comparison mode."
  [metrics implementations]
  (->> metrics
       (sort-by key)
       (keep
        (fn [[metric-id {:keys [metric data]}]]
          (let [;; Build lookup: impl -> bootstrap stats value map
                lookup (reduce
                        (fn [acc [impl-val entries]]
                          (reduce
                           (fn [acc2 {:keys [value]}]
                             (assoc acc2 impl-val value))
                           acc
                           entries))
                        {}
                        data)
                ;; Get all values from lookup
                all-raw-values (keep #(get lookup %) implementations)
                ;; Check if all values have required box plot fields
                all-have-box-data? (every? core/has-box-plot-data? all-raw-values)]
            (if-not all-have-box-data?
              (do
                (core/warn-missing-bootstrap-stats metric-id)
                nil)
              (let [;; Get median values for SI scaling
                    all-medians (map :median all-raw-values)
                    {:keys [^double total-scale unit]}
                    (core/compute-si-scaling metric all-medians)
                    ;; Build y-axis title with unit
                    metric-name (name metric-id)
                    base-title (str "median " metric-name)
                    y-title (if (seq unit)
                              (str base-title " (" unit ")")
                              base-title)
                    ;; Build chart data
                    chart-data (mapv
                                (fn [impl]
                                  (let [v (get lookup impl)]
                                    (cond-> {"impl" (name impl)
                                             "median" (* (double (:median v)) total-scale)
                                             "p10" (* (double (:p10 v)) total-scale)
                                             "p90" (* (double (:p90 v)) total-scale)}
                                      (contains? v :ci-lower)
                                      (assoc "ciLower" (* (double (:ci-lower v)) total-scale))
                                      (contains? v :ci-upper)
                                      (assoc "ciUpper" (* (double (:ci-upper v)) total-scale)))))
                                implementations)]
                {:metric-id metric-id
                 :metric-path metric
                 :y-title y-title
                 :data chart-data})))))
       vec))

(defn- prepare-comparison-box-data-single-metric
  "Prepare box data for single-metric comparison mode."
  [metric implementations data]
  (let [;; Build lookup: impl -> bootstrap stats value map
        lookup (reduce
                (fn [acc [impl-val entries]]
                  (reduce
                   (fn [acc2 {:keys [value]}]
                     (assoc acc2 impl-val value))
                   acc
                   entries))
                {}
                data)
        ;; Get all values from lookup
        all-raw-values (keep #(get lookup %) implementations)
        ;; Check if all values have required box plot fields
        all-have-box-data? (every? core/has-box-plot-data? all-raw-values)]
    (if-not all-have-box-data?
      (do
        (core/warn-missing-bootstrap-stats (pr-str metric))
        [])
      (let [;; Get median values for SI scaling
            all-medians (map :median all-raw-values)
            {:keys [^double total-scale unit]}
            (core/compute-si-scaling metric all-medians)
            ;; Build y-axis title with unit
            base-title (str "median " (pr-str metric))
            y-title (if (seq unit)
                      (str base-title " (" unit ")")
                      base-title)
            ;; Build chart data
            chart-data (mapv
                        (fn [impl]
                          (let [v (get lookup impl)]
                            (cond-> {"impl" (name impl)
                                     "median" (* (double (:median v)) total-scale)
                                     "p10" (* (double (:p10 v)) total-scale)
                                     "p90" (* (double (:p90 v)) total-scale)}
                              (contains? v :ci-lower)
                              (assoc "ciLower" (* (double (:ci-lower v)) total-scale))
                              (contains? v :ci-upper)
                              (assoc "ciUpper" (* (double (:ci-upper v)) total-scale)))))
                        implementations)]
        [{:metric-id nil
          :metric-path metric
          :y-title y-title
          :data chart-data}]))))

(defn prepare-comparison-box-data
  "Prepare data for single-point box plot from domain comparison.
  Extracts bootstrap statistics (median, CI, percentiles) from embedded data.

  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword (or nil for single-metric mode)
    :metric-path - the metric path vector
    :y-title - y-axis title with SI unit (uses 'median' prefix)
    :data - vector of maps with string keys:
            {\"impl\" string \"median\" number \"ciLower\" number \"ciUpper\" number
             \"p10\" number \"p90\" number}
            (ciLower/ciUpper omitted when CI bounds not available)

  If bootstrap stats are missing for a metric, warns to stdout and returns nil
  for that metric entry (filtered from result)."
  [comparison]
  (let [{:keys [metric metrics implementations data]} comparison]
    (if metrics
      (prepare-comparison-box-data-multi-metric metrics implementations)
      (prepare-comparison-box-data-single-metric metric implementations data))))

;;; Bar chart data preparation

(defn prepare-comparison-bar-data
  "Prepare data for single-point bar chart from domain comparison.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword (or nil for single-metric mode)
    :metric-path - the metric path vector
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {:impl string :value number :valueLower number :valueUpper number} maps
            (valueLower/valueUpper only present when error bounds exist)"
  [comparison]
  (let [{:keys [metric metrics implementations data]} comparison]
    (if metrics
      ;; Multi-metric mode
      (mapv
       (fn [[metric-id {:keys [metric data]}]]
         (let [;; Build lookup: impl -> full value (may be map with :value/:lower/:upper)
               lookup (reduce
                       (fn [acc [impl-val entries]]
                         (reduce
                          (fn [acc2 {:keys [value]}]
                            (assoc acc2 impl-val value))
                          acc
                          entries))
                       {}
                       data)
               ;; Get all values from lookup for error bounds check and SI scaling
               all-raw-values (keep #(get lookup %) implementations)
               ;; Check if any values have error bounds
               has-error-bounds? (core/values-have-error-bounds? all-raw-values)
               ;; Get numeric values for SI scaling
               get-numeric (fn [v]
                             (if (and (map? v) (contains? v :value))
                               (:value v)
                               v))
               all-values (map get-numeric all-raw-values)
               {:keys [^double total-scale unit]}
               (core/compute-si-scaling metric all-values)
               ;; Build y-axis title with unit
               metric-name (name metric-id)
               base-title (if has-error-bounds?
                            (str "mean " metric-name)
                            metric-name)
               y-title (if (seq unit)
                         (str base-title " (" unit ")")
                         base-title)
               ;; Build chart data
               chart-data (mapv
                           (fn [impl]
                             (let [v (get lookup impl)
                                   raw-value (get-numeric v)]
                               (cond-> {"impl" (name impl)
                                        "value" (when raw-value
                                                  (* (double raw-value) total-scale))}
                                 (and has-error-bounds?
                                      (map? v)
                                      (contains? v :lower))
                                 (assoc "valueLower" (* (double (:lower v)) total-scale))
                                 (and has-error-bounds?
                                      (map? v)
                                      (contains? v :upper))
                                 (assoc "valueUpper" (* (double (:upper v)) total-scale)))))
                           implementations)]
           {:metric-id metric-id
            :metric-path metric
            :y-title y-title
            :has-error-bounds? has-error-bounds?
            :data chart-data}))
       (sort-by key metrics))
      ;; Single-metric mode
      (let [;; Build lookup: impl -> full value (may be map with :value/:lower/:upper)
            lookup (reduce
                    (fn [acc [impl-val entries]]
                      (reduce
                       (fn [acc2 {:keys [value]}]
                         (assoc acc2 impl-val value))
                       acc
                       entries))
                    {}
                    data)
            ;; Get all values from lookup for error bounds check and SI scaling
            all-raw-values (keep #(get lookup %) implementations)
            ;; Check if any values have error bounds
            has-error-bounds? (core/values-have-error-bounds? all-raw-values)
            ;; Get numeric values for SI scaling
            get-numeric (fn [v]
                          (if (and (map? v) (contains? v :value))
                            (:value v)
                            v))
            all-values (map get-numeric all-raw-values)
            {:keys [^double total-scale unit]}
            (core/compute-si-scaling metric all-values)
            ;; Build y-axis title with unit
            base-title (if has-error-bounds?
                         (str "mean " (pr-str metric))
                         (pr-str metric))
            y-title (if (seq unit)
                      (str base-title " (" unit ")")
                      base-title)
            ;; Build chart data
            chart-data (mapv
                        (fn [impl]
                          (let [v (get lookup impl)
                                raw-value (get-numeric v)]
                            (cond-> {"impl" (name impl)
                                     "value" (when raw-value
                                               (* (double raw-value) total-scale))}
                              (and has-error-bounds?
                                   (map? v)
                                   (contains? v :lower))
                              (assoc "valueLower" (* (double (:lower v)) total-scale))
                              (and has-error-bounds?
                                   (map? v)
                                   (contains? v :upper))
                              (assoc "valueUpper" (* (double (:upper v)) total-scale)))))
                        implementations)]
        [{:metric-id nil
          :metric-path metric
          :y-title y-title
          :has-error-bounds? has-error-bounds?
          :data chart-data}]))))

;;; Line chart data preparation

(defn prepare-line-chart-data
  "Prepare data for line chart from domain extract.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword
    :metric-path - the metric path vector
    :x-title - x-axis title (the axis name)
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {\"x\" number \"y\" number \"impl\" string} maps
            (yLower/yUpper only present when error bounds exist)"
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        metrics (:metrics extract)
        ;; Find the non-impl axis key
        first-metric-data (:data (val (first metrics)))
        first-coord (first (first first-metric-data))
        non-impl-keys (when (map? first-coord)
                        (disj (set (keys first-coord)) impl-axis-key))
        axis-key (first non-impl-keys)]
    (mapv
     (fn [[metric-id {:keys [metric data]}]]
       (let [;; Get all raw values for error detection and SI scaling
             all-raw-values (keep (fn [[_coord value]] value) data)
             ;; Check if any values have error bounds (with :lower/:upper)
             has-error-bounds? (core/values-have-error-bounds? all-raw-values)
             ;; Also check for error-bound-value? (with :value key) for y-title
             has-error-bound-format (some core/error-bound-value? all-raw-values)
             all-values (map core/get-numeric-value all-raw-values)
             {:keys [^double total-scale unit]}
             (core/compute-si-scaling metric all-values)
             ;; Build axis titles
             x-title (name axis-key)
             metric-name (name metric-id)
             base-title (if (or has-error-bounds? has-error-bound-format)
                          (str "mean " metric-name)
                          metric-name)
             y-title (if (seq unit)
                       (str base-title " (" unit ")")
                       base-title)
             ;; Build chart data points
             chart-data (mapv
                         (fn [[coord value]]
                           (let [raw-value (core/get-numeric-value value)
                                 x-val (get coord axis-key)
                                 impl-val (get coord impl-axis-key)]
                             (cond-> {"x" x-val
                                      "y" (when raw-value
                                            (* (double raw-value) total-scale))
                                      "impl" (name impl-val)}
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :lower))
                               (assoc "yLower" (* (double (:lower value)) total-scale))
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :upper))
                               (assoc "yUpper" (* (double (:upper value)) total-scale)))))
                         data)]
         {:metric-id metric-id
          :metric-path metric
          :x-title x-title
          :y-title y-title
          :has-error-bounds? has-error-bounds?
          :data chart-data}))
     (sort-by key metrics))))

(defn- find-non-axis-key
  "Find the non-axis coordinate key from comparison data.
  Returns the single non-axis key if coords have exactly one, else nil."
  [{:keys [axis metrics data]}]
  (let [first-entries (if metrics
                        (some-> metrics vals first :data vals first)
                        (some-> data vals first))
        first-coord (some-> first-entries first :coord)]
    (when (map? first-coord)
      (let [non-axis-keys (disj (set (keys first-coord)) axis)]
        (when (= 1 (count non-axis-keys))
          (first non-axis-keys))))))

(defn prepare-comparison-line-data
  "Prepare data for line chart from domain comparison.
  Returns a vector of maps, one per metric, each containing:
    :metric-id - the metric keyword (or nil for single-metric mode)
    :metric-path - the metric path vector
    :x-title - x-axis title (the non-axis coordinate name)
    :y-title - y-axis title with SI unit
    :has-error-bounds? - true if error bounds data is present
    :data - vector of {\"x\" number \"y\" number \"impl\" string} maps
            (yLower/yUpper only present when error bounds exist)

  When the comparison axis is the implementation axis (i.e., implementations
  are present), uses the non-axis coordinate key for the x-axis."
  [comparison]
  (let [{:keys [axis metric metrics data implementations]} comparison
        ;; Use non-axis coord key for x when comparing implementations
        x-key (if implementations
                (or (find-non-axis-key comparison) axis)
                axis)
        x-title (name x-key)]
    (if metrics
      ;; Multi-metric mode
      (mapv
       (fn [[metric-id {:keys [metric data]}]]
         (let [;; Get all raw values for error detection and SI scaling
               all-raw-values (->> data
                                   vals
                                   (mapcat (fn [entries]
                                             (keep :value entries))))
               ;; Check if any values have error bounds (with :lower/:upper)
               has-error-bounds? (core/values-have-error-bounds? all-raw-values)
               ;; Also check for error-bound-value? (with :value key) for y-title
               has-error-bound-format (some core/error-bound-value? all-raw-values)
               all-values (map core/get-numeric-value all-raw-values)
               {:keys [^double total-scale unit]}
               (core/compute-si-scaling metric all-values)
               ;; Build y-axis title
               metric-name (name metric-id)
               base-title (if (or has-error-bounds? has-error-bound-format)
                            (str "mean " metric-name)
                            metric-name)
               y-title (if (seq unit)
                         (str base-title " (" unit ")")
                         base-title)
               ;; Build chart data points
               chart-data (vec
                           (for [[impl-val entries] data
                                 {:keys [coord value]} entries
                                 :let [raw-value (core/get-numeric-value value)
                                       x-val (get coord x-key)]
                                 :when (some? raw-value)]
                             (cond-> {"x" x-val
                                      "y" (* (double raw-value) total-scale)
                                      "impl" (name impl-val)}
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :lower))
                               (assoc "yLower" (* (double (:lower value)) total-scale))
                               (and has-error-bounds?
                                    (map? value)
                                    (contains? value :upper))
                               (assoc "yUpper" (* (double (:upper value)) total-scale)))))]
           {:metric-id metric-id
            :metric-path metric
            :x-title x-title
            :y-title y-title
            :has-error-bounds? has-error-bounds?
            :data chart-data}))
       (sort-by key metrics))
      ;; Single-metric mode
      (let [;; Get all raw values for error detection and SI scaling
            all-raw-values (->> data
                                vals
                                (mapcat (fn [entries]
                                          (keep :value entries))))
            ;; Check if any values have error bounds (with :lower/:upper)
            has-error-bounds? (core/values-have-error-bounds? all-raw-values)
            ;; Also check for error-bound-value? (with :value key) for y-title
            has-error-bound-format (some core/error-bound-value? all-raw-values)
            all-values (map core/get-numeric-value all-raw-values)
            {:keys [^double total-scale unit]}
            (core/compute-si-scaling metric all-values)
            ;; Build y-axis title
            base-title (if (or has-error-bounds? has-error-bound-format)
                         (str "mean " (pr-str metric))
                         (pr-str metric))
            y-title (if (seq unit)
                      (str base-title " (" unit ")")
                      base-title)
            ;; Build chart data points
            chart-data (vec
                        (for [[impl-val entries] data
                              {:keys [coord value]} entries
                              :let [raw-value (core/get-numeric-value value)
                                    x-val (get coord x-key)]
                              :when (some? raw-value)]
                          (cond-> {"x" x-val
                                   "y" (* (double raw-value) total-scale)
                                   "impl" (name impl-val)}
                            (and has-error-bounds?
                                 (map? value)
                                 (contains? value :lower))
                            (assoc "yLower" (* (double (:lower value)) total-scale))
                            (and has-error-bounds?
                                 (map? value)
                                 (contains? value :upper))
                            (assoc "yUpper" (* (double (:upper value)) total-scale)))))]
        [{:metric-id nil
          :metric-path metric
          :x-title x-title
          :y-title y-title
          :has-error-bounds? has-error-bounds?
          :data chart-data}]))))

;;; Table preparation

(defn- extract-row-key
  "Extract row key from coord, removing axis key for map coords."
  [coord axis]
  (if (map? coord)
    (dissoc coord axis)
    coord))

(defn- build-absolute-value-table
  "Build a table spec for absolute value display (no implementations)."
  [axis metric data]
  (let [axis-vals (sort-by str (keys data))]
    (when (and (seq data) (some #(seq (second %)) data))
      (let [all-entries (mapcat
                         (fn [[axis-val entries]]
                           (mapv #(assoc % :axis-val axis-val) entries))
                         data)
            raw-row-keys (->> all-entries
                              (mapv
                               #(extract-row-key (:coord %) axis)) distinct)
            single-key-info (core/single-key-coord-info raw-row-keys)
            row-keys (core/sort-row-keys
                      raw-row-keys
                      single-key-info)
            coord-header (core/coord-column-header single-key-info)
            lookup (reduce
                    (fn [acc {:keys [coord value axis-val]}]
                      (let [row-key (extract-row-key coord axis)]
                        (assoc-in acc [row-key axis-val] value)))
                    {}
                    all-entries)
            all-values (keep :value all-entries)
            {:keys [^double total-scale unit]}
            (core/compute-si-scaling metric all-values)
            heading (str
                     "Domain Comparison by "
                     (name axis) ": " (pr-str metric)
                     (when (seq unit) (str " (" unit ")")))
            col-headers (mapv str axis-vals)
            table-rows
            (mapv (fn [row-key]
                    (into {coord-header (core/format-row-key-value
                                         row-key
                                         single-key-info)}
                          (map (fn [av]
                                 (let [raw-value (double
                                                  (get-in lookup [row-key av]))]
                                   [(str av)
                                    (when raw-value
                                      (format
                                       "%.3g"
                                       (* raw-value total-scale)))]))
                               axis-vals)))
                  row-keys)]
        {:heading heading
         :coord-header coord-header
         :col-headers col-headers
         :rows table-rows}))))

(defn- build-factor-table-single-metric
  "Build a table spec for single-metric factor display (with implementations)."
  [axis metric implementations data]
  (let [data-keys (set (keys data))
        missing (remove data-keys implementations)]
    (when (seq missing)
      (throw (ex-info "Domain :implementations do not match comparison data keys"
                      {:implementations implementations
                       :data-keys (keys data)
                       :missing missing})))
    (let [baseline-impl (first implementations)
          other-impls (rest implementations)
          all-row-keys (->> (vals data)
                            (mapcat (fn [entries]
                                      (map #(extract-row-key (:coord %) axis) entries)))
                            distinct)
          single-key-info (core/single-key-coord-info all-row-keys)
          row-keys (core/sort-row-keys all-row-keys single-key-info)
          coord-header (core/coord-column-header single-key-info)
          lookup (reduce (fn [acc [impl-val entries]]
                           (reduce (fn [acc2 {:keys [coord value]}]
                                     (let [row-key (extract-row-key coord axis)]
                                       (assoc-in acc2 [impl-val row-key] value)))
                                   acc
                                   entries))
                         {}
                         data)
          ;; Build column specs: baseline shows value, others show value + factor
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
          table-rows
          (mapv
           (fn [row-key]
             (into {coord-header (core/format-row-key-value row-key single-key-info)}
                   (map (fn [{:keys [type impl]} header]
                          (let [raw-value (core/get-numeric-value
                                           (get-in lookup [impl row-key]))
                                value (when raw-value (double raw-value))
                                raw-baseline (core/get-numeric-value
                                              (get-in
                                               lookup
                                               [baseline-impl row-key]))
                                baseline-value (when raw-baseline
                                                 (double raw-baseline))]
                            [header
                             (case type
                               :baseline (core/format-value-with-unit value metric)
                               :value (core/format-value-with-unit value metric)
                               :factor (cond
                                         (nil? value) "-"
                                         (nil? baseline-value) "-"
                                         (zero? ^double baseline-value) "-"
                                         :else
                                         (format
                                          "%.2f"
                                          (/ ^double value
                                             ^double baseline-value))))]))
                        col-specs col-headers)))
           row-keys)]
      {:heading (str "Domain Comparison by " (name axis) ": " (pr-str metric))
       :coord-header coord-header
       :col-headers col-headers
       :rows table-rows})))

(defn- build-factor-table-multi-metric
  "Build a table spec for multi-metric factor display (with implementations)."
  [axis implementations metrics]
  (let [baseline-impl (first implementations)
        other-impls (rest implementations)
        metric-ids (sort (keys metrics))
        all-row-keys (->> (vals metrics)
                          (mapcat (fn [{:keys [data]}]
                                    (mapcat (fn [[_impl entries]]
                                              (map #(extract-row-key (:coord %) axis) entries))
                                            data)))
                          distinct)
        single-key-info (core/single-key-coord-info all-row-keys)
        row-keys (core/sort-row-keys all-row-keys single-key-info)
        coord-header (core/coord-column-header single-key-info)
        lookup (reduce (fn [acc [metric-id {:keys [data]}]]
                         (reduce (fn [acc2 [impl-val entries]]
                                   (reduce (fn [acc3 {:keys [coord value]}]
                                             (let [row-key (extract-row-key coord axis)]
                                               (assoc-in acc3 [metric-id impl-val row-key] value)))
                                           acc2
                                           entries))
                                 acc
                                 data))
                       {}
                       metrics)
        ;; Build column specs: baseline shows value, others show value + factor
        col-specs (vec (mapcat (fn [metric-id]
                                 (let [metric-path (get-in metrics [metric-id :metric])]
                                   (cons {:type :baseline
                                          :metric-id metric-id
                                          :metric-path metric-path
                                          :impl baseline-impl}
                                         (mapcat (fn [impl]
                                                   [{:type :value
                                                     :metric-id metric-id
                                                     :metric-path metric-path
                                                     :impl impl}
                                                    {:type :factor
                                                     :metric-id metric-id
                                                     :metric-path metric-path
                                                     :impl impl}])
                                                 other-impls))))
                               metric-ids))
        col-headers (mapv (fn [{:keys [type metric-id impl]}]
                            (case type
                              :baseline (str (name impl) " " (name metric-id))
                              :value (str (name impl) " " (name metric-id))
                              :factor (str (name impl) " ×")))
                          col-specs)
        table-rows
        (mapv
         (fn [row-key]
           (into
            {coord-header (core/format-row-key-value row-key single-key-info)}
            (map
             (fn [{:keys [type metric-id metric-path impl]} header]
               (let [raw-value (core/get-numeric-value
                                (get-in lookup [metric-id impl row-key]))
                     value (when raw-value (double raw-value))
                     raw-baseline (core/get-numeric-value
                                   (get-in
                                    lookup
                                    [metric-id baseline-impl row-key]))
                     baseline-value (when raw-baseline (double raw-baseline))]
                 [header
                  (case type
                    :baseline (core/format-value-with-unit value metric-path)
                    :value (core/format-value-with-unit value metric-path)
                    :factor (cond
                              (nil? value) "-"
                              (nil? baseline-value) "-"
                              (zero? ^double baseline-value) "-"
                              :else (format "%.2f"
                                            (/ ^double value
                                               ^double baseline-value))))]))
             col-specs col-headers)))
         row-keys)]
    {:heading (str "Domain Comparison by " (name axis))
     :coord-header coord-header
     :col-headers col-headers
     :rows table-rows}))

(defn prepare-domain-comparison-tables
  "Prepare domain-comparison data for table rendering.
  Returns a vector of table specs, each with:
    {:heading string :coord-header string :col-headers [string...] :rows [{...}...]}
  Returns nil if comparison is nil.

  Handles 4 modes:
  - Multi-metric with implementations: single table with factor display
  - Multi-metric without implementations: multiple tables (one per metric)
  - Single-metric with implementations: single table with factor display
  - Single-metric without implementations: single table with absolute values"
  [comparison]
  (when comparison
    (let [{:keys [axis metric metrics implementations data]} comparison]
      (if metrics
        ;; Multi-metric mode
        (if implementations
          ;; Multi-metric with implementations - factor display
          [(build-factor-table-multi-metric axis implementations metrics)]
          ;; Multi-metric without implementations - one table per metric
          (vec (keep (fn [[_metric-id {:keys [metric data]}]]
                       (build-absolute-value-table axis metric data))
                     metrics)))
        ;; Single-metric mode
        (if implementations
          ;; Single-metric with implementations - factor display
          [(build-factor-table-single-metric axis metric implementations data)]
          ;; Single-metric without implementations - absolute values
          (when-let [table (build-absolute-value-table axis metric data)]
            [table]))))))

(defn- extract-median-value
  "Extract median value from a bootstrap stats value map.
  Falls back to :value then raw value for backward compatibility."
  [value]
  (cond
    (and (map? value) (contains? value :median)) (:median value)
    (and (map? value) (contains? value :value)) (:value value)
    :else value))

(defn- has-ci-bounds?
  "Check if a value map has confidence interval bounds."
  [value]
  (and (map? value)
       (contains? value :ci-lower)
       (contains? value :ci-upper)))

(defn prepare-domain-comparison-table-transposed
  "Prepare transposed domain-comparison table for single-point multi-impl scenarios.
  Returns {:heading :col-headers :rows} where each row is one implementation.

  Columns include implementation name, then for each metric: median value,
  CI bounds (when available), and factor. Factor is relative to baseline
  (first implementation), calculated using median values."
  [comparison]
  (when comparison
    (let [{:keys [axis metric metrics implementations data]} comparison
          baseline-impl (first implementations)
          ;; Normalize to multi-metric structure
          ;; For single-metric mode, derive a meaningful key from metric path
          metrics-map (or metrics
                          (let [metric-id (or (second metric) :value)]
                            {metric-id {:metric metric :data data}}))
          metric-ids (sort (keys metrics-map))

          ;; Build lookup: {[impl metric-id] -> full-value-map}
          ;; Keep the full value map so we can extract CI bounds
          value-lookup
          (reduce
           (fn [acc [metric-id {:keys [data]}]]
             (reduce
              (fn [acc2 [impl-val entries]]
                (reduce
                 (fn [acc3 {:keys [value]}]
                   (assoc acc3 [impl-val metric-id] value))
                 acc2
                 entries))
              acc
              data))
           {}
           metrics-map)

          ;; Build median lookup for SI scaling and factor calculation
          median-lookup
          (into {}
                (map (fn [[k v]] [k (extract-median-value v)]))
                value-lookup)

          ;; Check which metrics have CI bounds available
          metric-has-ci
          (into {}
                (map (fn [metric-id]
                       [metric-id
                        (some (fn [impl]
                                (has-ci-bounds? (get value-lookup [impl metric-id])))
                              implementations)]))
                metric-ids)

          ;; Compute SI scaling per metric (using median values)
          metric-scales
          (into {}
                (map (fn [metric-id]
                       (let [metric-path (get-in metrics-map [metric-id :metric])
                             all-values (keep (fn [impl]
                                                (get median-lookup [impl metric-id]))
                                              implementations)]
                         [metric-id (core/compute-si-scaling metric-path all-values)])))
                metric-ids)

          ;; Build column headers: Implementation, then for each metric:
          ;; median value, CI (when available), and factor
          col-headers
          (into ["Implementation"]
                (mapcat (fn [metric-id]
                          (let [{:keys [unit]} (get metric-scales metric-id)
                                metric-name (name metric-id)
                                value-header (if (seq unit)
                                               (str "median " metric-name " (" unit ")")
                                               (str "median " metric-name))
                                ci-header (str metric-name " CI")
                                factor-header (str metric-name " ×")]
                            (if (get metric-has-ci metric-id)
                              [value-header ci-header factor-header]
                              [value-header factor-header])))
                        metric-ids))

          ;; Build table rows: one per implementation
          table-rows
          (mapv
           (fn [impl]
             (into {"Implementation" (name impl)}
                   (mapcat
                    (fn [metric-id]
                      (let [{:keys [unit ^double total-scale]}
                            (get metric-scales metric-id)
                            metric-name (name metric-id)
                            value-header (if (seq unit)
                                           (str "median " metric-name " (" unit ")")
                                           (str "median " metric-name))
                            ci-header (str metric-name " CI")
                            factor-header (str metric-name " ×")
                            full-value (get value-lookup [impl metric-id])
                            median-value (get median-lookup [impl metric-id])
                            baseline-median (get median-lookup [baseline-impl metric-id])
                            formatted-value (when median-value
                                              (format "%.3g"
                                                      (* (double median-value)
                                                         total-scale)))
                            ;; Format CI as "lower - upper" when available
                            formatted-ci (when (has-ci-bounds? full-value)
                                           (format "%.3g - %.3g"
                                                   (* (double (:ci-lower full-value))
                                                      total-scale)
                                                   (* (double (:ci-upper full-value))
                                                      total-scale)))
                            factor (when (and median-value baseline-median
                                              (not (zero? (double baseline-median))))
                                     (format "%.2f"
                                             (/ (double median-value)
                                                (double baseline-median))))]
                        (if (get metric-has-ci metric-id)
                          [[value-header formatted-value]
                           [ci-header formatted-ci]
                           [factor-header factor]]
                          [[value-header formatted-value]
                           [factor-header factor]])))
                    metric-ids)))
           implementations)]

      {:heading (str "Domain Comparison by " (name axis))
       :col-headers col-headers
       :rows table-rows})))
