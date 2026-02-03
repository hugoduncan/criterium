(ns criterium.viewer.common.domain.extract
  "Domain extract table preparation functions.

  Provides functions to prepare domain-extract data for table rendering,
  including transposed tables for single-point multi-impl scenarios and
  grouped data tables."
  (:require
   [criterium.viewer.common.core :as core]))

(defn- format-value-with-ci
  "Format a value with optional inline confidence interval.
  Returns string like '1.23' or '1.23 (1.20-1.26)' when bounds present."
  [value lower upper ^double total-scale]
  (let [scaled-value (* (double value) total-scale)
        base-str (format "%.3g" scaled-value)]
    (if (and lower upper)
      (let [scaled-lower (* (double lower) total-scale)
            scaled-upper (* (double upper) total-scale)]
        (format "%s (%.3g-%.3g)" base-str scaled-lower scaled-upper))
      base-str)))

(defn- metric-type-prefix
  "Extract metric type (mean/median) from metric path for column header."
  [metric-path]
  (when (and (vector? metric-path) (>= (count metric-path) 3))
    (let [value-key (nth metric-path 2)]
      (when (#{:mean :median} value-key)
        (name value-key)))))

(defn prepare-domain-extract-table
  "Prepare domain-extract data for table rendering.
  Returns {:heading string :coord-header string :col-headers [string...]
           :rows [{col-header value...}...]} or nil if extract is nil.

  Column headers include metric type (mean/median) when available.
  Values with error bounds are formatted inline as 'value (lower-upper)'.

  Options:
    :header-sep - separator between impl and metric in multi-impl headers
                  (default \" \")"
  [extract {:keys [header-sep] :or {header-sep " "}}]
  (when extract
    (let [impl-axis-key (:impl-axis extract)
          multi-impl? (> (count (:implementations extract)) 1)
          metrics (:metrics extract)
          metric-ids (sort (keys metrics))

          ;; Collect all data points with their full coords
          ;; Preserve original value for error bounds extraction
          all-data (for [[metric-id {:keys [metric data]}] metrics
                         [coord value] data]
                     {:metric-id metric-id
                      :metric metric
                      :coord coord
                      :raw-value value
                      :value (core/get-numeric-value value)})

          ;; Collect all coordinates to detect uniform axes
          all-coords (map :coord all-data)

          ;; Find axes with uniform values across all coords (e.g., :impl
          ;; :default)
          ;; Only use uniform axes if stripping them leaves at least one key
          uniform-axes (core/detect-uniform-axes all-coords)
          first-coord (first all-coords)
          remaining-after-strip (when (and (map? first-coord)
                                           (seq uniform-axes))
                                  (count
                                   (apply dissoc first-coord uniform-axes)))
          use-uniform-axes? (and (seq uniform-axes)
                                 (some? remaining-after-strip)
                                 (pos? (long remaining-after-strip)))

          ;; Determine row key: strip impl axis for multi-impl, or uniform axes
          row-key-fn (cond
                       multi-impl?
                       (fn [coord] (dissoc coord impl-axis-key))

                       use-uniform-axes?
                       (fn [coord] (apply dissoc coord uniform-axes))

                       :else
                       identity)

          ;; Collect unique row keys
          raw-row-keys (->> all-data
                            (map (comp row-key-fn :coord))
                            distinct)

          ;; Detect single-key pattern and sort appropriately
          single-key-info (core/single-key-coord-info raw-row-keys)
          row-keys (core/sort-row-keys raw-row-keys single-key-info)
          coord-header (core/coord-column-header single-key-info)

          impl-vals (when multi-impl?
                      (->> all-data
                           (keep #(get (:coord %) impl-axis-key))
                           distinct
                           (sort-by str)))

          ;; Build column specs: [{:metric-id :impl (optional)}...]
          col-specs (if multi-impl?
                      (for [metric-id metric-ids
                            impl impl-vals]
                        {:metric-id metric-id :impl impl})
                      (for [metric-id metric-ids]
                        {:metric-id metric-id}))

          ;; Build lookup:
          ;;   {[row-key metric-id impl?] -> {:value V :lower L :upper U}}
          ;; Store full value map for error bounds
          lookup (reduce (fn [acc {:keys [metric-id coord raw-value value]}]
                           (let [row-key (row-key-fn coord)
                                 impl-val (when multi-impl?
                                            (get coord impl-axis-key))
                                 lookup-key (if multi-impl?
                                              [row-key metric-id impl-val]
                                              [row-key metric-id])
                                 entry (if (and (map? raw-value)
                                                (contains? raw-value :value))
                                         {:value value
                                          :lower (:lower raw-value)
                                          :upper (:upper raw-value)}
                                         {:value value})]
                             (assoc acc lookup-key entry)))
                         {}
                         all-data)

          ;; Compute SI scale and unit per column
          col-scales
          (into
           {}
           (map (fn [col-spec]
                  (let [{:keys [metric-id impl]}
                        col-spec
                        metric-path (get-in metrics [metric-id :metric])
                        col-values (for [row-key row-keys
                                         :let [lk (if multi-impl?
                                                    [row-key metric-id impl]
                                                    [row-key metric-id])
                                               entry (get lookup lk)
                                               v (:value entry)]
                                         :when (some? v)]
                                     v)]
                    [col-spec
                     (core/compute-si-scaling metric-path col-values)])))
           col-specs)

          ;; Build column headers with metric type prefix
          col-headers
          (mapv (fn [col-spec]
                  (let [{:keys [metric-id impl]}
                        col-spec
                        {:keys [unit]} (get col-scales col-spec)
                        metric-path (get-in metrics [metric-id :metric])
                        type-prefix (metric-type-prefix metric-path)
                        metric-name (name metric-id)
                        header-base (cond-> ""
                                      type-prefix (str type-prefix " ")
                                      true (str metric-name)
                                      (seq unit) (str " (" unit ")"))]
                    (if multi-impl?
                      (str (name impl) header-sep header-base)
                      header-base)))
                col-specs)

          ;; Build table rows with inline CI
          table-rows
          (mapv (fn [row-key]
                  (into {coord-header
                         (core/format-row-key-value row-key single-key-info)}
                        (map-indexed
                         (fn [idx col-spec]
                           (let [{:keys [metric-id impl]}
                                 col-spec
                                 lk (if multi-impl?
                                      [row-key metric-id impl]
                                      [row-key metric-id])
                                 {:keys [value lower upper]} (get lookup lk)
                                 {:keys [^double total-scale]}
                                 (get col-scales col-spec)
                                 header (nth col-headers idx)]
                             [header (when value
                                       (format-value-with-ci
                                        value lower upper total-scale))]))
                         col-specs)))
                row-keys)]

      {:heading "Domain Extract"
       :coord-header coord-header
       :col-headers col-headers
       :rows table-rows})))

(defn prepare-domain-extract-table-transposed
  "Prepare transposed domain-extract table for single-point multi-impl.
  Returns {:heading :col-headers :rows} where each row is one implementation.

  Columns include implementation name, then for each metric: value and factor.
  Factor is relative to baseline (first implementation).

  Column headers include metric type (mean/median) when available.
  Values with error bounds are formatted inline as 'value (lower-upper)'."
  [extract]
  (when extract
    (let [impl-axis-key (:impl-axis extract)
          implementations (:implementations extract)
          baseline-impl (first implementations)
          metrics (:metrics extract)
          metric-ids (sort (keys metrics))

          ;; Build lookup: {[impl metric-id] -> {:value V :lower L :upper U}}
          lookup
          (reduce
           (fn [acc [metric-id {:keys [data]}]]
             (reduce
              (fn [acc2 [coord value]]
                (let [impl-val (get coord impl-axis-key)
                      entry (if (and (map? value) (contains? value :value))
                              {:value (:value value)
                               :lower (:lower value)
                               :upper (:upper value)}
                              {:value value})]
                  (assoc acc2 [impl-val metric-id] entry)))
              acc
              data))
           {}
           metrics)

          ;; Compute SI scaling per metric (using all values for that metric)
          metric-scales
          (into {}
                (map (fn [metric-id]
                       (let [metric-path (get-in metrics [metric-id :metric])
                             all-values (keep (fn [impl]
                                                (:value
                                                 (get lookup [impl metric-id])))
                                              implementations)]
                         [metric-id
                          (core/compute-si-scaling metric-path all-values)])))
                metric-ids)

          ;; Build column headers with metric type prefix
          col-headers
          (into ["Implementation"]
                (mapcat (fn [metric-id]
                          (let [{:keys [unit]} (get metric-scales metric-id)
                                metric-path (get-in metrics [metric-id :metric])
                                type-prefix (metric-type-prefix metric-path)
                                metric-name (name metric-id)
                                value-header (cond-> ""
                                               type-prefix (str type-prefix " ")
                                               true (str metric-name)
                                               (seq unit) (str " (" unit ")"))]
                            [value-header (str metric-name " ×")]))
                        metric-ids))

          ;; Build table rows with inline CI
          table-rows
          (mapv
           (fn [impl]
             (into {"Implementation" (name impl)}
                   (mapcat
                    (fn [metric-id]
                      (let [{:keys [unit ^double total-scale]}
                            (get metric-scales metric-id)
                            metric-path (get-in metrics [metric-id :metric])
                            type-prefix (metric-type-prefix metric-path)
                            metric-name (name metric-id)
                            value-header (cond-> ""
                                           type-prefix (str type-prefix " ")
                                           true (str metric-name)
                                           (seq unit) (str " (" unit ")"))
                            factor-header (str metric-name " ×")
                            impl-entry (get lookup [impl metric-id])
                            impl-value (:value impl-entry)
                            baseline-entry (get
                                            lookup
                                            [baseline-impl metric-id])
                            baseline-value (:value baseline-entry)
                            formatted-value (when impl-value
                                              (format-value-with-ci
                                               impl-value
                                               (:lower impl-entry)
                                               (:upper impl-entry)
                                               total-scale))
                            factor (when (and impl-value baseline-value
                                              (not
                                               (zero? (double baseline-value))))
                                     (format "%.2f"
                                             (/ (double impl-value)
                                                (double baseline-value))))]
                        [[value-header formatted-value]
                         [factor-header factor]]))
                    metric-ids)))
           implementations)]

      {:heading "Domain Extract"
       :col-headers col-headers
       :rows table-rows})))

(defn prepare-domain-grouped-table
  "Prepare domain-grouped data for table rendering.
  Returns {:heading string :rows [{:axis-value string :run-count int}...]}
  or nil if grouped is nil."
  [grouped]
  (when grouped
    (let [{:keys [axis data]} grouped]
      {:heading (str "Domain Grouped by: " (name axis))
       :rows (mapv (fn [[axis-val sub-domain]]
                     {:axis-value (if (nil? axis-val)
                                    "<nil>"
                                    (str axis-val))
                      :run-count (count (:runs sub-domain))})
                   (sort-by (comp str key) data))})))
