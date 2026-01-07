(ns criterium.viewer.common.domain.extract
  "Domain extract table preparation functions.

  Provides functions to prepare domain-extract data for table rendering,
  including transposed tables for single-point multi-impl scenarios and
  grouped data tables."
  (:require
   [criterium.viewer.common.core :as core]))

(defn prepare-domain-extract-table
  "Prepare domain-extract data for table rendering.
  Returns {:heading string :coord-header string :col-headers [string...]
           :rows [{col-header value...}...]} or nil if extract is nil.
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
          all-data (for [[metric-id {:keys [metric data]}] metrics
                         [coord value] data]
                     {:metric-id metric-id
                      :metric metric
                      :coord coord
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

          ;; Build lookup: {[row-key metric-id impl?] -> value}
          lookup (reduce (fn [acc {:keys [metric-id coord value]}]
                           (let [row-key (row-key-fn coord)
                                 impl-val (when
                                           multi-impl?
                                            (get coord impl-axis-key))
                                 lookup-key (if multi-impl?
                                              [row-key metric-id impl-val]
                                              [row-key metric-id])]
                             (assoc acc lookup-key value)))
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
                                               v (get lookup lk)]
                                         :when (some? v)]
                                     v)]
                    [col-spec (core/compute-si-scaling metric-path col-values)])))
           col-specs)

          ;; Build column headers
          col-headers
          (mapv (fn [col-spec]
                  (let [{:keys [metric-id impl]}
                        col-spec
                        {:keys [unit]} (get col-scales col-spec)
                        metric-name (name metric-id)
                        header-base (if (seq unit)
                                      (str metric-name " (" unit ")")
                                      metric-name)]
                    (if multi-impl?
                      (str (name impl) header-sep header-base)
                      header-base)))
                col-specs)

          ;; Build table rows
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
                                 raw-value (get lookup lk)
                                 {:keys [^double total-scale]}
                                 (get col-scales col-spec)
                                 header (nth col-headers idx)]
                             [header (when raw-value
                                       (format
                                        "%.3g"
                                        (* (double raw-value) total-scale)))]))
                         col-specs)))
                row-keys)]

      {:heading "Domain Extract"
       :coord-header coord-header
       :col-headers col-headers
       :rows table-rows})))

(defn prepare-domain-extract-table-transposed
  "Prepare transposed domain-extract table for single-point multi-impl scenarios.
  Returns {:heading :col-headers :rows} where each row is one implementation.

  Columns include implementation name, then for each metric: value and factor.
  Factor is relative to baseline (first implementation)."
  [extract]
  (when extract
    (let [impl-axis-key (:impl-axis extract)
          implementations (:implementations extract)
          baseline-impl (first implementations)
          metrics (:metrics extract)
          metric-ids (sort (keys metrics))

          ;; Build lookup: {[impl metric-id] -> raw-value}
          lookup
          (reduce
           (fn [acc [metric-id {:keys [data]}]]
             (reduce
              (fn [acc2 [coord value]]
                (let [impl-val (get coord impl-axis-key)
                      raw-value (if (and (map? value) (contains? value :value))
                                  (:value value)
                                  value)]
                  (assoc acc2 [impl-val metric-id] raw-value)))
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
                                                (get lookup [impl metric-id]))
                                              implementations)]
                         [metric-id (core/compute-si-scaling metric-path all-values)])))
                metric-ids)

          ;; Build column headers: Implementation, then for each metric: value and ×
          col-headers
          (into ["Implementation"]
                (mapcat (fn [metric-id]
                          (let [{:keys [unit]} (get metric-scales metric-id)
                                metric-name (name metric-id)
                                value-header (if (seq unit)
                                               (str metric-name " (" unit ")")
                                               metric-name)]
                            [value-header (str metric-name " ×")]))
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
                                           (str metric-name " (" unit ")")
                                           metric-name)
                            factor-header (str metric-name " ×")
                            raw-value (get lookup [impl metric-id])
                            baseline-value (get lookup [baseline-impl metric-id])
                            formatted-value (when raw-value
                                              (format "%.3g"
                                                      (* (double raw-value)
                                                         total-scale)))
                            factor (when (and raw-value baseline-value
                                              (not (zero? (double baseline-value))))
                                     (format "%.2f"
                                             (/ (double raw-value)
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
