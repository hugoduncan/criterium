(ns criterium.viewer.common.domain.detection
  "Domain shape detection predicates for visualization strategy selection.

  Provides functions to analyze the structure of domain extract and comparison
  data to determine the appropriate visualization strategy (box plot, line chart,
  or table).")

;;; Domain extract shape detection

(defn single-point-multi-impl?
  "Return true when extract has exactly one non-impl axis with one value
  and multiple implementations.

  This detects the 'single-point comparison' scenario where we're comparing
  multiple implementations at a single parameter point."
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        impls (:implementations extract)
        multi-impl? (and impls (> (count impls) 1))]
    (when multi-impl?
      (let [metrics (:metrics extract)]
        (when (seq metrics)
          (let [;; Get all coordinates from first metric
                first-metric-data (:data (val (first metrics)))
                all-coords (map first first-metric-data)
                ;; Get the non-impl axis keys from first coordinate
                first-coord (first all-coords)
                non-impl-keys (when (map? first-coord)
                                (disj (set (keys first-coord)) impl-axis-key))
                ;; Single axis with single unique value?
                single-axis? (= 1 (count non-impl-keys))]
            (when single-axis?
              (let [axis-key (first non-impl-keys)
                    axis-values (into #{} (map #(get % axis-key)) all-coords)]
                (= 1 (count axis-values))))))))))

(defn single-axis-multi-point?
  "Return true when extract has exactly one non-impl axis with multiple values
  and multiple implementations.

  This detects the 'line chart' scenario where we're comparing multiple
  implementations across a range of parameter values on a single axis."
  [extract]
  (let [impl-axis-key (:impl-axis extract)
        impls (:implementations extract)
        multi-impl? (and impls (> (count impls) 1))]
    (when multi-impl?
      (let [metrics (:metrics extract)]
        (when (seq metrics)
          (let [;; Get all coordinates from first metric
                first-metric-data (:data (val (first metrics)))
                all-coords (map first first-metric-data)
                ;; Get the non-impl axis keys from first coordinate
                first-coord (first all-coords)
                non-impl-keys (when (map? first-coord)
                                (disj (set (keys first-coord)) impl-axis-key))
                ;; Single axis?
                single-axis? (= 1 (count non-impl-keys))]
            (when single-axis?
              (let [axis-key (first non-impl-keys)
                    axis-values (into #{} (map #(get % axis-key)) all-coords)]
                (> (count axis-values) 1)))))))))

(defn visualization-strategy
  "Determine the visualization strategy for domain extract data.

  Returns one of:
  - :single-point  - single parameter point, multiple implementations
                     (box plot showing median with CI and percentiles)
  - :multi-point   - multiple parameter points, single axis (line chart)
  - :default-table - regular table format (no chart)"
  [extract]
  (cond
    (single-point-multi-impl? extract) :single-point
    (single-axis-multi-point? extract) :multi-point
    :else :default-table))

;;; Domain comparison shape detection

(defn- comparison-all-entries
  "Iterate over all entries in a comparison, handling both single and multi-metric modes.
  Returns a lazy sequence of entry maps (each containing :coord, :value)."
  [{:keys [data metrics]}]
  (if metrics
    (->> metrics
         vals
         (mapcat (fn [{:keys [data]}]
                   (mapcat val data))))
    (mapcat val data)))

(defn- comparison-point-count
  "Count unique parameter points in a comparison.

  For implementation comparison (axis=:impl, coords have :n), counts unique
  non-axis coords (e.g., {:n 10}, {:n 100}).

  For parameter comparison (axis=:n, coords only have :n), counts unique
  axis values since non-axis would be empty.

  Returns {:count N :has-non-axis-key? bool :single-non-axis-key? bool}"
  [{:keys [axis] :as comparison}]
  (let [all-coords (into #{} (map :coord) (comparison-all-entries comparison))
        non-axis-coords (into #{} (map #(dissoc % axis)) all-coords)
        axis-values (into #{} (map #(get % axis)) all-coords)
        first-non-axis (first non-axis-coords)
        has-non-axis-key? (and (map? first-non-axis) (seq first-non-axis))
        single-non-axis-key? (and has-non-axis-key?
                                  (= 1 (count first-non-axis)))]
    {:count (if has-non-axis-key?
              (count non-axis-coords)
              (count axis-values))
     :has-non-axis-key? has-non-axis-key?
     :single-non-axis-key? single-non-axis-key?}))

(defn single-point-multi-impl-comparison?
  "Return true when comparison has multiple implementations at a single parameter point.

  Bar chart scenario: comparing implementations without varying parameters.
  - axis = :impl with no other params → true (bar chart)
  - axis = :impl with same n value across all → true (bar chart)
  - axis = :n with single n value → true (bar chart)
  - axis = :n with multiple n values → false (use line chart instead)"
  [comparison]
  (let [{:keys [implementations axis]} comparison
        multi-impl? (and implementations (> (count implementations) 1))]
    (when multi-impl?
      (let [{:keys [count has-non-axis-key?]} (comparison-point-count comparison)]
        (if (= axis :impl)
          ;; For axis = :impl, single point means no non-axis variation
          (or (not has-non-axis-key?) (= 1 count))
          ;; For axis != :impl, single point means only one axis value
          (= 1 count))))))

(defn single-axis-multi-point-comparison?
  "Return true when comparison has multiple implementations across multiple parameter points.

  Line chart scenario: comparing implementations with a varying parameter axis.
  - axis = :impl with different n values → true (line chart)
  - axis = :impl with no n or same n → false (use bar chart)
  - axis = :n with multiple n values → true (line chart)"
  [comparison]
  (let [{:keys [implementations axis]} comparison
        multi-impl? (and implementations (> (count implementations) 1))]
    (when multi-impl?
      (let [{:keys [count has-non-axis-key? single-non-axis-key?]}
            (comparison-point-count comparison)]
        ;; Multi-point if there are multiple parameter values
        (if (= axis :impl)
          ;; axis = :impl: need non-axis variation
          (and has-non-axis-key?
               (> (long count) 1)
               single-non-axis-key?)
          ;; axis != :impl (e.g., :n): axis itself is the varying parameter
          (> (long count) 1))))))

(defn comparison-visualization-strategy
  "Determine the visualization strategy for domain comparison data.

  Returns one of:
  - :single-point  - single parameter point, multiple implementations
                     (box plot showing median with CI and percentiles)
  - :multi-point   - multiple parameter points, single axis (line chart)
  - :default-table - regular table format (no chart)"
  [comparison]
  (cond
    (single-point-multi-impl-comparison? comparison) :single-point
    (single-axis-multi-point-comparison? comparison) :multi-point
    :else :default-table))
