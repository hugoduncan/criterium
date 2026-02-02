(ns criterium.viewer.print.allocation
  "Print viewer functions for allocation profiling display.

  Provides text output for:
  - allocation summary (totals, counts, freed ratio)
  - allocation hotspots (call sites with highest allocations)
  - allocations by type (aggregated by object type)
  - allocation treemap (ASCII tree visualization)"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common.allocation :as allocation]))

(set! *unchecked-math* false)

;;; Allocation Summary

(defn print-allocation-summary
  "Print allocation summary statistics.

  Displays total allocated/freed bytes, retained memory,
  allocation/freed counts, and freed ratio percentage."
  [{:keys [total-allocated total-freed num-allocations num-freed
           freed-ratio]}]
  (let [total-allocated (long total-allocated)
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
                     (* 100.0 (double freed-ratio))))))

(defmethod view/allocation-summary* :print
  [_ {:keys [summary-id]} data-map]
  (let [summary-id (or summary-id :allocation-summary)
        summary (data-map summary-id)]
    (when summary
      (print-allocation-summary summary))))

;;; Allocation Hotspots

(defn print-allocation-hotspots
  "Print allocation hotspots table.

  Each row shows count, bytes, freed stats, object type, and call site.
  Object types longer than 30 chars are truncated from the start."
  [hotspots]
  (let [type-col-width 30
        truncate-type (fn [s]
                        (if (and s (> (count s) type-col-width))
                          (str "…" (subs s (- (count s) (- type-col-width 1))))
                          (or s "")))]
    (when (seq hotspots)
      (println)
      (println "Allocation Hotspots:")
      (println (format
                "%8s %12s %8s %12s  %-30s  %s"
                "Count"
                "Bytes"
                "Freed"
                "Freed Bytes"
                "Object Type"
                "Call Site"))
      (println (apply str (repeat 110 "-")))
      (doseq [{:keys [call-site
                      object-type
                      count
                      bytes
                      freed-count
                      freed-bytes]}
              hotspots]
        (println (format "%8d %12d %8d %12d  %-30s  %s"
                         count
                         bytes
                         freed-count
                         freed-bytes
                         (truncate-type object-type)
                         (allocation/format-call-site call-site nil)))))))

(defmethod view/allocation-hotspots* :print
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (print-allocation-hotspots (:hotspots hotspots-map)))))

;;; Allocations by Type

(defn print-allocation-by-type
  "Print allocations grouped by object type.

  Sorted by bytes descending. Shows count, bytes, freed stats, and type name."
  [by-type]
  (let [sorted (sort-by (comp :bytes second) > by-type)]
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
                         type-name))))))

(defmethod view/allocation-by-type* :print
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (print-allocation-by-type (:by-type by-type-map)))))

;;; Allocation Treemap

(defmethod view/allocation-treemap* :print
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (println)
      (println (allocation/render-ascii-treemap treemap-data)))))
