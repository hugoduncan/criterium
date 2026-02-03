(ns criterium.viewer.portal.allocation
  "Portal viewer functions for allocation profiling display.

  Provides Portal output for:
  - allocation summary (totals, counts, freed ratio)
  - allocation hotspots (call sites with highest allocations)
  - allocations by type (aggregated by object type)
  - allocation treemap (Vega treemap visualization)"
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.portal.core :as portal.core]))

;;; Allocation Summary

(defmethod view/allocation-summary* :portal
  [_ {:keys [summary-id]} data-map]
  (let [summary-id (or summary-id :allocation-summary)
        summary (data-map summary-id)]
    (when summary
      (let [{:keys [total-allocated total-freed num-allocations num-freed
                    freed-ratio]}
            summary
            total-allocated (long total-allocated)
            total-freed (long total-freed)
            retained (- total-allocated total-freed)]
        (portal.core/heading "Allocation Summary")
        (portal.core/portal-table
         [{:metric "Total allocated" :value total-allocated}
          {:metric "Total freed" :value total-freed}
          {:metric "Retained" :value retained}
          {:metric "Allocation count" :value num-allocations}
          {:metric "Freed count" :value num-freed}
          {:metric "Freed ratio"
           :value (format "%.1f%%" (* 100.0 (double freed-ratio)))}])))))

;;; Allocation Hotspots

(defmethod view/allocation-hotspots* :portal
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)]
        (when (seq hotspots)
          (portal.core/heading "Allocation Hotspots")
          (portal.core/portal-table
           (mapv
            (fn [{:keys [call-site
                         object-type
                         count
                         bytes
                         freed-count
                         freed-bytes]}]
              {:call-site (allocation/format-call-site call-site nil)
               :object-type (or object-type "")
               :count count
               :bytes bytes
               :freed-count freed-count
               :freed-bytes freed-bytes})
            hotspots)))))))

;;; Allocations by Type

(defmethod view/allocation-by-type* :portal
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (portal.core/heading "Allocations by Type")
          (portal.core/portal-table
           (mapv (fn [[type-name {:keys [count bytes freed-count freed-bytes]}]]
                   {:type type-name
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 sorted)))))))

;;; Allocation Treemap

(defmethod view/allocation-treemap* :portal
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (portal.core/heading "Allocation Treemap")
      (portal.core/portal-vega
       (charts.profile/treemap-vega-spec treemap-data {})))))
