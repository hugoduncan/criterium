(ns criterium.viewer.kindly.allocation
  "Kindly viewer implementations for allocation profiling views.

  Provides allocation-summary, allocation-hotspots, allocation-by-type,
  and allocation-treemap views that output Kindly-annotated tables and charts."
  (:require
   [criterium.view :as view]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.common.allocation :as allocation]
   [criterium.viewer.kindly.core :as core]))

(defmethod view/allocation-summary* :kindly
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
        (core/kindly-heading "Allocation Summary")
        (core/kindly-table
         [{:metric "Total allocated"
           :value (str total-allocated " bytes")}
          {:metric "Total freed"
           :value (str total-freed " bytes")}
          {:metric "Retained"
           :value (str retained " bytes")}
          {:metric "Allocation count"
           :value num-allocations}
          {:metric "Freed count"
           :value num-freed}
          {:metric "Freed ratio"
           :value (clojure.core/format "%.1f%%" (* 100.0 (double freed-ratio)))}])))))

(defmethod view/allocation-hotspots* :kindly
  [_ {:keys [hotspots-id]} data-map]
  (let [hotspots-id (or hotspots-id :allocation-hotspots)
        hotspots-map (data-map hotspots-id)]
    (when hotspots-map
      (let [hotspots (:hotspots hotspots-map)]
        (when (seq hotspots)
          (core/kindly-heading "Allocation Hotspots")
          (core/kindly-table
           (mapv (fn [{:keys [call-site object-type count bytes freed-count freed-bytes]}]
                   {:call-site (allocation/format-call-site call-site nil)
                    :object-type (or object-type "")
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 hotspots)))))))

(defmethod view/allocation-by-type* :kindly
  [_ {:keys [by-type-id]} data-map]
  (let [by-type-id (or by-type-id :allocation-by-type)
        by-type-map (data-map by-type-id)]
    (when by-type-map
      (let [by-type (:by-type by-type-map)
            sorted (sort-by (comp :bytes second) > by-type)]
        (when (seq sorted)
          (core/kindly-heading "Allocations by Type")
          (core/kindly-table
           (mapv (fn [[type-name {:keys [count bytes freed-count freed-bytes]}]]
                   {:type type-name
                    :count count
                    :bytes bytes
                    :freed-count freed-count
                    :freed-bytes freed-bytes})
                 sorted)))))))

(defmethod view/allocation-treemap* :kindly
  [_ {:keys [treemap-id]} data-map]
  (let [treemap-id (or treemap-id :allocation-treemap)
        treemap-data (data-map treemap-id)]
    (when (and treemap-data (:root treemap-data))
      (core/kindly-heading "Allocation Treemap")
      (core/kindly-vega (charts.profile/treemap-vega-spec treemap-data {})))))
