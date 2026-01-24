(ns criterium.viewer.portal.modal
  "Modal analysis views for portal viewer.

  Contains multimodal distribution warning display."
  (:require
   [criterium.view :as view]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.portal.core :as portal.core]))

(defmethod view/multimodal-warning* :portal
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (portal.core/heading (str "WARNING: Multimodal distribution - "
                               (:label metric-config)))
     (portal.core/portal-table
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (portal.core/portal-heading [:em "Mode locations:"])
       (portal.core/portal-table
        (mapv (fn [{:keys [location density]}]
                {:location (modal/format-mode-location
                            location metric-config transforms)
                 :density (format "%.4g" density)})
              modes))))))
