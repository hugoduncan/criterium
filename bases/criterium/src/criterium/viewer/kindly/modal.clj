(ns criterium.viewer.kindly.modal
  "Modal analysis views for kindly viewer.

  Contains multimodal distribution warning display."
  (:require
   [criterium.view :as view]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.kindly.core :as kindly.core]))

(defmethod view/multimodal-warning* :kindly
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config n-modes modes transforms]}]
     (kindly.core/kindly-heading (str "WARNING: Multimodal distribution - "
                                      (:label metric-config)))
     (kindly.core/kindly-table
      [{:metric "Mode count" :value n-modes}])
     (when (seq modes)
       (kindly.core/kindly-add
        (with-meta
          ["*Mode locations:*"]
          {:kindly/kind :kind/md}))
       (kindly.core/kindly-table
        (mapv (fn [{:keys [location density]}]
                {:location (modal/format-mode-location
                            location metric-config transforms)
                 :density (format "%.4g" density)})
              modes))))))
