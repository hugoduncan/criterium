(ns criterium.viewer.print.modal
  "Modal analysis views for print viewer.

  Contains multimodal distribution warning display."
  (:require
   [clojure.string :as str]
   [criterium.view :as view]
   [criterium.viewer.common.modal :as modal]))

(defmethod view/multimodal-warning* :print
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config modes transforms]}]
     (println
      (format "%32s: Multimodal distribution detected"
              (:label metric-config)))
     (when (seq modes)
       (let [locations (map #(modal/format-mode-location
                              (:location %)
                              metric-config
                              transforms)
                            modes)]
         (println (format "%32s  Mode locations: %s" ""
                          (str/join ", " locations))))))))
