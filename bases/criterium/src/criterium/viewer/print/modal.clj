(ns criterium.viewer.print.modal
  "Modal analysis views for print viewer.

  Contains multimodal distribution warning display."
  (:require
   [clojure.string :as str]
   [criterium.view :as view]
   [criterium.viewer.common.modal :as modal]
   [criterium.viewer.print.core :as print-core]))

(defmethod view/multimodal-warning* :print
  [_ {:keys [modes-id]} data-map]
  (modal/for-each-multimodal-metric
   data-map modes-id
   (fn [{:keys [metric-config modes transforms]}]
     (println
      (format "%s Multimodal distribution detected"
              (print-core/format-sublabel (:label metric-config))))
     (when (seq modes)
       (let [locations (map #(modal/format-mode-location
                              (:location %)
                              metric-config
                              transforms)
                            modes)]
         (println (format "%s  Mode locations: %s" (print-core/sublabel-str "")
                          (str/join ", " locations))))))))
