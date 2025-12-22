(ns criterium.view
  (:require
   [criterium.util.debug :as debug]
   [criterium.util.invariant :refer [have]]))

(defn def-multi-view* [n]
  (let [mm-name (symbol (str (name n) "*"))]
    `(do
       (ns-unmap *ns* '~mm-name)
       (defmulti ~mm-name
         (fn [viewer# options# data-map#]
           (have keyword? viewer#)))
       (defn ~n
         ([] (~n {}))
         ([options#]
          (fn ~n [viewer# data-map#]
            (debug/dtap> {:view '~n})
            (~mm-name viewer# options# data-map#)))))))

(defmacro def-multi-view [n]
  (def-multi-view* n))

(def-multi-view bootstrap-stats)
(def-multi-view event-stats)
(def-multi-view final-gc-warnings)
(def-multi-view histogram)
(def-multi-view metrics)
(def-multi-view os)
(def-multi-view outlier-counts)
(def-multi-view outlier-significance)
(def-multi-view quantiles)
(def-multi-view runtime)
(def-multi-view sample-percentiles)
(def-multi-view sample-diffs)
(def-multi-view collect-plan)
(def-multi-view samples)
(def-multi-view stats)

;;; Allocation Views

(def-multi-view allocation-summary)
(def-multi-view allocation-hotspots)
(def-multi-view allocation-by-type)

(def-multi-view allocation-treemap)

;;; Domain Views

(def-multi-view domain-extract)
(def-multi-view domain-grouped)
(def-multi-view domain-comparison)
(def-multi-view domain-regression)

(defmulti flush-viewer (fn [viewer] viewer))
(defmethod flush-viewer :default [_])

;; Null Viewer

(defmethod bootstrap-stats* :none [_ _ _])
(defmethod event-stats* :none [_ _ _])
(defmethod final-gc-warnings* :none [_ _ _])
(defmethod histogram* :none [_ _ _])
(defmethod metrics* :none [_ _ _])
(defmethod os* :none [_ _ _])
(defmethod outlier-counts* :none [_ _ _])
(defmethod outlier-significance* :none [_ _ _])
(defmethod quantiles* :none [_ _ _])
(defmethod runtime* :none [_ _ _])
(defmethod sample-percentiles* :none [_ _ _])
(defmethod sample-diffs* :none [_ _ _])
(defmethod collect-plan* :none [_ _ _])
(defmethod samples* :none [_ _ _])
(defmethod stats* :none [_ _ _])

;; Allocation Null Viewer
(defmethod allocation-summary* :none [_ _ _])
(defmethod allocation-hotspots* :none [_ _ _])
(defmethod allocation-by-type* :none [_ _ _])

(defmethod allocation-treemap* :none [_ _ _])

;; Domain Null Viewer
(defmethod domain-extract* :none [_ _ _])
(defmethod domain-grouped* :none [_ _ _])
(defmethod domain-comparison* :none [_ _ _])
(defmethod domain-regression* :none [_ _ _])
