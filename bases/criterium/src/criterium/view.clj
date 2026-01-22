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

(def-multi-view kde)
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
(def-multi-view shape-stats)
(def-multi-view extremes)

;;; Distribution Fit Views

(def-multi-view distribution-models)
(def-multi-view distribution-parameter-cis)
(def-multi-view distribution-pdf)
(def-multi-view distribution-cdf)
(def-multi-view distribution-qq)

;;; Tail Analysis Views

(def-multi-view tail-summary)
(def-multi-view tail-ratios)
(def-multi-view tail-high-quantiles)
(def-multi-view tail-ratios-chart)
(def-multi-view hill-plot)
(def-multi-view mrl-plot)
(def-multi-view zipf-plot)
(def-multi-view exponential-qq-plot)
(def-multi-view gpd-qq-plot)

;;; Modal Analysis Views

(def-multi-view multimodal-warning)

;;; Autocorrelation Views

(def-multi-view autocorrelation)
(def-multi-view acf-plot)

;;; Allocation Views

(def-multi-view allocation-summary)
(def-multi-view allocation-hotspots)
(def-multi-view allocation-by-type)

(def-multi-view allocation-treemap)

;;; Call Tracing Views

(def-multi-view call-tree)
(def-multi-view call-flame)
(def-multi-view most-called)

;;; Domain Views

(def-multi-view domain-extract-table)
(def-multi-view domain-extract-chart)
(def-multi-view domain-grouped)
(def-multi-view domain-comparison-table)
(def-multi-view domain-comparison-chart)
(def-multi-view domain-regression)
(def-multi-view domain-apply)

(defmulti flush-viewer (fn [viewer] viewer))
(defmethod flush-viewer :default [_])

;; Null Viewer

(defmethod bootstrap-stats* :none [_ _ _])
(defmethod event-stats* :none [_ _ _])
(defmethod final-gc-warnings* :none [_ _ _])
(defmethod histogram* :none [_ _ _])

(defmethod kde* :none [_ _ _])
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
(defmethod shape-stats* :none [_ _ _])
(defmethod extremes* :none [_ _ _])

;; Distribution Fit Null Viewer
(defmethod distribution-models* :none [_ _ _])
(defmethod distribution-parameter-cis* :none [_ _ _])
(defmethod distribution-pdf* :none [_ _ _])
(defmethod distribution-cdf* :none [_ _ _])
(defmethod distribution-qq* :none [_ _ _])

;; Tail Analysis Null Viewer
(defmethod tail-summary* :none [_ _ _])
(defmethod tail-ratios* :none [_ _ _])
(defmethod tail-high-quantiles* :none [_ _ _])
(defmethod tail-ratios-chart* :none [_ _ _])
(defmethod hill-plot* :none [_ _ _])
(defmethod mrl-plot* :none [_ _ _])
(defmethod zipf-plot* :none [_ _ _])
(defmethod exponential-qq-plot* :none [_ _ _])
(defmethod gpd-qq-plot* :none [_ _ _])

;; Modal Analysis Null Viewer
(defmethod multimodal-warning* :none [_ _ _])

;; Autocorrelation Null Viewer
(defmethod autocorrelation* :none [_ _ _])
(defmethod acf-plot* :none [_ _ _])

;; Allocation Null Viewer
(defmethod allocation-summary* :none [_ _ _])
(defmethod allocation-hotspots* :none [_ _ _])
(defmethod allocation-by-type* :none [_ _ _])

(defmethod allocation-treemap* :none [_ _ _])

;; Call Tracing Null Viewer
(defmethod call-tree* :none [_ _ _])
(defmethod call-flame* :none [_ _ _])
(defmethod most-called* :none [_ _ _])

;; Domain Null Viewer
(defmethod domain-extract-table* :none [_ _ _])
(defmethod domain-extract-chart* :none [_ _ _])
(defmethod domain-grouped* :none [_ _ _])
(defmethod domain-comparison-table* :none [_ _ _])
(defmethod domain-comparison-chart* :none [_ _ _])
(defmethod domain-regression* :none [_ _ _])
(defmethod domain-apply* :none [_ _ _])
