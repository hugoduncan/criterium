(ns criterium.view
  (:require
   [criterium.util.debug :as debug]))

(defn viewer [_options sampled]
  (:viewer sampled))

(defn def-multi-view* [n]
  (let [mm-name (symbol (str (name n) "*"))]
    `(do
       (ns-unmap *ns* '~mm-name)
       (defmulti ~mm-name viewer)
       (defn ~n
         ([] (~n {}))
         ([options#]
          (fn ~n [sampled#]
            (debug/dtap> {:view '~n})
            (~mm-name options# sampled#)))))))

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
