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
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view histogram)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view metrics)
(def-multi-view os)
(def-multi-view outlier-counts)
(def-multi-view outlier-significance)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view quantiles)
(def-multi-view runtime)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view sample-percentiles)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view sample-diffs)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view collect-plan)
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def-multi-view samples)
(def-multi-view stats)

(defmulti flush-viewer (fn [viewer] viewer))
(defmethod flush-viewer :default  [_])

;; Null Viewer

(defmethod bootstrap-stats* :none [_ _ _])
(defmethod event-stats* :none [_ _ _])
(defmethod final-gc-warnings* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod histogram* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod metrics* :none [_ _ _])
(defmethod os* :none [_ _ _])
(defmethod outlier-counts* :none [_ _ _])
(defmethod outlier-significance* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod quantiles* :none [_ _ _])
(defmethod runtime* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod sample-percentiles* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod sample-diffs* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod collect-plan* :none [_ _ _])
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmethod samples* :none [_ _ _])
(defmethod stats* :none [_ _ _])
