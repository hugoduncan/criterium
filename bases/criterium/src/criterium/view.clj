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
