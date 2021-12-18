(ns criterium.util.debug)

(def _debug (volatile! nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn debug!
  ([]
   (debug! true))
  ([flag]
   (vreset! _debug flag)))

(defn dtap> [x]
  (when @_debug
    (tap> x)))
