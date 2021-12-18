(ns criterium.util.output)

(def ^:dynamic *report-progress*
  "Flag to control output of progress messages"
  nil)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:skip-wiki progress
  "Conditionally report progress to *out*."
  [& message]
  (when *report-progress*
    (apply println message)))

(defmacro with-progress-reporting [flag & body]
  `(binding [*report-progress* ~flag]
     ~@body))
