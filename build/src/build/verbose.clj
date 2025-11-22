(ns build.verbose
  (:refer-clojure :exclude [println]))

(defn println
  [{:keys [verbose]} & args]
  (when verbose
    (apply clojure.core/println args)))
