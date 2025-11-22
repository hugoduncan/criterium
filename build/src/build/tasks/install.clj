(ns build.tasks.install
  (:require
   [build.verbose :as verbose]
   [clojure.tools.build.api :as b]))

(defn install
  [params]
  (let [opts (merge
              params
              {:classifier nil})]
    (verbose/println
     opts
     "Install jar"
     (:jar-file opts) "as" (:lib opts) (:version opts))
    (b/install opts)
    opts))
