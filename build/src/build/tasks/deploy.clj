(ns build.tasks.deploy
  (:require
   [build.verbose :as verbose]
   [clojure.tools.build.api :as b]
   [deps-deploy.deps-deploy :as dd]
   [taoensso.truss :refer [have]]))

(defn deploy
  [{:keys [jar-file pom-file name] :as params}]
  (let [lib (symbol name)
        basis (b/create-basis)
        opts (merge
              params
              {:basis basis
               :lib lib
               :classifier nil
               :installer :remote
               :artifact jar-file
               :pom-file (str (have pom-file))})]
    (verbose/println
     opts
     "Deploy jar"
     (:jar-file opts) "as" (:lib opts) (:version opts))
    (dd/deploy opts)))
