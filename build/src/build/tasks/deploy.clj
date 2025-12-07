(ns build.tasks.deploy
  (:require
   [babashka.fs :as fs]
   [build.common :as common]
   [build.verbose :as verbose]
   [clojure.tools.build.api :as b]
   [clojure.tools.deps.util.dir :refer [with-dir]]
   [deps-deploy.deps-deploy :as dd]
   [taoensso.truss :refer [have]]))

(defn deploy
  [{:keys [basis jar-file pom-file project] :as params}]
  (let [project-root (common/ensure-project-root "jar" project)
        aliases      (with-dir
                       (fs/file project-root)
                       (common/get-project-aliases))
        group-id     (:group-id params)
        artifact-id  (:name params)
        lib          (symbol (name group-id) (name artifact-id))
        basis        (b/create-basis)
        opts         (merge
                      params
                      {:basis      basis
                       :lib        lib
                       :classifier nil

                       :installer :remote
                       :artifact  jar-file
                       :pom-file  (str (have pom-file))})]
    (verbose/println
     opts
     "Deploy jar"
     (:jar-file opts) "as" (:lib opts) (:version opts))
    (dd/deploy opts)))
