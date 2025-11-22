(ns build.common
  (:require
   [babashka.fs :as fs]
   [clojure.tools.deps :as t]
   [taoensso.truss :as truss :refer [have]]))

(defn get-project-aliases []
  (let [edn-fn (juxt :root-edn :project-edn)]
    (-> (t/find-edn-maps)
        (edn-fn)
        (t/merge-edns)
        :aliases)))

(defn ensure-project-root
  "Given a task name and a project name, ensure the project
   exists and seems valid, and return the absolute path to it."
  [task project]
  (let [project-root (fs/path
                      (System/getProperty "user.dir")
                      "projects"
                      (name project))]
    (when-not (and project
                   (fs/exists? (fs/file project-root))
                   (fs/exists? (fs/file project-root "deps.edn")))
      (throw
       (ex-info
        (str task " task requires a valid :project option")
        {:project project})))
    project-root))

(defn project-root
  [params]
  (assoc params :project-root (ensure-project-root
                               (have (:task params))
                               (have (:project params)))))
