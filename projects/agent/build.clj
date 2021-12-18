(ns build
  (:require
   [babashka.fs :as fs]
   [makejack.tasks :as tasks]))

(tasks/require
 help
 clean
 compile-clj
 install
 project-data
 read-version-file
 write-version)


(defn load-project-and-write-version
  [{:keys [dir] :or {dir "."} :as params}]
  (let [path   (fs/file dir ".." "criterium" "version.edn")
        params (-> (assoc params :path path)
                   project-data
                   read-version-file
                   write-version)]
    (println "Updating project to" (:version params))
    params))

(defn ^{:params []} jar
  "Build jarfile"
  [params]
  (-> params
      load-project-and-write-version
      (assoc :manifest {"Agent-Class" "criterium.agent"})
      tasks/jar))

(defn ^{:params []}  build
  "Build projects"
  [params]
  (-> params
      clean
      compile-clj
      jar
      install))
