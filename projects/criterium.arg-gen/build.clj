(ns build
  (:require
   [babashka.fs :as fs]
   [makejack.tasks :as tasks]))

(tasks/require
 help
 clean
 install
 project-data
 read-version-file
 update-dep
 write-version)


(defn load-project-and-write-version
  [{:keys [dir] :or {dir "."} :as params}]
  (let [path   (fs/file dir ".." "criterium" "version.edn")
        params (-> (assoc params :path path)
                   project-data
                   read-version-file
                   write-version)]
    (println "Updating project to" (:version params))
    (update-dep (merge (select-keys params [:dir])
                       {:artifact-name 'criterium/criterium
                        :mvn/version   (:version params)}))
    params))

(defn ^{:params []} jar
  "Build jarfile"
  [params]
  (-> params
      load-project-and-write-version
      tasks/jar))

(defn ^{:params []}  build
  "Build projects"
  [params]
  (-> params
      clean
      jar
      install))
