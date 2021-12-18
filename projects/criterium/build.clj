(ns build
  (:require
   [makejack.tasks :as tasks]))

(tasks/require
 help
 clean
 install
 changelog-release
 tag-version
 write-version-file)

(defn ^{:params []} jar
  "Build jarfile"
  [params]
  (-> params
      tasks/jar
      write-version-file))

(defn ^{:params []} build
  "Build projects"
  [params]
  (-> params
      clean
      jar
      install))
