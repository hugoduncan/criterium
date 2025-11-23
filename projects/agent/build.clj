(ns build
    (:require
     [build.tasks :as tasks]))

(defn ^{:params []} clean
      "Clean built files."
      [params]
      (tasks/clean (assoc params :project :agent)))

(defn ^{:params []} jar
      "Build jarfile"
      [params]
      (tasks/jar (assoc params :project :agent)))

(defn ^{:params []} install
      "Install jar."
      [params]
      (tasks/install (assoc params :project :agent)))

(defn ^{:params []} deploy
      "Deploy jar."
      [params]
      (tasks/deploy (assoc params :project :agent)))

(defn ^{:params []} build
      "Build agent project"
      [params]
      (-> params
          clean
          jar
          install))
