(ns build
  (:require
   [build.tasks :as tasks]))

(defn ^{:params []} clean
  "Clean built files."
  [params]
  (tasks/clean (assoc params :project :arg-gen)))

(defn ^{:params []} jar
  "Build jarfile"
  [params]
  (tasks/jar (assoc params :project :arg-gen)))

(defn ^{:params []} install
  "Install jar."
  [params]
  (tasks/install (assoc params :project :arg-gen)))

(defn ^{:params []} deploy
  "Deploy jar."
  [params]
  (tasks/deploy (assoc params :project :arg-gen)))

(defn ^{:params []} build
  "Build arg-gen project"
  [params]
  (-> params
      clean
      jar
      install))
