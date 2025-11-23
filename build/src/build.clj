(ns build
    (:refer-clojure :exclude [test])
    (:require
     [build.tasks :as tasks]))

(defn ^{:params []} clean
      "Clean projects."
      [params]
      (tasks/clean params))

(defn ^{:params []} jar
      "Build jar."
      [params]
      (tasks/jar params))

(defn ^{:params []} install
      "Install jar."
      [params]
      (tasks/install params))
