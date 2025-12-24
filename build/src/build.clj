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

(defn ^{:params []} install-local
  "Build and install JAR with only current platform's agent.
  Builds agent from agent-cpp/ source and installs to local maven repo.
  For development use - does not require all platform binaries."
  [params]
  ((requiring-resolve 'build.agent/build-and-copy-agent!))
  (tasks/install (assoc params :project :criterium)))

