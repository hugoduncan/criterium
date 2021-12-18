(ns build
  (:refer-clojure :exclude [test])
  (:require
   [babashka.process :as process]
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [clojure.tools.build.api :as b]
   [makejack.tasks :as tasks]))


(tasks/require
 ns-tree
 help)

(defn ^{:params []}  build
  "Build projects"
  [params]
  (tasks/poly-tool
   (merge params {:aliases        [:build]
                  :exec-fn        'build
                  :exec-args      {}
                  :no-propagation true
                  :elements       [:projects]})))

(defn ^{:params []} clean
  "Clean projects"
  [params]
  (tasks/poly-tool
   (merge params {:aliases        [:build]
                  :exec-fn        'clean
                  :exec-args      {}
                  :no-propagation true})))

(defn ^{:params []} cljfmt
  "Run `cljfmt check` on workspace"
  [params]
  (tasks/poly-main
   (merge
    (select-keys params [:verbose])
    {:aliases [:cljfmt]
     :args    ["check"]})))

(defn ^{:params []} cljfmt-fix
  "Run `cljfmt fix` on workspace"
  [params]
  (merge
   (select-keys params [:verbose])
   (tasks/poly-main
    {:aliases [:cljfmt]
     :args    ["fix"]})))

(defn ^{:params []} test
  "Run `poly test` on workspace.

  Note: you can run `poly test` directly."
  [_params]
  (b/process {:command-args ["poly" "test"] :out :inherit}))

(defn- java-home
  []
  (or (System/getProperty "java.home")  ; in clojure
      (edn/read-string
       (:out (shell/sh "clojure" "-M" "-e" "(System/getProperty \"java.home\")")))))

(defn- os-name
  []
  (let [n (System/getProperty "os.name")]
    (cond
      (= "Mac OS X" n)
      "darwin"

      (= "Linux" n)
      "linux"

      :else
      n)))

(defn compile-agent-cpp [args]
  (let [os-name   (os-name)
        java-home (java-home)]
    ;; temporary prn to see values on gitub
    (prn :java-home java-home
         :os-name os-name)
    (process/check
     (process/process
      ["make"]
      {:out       :inherit
       :err       :inherit
       :dir       "agent-cpp"
       :extra-env (cond-> {"JAVA_HOME" java-home}
                    os-name (assoc "OSNAME" os-name))}))))

(defn javac-agent [args]
  #_(tasks/poly {:on   ":base:agent"
                 :task "javac"})
  (process/check
   (process/process
    ["mj" "javac"]
    {:out :inherit
     :err :inherit
     :dir "bases/agent"})))
