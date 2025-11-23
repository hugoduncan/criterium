(ns build.tasks.javac
    "Java source compilation using tools.build."
    (:require
     [babashka.fs :as fs]
     [build.verbose :as verbose]
     [clojure.tools.build.api :as b]))

(defn javac
      "Compile Java source files to class files.

  Options:
  * :project - optional, project identifier (:agent, :criterium)
  * :class-dir - required, directory to write compiled classes
  * :src-dirs - optional, collection of Java source directories;
    defaults to bases/<project>/src/java if :project is specified
  * :basis - optional, classpath basis; will create if not provided
  * :javac-opts - optional, Java compiler options;
    defaults to [\"-source\" \"11\" \"-target\" \"11\"]

  Returns:
  * the input opts with :src-dirs, :basis, and :javac-opts populated."
      [{:keys [project class-dir src-dirs basis javac-opts] :as opts}]
      (let [src-dirs (or src-dirs
                         (when project
                               [(str (fs/path "bases" (name project) "src" "java"))]))
            basis (or basis (b/create-basis))
            javac-opts (or javac-opts ["-source" "11" "-target" "11"])
            opts (assoc opts
                        :src-dirs src-dirs
                        :basis basis
                        :javac-opts javac-opts)
            existing-src-dirs (filter #(fs/exists? %) src-dirs)]
           (when (seq existing-src-dirs)
                 (verbose/println "Compiling Java sources from" existing-src-dirs "to" class-dir)
                 (b/javac (assoc opts :src-dirs existing-src-dirs))
                 (verbose/println "Java compilation complete."))
           opts))
