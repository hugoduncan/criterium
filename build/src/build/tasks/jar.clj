(ns build.tasks.jar
    (:require
     [babashka.fs :as fs]
     [build.common :as common]
     [build.tasks.pom :as pom]
     [build.verbose :as verbose]
     [clojure.tools.build.api :as b]
     [clojure.tools.build.util.file :as file]
     [clojure.tools.deps :as t]
     [clojure.tools.deps.util.dir :refer [with-dir]]
     [taoensso.truss :as truss :refer [have]]))

(defn- explode
       [lib-path {:keys [class-dir]}]
       (when (and (fs/exists? lib-path) (fs/directory? lib-path))
             (file/copy-contents (fs/file lib-path) (fs/file class-dir))))

(defn- add-local-roots [context basis]
       (let [{:keys [libs]} basis]
            (doseq [[_lib coord] libs]
                   (doseq [path (:paths coord)]
                          (explode path context)))))

(defn jar
      "Builds a jar for the specified project.

   Options:
   * :project - required, the name of the project to build,
   * :jar-file - optional, the path of the JAR file to build,
     relative to the project folder; can also be specified in
     the :uberjar alias in the project's deps.edn file; will
     default to target/PROJECT.jar if not specified.
   * :manifest - optional, map of custom JAR manifest attributes.

   Returns:
   * the input opts with :class-dir, :compile-opts, :main, and :jar-file
     computed."
      [{:keys [basis class-dir jar-file project project-root name version] :as opts}]
      (let [aliases (with-dir
                     (fs/file (have project-root))
                     (common/get-project-aliases))]
           (b/with-project-root (fs/file project-root)
                                (let [basis (or basis (b/create-basis))
                                      jar-file (or jar-file
                                                   (-> aliases :jar :jar-file)
                                                   (fs/path "target" (str (clojure.core/name project) ".jar")))

                                      opts (merge opts
                                                  {:basis basis
                                                   :class-dir (str class-dir)
                                                   :jar-file (str (fs/path project-root jar-file))})]
                                     (verbose/println "Building jar" (str jar-file "..."))
                                     (add-local-roots opts basis)

                                     (pom/write-pom opts)

                                     (b/jar opts)
                                     (verbose/println "jar is built.")
                                     opts))))
