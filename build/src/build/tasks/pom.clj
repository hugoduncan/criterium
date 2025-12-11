(ns build.tasks.pom
  (:require
   [babashka.fs :as fs]
   [build.verbose :as verbose]
   [clojure.string :as str]
   [clojure.tools.build.api :as b]
   [taoensso.truss :as truss :refer [have]]))

(def ^:private project-metadata
  "Project-specific metadata for POM generation"
  {:criterium
   {:description "Benchmarking library for Clojure with statistical rigor"
    :url "https://github.com/hugoduncan/criterium"}
   :agent
   {:description "JVM agent for allocation tracking in Criterium benchmarks"
    :url "https://github.com/hugoduncan/criterium"}
   :arg-gen
   {:description "Argument generation for Criterium benchmarks using test.check"
    :url "https://github.com/hugoduncan/criterium"}})

(defn- build-pom-data
  "Build hiccup-style POM metadata including licenses, SCM, and developers.
   
   Returns a vector of POM elements suitable for tools.build's :pom-data parameter."
  [project]
  (let [{:keys [description url]} (get project-metadata project)]
    [[:description description]
     [:url url]
     [:licenses
      [:license
       [:name "Eclipse Public License"]
       [:url "http://www.eclipse.org/legal/epl-v10.html"]]]
     [:scm
      [:url "https://github.com/hugoduncan/criterium"]
      [:connection "scm:git:git://github.com/hugoduncan/criterium.git"]
      [:developerConnection "scm:git:ssh://git@github.com/hugoduncan/criterium.git"]]
     [:developers
      [:developer
       [:name "Hugo Duncan"]]]]))

(defn- augment-basis-with-pom-deps
  "Add pom-deps to basis, replacing local/root deps with Maven coordinates.

  This ensures the generated POM declares proper Maven dependencies instead
  of local path references. Both :deps and :libs are modified since write-pom
  uses :deps to determine POM dependencies."
  [basis pom-deps]
  (if (seq pom-deps)
    (let [;; Remove local/root deps from :deps
          non-local-deps (into {}
                               (remove (fn [[_lib coord]]
                                         (:local/root coord)))
                               (:deps basis))
          ;; Add pom-deps to :deps
          augmented-deps (merge non-local-deps pom-deps)
          ;; Also update :libs for consistency
          non-local-libs (into {}
                               (remove (fn [[_lib coord]]
                                         (:local/root coord)))
                               (:libs basis))
          augmented-libs (merge non-local-libs pom-deps)]
      (assoc basis
             :deps augmented-deps
             :libs augmented-libs))
    basis))

(defn write-pom
  "Generate POM file with full Maven Central/Clojars metadata.

  Options:
  * :project - required, the name of the project (:criterium or :agent)
  * :basis - optional, the basis to use for dependency resolution
  * :class-dir - required, directory where POM will be written
  * :name - the library name (e.g., \"criterium/criterium\")
  * :version - the version string
  * :pom-deps - optional, Maven dependencies to include in the POM
    (replacing local/root dependencies)

  Returns the input opts with :pom-file path added."
  [{:keys [basis class-dir name pom-deps project version] :as opts}]
  (have project)
  (have class-dir)
  (have name)
  (have version)

  (let [basis (-> (or basis (b/create-basis))
                  (augment-basis-with-pom-deps pom-deps))
        [group-id artifact-id] (str/split (clojure.core/name name) #"/")
        pom-data (build-pom-data project)
        pom-dir (fs/path class-dir "META-INF" "maven" group-id artifact-id)
        pom-file (fs/path pom-dir "pom.xml")]

    (verbose/println "Writing POM to" (str pom-file))

    (b/write-pom
     {:basis basis
      :class-dir (str class-dir)
      :lib (symbol name)
      :version version
      :src-pom :none
      :pom-data pom-data})

    (verbose/println "POM written successfully")

    (assoc opts :pom-file (str pom-file))))

(defn pom
  "Wrapper for write-pom that integrates with the build pipeline.

  This function is called by the install and deploy tasks."
  [opts]
  (write-pom opts))
