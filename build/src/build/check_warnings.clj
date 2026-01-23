(ns build.check-warnings
  "Namespace discovery and warning check utilities.

   Discovers project namespaces using tools.deps and provides
   utilities for checking boxed math and reflection warnings."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.tools.deps :as deps]))

;;; Namespace Discovery

(defn- clj-file->ns-symbol
  "Convert a Clojure file path to a namespace symbol.

   The source-root is the path prefix to strip from the file path.
   Returns a namespace symbol, or nil if the file doesn't look like a namespace."
  [source-root file]
  (let [source-root-str (str source-root)
        file-str (str file)
        rel-path (if (str/starts-with? file-str source-root-str)
                   (subs file-str (count source-root-str))
                   file-str)
        ;; Remove leading slash if present
        rel-path (if (str/starts-with? rel-path "/")
                   (subs rel-path 1)
                   rel-path)]
    (when (str/ends-with? rel-path ".clj")
      (-> rel-path
          (str/replace #"\.clj$" "")
          (str/replace "/" ".")
          (str/replace "_" "-")
          symbol))))

(defn- find-clj-files
  "Find all .clj files under a directory."
  [dir]
  (when (fs/exists? dir)
    (->> (fs/glob dir "**/*.clj")
         (map str))))

(defn- discover-namespaces-in-path
  "Discover namespaces under a source path.

   Returns a vector of namespace symbols found under the path."
  [source-root]
  (when (fs/exists? source-root)
    (->> (find-clj-files source-root)
         (keep (partial clj-file->ns-symbol source-root))
         vec)))

(defn- project-source-path?
  "Return true if the path is a project source directory (not a JAR or maven repo)."
  [path]
  (let [path-str (str path)]
    (and (not (str/ends-with? path-str ".jar"))
         (not (str/includes? path-str ".m2/repository"))
         (fs/directory? path-str))))

(defn- excluded-path?
  "Return true if the path should be excluded from warning checks.

   Excludes:
   - development/ directory
   - projects/ directory (contains test fixtures, not real code)
   - */resources directories (contain clj-kondo exports, not project code)"
  [project-root path]
  (let [path-str (str path)
        rel-path (if (str/starts-with? path-str (str project-root))
                   (subs path-str (count (str project-root)))
                   path-str)
        rel-path (if (str/starts-with? rel-path "/")
                   (subs rel-path 1)
                   rel-path)]
    (or (str/starts-with? rel-path "development")
        (str/starts-with? rel-path "projects/")
        (str/ends-with? path-str "/resources"))))

(defn discover-project-namespaces
  "Discover all project namespaces using tools.deps.

   Uses the :dev and :test aliases to build a basis and extracts
   source paths from the classpath roots.

   Returns a sorted vector of namespace symbols found in project
   source and test directories.

   Excludes:
   - development/ directory
   - JAR files
   - Maven repository paths"
  ([]
   (discover-project-namespaces "."))
  ([project-root]
   (let [basis (deps/create-basis {:aliases [:dev :test]
                                   :dir project-root})
         classpath-roots (:classpath-roots basis)
         ;; Normalize the project root path (remove trailing /.)
         project-root-abs (-> (fs/absolutize project-root)
                              fs/normalize
                              str)
         source-paths (->> classpath-roots
                           (filter project-source-path?)
                           (map #(str (fs/absolutize %)))
                           (remove #(excluded-path? project-root-abs %)))]
     (->> source-paths
          (mapcat discover-namespaces-in-path)
          (filter some?)
          distinct
          sort
          vec))))
