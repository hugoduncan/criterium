(ns build.check-warnings
  "Namespace discovery and warning check utilities.

   Discovers project namespaces using tools.deps and provides
   utilities for checking boxed math and reflection warnings."
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
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

;;; Noisy Namespace Pre-loading

(def noisy-namespaces
  "Third-party namespaces that emit reflection or boxed math warnings.

   These must be loaded before enabling warnings to avoid false positives.
   Loaded from build/noisy-namespaces.edn resource."
  (-> (io/resource "build/noisy-namespaces.edn")
      slurp
      edn/read-string))

;;; Exemption Detection

(def ^:private exemption-patterns
  "Regex patterns that indicate a namespace explicitly disables warnings."
  [#"\(\s*set!\s+\*warn-on-reflection\*\s+false\s*\)"
   #"\(\s*set!\s+\*unchecked-math\*\s+false\s*\)"])

(defn- ns-symbol->file-path
  "Convert a namespace symbol to a relative file path."
  [ns-sym]
  (-> (str ns-sym)
      (str/replace "." "/")
      (str/replace "-" "_")
      (str ".clj")))

(defn- find-namespace-source-file
  "Find the source file for a namespace symbol.

   Searches the classpath roots from tools.deps for a matching file.
   Returns the absolute path as a string, or nil if not found."
  [ns-sym project-root]
  (let [basis (deps/create-basis {:aliases [:dev :test]
                                  :dir project-root})
        classpath-roots (:classpath-roots basis)
        rel-path (ns-symbol->file-path ns-sym)
        ;; Resolve classpath roots relative to project root
        resolve-root (fn [root]
                       (let [root-path (fs/path project-root root)]
                         (when (fs/directory? root-path)
                           root-path)))]
    (some->> classpath-roots
             (keep resolve-root)
             (filter project-source-path?)
             (map #(fs/path % rel-path))
             (filter fs/exists?)
             first
             str)))

(defn namespace-has-exemption?
  "Check if a namespace source file contains an exemption declaration.

   Reads the source file and checks for patterns like:
   - (set! *warn-on-reflection* false)
   - (set! *unchecked-math* false)

   Returns true if any exemption pattern is found, false otherwise.
   Returns false if the source file cannot be found or read."
  ([ns-sym]
   (namespace-has-exemption? ns-sym "."))
  ([ns-sym project-root]
   (if-let [source-file (find-namespace-source-file ns-sym project-root)]
     (try
       (let [content (slurp source-file)]
         (boolean (some #(re-find % content) exemption-patterns)))
       (catch Exception _
         false))
     false)))

(defn preload-noisy-namespaces!
  "Pre-load third-party namespaces that emit warnings with warnings disabled.

   Loads each namespace in `noisy-namespaces` with *warn-on-reflection* and
   *unchecked-math* set to false. Missing namespaces are silently ignored
   since not all dependencies may be on the classpath in all configurations.

   Returns a map with :loaded and :missing vectors of namespace symbols."
  []
  (let [results (atom {:loaded [] :missing []})]
    (binding [*warn-on-reflection* false
              *unchecked-math* false]
      (doseq [ns-sym noisy-namespaces]
        (try
          (require ns-sym)
          (swap! results update :loaded conj ns-sym)
          (catch Exception _
            (swap! results update :missing conj ns-sym)))))
    @results))

;;; Warning Capture and Compilation

(defn- parse-warnings
  "Parse warning messages from compiler output.

   Returns a vector of maps with :type (:reflection or :boxed-math) and :message."
  [output]
  (let [lines (str/split-lines output)]
    (->> lines
         (keep (fn [line]
                 (cond
                   (str/includes? line "Reflection warning")
                   {:type :reflection :message line}

                   (str/includes? line "Boxed math warning")
                   {:type :boxed-math :message line}

                   :else nil)))
         vec)))

(defn check-namespace-warnings
  "Check a single namespace for reflection and boxed math warnings.

   Compiles the namespace with warnings enabled and captures any emitted warnings.
   Returns a map with:
   - :namespace - the namespace symbol
   - :warnings - vector of {:type :message} maps
   - :error - exception message if compilation failed (optional)"
  [ns-sym]
  (let [err-writer (java.io.StringWriter.)
        result (binding [*warn-on-reflection* true
                         *unchecked-math* :warn-on-boxed
                         *err* (java.io.PrintWriter. err-writer)]
                 (try
                   (require ns-sym :reload)
                   {:namespace ns-sym}
                   (catch Exception e
                     {:namespace ns-sym
                      :error (.getMessage e)})))
        err-output (str err-writer)
        warnings (parse-warnings err-output)]
    (if (seq warnings)
      (assoc result :warnings warnings)
      result)))

(defn check-all-namespaces
  "Check all project namespaces for reflection and boxed math warnings.

   Process:
   1. Pre-loads third-party namespaces with warnings disabled
   2. Discovers project namespaces
   3. Filters out namespaces with explicit exemptions
   4. Checks each namespace for warnings

   Returns a map with:
   - :preload-result - result from preload-noisy-namespaces!
   - :total-namespaces - count of discovered namespaces
   - :exempt-namespaces - namespaces skipped due to exemptions
   - :checked-namespaces - count of namespaces checked
   - :results - vector of check results (only those with warnings or errors)
   - :warning-count - total number of warnings found"
  ([]
   (check-all-namespaces "."))
  ([project-root]
   (let [preload-result (preload-noisy-namespaces!)
         all-namespaces (discover-project-namespaces project-root)
         exempt-nses (filterv #(namespace-has-exemption? % project-root) all-namespaces)
         exempt-set (set exempt-nses)
         namespaces-to-check (remove exempt-set all-namespaces)
         results (mapv check-namespace-warnings namespaces-to-check)
         with-issues (filterv #(or (:warnings %) (:error %)) results)
         warning-count (->> results
                            (mapcat :warnings)
                            count)]
     {:preload-result preload-result
      :total-namespaces (count all-namespaces)
      :exempt-namespaces exempt-nses
      :checked-namespaces (count namespaces-to-check)
      :results with-issues
      :warning-count warning-count})))

;;; Reporting

(defn report-warnings
  "Print human-readable report of warnings.

   Groups warnings by namespace and prints in format:
   namespace.name: N warnings
     - Reflection warning: ...
     - Boxed math warning: ...

   Prints summary at end: Found N warnings in M namespaces"
  [check-result]
  (let [{:keys [preload-result total-namespaces exempt-namespaces
                checked-namespaces results warning-count]} check-result
        namespaces-with-warnings (filter :warnings results)
        namespaces-with-errors (filter :error results)]
    ;; Header
    (println "=== Boxed Math and Reflection Warning Check ===")
    (println)
    ;; Preload summary
    (println "Pre-loaded" (count (:loaded preload-result)) "third-party namespaces")
    (when (seq (:missing preload-result))
      (println "  (skipped" (count (:missing preload-result)) "missing namespaces)"))
    (println)
    ;; Namespace counts
    (println "Total project namespaces:" total-namespaces)
    (when (seq exempt-namespaces)
      (println "Exempt namespaces:" (count exempt-namespaces))
      (doseq [ns-sym exempt-namespaces]
        (println "  -" ns-sym)))
    (println "Checked namespaces:" checked-namespaces)
    (println)
    ;; Errors
    (when (seq namespaces-with-errors)
      (println "=== Compilation Errors ===")
      (doseq [{:keys [namespace error]} namespaces-with-errors]
        (println (str namespace ": " error)))
      (println))
    ;; Warnings
    (if (seq namespaces-with-warnings)
      (do
        (println "=== Warnings ===")
        (doseq [{:keys [namespace warnings]} namespaces-with-warnings]
          (println (str namespace ": " (count warnings) " warning(s)"))
          (doseq [{:keys [message]} warnings]
            (println "  -" message)))
        (println)
        (println "Found" warning-count "warning(s) in"
                 (count namespaces-with-warnings) "namespace(s)"))
      (println "No warnings found."))
    (println)))

(defn -main
  "Run warning check and exit with appropriate code.

   Exits with 0 if no warnings found, 1 otherwise."
  [& _args]
  (let [result (check-all-namespaces)
        {:keys [warning-count results]} result
        error-count (count (filter :error results))]
    (report-warnings result)
    (System/exit (if (or (pos? warning-count) (pos? error-count)) 1 0))))
