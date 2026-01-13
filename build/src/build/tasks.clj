(ns build.tasks
  (:require [babashka.fs :as fs]
            [build.common :as common]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn clean
  "Remove all built files."
  [params]
  ((requiring-resolve 'build.tasks.clean/clean)
   params))

(defn jar
  "Build a jar file."
  [params]
  (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
    (let [params (merge {:task :jar
                         :class-dir (str class-dir)}
                        params)
          params (common/resolve-project-data params)]
      (when-let [validate-fn (:validate-fn params)]
        ((requiring-resolve validate-fn)))
      ((requiring-resolve 'build.tasks.jar/jar) params))))

(defn install
  "Install a jar file."
  [params]
  (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
    (let [params (merge {:task :install
                         :class-dir (str class-dir)}
                        params)
          params (common/resolve-project-data params)]
      (-> params
          ((requiring-resolve 'build.tasks.pom/pom))
          ((requiring-resolve 'build.tasks.jar/jar))
          ((requiring-resolve 'build.tasks.install/install))))))

(defn deploy
  "Deploy a jar file."
  [params]
  (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
    (let [params (merge {:task :deploy
                         :class-dir (str class-dir)}
                        params)
          params (common/resolve-project-data params)]
      (-> params
          ((requiring-resolve 'build.tasks.pom/pom))
          ((requiring-resolve 'build.tasks.jar/jar))
          ((requiring-resolve 'build.tasks.install/install))
          ((requiring-resolve 'build.tasks.deploy/deploy))))))

(defn javac
  "Compile Java sources.

  When called with :class-dir, uses that directory (for deps prep).
  Otherwise creates a temporary directory (for regular builds)."
  [params]
  (let [compile-fn (fn [params]
                     (let [params (assoc params :task :javac)
                           params (common/resolve-project-data params)]
                       ((requiring-resolve 'build.tasks.javac/javac) params)))]
    (if (:class-dir params)
      (compile-fn params)
      (fs/with-temp-dir [class-dir {:prefix "buildjavac"}]
        (compile-fn (assoc params :class-dir (str class-dir)))))))

(defn- read-deps-edn
  "Read and parse deps.edn file."
  [path]
  (when (.exists (io/file path))
    (edn/read-string (slurp path))))

(defn- get-local-root-deps
  "Extract :local/root dependencies from deps.edn aliases."
  [deps-edn alias-keys]
  (let [aliases (select-keys (:aliases deps-edn) alias-keys)]
    (->> aliases
         vals
         (mapcat :extra-deps)
         (keep (fn [[_k v]] (when (:local/root v) (:local/root v))))
         distinct)))

(defn- get-paths-from-deps
  "Get paths from a deps.edn file."
  [deps-path]
  (when-let [deps (read-deps-edn deps-path)]
    {:paths (:paths deps)
     :src-paths (get-in deps [:aliases :dev :extra-paths] [])
     :test-paths (get-in deps [:aliases :test :extra-paths] [])}))

(defn- collect-all-paths
  "Recursively collect paths from project and local/root dependencies."
  [root-dir alias-keys visited]
  (let [deps-path (str root-dir "/deps.edn")]
    (if (visited root-dir)
      []
      (when-let [deps (read-deps-edn deps-path)]
        (let [local-deps (get-local-root-deps deps alias-keys)
              {:keys [paths src-paths test-paths]} (get-paths-from-deps deps-path)
              all-paths (concat paths src-paths test-paths)
              ;; Make paths absolute
              abs-paths (map #(str root-dir "/" %) all-paths)
              ;; Recursively get paths from local deps
              dep-paths (mapcat #(collect-all-paths
                                  (str root-dir "/" %)
                                  alias-keys
                                  (conj visited root-dir))
                                local-deps)]
          (concat abs-paths dep-paths))))))

(defn source-paths
  "Return all source and test paths for the project and its :local/root dependencies.

  Recursively walks through deps.edn to find all :local/root dependencies and
  collects their :paths and paths from :dev and :test aliases.

  Usage:
    clojure -T:build source-paths

  Returns: Space-separated paths printed to stdout."
  [_params]
  (let [paths (distinct (collect-all-paths "." [:dev :test] #{}))]
    (println (str/join " " paths))
    nil))

(defn version
  "Print the version string for a project.

  Resolves dynamic components like {{git-rev-count}} and prints the result.

  Usage:
    clojure -T:build version :project :criterium

  Returns: Version string printed to stdout."
  [params]
  (let [params ((requiring-resolve 'build.project-data/project-data) params)]
    (println (:version params))
    nil))

(defn notebooks
  "Render all notebooks to HTML documentation.

  Shells out to clojure with :render-docs alias since the notebook
  dependencies require poly/agent and poly/blackhole to be prepped.

  Options:
    :aliases - additional aliases to include (e.g., :with-agent-mac)

  Usage:
    clojure -T:build notebooks
    clojure -T:build notebooks :aliases :with-agent-mac

  Returns: nil"
  [{:keys [aliases]}]
  (let [alias-str (if aliases
                    (str ":render-docs:" (name aliases))
                    ":render-docs")
        pb (ProcessBuilder. ["clojure" (str "-M" alias-str)])
        _ (.inheritIO pb)
        proc (.start pb)
        exit (.waitFor proc)]
    (when-not (zero? exit)
      (throw (ex-info "Failed to render notebooks" {:exit exit})))))
