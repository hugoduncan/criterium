(ns build
  (:require
   [build.tasks :as tasks]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn ^{:params []} clean
  "Clean built files."
  [params]
  (tasks/clean (assoc params :project :agent)))

(defn- validate-agent-binaries!
  "Validates that required agent binaries are present before building JAR.

  Agent binaries are downloaded from CI and placed in bases/agent/resources/native/{platform}/
  but are NOT committed to version control (they are in .gitignore).

  Throws an exception with helpful error message if binaries are missing."
  []
  (let [base-path "bases/agent/resources/native"
        platforms ["linux-x64" "macos-x64" "macos-arm64"]
        required-files {"linux-x64" ["libcriterium.so" "libcriterium.so.sha256"]
                        "macos-x64" ["libcriterium.dylib" "libcriterium.dylib.sha256"]
                        "macos-arm64" ["libcriterium.dylib" "libcriterium.dylib.sha256"]}
        missing-files (for [platform platforms
                            file (required-files platform)
                            :let [path (str base-path "/" platform "/" file)
                                  file-obj (io/file path)]
                            :when (not (.exists file-obj))]
                        path)]
    (when (seq missing-files)
      (throw (ex-info
              (str "Missing required agent binaries. Agent binaries must be downloaded from CI before building release JARs.\n\n"
                   "Missing files:\n"
                   (str/join "\n" (map #(str "  - " %) missing-files))
                   "\n\n"
                   "To fix this:\n"
                   "1. Find the latest CI workflow run:\n"
                   "   gh run list --workflow=\"agent-cpp.yml\" --limit 5\n\n"
                   "2. Download artifacts:\n"
                   "   gh run download <run-id> -n agent-cpp-linux-x64\n"
                   "   gh run download <run-id> -n agent-cpp-macos-x64\n"
                   "   gh run download <run-id> -n agent-cpp-macos-arm64\n\n"
                   "3. Copy binaries to resources:\n"
                   "   mkdir -p " base-path "/linux-x64 " base-path "/macos-x64 " base-path "/macos-arm64\n"
                   "   cp libcriterium.so libcriterium.so.sha256 " base-path "/linux-x64/\n"
                   "   # For macos-x64 artifact:\n"
                   "   cp libcriterium.dylib libcriterium.dylib.sha256 " base-path "/macos-x64/\n"
                   "   # For macos-arm64 artifact:\n"
                   "   cp libcriterium.dylib libcriterium.dylib.sha256 " base-path "/macos-arm64/\n\n"
                   "See docs/contributor/building-agent.md for detailed instructions.")
              {:missing-files missing-files})))))

(defn ^{:params []} jar
  "Build jarfile"
  [params]
  (validate-agent-binaries!)
  (tasks/jar (assoc params :project :agent)))

(defn ^{:params []} install
  "Install jar."
  [params]
  (tasks/install (assoc params :project :agent)))

(defn ^{:params []} deploy
  "Deploy jar."
  [params]
  (tasks/deploy (assoc params :project :agent)))

(defn ^{:params []} build
  "Build agent project"
  [params]
  (-> params
      clean
      jar
      install))
