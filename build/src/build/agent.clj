(ns build.agent
  "Agent build utilities for platform detection and agent compilation."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.security MessageDigest]
   [java.nio.file Files]))

;;; Platform Detection

(defn detect-os
  "Returns the current operating system as a keyword.
  Returns :linux, :macos, or :unknown."
  []
  (let [os-name (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os-name "linux") :linux
      (str/includes? os-name "mac") :macos
      :else :unknown)))

(defn detect-arch
  "Returns the current architecture as a keyword.
  Returns :x64, :arm64, or :unknown."
  []
  (let [os-arch (System/getProperty "os.arch")]
    (case os-arch
      ("amd64" "x86_64") :x64
      ("aarch64" "arm64") :arm64
      :unknown)))

(defn current-platform
  "Returns the current platform identifier string.
  Returns one of: linux-x64, macos-x64, macos-arm64.
  Throws if platform is unsupported."
  []
  (let [os (detect-os)
        arch (detect-arch)
        platform (str (name os) "-" (name arch))]
    (if (contains? #{"linux-x64" "macos-x64" "macos-arm64"} platform)
      platform
      (throw (ex-info (str "Unsupported platform: " platform)
                      {:os os :arch arch :platform platform})))))

(defn library-name
  "Returns the library filename for the given platform.
  E.g., \"libcriterium.so\" for linux, \"libcriterium.dylib\" for macos."
  [platform]
  (if (str/starts-with? platform "linux")
    "libcriterium.so"
    "libcriterium.dylib"))

(defn resources-base-dir
  "Returns the base directory for agent resources.
  This function exists to allow tests to override the target directory."
  []
  (io/file "bases/agent/resources/native"))

;;; Agent Build and Copy

(defn- compute-sha256
  "Computes SHA256 hash of a file, returns lowercase hex string."
  [file]
  (let [path (.toPath (io/file file))
        bytes (Files/readAllBytes path)
        digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest bytes)]
    (apply str (map #(format "%02x" %) hash-bytes))))

(defn- run-process!
  "Runs a process with the given args in the specified directory.
  Returns exit code. Throws on non-zero exit."
  [args ^java.io.File dir description]
  (let [pb (ProcessBuilder. ^java.util.List args)
        _ (.directory pb dir)
        _ (.inheritIO pb)
        proc (.start pb)
        exit-code (.waitFor proc)]
    (when-not (zero? exit-code)
      (throw (ex-info (str description " failed with exit code " exit-code)
                      {:exit-code exit-code
                       :command args
                       :directory (.getAbsolutePath dir)})))
    exit-code))

(defn agent-cpp-dir
  "Returns the path to the agent-cpp source directory."
  []
  (io/file "agent-cpp"))

(defn build-agent-cpp!
  "Builds the agent from agent-cpp/ source for the current platform using CMake.
  Returns the path to the built library file.
  Optionally accepts a custom build directory (defaults to agent-cpp/build).
  Throws on build failure or unsupported platform."
  ([] (build-agent-cpp! nil))
  ([opts]
   (let [platform (current-platform)
         ^java.io.File agent-dir (agent-cpp-dir)
         build-dir (or (:build-dir opts) (io/file agent-dir "build"))
         lib-name (library-name platform)
         lib-file (io/file build-dir lib-name)]
     (when-not (.isDirectory agent-dir)
       (throw (ex-info "agent-cpp directory not found"
                       {:agent-dir (.getAbsolutePath agent-dir)})))
     (.mkdirs build-dir)
     (println "Building agent for" platform "using CMake...")
     (run-process! ["cmake" (.getAbsolutePath agent-dir)] build-dir "CMake configure")
     (run-process! ["cmake" "--build" "."] build-dir "CMake build")
     (when-not (.exists lib-file)
       (throw (ex-info "Build succeeded but library file not found"
                       {:expected (.getAbsolutePath lib-file)})))
     (.getAbsolutePath lib-file))))

(defn copy-agent-binary!
  "Copies the built agent binary to resources and generates SHA256 hash.
  Creates the target directory if needed.
  Returns a map with :binary-path and :hash-path."
  [source-path platform]
  (let [lib-name (library-name platform)
        target-dir (io/file (resources-base-dir) platform)
        target-file (io/file target-dir lib-name)
        hash-file (io/file target-dir (str lib-name ".sha256"))
        source-file (io/file source-path)]
    (when-not (.exists source-file)
      (throw (ex-info "Source binary not found"
                      {:source-path source-path})))
    (.mkdirs target-dir)
    (io/copy source-file target-file)
    (let [hash-value (compute-sha256 target-file)]
      (spit hash-file (str hash-value "  " lib-name "\n")))
    (println "Copied" lib-name "to" (.getPath target-dir))
    {:binary-path (.getPath target-file)
     :hash-path (.getPath hash-file)}))

(defn build-and-copy-agent!
  "Builds the agent and copies it to resources for the current platform.
  Convenience function combining build-agent-cpp! and copy-agent-binary!."
  []
  (let [platform (current-platform)
        built-lib (build-agent-cpp!)]
    (copy-agent-binary! built-lib platform)))

;;; Release Validation

(defn validate-agent-binaries!
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
