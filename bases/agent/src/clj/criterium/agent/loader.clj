(ns criterium.agent.loader
    "Runtime extraction of bundled native agent binaries.

  Extracts platform-specific agent binaries from JAR resources to temporary
  directory for loading. Handles concurrent extraction safely using file
  locking and registers shutdown hooks for cleanup.

  The extraction process:
  1. Detects current platform
  2. Reads SHA256 hash from bundled .sha256 file
  3. Checks if agent already extracted to temp directory
  4. If not, locks and extracts atomically
  5. Registers cleanup hook on first extraction
  6. Returns absolute path to extracted agent

  Thread-safe and safe for concurrent JVM processes."
    (:require
     [clojure.java.io :as io]
     [clojure.string :as str]
     [criterium.agent.platform :as platform])
    (:import
     [java.nio.channels FileChannel]
     [java.nio.file Files Path StandardCopyOption StandardOpenOption]
     [java.nio.file.attribute FileAttribute]))

(defn- read-hash
       "Read SHA256 hash from bundled .sha256 file.

  Returns the hash string or nil if the file doesn't exist."
       [platform]
       (when-let [resource-path (platform/resource-path platform)]
                 (let [hash-path (str resource-path ".sha256")
                       resource (io/resource hash-path)]
                      (when resource
                            (-> resource
                                slurp
                                str/trim
                                (str/split #"\s+")
                                first)))))

(defn- temp-path
       "Build path to extracted agent in temp directory.

  Format: {tmpdir}/criterium-agent-{platform}-{hash}.{ext}"
       [platform hash]
       (let [ext (platform/extension platform)
             filename (str "criterium-agent-" platform "-" hash "." ext)
             tmpdir (System/getProperty "java.io.tmpdir")]
            (str tmpdir (when-not (str/ends-with? tmpdir "/") "/") filename)))

(defn- lock-path
       "Build path to lock file for extraction."
       [agent-path]
       (str agent-path ".lock"))

(def ^:private cleanup-registered?
     "Atom tracking whether cleanup hook has been registered."
     (atom false))

(def ^:private extracted-agents
     "Atom tracking extracted agent paths for cleanup."
     (atom #{}))

(defn- register-cleanup-hook!
       "Register shutdown hook to delete extracted agents.

  Only registers once per JVM lifetime."
       []
       (when (compare-and-set! cleanup-registered? false true)
             (.addShutdownHook
              (Runtime/getRuntime)
              (Thread.
               ^Runnable
               (fn []
                   (doseq [path @extracted-agents]
                          (try
                           (.delete (io/file path))
                           (catch Exception _))))))))

(defn- extract-with-lock
       "Extract agent binary to target path using file locking.

  Uses lock file to coordinate between concurrent processes.
  Returns true if extraction succeeded, false otherwise."
       [resource-path target-path]
       (let [lock-file (io/file (lock-path target-path))
             target-file (io/file target-path)]
            (try
      ;; Create lock file parent directory if needed
             (.mkdirs (.getParentFile lock-file))

      ;; Acquire exclusive lock
             (with-open [lock-channel (FileChannel/open
                                       (.toPath lock-file)
                                       (into-array [StandardOpenOption/CREATE
                                                    StandardOpenOption/WRITE]))]
                        (with-open [lock (.lock lock-channel)]
          ;; Double-check: another process may have extracted while we waited
                                   (when-not (.exists target-file)
                                             (let [temp-file (Files/createTempFile
                                                              "criterium-agent-"
                                                              (str "." (platform/extension
                                                                        (second (re-find #"criterium-agent-([^-]+)-" target-path))))
                                                              (into-array FileAttribute []))]
              ;; Copy resource to temp file
                                                  (with-open [in (io/input-stream (io/resource resource-path))]
                                                             (Files/copy ^java.io.InputStream in
                                                                         ^Path temp-file
                                                                         ^"[Ljava.nio.file.CopyOption;" (into-array [StandardCopyOption/REPLACE_EXISTING])))

              ;; Atomic move to target
                                                  (Files/move temp-file
                                                              (.toPath target-file)
                                                              (into-array [StandardCopyOption/ATOMIC_MOVE
                                                                           StandardCopyOption/REPLACE_EXISTING]))))))
      ;; Clean up lock file
             (.delete lock-file)
             true
             (catch Exception e
                    false))))

(defn extract-agent
      "Extract bundled agent binary to temp directory.

  Returns absolute path to extracted agent or nil if:
  - Platform is not supported
  - Hash file is not found in resources
  - Agent binary is not found in resources

  The extracted file is registered for cleanup on JVM shutdown.

  Safe to call concurrently from multiple threads or processes."
      []
      (when-let [platform (platform/detect)]
                (when-let [hash (read-hash platform)]
                          (let [target-path (temp-path platform hash)
                                target-file (io/file target-path)]
                               (when-not (.exists target-file)
                                         (when-let [resource-path (platform/resource-path platform)]
            ;; Verify resource exists
                                                   (when (io/resource resource-path)
                                                         (extract-with-lock resource-path target-path))))

        ;; Register cleanup and track
                               (when (.exists target-file)
                                     (register-cleanup-hook!)
                                     (swap! extracted-agents conj target-path)
                                     target-path)))))
