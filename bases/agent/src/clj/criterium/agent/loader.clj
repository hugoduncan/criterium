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
  5. Sets appropriate file permissions (executable on Unix)
  6. Verifies file permissions
  7. Registers cleanup hook on first extraction
  8. Returns absolute path to extracted agent

  Thread-safe and safe for concurrent JVM processes."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.agent.platform :as platform])
  (:import
   [java.nio.channels FileChannel]
   [java.nio.file Files Path StandardCopyOption StandardOpenOption]
   [java.nio.file.attribute FileAttribute PosixFilePermission]))

(defn- read-hash
  "Read SHA256 hash from bundled .sha256 file.

  Returns the hash string if found.

  Throws RuntimeException if the platform is supported but the hash file is missing,
  which indicates incomplete JAR packaging."
  [platform]
  (when-let [resource-path (platform/resource-path platform)]
    (let [hash-path (str resource-path ".sha256")
          resource (io/resource hash-path)]
      (if resource
        (-> resource
            slurp
            str/trim
            (str/split #"\s+")
            first)
        (throw (RuntimeException.
                (str "SHA256 hash file not found in JAR resources: " hash-path "\n"
                     "This indicates the agent binary was not properly packaged.\n"
                     "See docs/contributor/building-agent.md for bundling instructions.")))))))

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

(defn- set-executable!
  "Set executable permissions on Unix platforms.

  Uses POSIX file permissions to set rwxr-xr-x (755).
  No-op on Windows."
  [^Path path]
  (when (not= (platform/detect) nil)
    (try
      ;; Set POSIX permissions: rwxr-xr-x (owner can read/write/exec, others can read/exec)
      (let [perms #{PosixFilePermission/OWNER_READ
                    PosixFilePermission/OWNER_WRITE
                    PosixFilePermission/OWNER_EXECUTE
                    PosixFilePermission/GROUP_READ
                    PosixFilePermission/GROUP_EXECUTE
                    PosixFilePermission/OTHERS_READ
                    PosixFilePermission/OTHERS_EXECUTE}]
        (Files/setPosixFilePermissions path perms))
      (catch UnsupportedOperationException _
        ;; Windows or non-POSIX filesystem, skip
        nil))))

(defn- verify-permissions!
  "Verify extracted file has appropriate permissions.

  Checks that file is readable and executable.
  Throws RuntimeException with clear message if verification fails."
  [^Path path]
  (when-not (Files/isReadable path)
    (throw (RuntimeException.
            (str "Extracted agent is not readable: " path))))
  (when-not (Files/isExecutable path)
    (throw (RuntimeException.
            (str "Extracted agent is not executable: " path
                 ". This may indicate a permission issue.")))))

(defn- extract-with-lock
  "Extract agent binary to target path using file locking.

  Uses lock file to coordinate between concurrent processes.
  Sets executable permissions and verifies file is readable/executable.
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
        (with-open [_lock (.lock lock-channel)]
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
                                       StandardCopyOption/REPLACE_EXISTING]))

              ;; Set executable permissions on Unix platforms
              (set-executable! (.toPath target-file))

              ;; Verify file has appropriate permissions
              (verify-permissions! (.toPath target-file))))))
      ;; Clean up lock file
      (.delete lock-file)
      true
      (catch Exception _e
        false))))

(defn extract-agent
  "Extract bundled agent binary to temp directory.

  Returns absolute path to extracted agent or nil if:
  - Platform is not supported
  - Hash file is not found in resources
  - Agent binary is not found in resources
  - Extraction fails

  The extracted file is registered for cleanup on JVM shutdown.

  Safe to call concurrently from multiple threads or processes."
  []
  (when-let [platform (platform/detect)]
    (when-let [hash (read-hash platform)]
      (let [target-path (temp-path platform hash)
            target-file (io/file target-path)]
        (cond
          ;; File already exists, use it
          (.exists target-file)
          (do
            (register-cleanup-hook!)
            (swap! extracted-agents conj target-path)
            target-path)

          ;; File doesn't exist, attempt extraction
          :else
          (when-let [resource-path (platform/resource-path platform)]
            (when (io/resource resource-path)
              (when (extract-with-lock resource-path target-path)
                ;; Extraction succeeded, register and return path
                (register-cleanup-hook!)
                (swap! extracted-agents conj target-path)
                target-path))))))))
