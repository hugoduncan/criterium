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
  7. Returns absolute path to extracted agent

  Extraction is persistent by default: the extracted binary is NOT deleted
  when the JVM exits. The temp filename is content-addressed (includes the
  SHA256 hash), so the file is reused across runs and JAR versions, and a path
  produced in one JVM remains valid for a separately-launched JVM (for example
  the `-agentpath` returned by `criterium.agent/jvm-opts`). Deletion on JVM exit
  is available as an explicit opt-in for ephemeral in-process use.

  Thread-safe and safe for concurrent JVM processes."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.agent.platform :as platform])
  (:import
   [java.nio.channels FileChannel]
   [java.nio.file Files Path StandardCopyOption StandardOpenOption]
   [java.nio.file.attribute FileAttribute PosixFilePermission]
   [java.security MessageDigest]))

(defn- warn
  "Emit a single diagnostic line to stderr. Never silent, never on stdout."
  [& parts]
  (binding [*out* *err*]
    (apply println "criterium:" parts)))

(defn- extraction-error
  "Throw a structured agent-extraction failure.

  Carries the failing :stage and contextual data so callers can diagnose the
  cause instead of seeing a bare nil/false. The original throwable, when
  present, is attached as the exception cause. This is never used for the
  legitimate \"unsupported platform\" case (which returns nil)."
  ([stage msg data] (extraction-error stage msg data nil))
  ([stage msg data ^Throwable cause]
   (throw (ex-info (str "criterium agent extraction failed [" (name stage) "]: " msg)
                   (merge {:criterium.agent/extraction-failure true
                           :stage stage}
                          data)
                   cause))))

(defn- compute-sha256
  "Compute the lowercase hex SHA-256 of the file at the given path."
  [^Path path]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (Files/readAllBytes path)]
    (apply str (map #(format "%02x" %) (.digest digest bytes)))))

(defn- verify-hash!
  "Verify the extracted file's SHA-256 matches the expected bundled hash.

  Promotes the hash from naming-only to an integrity check: a corrupt or
  truncated bundled binary is detected here rather than failing later at load
  time. Throws a structured :verify-hash error on mismatch. No-op when no
  expected hash is available."
  [^Path path expected-hash ctx]
  (when (and expected-hash (not (str/blank? expected-hash)))
    (let [actual (compute-sha256 path)]
      (when-not (= (str/lower-case actual) (str/lower-case expected-hash))
        (extraction-error
         :verify-hash
         (str "SHA-256 mismatch for extracted agent (corrupt or truncated binary)."
              " expected=" expected-hash " actual=" actual)
         (assoc ctx :expected-hash expected-hash :actual-hash actual))))))

(defn- read-hash
  "Read SHA256 hash from bundled .sha256 file.

  Returns the hash string if found.

  Throws a structured :read-hash error if the platform is supported but the
  hash file is missing or unreadable, which indicates incomplete JAR packaging."
  [platform]
  (when-let [resource-path (platform/resource-path platform)]
    (let [hash-path (str resource-path ".sha256")
          resource (io/resource hash-path)]
      (if resource
        (try
          (-> resource
              slurp
              str/trim
              (str/split #"\s+")
              first)
          (catch Exception e
            (extraction-error
             :read-hash
             (str "SHA256 hash file could not be read: " hash-path)
             {:platform platform :resource-path hash-path}
             e)))
        (extraction-error
         :read-hash
         (str "SHA256 hash file not found in JAR resources: " hash-path "\n"
              "This indicates the agent binary was not properly packaged.\n"
              "See docs/contributor/building-agent.md for bundling instructions.")
         {:platform platform :resource-path hash-path})))))

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

  Only registers once per JVM lifetime.

  Opt-in only: used when an extraction explicitly requests deletion on JVM exit.
  The default extraction path leaves the binary in place so it can outlive the
  producing JVM."
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

(defn- step
  "Run a side-effecting extraction step, tagging any failure with its stage.

  Already-structured extraction failures propagate unchanged; any other
  throwable is wrapped in a structured error naming the failing stage so no
  failure is ever silently swallowed."
  [stage ctx msg thunk]
  (try
    (thunk)
    (catch clojure.lang.ExceptionInfo e
      (if (:criterium.agent/extraction-failure (ex-data e))
        (throw e)
        (extraction-error stage (str msg ": " (ex-message e)) ctx e)))
    (catch Exception e
      (extraction-error stage (str msg ": " (.getMessage e)) ctx e))))

(defn- extract-with-lock
  "Extract agent binary to target path using file locking.

  Uses a lock file to coordinate between concurrent processes, verifies the
  extracted bytes against the expected SHA-256, sets executable permissions and
  verifies the file is readable/executable.

  Returns true on success. Throws a structured per-stage extraction error on any
  failure (never returns false / swallows the cause). On failure, partial
  artifacts are cleaned up."
  [resource-path target-path platform expected-hash]
  (let [lock-file (io/file (lock-path target-path))
        target-file (io/file target-path)
        ctx {:platform platform
             :resource-path resource-path
             :target-path target-path
             :tmpdir (System/getProperty "java.io.tmpdir")}]
    (try
      ;; Create lock file parent directory if needed
      (step :lock-dir-create ctx "could not create lock directory"
            #(.mkdirs (.getParentFile lock-file)))

      ;; Acquire exclusive lock
      (with-open [^FileChannel lock-channel
                  (step :open-lock ctx
                        (str "could not open lock file " lock-file
                             " (filesystem may not support file locking)")
                        #(FileChannel/open
                          (.toPath lock-file)
                          (into-array [StandardOpenOption/CREATE
                                       StandardOpenOption/WRITE])))]
        (with-open [^java.nio.channels.FileLock _lock
                    (step :acquire-lock ctx "could not acquire extraction lock"
                          #(.lock lock-channel))]
          ;; Double-check: another process may have extracted while we waited
          (when-not (.exists target-file)
            (let [^Path temp-file (step :create-temp ctx
                                        "could not create temp file for extraction"
                                        #(Files/createTempFile
                                          "criterium-agent-"
                                          (str "." (platform/extension platform))
                                          (into-array FileAttribute [])))]
              (try
                ;; Copy resource to temp file
                (step :copy ctx "failed copying agent resource to temp file (disk full?)"
                      (fn []
                        (with-open [in (io/input-stream (io/resource resource-path))]
                          (Files/copy ^java.io.InputStream in
                                      ^Path temp-file
                                      ^"[Ljava.nio.file.CopyOption;"
                                      (into-array [StandardCopyOption/REPLACE_EXISTING])))))

                ;; Verify integrity before publishing the file into place
                (verify-hash! temp-file expected-hash ctx)

                ;; Atomic move to target
                (step :atomic-move ctx
                      (str "atomic move failed (temp dir and target may be on "
                           "different filesystems)")
                      #(Files/move temp-file
                                   (.toPath target-file)
                                   (into-array [StandardCopyOption/ATOMIC_MOVE
                                                StandardCopyOption/REPLACE_EXISTING])))
                (catch clojure.lang.ExceptionInfo e
                  ;; Clean up the un-published temp file before propagating
                  (try (Files/deleteIfExists temp-file) (catch Exception _))
                  (throw e)))

              ;; Set executable permissions on Unix platforms
              (step :set-executable ctx "could not set executable permissions"
                    #(set-executable! (.toPath target-file)))

              ;; Verify file has appropriate permissions
              (step :verify-permissions ctx
                    (str "extracted agent failed permission verification "
                         "(the temp directory may be mounted noexec)")
                    #(verify-permissions! (.toPath target-file)))))))
      ;; Clean up lock file (best effort)
      (try (.delete lock-file)
           (catch Exception e
             (warn "could not delete lock file" (str lock-file) "-" (.getMessage e))))
      true
      (catch clojure.lang.ExceptionInfo e
        ;; Structured failure: remove any partially-published target and rethrow
        (try (.delete target-file) (catch Exception _))
        (try (.delete lock-file) (catch Exception _))
        (throw e)))))

(defn extract-agent
  "Extract bundled agent binary to temp directory.

  Returns the absolute path to the extracted agent, or nil only when the
  platform is unsupported (a legitimate, non-error outcome).

  Throws a structured extraction error (carrying :stage and context) for any
  genuine failure - missing hash file, missing binary resource, copy/lock/move
  failures, hash mismatch, or permission problems. Failures are never silently
  swallowed.

  By default the extracted file is persistent: it is NOT deleted on JVM
  shutdown, so the returned path remains valid for a separately-launched JVM
  (such as one started with the `-agentpath` produced by
  `criterium.agent/jvm-opts`). The temp filename is content-addressed, so the
  file is safely reused across runs.

  Options:
  - :cleanup-on-exit? (default false) - when true, registers a JVM shutdown
    hook to delete the extracted file on exit. Only appropriate for ephemeral,
    in-process use where no other JVM will load the path.

  Safe to call concurrently from multiple threads or processes."
  ([] (extract-agent {}))
  ([{:keys [cleanup-on-exit?] :or {cleanup-on-exit? false}}]
   (when-let [platform (platform/detect)]
     (when-let [hash (read-hash platform)]
       (let [target-path (temp-path platform hash)
             target-file (io/file target-path)
             register! (fn []
                         (when cleanup-on-exit?
                           (register-cleanup-hook!)
                           (swap! extracted-agents conj target-path)))]
         (cond
           ;; File already exists, use it
           (.exists target-file)
           (do
             (register!)
             target-path)

           ;; File doesn't exist, attempt extraction
           :else
           (let [resource-path (platform/resource-path platform)]
             (when-not (io/resource resource-path)
               (extraction-error
                :resolve-binary
                (str "agent binary not found in JAR resources: " resource-path "\n"
                     "This indicates the agent binary was not properly packaged.\n"
                     "See docs/contributor/building-agent.md for bundling instructions.")
                {:platform platform :resource-path resource-path}))
             (extract-with-lock resource-path target-path platform hash)
             ;; Extraction succeeded; register cleanup only if requested
             (register!)
             target-path)))))))
