(ns criterium.agent.runtime
  "Runtime agent loading and management without Agent class dependencies.

  This namespace provides functions for loading the native agent at runtime
  without requiring the Agent classes to be available at compile time. Use
  this namespace to load the agent before requiring criterium.agent.core.

  Key Functions:
  - pid: Get current JVM process ID
  - agent-path: Extract bundled agent to temp directory
  - load-agent!: Programmatically load agent into running JVM
  - loaded?: Check if agent is currently loaded

  Example:
    ;; Load agent at runtime
    (require '[criterium.agent.runtime :as runtime])
    (runtime/load-agent!)

    ;; Now safe to require core namespace
    (require '[criterium.agent.core :as core])

  The agent can also be loaded via -agentpath JVM argument, in which case
  load-agent! will detect it's already loaded and skip loading."
  (:require
   [criterium.agent.loader :as loader]
   [criterium.agent.platform :as platform])
  (:import
   [com.sun.tools.attach VirtualMachine]
   [java.lang.management ManagementFactory]))

(defn pid
  "Return the PID of the current JVM.

  Extracts the process ID from the runtime MXBean name.
  This works on most JVM/OS combinations.

  Returns the PID as a string."
  []
  (re-find #"\d+" (.getName (ManagementFactory/getRuntimeMXBean))))

(defn agent-path
  "Returns the absolute path to the extracted native agent, or nil if unavailable.

  Extracts the bundled agent binary to a temporary directory on first call.

  Returns nil only when the platform is unsupported - a legitimate outcome,
  logged once to stderr with the detected os.name/os.arch so the reason is
  never silent. Genuine extraction failures are caught and logged to stderr
  with their failing stage and context (rather than throwing, to preserve
  graceful degradation of higher-level allocation tracking) and also return
  nil; they are never silently swallowed.

  The extracted file is persistent (not deleted on JVM exit), so the returned
  path stays valid for a separately-launched JVM, e.g. one started with the
  `-agentpath` from `criterium.agent/jvm-opts`.

  The path points to a platform-specific shared library (.so or .dylib) that
  can be loaded via -agentpath or VirtualMachine.loadAgent().

  Thread-safe - concurrent calls will safely extract to the same location."
  []
  (try
    (if-let [path (loader/extract-agent)]
      path
      (do
        (binding [*out* *err*]
          (println "criterium: native agent not available -"
                   "unsupported platform:" (pr-str (platform/describe))))
        nil))
    (catch Exception e
      (let [data (ex-data e)]
        (binding [*out* *err*]
          (println "WARNING: Failed to extract agent:"
                   (ex-message e)
                   (if data
                     (pr-str (select-keys data [:stage :platform
                                                :resource-path :target-path
                                                :tmpdir]))
                     ""))))
      nil)))

(defn extract-agent!
  "Explicitly extract the bundled native agent, surfacing all errors.

  This is the explicit, fail-loud counterpart to `agent-path`. Where
  `agent-path` degrades gracefully (logs and returns nil on failure, to keep
  higher-level allocation tracking working without an agent), `extract-agent!`
  performs the same extraction but *throws* a structured failure so callers can
  detect and diagnose problems - useful for tooling that wires `-agentpath` and
  must fail clearly if the agent cannot be extracted.

  Options (map, optional):
  - :cleanup-on-exit? (default false) - register a JVM shutdown hook to delete
    the extracted file on exit. Only appropriate for ephemeral in-process use;
    do NOT use when handing the path to a separately-launched JVM.

  Returns:
  - the absolute path to the extracted, persistent agent binary on success;
  - nil only when the current platform is unsupported (a legitimate outcome).

  Throws:
  - clojure.lang.ExceptionInfo on any genuine failure. The ex-data carries:
    - :criterium.agent/extraction-failure true
    - :stage - the failing stage keyword, one of :read-hash, :resolve-binary,
      :lock-dir-create, :open-lock, :acquire-lock, :create-temp, :copy,
      :verify-hash, :atomic-move, :set-executable, :verify-permissions
    - :platform, :resource-path, :target-path, :tmpdir - context for diagnosis
    with the underlying throwable attached as the exception cause.

  The extracted file is persistent (not deleted on JVM exit unless
  :cleanup-on-exit? is set), so the returned path is valid for a
  separately-launched JVM (e.g. via -agentpath).

  Thread-safe - concurrent calls will safely extract to the same location."
  ([] (loader/extract-agent))
  ([opts] (loader/extract-agent opts)))

(defn loaded?
  "Returns true if the Criterium native agent is currently loaded.

  Checks whether the agent was loaded via -agentpath JVM arguments or
  programmatically via load-agent!. Uses reflection to avoid compile-time
  dependency on Agent classes.

  This is safe to call even if the agent is not loaded."
  []
  (try
    (let [agent-class (Class/forName "criterium.agent.Agent")
          method (.getMethod agent-class "getState" (make-array Class 0))
          state (.invoke method (.newInstance agent-class) (make-array Object 0))]
      (not= -1 (long state)))
    (catch ClassNotFoundException _
      false)
    (catch Exception _
      false)))

(defn load-agent!
  "Programmatically loads the Criterium native agent into the current JVM.

  This function enables runtime loading of the agent without requiring
  -agentpath in JVM arguments. The agent is extracted from bundled resources
  and attached using the Java Attach API.

  Throws:
  - IllegalStateException if agent is already loaded
  - RuntimeException if agent unavailable for current platform
  - RuntimeException if loading fails

  The agent must not be already attached via -agentpath or previous load-agent!
  call. Use (loaded?) to check before calling."
  []
  (when (loaded?)
    (throw (IllegalStateException. "Agent is already loaded")))
  (if-let [path (agent-path)]
    (try
      (.loadAgentPath
       ^VirtualMachine (VirtualMachine/attach ^String (pid))
       ^String path)
      (catch Exception e
        (throw (RuntimeException. (str "Failed to load agent from " path) e))))
    (throw (RuntimeException. "Agent not available for current platform"))))
