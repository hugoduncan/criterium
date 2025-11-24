(ns criterium.agent.core
    "Low-level interface to the Criterium native agent for allocation tracking.

  This namespace provides the core implementation for interacting with the
  agent that tracks JVM heap allocations. It manages agent state, handles
  allocation records, and provides primitives for the high-level API.

  Key Components:
  - Native Agent Commands: Protocol for controlling agent behavior
  - State Management: Track and validate agent state transitions
  - Allocation Recording: Capture and store allocation events
  - Data Processing: Transform raw allocation data into usable records

  Implementation Notes:
  - Uses JNI bindings to communicate with native agent
  - Manages thread-local and global agent state
  - Optimized for minimal allocation overhead during tracing
  - Handles concurrent access to shared state

  Warning: This is an internal implementation namespace. Most users should
  use criterium.agent instead."
    (:require
     [clojure.string :as str]
     [criterium.agent.loader :as loader]
     [criterium.util.invariant :refer [have?]])
    (:import
     [com.sun.tools.attach
      VirtualMachine]
     [criterium.agent
      Agent]
     [java.lang.management
      ManagementFactory]))

;;; Native Agent

(def ^:internal records
     "Atom containing the current set of allocation records.

  Records are captured during allocation tracing and stored here until
  retrieved.  The atom is cleared at the start of each tracing session.

  Structure:
  Vector of maps, each containing allocation details like:
  - :object-type  - Class of allocated object
  - :object_size  - Size in bytes
  - :call-*       - Allocation call site information
  - :alloc-*      - Allocator information
  - :thread       - Thread ID
  - :freed        - GC status"
     (atom []))

(defn internal->class-name
      "Convert an internal JVM class name to standard Java/Clojure class name.

   Parameter:
     internal-name - String in JVM internal format (e.g. 'Ljava/lang/String;')

   Return:
     Standard class name with dot notation (e.g. 'java.lang.String')

   Example:
     (internal->class-name \"Ljava/util/List;\")
     => \"java.util.List\""
      [internal-name]
      {:pre [(have? string? internal-name)
             (have? #(str/starts-with? % "L") internal-name)
             (have? #(str/includes? % "/") internal-name)
             (have? #(str/ends-with? % ";") internal-name)]}
      (-> internal-name
          (subs 1 (dec (count internal-name))) ; Remove L and ;
          (str/replace "/" ".")))

(def ^:private allocation-start-marker-jvm-type
     "Lcriterium/agent/Agent$AllocationStartMarker;")

(def ^:private allocation-finish-marker-jvm-type
     "Lcriterium/agent/Agent$AllocationFinishMarker;")

(defn- blank->nil [s]
       (when-not (str/blank? s)
                 s))

(defn- data-fn
       "Callback function invoked by the native agent for allocation events.

  Processes allocation events from the native agent and stores them in
  the records atom. Handles both object and primitive allocation
  records.

  Implementation Notes:
  - Called from native code via JNI
  - Must be thread-safe
  - Minimizes allocations during processing
  - Filters out internal marker allocations

  Called in two forms:
  1. Single object form for complex allocations
  2. Multi-argument form for primitive allocations"
       ([object]
        (cond
         (instance? criterium.agent.Allocation object)
         (let [a ^criterium.agent.Allocation object]
              (when (and (not= (.object_type a) allocation-start-marker-jvm-type)
                         (not= (.object_type a) allocation-finish-marker-jvm-type))
                    (swap! records conj
                           {:object-type (internal->class-name (.object_type a))
                            :object_size (.object_size a)
                            :call-class (some->
                                         (.call_class a)
                                         blank->nil
                                         internal->class-name)
                            :call-method (.call_method a)
                            :call-file (.call_file a)
                            :call-line (.call_line a)
                            :alloc-class (some->
                                          (.alloc_class a)
                                          blank->nil
                                          internal->class-name)
                            :alloc-method (.alloc_method a)
                            :alloc-file (.alloc_file a)
                            :alloc-line (.alloc_line a)
                            :thread (.thread a)
                            :freed (.freed a)})))
         :else
         (prn :received object (type object))))
       ([a b c d e f g h]
        (when (and (not= c "allocation_start_marker")
                   (not= c "allocation_finish_marker"))
              (swap! records conj
                     {:object-type a
                      :call-class b
                      :call-method c
                      :file d
                      :size (Long/parseLong e)
                      :thread (Long/parseLong f)
                      :line (Long/parseLong g)
                      :freed (Long/parseLong h)}))))

(Agent/set_handler data-fn);

(def ^:private commands
     "Map of command keywords to their numeric protocol values.

  Commands control agent behavior:
  - :ping - Check agent responsiveness
  - :sync-state - Synchronize agent state
  - :start-allocation-tracing - Begin allocation tracking
  - :stop-allocation-tracing - End allocation tracking
  - :report-allocation-tracing - Retrieve allocation data

  Values correspond to the native agent protocol constants."
     {:ping 0
      :sync-state 1
      :start-allocation-tracing 10
      :stop-allocation-tracing 11
      :report-allocation-tracing 12})

(def ^:private states
     "Map of numeric state codes to their keyword representations.

  Agent States and Transitions:
  :not-attached (-1) - Agent not loaded or initialized
  :passive (0) - Agent loaded but inactive
  :allocation-tracing-starting (10) -> :allocation-tracing-active
  :allocation-tracing-active (11) - Collecting allocation data
  :allocation-tracing-stopping (15) -> :allocation-tracing-flushing
  :allocation-tracing-flushing (16) -> :allocation-tracing-flushed
  :allocation-tracing-flushed (17) - Data ready for collection

  State transitions are managed by agent commands."
     {-1 :not-attached
      0 :passive
      10 :allocation-tracing-starting
      11 :allocation-tracing-active
      15 :allocation-tracing-stopping
      16 :allocation-tracing-flushing
      17 :allocation-tracing-flushed
      18 :allocation-tracing-reporting
      19 :allocation-tracing-reported})

(defn ^:internal agent-command
      "Send a command to the native agent.

  Commands are sent via JNI and may block until the agent responds.
  See commands map for valid command values.

  Implementation Notes:
  - Thread-safe but may synchronize on agent state
  - May trigger state transitions
  - Command acknowledgement is synchronous"
      [cmd]
      (let [cmd-num (commands cmd)]
           (when-not cmd-num
                     (throw
                      (IllegalArgumentException. (str "Unkonwn command: " (pr-str cmd)))))
           (Agent/command cmd-num)))

;; Direct linking is used here to avoid var lookups, which can cause garbage
;; which we want to avoid in the sample collection path.
(binding [*compiler-options* (assoc *compiler-options* :direct-linking true)]
  ;; Direct-linked implementation to minimize allocation overhead

         (defn ^:internal agent-state**
               "Get raw numeric state from native agent instance.

  Performance critical path - uses type hints and direct linking.
  Returns the raw state value for translation by agent-state.

  Implementation Notes:
  - Type hinted for performance
  - Direct linked to avoid var lookup
  - Thread-safe but uncoordinated"
               ^long [^Agent agent]
               (. agent getState))

         (def agent-state* (partial agent-state** (new Agent)))

         (defn ^:internal agent-state []
               (get states (agent-state*)))

         (comment
          (agent-command :ping))

         (dotimes [_ 1000] (agent-state))

         (defn allocation-start-marker
               "Create a marker allocation to track start of allocation sequence.

    Used to synchronize the start of allocation tracking by creating a
    recognizable allocation pattern.

    Implementation Notes:
    - Creates a specific allocation pattern
    - Filtered from final results
    - Used for state transition timing"
               []
               (Agent/allocation_start_marker))

         (defn allocation-tracing-active?
               "Test if allocation tracing is currently active.

    Returns true only when the agent is in the :allocation-tracing-active state
    and fully initialized.

    Implementation Notes:
    - Thread-safe state check
    - Used to verify tracing preconditions
    - Optimized for frequent checking"
               []
               (= (agent-state) :allocation-tracing-active))

         (defn attached?
               "Test if the native agent is properly attached to the JVM.

    Returns true if the agent is loaded and initialized, false otherwise.
    This is a prerequisite for any allocation tracking operations.

    Implementation Notes:
    - Thread-safe state check
    - Does not modify agent state
    - Used to guard tracing operations"
               []
               (not= (agent-state) :not-attached))

         (defn ^:internal allocation-tracing-start!
               "Initialize and start allocation tracing.

    Sequence:
    1. Send start command to agent
    2. Wait for agent state transition
    3. Force GC to clear existing allocations
    4. Create start marker and verify state

    Implementation Notes:
    - Blocks until tracing is active
    - Creates synchronization allocations
    - May timeout if agent doesn't respond
    - Thread-safe but should not be called concurrently"
               []
               (agent-command :start-allocation-tracing)
    ;; (have #(= :allocation-tracing-starting %) (agent-state))
    ;; (assert (= :allocation-tracing-starting (agent-state)))
    ;; (make-array Object (* 512 1024))             ; flush this
               (System/gc)
               (System/gc)
               (System/gc)
               (loop [i 1000000]
                     (allocation-start-marker)
                     (when (and (pos? i) (not (allocation-tracing-active?)))
                           (System/gc)
                           (recur (unchecked-dec i))))
               (when (not= (agent-state) :allocation-tracing-active)
                     (println "WARNING allocation tracing failed to start promptly")))

         (defn ^:internal allocation-tracing-stop!
               "Stop allocation tracing and collect final results.

    Sequence:
    1. Send stop command to agent
    2. Create finish marker allocation
    3. Force GC to flush remaining allocations
    4. Wait for agent to finish processing

    Implementation Notes:
    - Blocks until processing complete
    - Creates marker allocations
    - Thread-safe but should not be called concurrently
    - May timeout if agent doesn't respond"
               []
               (agent-command :stop-allocation-tracing)
               (Agent/allocation_finish_marker)
               (System/gc)
               (loop [i 1000000]
                     (when (and (pos? i)
                                (not= (agent-state) :allocation-tracing-flushed))
                           (Agent/allocation_finish_marker)
                           (System/gc)
                           (recur (unchecked-dec i))))
               (when (not= (agent-state) :allocation-tracing-flushed)
                     (println "WARNING allocation tracing failed to stop promptly"))))

(defn collect-allocaton-records
      []
      (reset! records [])
      (agent-command :report-allocation-tracing)
      (loop [i 100000]
            (when (and (pos? i)
                       (not= (agent-state) :allocation-tracing-reported))
                  (Thread/yield)
                  (recur (unchecked-dec i))))
      (when (not= (agent-state) :allocation-tracing-reported)
            (println "WARNING allocation tracing failed to collect results promptly")))

(defn with-allocation-tracing-enabled [body]
      `(let [active?# (allocation-tracing-active?)
             res# (if active?#
                      (do
                       ~@body)
                      (try
                       (allocation-tracing-start!)
                       ~@body
                       (finally
                        (allocation-tracing-stop!))))]
            (collect-allocaton-records)
            [@records res#]))

(defn with-allocation-tracing-disabled [body]
      `[nil (do ~@body)])

;; (trace-allocation)

(defn allocation-on-thread?
      "Returns a predicate function for filtering allocation records by thread.

  The returned function takes an allocation record and returns true if the
  allocation occurred on the specified thread. Useful for filtering allocation
  records to analyze per-thread behavior.

  Parameters:
  thread-id - Thread ID to match against allocation records

  Returns a function that takes an allocation record and returns true if
  the record's thread matches the specified thread-id."
      [thread-id]
      (fn allocation-on-thread?
          [record]
          (= thread-id (:thread record))))

(defn allocation-freed?
      "Predicate that returns true if the allocation record indicates the object was freed.

  An object is considered freed when it has been garbage collected during the
  allocation tracking session. This helps identify temporary allocations vs
  retained objects.

  Parameters:
  record - The allocation record to check

  Returns true if the record indicates the object was freed during tracking."
      [record]
      (pos? (long (:freed record))))

(defn allocations-summary
      "Returns a summary of allocation statistics for the given records.

  Takes a sequence of allocation records and returns a map with:
  {:num-allocated   - Total number of objects allocated
   :num-freed      - Number of allocated objects that were freed
   :allocated-bytes - Total bytes allocated
   :freed-bytes    - Total bytes from freed objects}

  Parameters:
  records - Sequence of allocation records to summarize

  Returns a map containing allocation statistics."
      [records]
      (let [freed (filterv allocation-freed? records)]
           {:num-allocated (count records)
            :num-freed (count freed)
            :allocated-bytes (reduce + (map :object_size records))
            :freed-bytes (reduce + (map :object_size freed))}))

;;; Agent Loading API

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
  Returns nil for unsupported platforms with a logged warning.

  The path points to a platform-specific shared library (.so or .dylib) that
  can be loaded via -agentpath or VirtualMachine.loadAgent().

  Thread-safe - concurrent calls will safely extract to the same location."
      []
      (try
       (loader/extract-agent)
       (catch Exception e
              (println "WARNING: Failed to extract agent:" (.getMessage e))
              nil)))

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
  call. Use (attached?) to check before calling."
      []
      (when (attached?)
            (throw (IllegalStateException. "Agent is already loaded")))
      (if-let [path (agent-path)]
              (try
               (.loadAgent ^VirtualMachine (VirtualMachine/attach ^String (pid)) ^String path)
               (catch Exception e
                      (throw (RuntimeException. (str "Failed to load agent from " path) e))))
              (throw (RuntimeException. "Agent not available for current platform"))))

;;; Object Size Agent

(comment
  ;; TODO re-enable these
 (defn pid
       "Return the PID of the current JVM.
    This may not work on all JVM/OS instances."
       []
       (re-find #"\d+" (.getName (ManagementFactory/getRuntimeMXBean))))

 (defn- load-agent*
        "Attach the javaagent from a jar file to the current JVM."
        [^String jar-path]
        (.loadAgent (VirtualMachine/attach (pid)) jar-path))

 (defn- find-jar
        "Find the agent jar"
    ;; TODO use tools.deps.alpha
        []
        "criterium-agent.jar")

 #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
 (defn load-agent
       "Load the agent"
       []
       (load-agent* (find-jar)))

 #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
 (defn object-size
       "Return the approximate size of an object in bytes."
       ^long [_x]
       #_(agent/object-size x)
       0))
