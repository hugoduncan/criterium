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
  - Uses wrapper namespace for zero-allocation Agent access
  - Manages thread-local and global agent state
  - Optimized for minimal allocation overhead during tracing
  - Handles concurrent access to shared state
  - Safe to load even when agent classes are not available

  WARNING: The agent must be loaded before calling most functions in this
  namespace. Use criterium.agent.runtime/load-agent! or -agentpath JVM arg.

  This is an internal implementation namespace. Most users should use
  criterium.agent instead."
  (:require
   [clojure.string :as str]
   [criterium.util.invariant :refer [have?]]))

;;; Wrapper functions - zero-allocation direct access when available

(def ^:private wrapper-read-state
  "Lazily resolved wrapper/read-state, or nil if wrapper can't load."
  (delay
    (try
      @(requiring-resolve 'criterium.agent.wrapper/read-state)
      (catch Exception _ nil))))

(def ^:private wrapper-send-command
  "Lazily resolved wrapper/send-command, or nil if wrapper can't load."
  (delay
    (try
      @(requiring-resolve 'criterium.agent.wrapper/send-command)
      (catch Exception _ nil))))

(def ^:private wrapper-start-marker
  "Lazily resolved wrapper/allocation-start-marker, or nil if wrapper can't load."
  (delay
    (try
      (requiring-resolve 'criterium.agent.wrapper/allocation-start-marker)
      (catch Exception _ nil))))

(def ^:private wrapper-finish-marker
  "Lazily resolved wrapper/allocation-finish-marker, or nil if wrapper can't load."
  (delay
    (try
      (requiring-resolve 'criterium.agent.wrapper/allocation-finish-marker)
      (catch Exception _ nil))))

(def ^:private wrapper-set-handler
  "Lazily resolved wrapper/set-handler, or nil if wrapper can't load."
  (delay
    (try
      (requiring-resolve 'criterium.agent.wrapper/set-handler)
      (catch Exception _ nil))))

;;; Agent Class Access via Reflection

(def ^:private allocation-class
  "Lazily resolved Allocation class, or nil if not available."
  (delay
    (try
      (Class/forName "criterium.agent.Allocation")
      (catch ClassNotFoundException _
        nil))))

;;; Native Agent

(def ^:internal records
  "Atom containing the current set of allocation records.

  Records are captured during allocation tracing and stored here until
  retrieved. The atom is cleared at the start of each tracing session.

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
      (subs 1 (dec (count internal-name)))
      (str/replace "/" ".")))

(def ^:private allocation-start-marker-jvm-type
  "Lcriterium/agent/Agent$AllocationStartMarker;")

(def ^:private allocation-finish-marker-jvm-type
  "Lcriterium/agent/Agent$AllocationFinishMarker;")

(defn- get-allocation-class
  "Returns the allocation class with proper type hint to avoid reflection."
  ^Class []
  @allocation-class)

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
     ;; Use reflection to check instance type
     (and @allocation-class (.isInstance (get-allocation-class) object))
     (let [object-type (.getField (get-allocation-class) "object_type")
           object-size (.getField (get-allocation-class) "object_size")
           call-class (.getField (get-allocation-class) "call_class")
           call-method (.getField (get-allocation-class) "call_method")
           call-file (.getField (get-allocation-class) "call_file")
           call-line (.getField (get-allocation-class) "call_line")
           alloc-class (.getField (get-allocation-class) "alloc_class")
           alloc-method (.getField (get-allocation-class) "alloc_method")
           alloc-file (.getField (get-allocation-class) "alloc_file")
           alloc-line (.getField (get-allocation-class) "alloc_line")
           thread-field (.getField (get-allocation-class) "thread")
           freed (.getField (get-allocation-class) "freed")
           obj-type (.get object-type object)]
       (when (and (not= obj-type allocation-start-marker-jvm-type)
                  (not= obj-type allocation-finish-marker-jvm-type))
         (swap! records conj
                {:object-type (internal->class-name obj-type)
                 :object_size (.get object-size object)
                 :call-class (some->
                              (.get call-class object)
                              blank->nil
                              internal->class-name)
                 :call-method (.get call-method object)
                 :call-file (.get call-file object)
                 :call-line (.get call-line object)
                 :alloc-class (some->
                               (.get alloc-class object)
                               blank->nil
                               internal->class-name)
                 :alloc-method (.get alloc-method object)
                 :alloc-file (.get alloc-file object)
                 :alloc-line (.get alloc-line object)
                 :thread (.get thread-field object)
                 :freed (.get freed object)})))

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

;;; Lazy Agent Initialization

(def ^:private handler-set?
  "Track whether we've set the handler on the Agent class."
  (atom false))

(defn- ensure-handler-set!
  "Ensure the data-fn callback is registered with the Agent class.

  Only sets the handler once, even if called multiple times.
  Requires the Agent class to be loaded."
  []
  (when-not @handler-set?
    (assert @wrapper-set-handler "Agent not loaded")
    (@wrapper-set-handler data-fn)
    (reset! handler-set? true)))

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
  (ensure-handler-set!)
  (let [cmd-num (commands cmd)]
    (when-not cmd-num
      (throw
       (IllegalArgumentException. (str "Unknown command: " (pr-str cmd)))))
    (let [^clojure.lang.IFn$LO f @wrapper-send-command]
      (assert f "Agent not loaded")
      (.invokePrim f cmd-num))))

;;; Agent State Management

(defn ^:internal agent-state
  "Get current agent state as a keyword.

  Returns :not-attached if agent is not loaded or not available.

  Implementation Notes:
  - Thread-safe but uncoordinated
  - Returns state keywords from states map"
  []
  (let [^clojure.lang.IFn$L f @wrapper-read-state]
    (assert f "Agent not loaded")
    (get states (.invokePrim f) :not-attached)))

(defn attached?
  "Returns true if the Criterium native agent is currently loaded.

  Checks whether the agent was loaded via -agentpath JVM arguments or
  programmatically via load-agent!. The agent is considered attached if
  its state is anything other than :not-attached.

  This is the core implementation used by criterium.agent/attached? and
  criterium.agent/loaded?."
  []
  (not= (agent-state) :not-attached))

;;; Allocation Tracing Control

(defn ^:internal allocation-start-marker
  "Create a marker allocation to track start of allocation sequence.

  Used to synchronize the start of allocation tracking by creating a
  recognizable allocation pattern.

  Implementation Notes:
  - Creates a specific allocation pattern
  - Filtered from final results
  - Used for state transition timing"
  []
  (assert @wrapper-start-marker "Agent not loaded")
  (@wrapper-start-marker))

(defn ^:internal allocation-finish-marker
  "Create a marker allocation to track end of allocation sequence."
  []
  (assert @wrapper-finish-marker "Agent not loaded")
  (@wrapper-finish-marker))

(defn ^:internal allocation-tracing-active?
  "Test if allocation tracing is currently active.

  Returns true only when the agent is in the :allocation-tracing-active state
  and fully initialized.

  Implementation Notes:
  - Thread-safe state check
  - Used to verify tracing preconditions
  - Optimized for frequent checking"
  []
  (= (agent-state) :allocation-tracing-active))

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
  (allocation-finish-marker)
  (System/gc)
  (loop [i 1000000]
    (when (and (pos? i)
               (not= (agent-state) :allocation-tracing-flushed))
      (allocation-finish-marker)
      (System/gc)
      (recur (unchecked-dec i))))
  (when (not= (agent-state) :allocation-tracing-flushed)
    (println "WARNING allocation tracing failed to stop promptly")))

(defn collect-allocation-records
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
     (collect-allocation-records)
     [@records res#]))

(defn with-allocation-tracing-disabled [body]
  `[nil (do ~@body)])

;;; Allocation Record Processing

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
