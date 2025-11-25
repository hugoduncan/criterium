(ns criterium.agent
    "Interface to the Criterium native agent for allocation tracking and profiling.

  This namespace provides functions for tracking JVM heap allocations and garbage
  collection during benchmark execution. It uses a native agent to capture detailed
  allocation information with minimal overhead.

  The key concepts are:
  - Native Agent: A JVM agent that hooks into allocation events
  - Allocation Records: Detailed data about each object allocation
  - Thread Filtering: Ability to focus on allocations from specific threads

  Auto-Loading:
  The agent automatically loads from bundled platform-specific binaries in the JAR
  for supported platforms (linux-x64, macos-x64) when you first use allocation
  tracking functions. For unsupported platforms or custom builds, use the -agentpath
  JVM argument.

  Example usage:

  ```clojure
  (let [[allocations result] (with-allocation-tracing
                              (your-code-here))]
    ;; Filter to current thread
    (let [thread-allocs (filter (allocation-on-thread?) allocations)]
      (allocations-summary thread-allocs)))
  ```

  Manual Loading:
  You can also manually control agent loading:

  ```clojure
  (require '[criterium.agent.runtime :as runtime])
  (runtime/load-agent!)  ; Explicitly load before use
  ```

  For more details, see the README in projects/agent/."
    (:require
     [criterium.agent.core :as core]
     [criterium.agent.runtime :as runtime]
     [criterium.jvm :as jvm]))

(defn attached?
      "Predicate for whether the criterium native agent is properly attached.

  Returns true if the agent was successfully loaded and initialized by the JVM,
  false otherwise. The agent must be attached for allocation tracking to work."
      []
      (core/attached?))

(defn loaded?
      "Returns true if the Criterium native agent is currently loaded.

  This is an alias for attached? and checks whether the agent was loaded
  via -agentpath JVM arguments or programmatically via load-agent!.

  Use this before attempting allocation tracking operations."
      []
      (runtime/loaded?))

(defn jvm-opts
      "Returns a vector of JVM arguments for loading the native agent.

  Returns a vector like [\"-agentpath:/tmp/criterium-agent-...\"] that can be
  used when spawning subprocesses or configuring REPL JVM options. Returns an
  empty vector if the agent is unavailable or the platform is unsupported.

  Useful for configuring JVM processes to use the bundled agent without manual
  -agentpath specification."
      []
      (if-let [path (runtime/agent-path)]
              [(str "-agentpath:" path)]
              []))

(defmacro with-allocation-tracing
          "Creates a scope in which all JVM heap allocations and releases are tracked.

  Auto-loads the native agent if it's not already loaded and available for the
  current platform. If the agent cannot be loaded, returns [nil result] without
  allocation tracking.

  Returns a vector of [allocation-records result] where:
  - allocation-records: A sequence of maps containing detailed allocation data:
    {:object-type  - Class of allocated object
     :object_size  - Size in bytes
     :call-class   - Class that triggered allocation
     :call-method  - Method that triggered allocation
     :call-file    - Source file of allocation
     :call-line    - Line number of allocation
     :alloc-class  - Class doing allocation
     :alloc-method - Method doing allocation
     :alloc-file   - Source file of allocator
     :alloc-line   - Line number of allocator
     :thread       - Thread ID of allocation
     :freed        - Whether object was freed}
  - result: The value returned by the body forms

  Note that the allocations tracked are not limited to the current
  thread. Filter the returned records with `allocation-on-thread?` if that
  is all you are concerned with."
          [& body]
          `(do
     ;; Auto-load agent if not already loaded
            (when-not (runtime/loaded?)
                      (try
                       (runtime/load-agent!)
                       (catch Exception e#
                              (println "WARNING: Failed to auto-load agent:" (.getMessage e#)))))
     ;; Execute with or without tracing based on availability
            (if (runtime/loaded?)
                ~(core/with-allocation-tracing-enabled body)
                ~(core/with-allocation-tracing-disabled body))))

(defn allocation-on-thread?
      "Returns a predicate function for filtering allocation records by thread.

  The returned function takes an allocation record and returns true if the
  allocation occurred on the specified thread. When called with no arguments,
  uses the current thread's ID.

  Useful for composing with filter/remove to analyze allocations by thread"
      ([] (core/allocation-on-thread? (jvm/current-thread-id)))
      ([thread-id] (core/allocation-on-thread? thread-id)))

(defn allocation-freed?
      "Predicate that returns true if the allocation record indicates the object was freed.

  An object is considered freed when it has been garbage collected during the
  allocation tracking session. This helps identify temporary allocations vs
  retained objects."
      [record] (core/allocation-freed? record))

(defn allocations-summary
      "Returns a summary of allocation statistics for the given records.

  Takes a sequence of allocation records and returns a map with:
  {:num-allocated   - Total number of objects allocated
   :num-freed      - Number of allocated objects that were freed
   :allocated-bytes - Total bytes allocated
   :freed-bytes    - Total bytes from freed objects}

  Useful for getting high-level metrics from allocation tracking results."
      [records]
      (core/allocations-summary records))

(with-allocation-tracing
 (comment
    ;; this is here to get the HeapSamplingInterval to come into effect.
  ))
