(ns criterium.agent
  "Interface to the Criterium native agent for allocation tracking and profiling.

  This namespace provides functions for tracking JVM heap allocations and garbage
  collection during benchmark execution. It uses a native agent to capture detailed
  allocation information with minimal overhead.

  The key concepts are:
  - Native Agent: A JVM agent that hooks into allocation events
  - Allocation Records: Detailed data about each object allocation
  - Thread Filtering: Ability to focus on allocations from specific threads

  Example usage:

  ```clojure
  (let [[allocations result] (with-allocation-tracing
                              (your-code-here))]
    ;; Filter to current thread
    (let [thread-allocs (filter (allocation-on-thread?) allocations)]
      (allocations-summary thread-allocs)))
  ```

  Note: The agent must be loaded via JVM args for allocation tracking to work."
  (:require
   [criterium.agent.core :as core]
   [criterium.jvm :as jvm]))

(defn attached?
  "Predicate for whether the criterium native agent is properly attached.

  Returns true if the agent was successfully loaded and initialized by the JVM,
  false otherwise. The agent must be attached for allocation tracking to work."
  []
  (core/attached?))

(defmacro with-allocation-tracing
  "Creates a scope in which all JVM heap allocations and releases are tracked.

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
  thread.  Filter the returned records with `thread-allocations` if that
  is all you are concerned with."
  [& body]
  (if (attached?)
    (core/with-allocation-tracing-enabled body)
    (core/with-allocation-tracing-disabled body)))

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
