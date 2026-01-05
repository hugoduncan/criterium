(ns criterium.agent
  "Interface to the Criterium native agent for allocation tracking and call tracing.

  This namespace provides functions for:
  - Tracking JVM heap allocations during benchmark execution
  - Tracing method calls to build call graphs

  Key Features:
  - Allocation Tracking: Capture detailed information about object allocations
  - Call Tracing: Record method entry/exit to build hierarchical call trees

  Agent Loading:
  Due to JVMTI limitations, the agent MUST be loaded at JVM startup using
  -agentpath for full functionality. Start your JVM with:
    clojure -J-agentpath:/path/to/libcriterium.dylib -M:dev

  Or use (jvm-opts) to get the correct path, then restart your JVM with that option.

  Example usage:

  ```clojure
  ;; Allocation tracking
  (let [[allocations result] (with-allocation-tracing
                              (your-code-here))]
    (let [thread-allocs (filter (allocation-on-thread?) allocations)]
      (allocations-summary thread-allocs)))

  ;; Call tracing
  (let [[call-tree result] (with-call-tracing
                            (your-code-here))]
    ;; call-tree is a nested map with :class, :method, :call-count, :children
    call-tree)
  ```

  For more details, see the README in projects/agent/."
  (:require
   [clojure.string :as str]
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
  `(if (attached?)
     (let [active?# (core/allocation-tracing-active?)
           res# (if active?#
                  (do ~@body)
                  (try
                    (core/allocation-tracing-start!)
                    ~@body
                    (finally
                      (core/allocation-tracing-stop!))))]
       (core/collect-allocation-records)
       [@core/records res#])
     [nil (do ~@body)]))

(defmacro with-call-tracing
  "Creates a scope in which all method calls are traced to build a call tree.

  Captures method entry and exit events during execution of body forms and
  builds a hierarchical call tree representing the observed call graph.

  If the agent is not attached, returns [nil result] without tracing.

  Returns a vector of [call-tree result] where:
  - call-tree: A nested map structure representing the call hierarchy:
    {:class       - Class name in standard format (e.g. \"myapp.Core\")
     :method      - Method name (e.g. \"process\")
     :file        - Source file name (may be nil)
     :line        - Line number (-1 if unknown)
     :call-count  - Number of times this call path was executed
     :children    - Vector of child call nodes}
  - result: The value returned by the body forms

  Note: Method tracing captures ALL method calls across all threads.
  The call tree represents the aggregated call graph, not per-thread traces.

  Warning: Method tracing has significant overhead. Use for profiling and
  debugging, not for production benchmarks."
  [& body]
  `(if (attached?)
     (let [active?# (core/method-tracing-active?)
           res# (if active?#
                  (do ~@body)
                  (try
                    (core/method-tracing-start!)
                    ~@body
                    (finally
                      (core/method-tracing-stop!))))
           ;; Filter out criterium infrastructure from the trace roots
           raw-trees# (core/collect-method-call-tree)
           filtered-trees# (keep #(filter-call-tree % criterium-infrastructure-filter)
                                 raw-trees#)
           ;; Return first filtered root (user code), or nil if none remain
           result-tree# (first filtered-trees#)]
       [result-tree# res#])
     [nil (do ~@body)]))

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

;;; Call Tree Filtering

(defn- matches-any-prefix?
  "Returns true if class-name starts with any of the given prefixes."
  [class-name prefixes]
  (boolean
   (and class-name
        (some #(str/starts-with? class-name %) prefixes))))

(defn- flatten-promoted
  "Flatten promoted children markers recursively."
  [children]
  (mapcat (fn [child]
            (if (:promoted-children child)
              (flatten-promoted (:promoted-children child))
              [child]))
          children))

(defn- filter-node
  "Filter a single call tree node according to filter options.
  Returns nil if node should be excluded, or the filtered node."
  [node {:keys [exclude-packages stop-at-packages max-depth] :as opts} ^long depth]
  (when node
    (let [class-name (:class node)]
      (cond
        ;; Depth limit reached - exclude this node
        (and max-depth (> depth (long max-depth)))
        nil

        ;; Excluded package - skip this node but process children
        (and exclude-packages (matches-any-prefix? class-name exclude-packages))
        (let [raw-children (keep #(filter-node % opts depth) (:children node))
              filtered-children (flatten-promoted raw-children)]
          ;; Return children promoted up (may be empty or multiple)
          ;; We return a special marker to indicate promotion
          (when (seq filtered-children)
            {:promoted-children filtered-children}))

        ;; Stop-at package - keep node but truncate children
        (and stop-at-packages (matches-any-prefix? class-name stop-at-packages))
        (assoc node :children [])

        ;; Normal case - filter children recursively
        :else
        (let [next-depth (inc depth)
              raw-children (keep #(filter-node % opts next-depth) (:children node))
              filtered-children (flatten-promoted raw-children)]
          (assoc node :children (vec filtered-children)))))))

(defn filter-call-tree
  "Filter a call tree according to filter options.

  Options:
  - :exclude-packages - Set of package prefixes to exclude entirely.
    Nodes with matching classes are removed, their children promoted up.
  - :stop-at-packages - Set of package prefixes where traversal stops.
    Matching nodes are kept but their children are truncated.
  - :max-depth - Maximum depth to include (1 = root only, 2 = root + children, etc.)

  Returns the filtered call tree, or nil if the root is excluded."
  [call-tree opts]
  (when call-tree
    (let [result (filter-node call-tree opts 1)]
      (cond
        (nil? result) nil
        (:promoted-children result) (first (:promoted-children result))
        :else result))))

;;; Predefined Filters

(def jdk-filter
  "Filter that excludes JDK internal packages.
  Use with filter-call-tree to remove JDK implementation details."
  {:exclude-packages #{"java." "javax." "jdk." "sun." "com.sun."}})

(def clojure-core-boundary-filter
  "Filter that stops traversal at clojure.core and clojure.lang boundaries.
  Shows calls into Clojure core but not the internal implementation."
  {:stop-at-packages #{"clojure.core" "clojure.lang."}})

(def criterium-infrastructure-filter
  "Filter that excludes criterium's own infrastructure from call traces.
  Applied by default in with-call-tracing to avoid capturing the tracing
  machinery itself (method-tracing-stop!, agent-command, etc.)."
  {:exclude-packages #{"criterium.agent"}})

(with-allocation-tracing
  (comment
    ;; this is here to get the HeapSamplingInterval to come into effect.
    ))
