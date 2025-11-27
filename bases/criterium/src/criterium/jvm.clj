(ns criterium.jvm
  "JVM monitoring and management interface.

  Provides zero-garbage access to JVM metrics and controls via JMX
  management beans.

  Core capabilities include:

  Time Management
  Memory Management
  Thread Management
  JMX Bean Access

  Key design principles:
  - Zero garbage sampling methods for performance measurement
  - Thread-safe monitoring capabilities
  - Consistent snapshot semantics
  - High-precision timing functions

  Performance characteristics:
  - Sampling functions avoid allocation
  - Low-overhead monitoring options
  - Batch collection capabilities

  Usage notes:
  - Use -sample variants for time series collection
  - Monitor allocation in performance-sensitive code
  - Verify timing precision requirements"
  (:require
   [criterium.jvm.impl :as impl]))

;;; Elapsed time measurement

(defmacro timestamp
  "Return the current value of the JVM's high-resolution time source.

  The value is in nanoseconds. but does not necessarily have nanosecond
  resolution. The actual resolution depends on the JVM and OS.

  The value can only be used meaningfully by passing it to elapsed-time.
  Direct subtraction will fail for intervals including a clock wrap-around.

  Performance:
  - Zero allocation

  Returns:
    long - The current timestamp in nanoseconds"
  [] `(System/nanoTime))

(defn elapsed-time
  "Return the elapsed nanoseconds between two nanosecond timestamps.

  Safely handles clock wrap-around that may occur between timestamps.

  Parameters:
    first-timestamp - The earlier timestamp in nanoseconds
    last-timestamp  - The later timestamp in nanoseconds

  Performance:
    - Zero allocation
    - Uses primitive arithmetic

  Returns:
    long - The elapsed time in nanoseconds, which may be negative if
           last-timestamp is before first-timestamp"
  ^long [^long first-timestamp ^long last-timestamp]
  (unchecked-subtract last-timestamp first-timestamp))

(defn wait
  "Busy-wait for a specified number of nanoseconds.

  Spins in a tight loop checking elapsed time until the requested
  interval has passed. Uses high-resolution timer for precise delays.

  This is a CPU-intensive operation that:
  - Does not release the thread
  - Does not allow other threads to run
  - Consumes CPU continuously

  Parameters:
    ns - Time to wait in nanoseconds

  Performance:
    - Zero garbage allocation
    - ~10 microsecond minimum practical resolution
    - High CPU usage during wait
    - May be affected by OS scheduling

  Threading:
    - Blocks only the calling thread
    - Other threads continue normally
    - Timer interrupts still occur

  Returns:
    nil - Avoids object allocation for benchmarking

  Notes:
    Intended for criterium internal testing only.
    Not recommended for production timing control."
  [^long ns]
  (let [start (timestamp)]
    (loop []
      (when (< (elapsed-time start (timestamp)) ns)
        (recur)))))

;;; GC control

(defn run-finalization!
  "Run the finalization methods of any objects pending finalization.

  Request that the JVM run finalization methods of objects pending
  finalization. On return, the JVM has made a best effort to complete
  all outstanding finalizations.

  This is a hint to the JVM, not a guarantee. Some finalizers may still
  be pending after the call returns.

  Performance:
    - Execution time varies with number of pending finalizations
    - May block calling thread
    - Should not be called in performance-sensitive code

  Threading:
    - May temporarily impact performance of other threads"
  []
  (System/runFinalization))

(defn force-gc!
  "Run the garbage collector.

  Suggest the JVM expend effort to collect unused objects. On return,
  the JVM has made a best effort to reclaim space from all unreferenced
  objects.

  This is a hint to the JVM, not a guarantee. The JVM may:
  - Choose to ignore the request
  - Delay collection until later
  - Perform only a partial collection

  Performance:
    - Execution time varies with heap size and object count
    - Will temporarily impact performance of other threads
    - Should not be called in performance-sensitive code

  Threading:
    - Will impact performance of all threads"
  []
  (System/gc))

(defn run-finalization-and-force-gc!
  "Run object finalization and then force GC.
  This cleans up memory.
  Repeated invocation may free up more memory."
  []
  (run-finalization!)
  (force-gc!))

;;; ClassLoadingMXBean

(defn class-loader-counts
  "Return a map of class loader statistics from the JVM's ClassLoadingMXBean.

  Return counts of classes loaded and unloaded since JVM start.
  These counts are cumulative and monotonically increasing.

  Return value contains:
    :loaded-count   - Total number of classes loaded (long)
    :unloaded-count - Total number of classes unloaded (long)

  Performance:
    - Zero garbage when used with class-loader-counts-change
    - Safe for frequent sampling
    - Negligible CPU overhead

  Threading:
    - Consistent snapshot of both counts"
  []
  (impl/class-loader-counts))

(defn class-loader-counts-change
  "Return a JvmClassLoaderState record with loaded class counts.

    Contains :loaded-count and :unloaded-count fields.

    Satisfies the StateChanged protocol, providing the state-changed?
    and state-delta methods.

    These are counts since the start of the JVM."
  [first-sample last-sample]
  (impl/class-loader-counts-change first-sample last-sample))

(defn set-verbose-classloading
  "Set whether the classloader is verbose or not."
  [flag]
  (impl/set-verbose-classloading flag))

;;; CompilationMXBean

(defn compilation-sample
  "Return the total JIT compilation time for the JVM instance.

  Provides a zero-garbage sample of the cumulative time spent in JIT
  compilation.  This value monotonically increases while the JVM is
  running.

  Performance:
    - Zero garbage allocation
    - Safe for frequent sampling
    - Negligible CPU overhead

  Threading:
    - Atomic read of compilation time

  Returns:
    long - Total compilation time in milliseconds, or -1 if unsupported"
  ^long []
  (impl/compilation-sample))

(defn compilation
  "Returns a compilation time map for the JVM instance in ms.

  The :time-ms key will contain the total compilation time, or -1 if
  unsupported."
  ([] (impl/compilation (impl/compilation-sample)))
  ([sample] (impl/compilation sample)))

(defn compilation-change
  [^long first-sample ^long last-sample]
  (impl/compilation-change first-sample last-sample))

(defn jit-name
  "Returns the name of the JIT compiler."
  []
  (impl/jit-name))

;;; MemoryMXBean

(defn finalization-sample
  "Return the pending finalization count for the JVM instance."
  ^long []
  (impl/finalization-sample))

(defn finalization
  "Return the pending finalization count map for the JVM instance."
  ([] (impl/finalization (impl/finalization-sample)))
  ([^long finalization-sample] (impl/finalization finalization-sample)))

(defn finalization-change
  "Return the change in pending finalization count for the JVM instance."
  [^long first-sample ^long last-sample]
  (impl/finalization-change first-sample last-sample))

(defn memory-sample
  "Return a sample of the memory usage from the MemoryMXBean.

  Captures current heap and non-heap memory usage statistics.
  The sample can be passed to memory or memory-change functions
  for detailed analysis.

  Performance:
    - Zero garbage allocation
    - Safe for frequent sampling
    - Negligible CPU overhead

  Threading:
    - Provides consistent snapshot of memory state

  Returns:
    Implementation specific opaque value suitable for memory and memory-change"
  []
  (impl/memory-sample))

(defn memory
  "Return a map of memory usage statistics for the JVM instance.

  Takes an optional memory-sample. If not provided, takes a new sample.
  Use with memory-sample when tracking changes over time.

  Return value contains:
    :heap     - Current heap memory usage in bytes
    :non-heap - Current non-heap memory usage in bytes
    Each contains:
      :committed - Amount of memory guaranteed to be available
      :init      - Initial amount of memory requested
      :max       - Maximum amount of memory available
      :used      - Amount of memory currently used

  Performance:
    - Allocates return map
    - Use memory-sample for zero-garbage time series

  Threading:
    - Values are consistent snapshot at sample time"
  ([] (impl/memory (impl/memory-sample)))
  ([memory-sample] (impl/memory memory-sample)))

(defn memory-change
  "Return a map of the change in memory usage between first and last samples."
  [first-memory-sample last-memory-sample]
  (impl/memory-change first-memory-sample last-memory-sample))

(defn set-memory-verbose!
  "Set whether the memory collection system emits verbose output."
  [flag]
  (impl/set-memory-verbose! flag))

;;; MemoryPoolMXBeans

(defn memory-pool-names
  "Return the names of the JVM's memory pools."
  []
  (impl/memory-pool-names))

(defn memory-pools-sample
  "Return a sample of all memory pool usage statistics.

  Captures usage statistics for all JVM memory pools including:
  - Heap pools (Eden, Survivor, Old/Tenured)
  - Non-heap pools (Metaspace, CodeCache)

  Performance:
    - Zero garbage allocation
    - Safe for frequent sampling
    - Negligible CPU overhead

  Threading:
    - Provides consistent snapshot across all pools

  Returns:
    Implementation specific opaque value for use with memory-pools
    or memory-pools-change functions"
  []
  (impl/memory-pools-sample))

(defn memory-pools
  "Return a map of memory pool usage statistics.

  Takes an optional memory-pools-sample. If not provided, takes a
  new sample. Use with memory-pools-sample for efficient time
  series collection.

  Return value is a map of pool-keyword to statistics:
      :init    - Initial size in bytes
      :used    - Currently used bytes
      :committed - Guaranteed available bytes
      :max     - Maximum possible bytes or -1 if undefined

  Performance:
    - Allocates return map
    - Use memory-pools-sample for zero-garbage time series

  Threading:
    - Values are consistent snapshot at sample time"
  ([]
   (impl/memory-pools (memory-pools-sample)))
  ([memory-pools-sample]
   (impl/memory-pools memory-pools-sample)))

(defn memory-pools-change
  "Return a map of the difference between two memory pool samples."
  [first-sample last-sample]
  (impl/memory-pools-change first-sample last-sample))

;;; GarbageCollectorMXBeans

(defn garbage-collector-names
  "Return the names of all garbage collectors in the JVM.

  The names are implementation dependent strings that identify
  the garbage collectors configured for this JVM instance.

  Performance:
    - Allocates string array
    - Result can be cached as collectors don't change
    - Not recommended for frequent calls

  Returns:
    seq of strings - Names of all garbage collectors"
  []
  (impl/garbage-collector-names))

(defn garbage-collector-keywords
  "Return the JVM garbage collector keywords."
  []
  impl/garbage-collector-keywords)

(defn garbage-collector-sample
  "Return a sample of garbage collector statistics.

  Captures the cumulative collection counts and times from all GC beans.
  Values monotonically increase during JVM lifetime.

  Performance:
    - Zero garbage allocation
    - Safe for frequent sampling
    - Negligible CPU overhead

  Threading:
    - Provides consistent snapshot across all collectors

  Returns:
    Implementation specific opaque value for use with garbage-collector
    or garbage-collector-change functions"
  []
  (impl/garbage-collector-sample))

(defn garbage-collector
  "Return a map of garbage collector statistics.

  Takes an optional garbage-collector-sample. If not provided,
  takes a new sample. Use with garbage-collector-sample for
  efficient time series collection.

  Return value contains map of collector-keyword to statistics:
    :count     - Number of collections (long)
    :time-ms   - Total collection time in ms (long)

  Performance:
    - Allocates return map
    - Use garbage-collector-sample for zero-garbage time series

  Threading:
    - Values are consistent snapshot at sample time"
  ([] (impl/garbage-collector (garbage-collector-sample)))
  ([sample] (impl/garbage-collector sample)))

(defn garbage-collector-change
  [first-sample last-sample]
  (impl/garbage-collector-change first-sample last-sample))

;;; ThreadMXBean

(defn thread-ids
  "Return thread ID's for all threads"
  []
  (impl/thread-ids))

(defn current-thread-id
  "Return the Thread ID of the current thread.

  This ID is unique within a single JVM session but may be reused
  after thread termination in a long-running process.

  Performance:
    - Zero garbage
    - Very low overhead
    - Safe for frequent calls

  Threading:
    - Each thread sees its own ID

  Returns:
    long - Unique identifier for the current thread"
  ^long []
  (impl/current-thread-id))

(defn thread-sample
  "Return a zero garbage sample of the thread,
  Defaults to the current thread."
  ([] (thread-sample (current-thread-id)))
  ([^long thread-id] (impl/thread-sample thread-id)))

(defn thread
  "Return a map of data on the thread,
  Defaults to the current thread."
  ([] (impl/thread (thread-sample (current-thread-id))))
  ([sample] (impl/thread sample)))

(defn thread-change
  "Return a map of data on the difference between two thread samples,
  Defaults to the current thread."
  [first-sample last-sample]
  (impl/thread-change first-sample last-sample))

(defn thread-cpu-time
  "Return the total CPU time consumed by a thread in nanoseconds.

  Returns the total CPU time for a given thread ID or the current thread
  if no ID is provided. Includes both user and system time.

  The value is monotonically increasing for a given thread.
  May return -1 if CPU time measurement is disabled.

  Parameters:
    id - Optional thread ID (defaults to current thread)

  Performance:
    - Zero garbage
    - Low overhead when enabled
    - Safe for frequent sampling

  Threading:
    - Minimal impact on target thread

  Returns:
    long - Nanoseconds of CPU time used, or -1 if unsupported"
  (^long [] (impl/thread-cpu-time (current-thread-id)))
  (^long [id] (impl/thread-cpu-time id)))

(defn thread-user-time
  "Return a sample of the total user time consumed by a thread.
   Defaults to the current thread."
  (^long [] (impl/thread-user-time (current-thread-id)))
  (^long [id] (impl/thread-user-time id)))

(defn thread-allocated-bytes
  "Return the total bytes allocated by a thread.

  Returns cumulative bytes allocated by given thread ID or current thread
  if no ID provided. This is a monotonically increasing value that
  includes both live and garbage collected objects.

  Parameters:
    id - Optional thread ID (defaults to current thread)

  Performance:
    - Zero garbage
    - Low overhead when enabled
    - Safe for frequent sampling

  Threading:
    - Each thread's allocations tracked independently

  Returns:
    long - Total bytes allocated by the thread, or -1 if unsupported"
  ^long
  (^long [] (impl/thread-allocated-bytes (current-thread-id)))
  (^long [id] (impl/thread-allocated-bytes id)))

(defn thread-allocated-change
  "Calculate the bytes allocated between two thread allocation samples.

  Takes two samples from thread-allocated-bytes and returns the
  difference, handling possible counter wrap-around.

  Parameters:
    first-sample - Earlier allocation sample
    last-sample  - Later allocation sample

  Performance:
    - Zero garbage
    - Uses primitive arithmetic

  Returns:
    long - Bytes allocated between samples, may be negative if
           last-sample is before first-sample"
  ^long [^long first-sample ^long last-sample]
  (- last-sample first-sample))

(defn set-thread-contention-monitoring-enabled [flag]
  (impl/set-thread-contention-monitoring-enabled flag))

(defn set-thread-cpu-time-enabled [flag]
  (impl/set-thread-cpu-time-enabled flag))

(defmacro allocated-bytes
  "Measure bytes allocated during execution of body expressions.

  Captures thread allocation before and after executing the body.
  Returns a vector containing the bytes allocated and the body's result.

  The allocation measurement includes both live objects and garbage.
  Thread allocation tracking must be enabled for accurate results.

  Performance:
    - Two allocation tracking calls
    - Body executes exactly once
    - Minimal overhead around body

  Returns:
    [long, any] - Vector containing:
      - Bytes allocated during body execution (may be negative)
      - Result of body evaluation"
  [& body]
  `(let [i#   (thread-allocated-bytes)
         res# (do ~@body)]
     [(unchecked-subtract (thread-allocated-bytes) i#) res#]))

;;; keyword based invocation

(def metric-fns
  {:class-loader-counts class-loader-counts
   :compilation         compilation
   :garbage-collector   garbage-collector
   :memory              memory
   :memory-pools        memory-pools
   :thread              thread})

(def metric-sample-fns
  {:class-loader-counts class-loader-counts
   :compilation         compilation-sample
   :garbage-collector   garbage-collector-sample
   :memory              memory-sample
   :memory-pools        memory-pools-sample
   :thread              thread-sample})

(defn collect-metric
  "Collect a single metric by key from the JVM management beans.

  Takes a keyword identifying the metric to collect. Valid keys are:
    :class-loader-counts - Class loading statistics
    :compilation        - JIT compilation statistics
    :garbage-collector  - GC statistics
    :memory            - Memory usage statistics
    :memory-pools      - Detailed memory pool statistics
    :thread            - Thread statistics

  Performance:
    - Allocates return value
    - May trigger JMX bean creation
    - Use metric-sample-fns for zero-garbage time series

  Returns:
    map - Metric data structure specific to the requested metric"
  [k]
  ((metric-fns k)))

(defn collect-metric-sample [k]
  ((metric-fns k)))

(defn collect
  "Collect multiple metrics from JVM management beans.

  Takes a sequence of metric keywords and returns a map of metric data.
  See collect-metric for valid metric keywords.

  Parameters:
    kws - Sequence of metric keywords to collect

  Performance:
    - Allocates return map and intermediate structures
    - Makes multiple JMX calls
    - Consider using sample functions for time series data

  Returns:
    map - Mapping of metric keywords to their current values"
  [kws]
  (zipmap kws (map collect-metric kws)))

;; (defn collect-diff
;;   "Collect the difference to the metrics in the given metric map."
;;   [ms]
;;   (reduce-kv
;;    (fn [result k v]
;;      (assoc result k (util/diff (collect-metric k) v)))
;;    {}
;;    ms))

;;; OperatingSysteMXBean

(defn os-details
  "Return information about the operating system.

  Provides static information about the OS where the JVM is running.
  This information does not change during JVM lifetime.

  Return value contains:
    :name                   - Operating system name
    :version                - OS version string
    :arch                   - OS architecture
    :available-processors   - Number of available processors

  Performance:
    - Allocates return map
    - Result can be cached as values don't change
    - Not recommended for frequent calls

  Returns:
    map - Operating system information"
  []
  (impl/os-details))

;;; RuntimeMXBean

(defn runtime-details
  "Return information about the Java runtime environment.

  Provides static information about the JVM instance.
  Most values do not change during JVM lifetime.

  Return value contains:
    :vm-name              - JVM name (eg 'Java HotSpot(TM) 64-Bit Server VM')
    :vm-vendor            - JVM vendor
    :vm-version           - JVM version
    :java-version         - JDK version
    :java-runtime-version - JDK runtime version
    :input-arguments      - List of JVM arguments

  Performance:
    - Allocates return map
    - Most values can be cached
    - Not recommended for frequent calls

  Returns:
    map - Java runtime information"
  []
  (impl/runtime-details))

(defn system-properties
  "Return the operating system details."
  []
  (impl/system-properties))

;;; Memory reporting

(defn heap-used
  "Return the current heap memory usage in bytes.

  Provides a quick but potentially inconsistent view of heap usage.
  The value may be incorrect if memory usage changes during measurement.

  This is a lightweight alternative to memory-sample when:
  - Approximate values are acceptable
  - Minimal overhead is required
  - Consistency guarantees aren't needed

  Performance:
    - Minimal overhead
    - Makes two JVM calls
    - May allocate on some JVMs

  Threading:
    - Thread safe
    - No consistency guarantee between measurements

  Returns:
    long - Approximate bytes of heap currently in use"
  []
  (let [runtime (Runtime/getRuntime)]
    (- (.totalMemory runtime) (.freeMemory runtime))))

(defn runtime-memory
  "Return a snapshot of JVM memory metrics.

  Provides a quick but potentially inconsistent view of memory usage.
  Values may be inconsistent if memory changes during collection.

  Return value contains:
    :free  - Currently unused memory in bytes
    :total - Current heap size in bytes
    :max   - Maximum possible heap size in bytes

  Performance:
    - Makes three JVM calls
    - Allocates return map
    - Lighter weight than memory-sample

  Threading:
    - Thread safe
    - No consistency guarantee between values

  Returns:
    map - Basic JVM memory statistics"
  []
  (let [runtime (Runtime/getRuntime)]
    {:free  (.freeMemory runtime)
     :total (.totalMemory runtime)
     :max   (.maxMemory runtime)}))
