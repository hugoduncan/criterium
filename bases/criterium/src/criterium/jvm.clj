(ns criterium.jvm
  "JVM data accessors"
  (:require
   [criterium.jvm.impl :as impl]))

;;; Elapsed time measurement

(defmacro timestamp
  "Return the current value of the JVM's high-resolution time source.

  The value is in nanoseconds. but does not necessarily have nanosecond
  resolution.

  aOnly to be used in calculating elapsed time using unchecked-subtract."
  [] `(System/nanoTime))

(defn elapsed-time
  "Return the elapsed nanoseconds between two nanosecond timestamps."
  ^long [^long first-timestamp ^long last-timestamp]
  (unchecked-subtract last-timestamp first-timestamp))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn wait
  "Utility function to wait the given time in ns.

  This busy waits, without releasing the thread in any way.  This is CPU
  intensive.

  It should have good accuracy down to about 10us.

  It is intended for use in verifying criterium itself.

  Returns nil, so as not to create any temporary objects when
  used in a measured.
  "
  [^long ns]
  (let [start (timestamp)]
    (loop []
      (if (>= (elapsed-time start (timestamp)) ns)
        nil
        (recur)))))

;;; GC control

(defn run-finalization!
  "Runs the finalization methods of any objects pending finalization.

  On return, the JVM has made a best effort to complete all outstanding
  finalizations."
  []
  (System/runFinalization))

(defn force-gc!
  "Runs the garbage collector.

  Suggest the JVM expend effort to collect unused objects.  On return,
  the JVM has made a best effort to reclaim space from all unreferenced
  objects."
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
  "Return a JvmClassLoaderState record with loaded class counts.

    Contains :loaded-count and :unloaded-count fields.

    Satisfies the StateChanged protocol, providing the state-changed?
    and state-delta methods.

    These are counts since the start of the JVM."
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-verbose-classloading
  "Set whether the classloader is verbose or not."
  [flag]
  (impl/set-verbose-classloading flag))

;;; CompilationMXBean

(defn compilation-sample
  "Return the total compilation time for the JVM instance in ms.

  Returns -1 if not supported."
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn finalization
  "Return the pending finalization count map for the JVM instance."
  ([] (impl/finalization (impl/finalization-sample)))
  ([^long finalization-sample] (impl/finalization finalization-sample)))

(defn finalization-change
  "Return the change in pending finalization count for the JVM instance."
  [^long first-sample ^long last-sample]
  (impl/finalization-change first-sample last-sample))

(defn memory-sample
  "Return a sample of the memory usage from the MemoryMxBean.
  This sample is a zero garbage sampler."
  []
  (impl/memory-sample))

(defn memory
  "Return a map of the memory usage for the JVM instance."
  ([] (impl/memory (impl/memory-sample)))
  ([memory-sample] (impl/memory memory-sample)))

(defn memory-change
  "Return a map of the change in memory usage between first and last samples."
  [first-memory-sample last-memory-sample]
  (impl/memory-change first-memory-sample last-memory-sample))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-memory-verbose!
  "Set whether the memory collection system emits verbose output."
  [flag]
  (impl/set-memory-verbose! flag))

;;; MemoryPoolMXBeans

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn memory-pool-names
  "Return the names of the JVM's memory pools."
  []
  (impl/memory-pool-names))

(defn memory-pools-sample
  "Return a sample of the memory pool usages for the JVM instance."
  []
  (impl/memory-pools-sample))

(defn memory-pools
  "Return a map of the the memory pool usages for the JVM instance."
  ([]
   (impl/memory-pools (memory-pools-sample)))
  ([memory-pools-sample]
   (impl/memory-pools memory-pools-sample)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn memory-pools-change
  "Return a map of the difference between two memory pool samples."
  [first-sample last-sample]
  (impl/memory-pools-change first-sample last-sample))

;;; GarbageCollectorMXBeans

(defn garbage-collector-names
  "Return the JVM garbage collector names."
  []
  (impl/garbage-collector-names))

(defn garbage-collector-keywords
  "Return the JVM garbage collector keywords."
  []
  impl/garbage-collector-keywords)

(defn garbage-collector-sample
  "Return a sample of the JVM garbage collector counts and times in ms."
  []
  (impl/garbage-collector-sample))

(defn garbage-collector
  "Return a map of the JVM garbage collector counts and times in ms."
  ([] (impl/garbage-collector (garbage-collector-sample)))
  ([sample] (impl/garbage-collector sample)))

(defn garbage-collector-change
  [first-sample last-sample]
  (impl/garbage-collector-change first-sample last-sample))

;;; ThreadMXBean

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn thread-ids
  "Return thread ID's for all threads"
  []
  (impl/thread-ids))

(defn current-thread-id
  "Return the Thread ID of the current thread."
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn thread-change
  "Return a map of data on the difference between two thread samples,
  Defaults to the current thread."
  [first-sample last-sample]
  (impl/thread-change first-sample last-sample))

(defn thread-cpu-time
  "Return a sample of the total cpu time consumed by a thread.
   Defaults to the current thread."
  (^long [] (impl/thread-cpu-time (current-thread-id)))
  (^long [id] (impl/thread-cpu-time id)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn thread-user-time
  "Return a sample of the total user time consumed by a thread.
   Defaults to the current thread."
  (^long [] (impl/thread-user-time (current-thread-id)))
  (^long [id] (impl/thread-user-time id)))

(defn thread-allocated-bytes ^long
  (^long [] (impl/thread-allocated-bytes (current-thread-id)))
  (^long [id] (impl/thread-allocated-bytes id)))

(defn thread-allocated-change
  (^long [^long first-sample ^long last-sample]
   (- last-sample first-sample)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-thread-contention-monitoring-enabled [flag]
  (impl/set-thread-contention-monitoring-enabled flag))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-thread-cpu-time-enabled [flag]
  (impl/set-thread-cpu-time-enabled flag))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro allocated-bytes
  "Return the difference in thread allocated bytes before and after the body."
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def metric-sample-fns
  {:class-loader-counts class-loader-counts
   :compilation         compilation-sample
   :garbage-collector   garbage-collector-sample
   :memory              memory-sample
   :memory-pools        memory-pools-sample
   :thread              thread-sample})

(defn collect-metric [k]
  ((metric-fns k)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn collect-metric-sample [k]
  ((metric-fns k)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn collect
  "Collect a metric map with the metrics specified by the keywords, kws."
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
  "Return the operating system details as a hash."
  []
  (impl/os-details))

;;; RuntimeMXBean

(defn runtime-details
  "Return the runtime details as a hash."
  []
  (impl/runtime-details))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn system-properties
  "Return the operating system details."
  []
  (impl/system-properties))

;;; Memory reporting

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn heap-used
  "Report a (inconsistent) snapshot of the heap memory used."
  []
  (let [runtime (Runtime/getRuntime)]
    (- (.totalMemory runtime) (.freeMemory runtime))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn runtime-memory
  "Report a (inconsistent) snapshot of the memory situation."
  []
  (let [runtime (Runtime/getRuntime)]
    {:free  (.freeMemory runtime)
     :total (.totalMemory runtime)
     :max   (.maxMemory runtime)}))
