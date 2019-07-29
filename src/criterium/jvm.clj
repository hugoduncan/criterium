(ns criterium.jvm
  "JVM data accessors"
  (:require [criterium
             [util :as util]]
            [clojure.string :as string])
  (:import [java.lang.management
            GarbageCollectorMXBean
            ManagementFactory
            MemoryPoolMXBean
            MemoryUsage]))


(defmacro timestamp
  "Obtain a timestamp using System/nanoTime."
  [] `(System/nanoTime))


(defn run-finalizers-and-gc
  "Run object finalizers and then GC"
  []
  (System/runFinalization)
  (System/gc))


(let [bean (.. ManagementFactory getClassLoadingMXBean)]
  (defn class-loader-counts
    "Return a JvmClassLoaderState record with loaded class counts.

    Contains :loaded-count and :unloaded-count fields.

    Satisfies the StateChanged protocol, providing the state-changed?
    and state-delta methods.

    These are counts since the start of the JVM."
    []
    {:loaded-count  (. bean getLoadedClassCount)
     :unloaded-count (. bean getUnloadedClassCount)}))


(let [bean (.. ManagementFactory getCompilationMXBean)]
  (defn compilation-time
    "Returns the total compilation time for the JVM instance.

    Returns -1 if not supported."
    []
    {:compilation-time (if (. bean isCompilationTimeMonitoringSupported)
                         (. bean getTotalCompilationTime)
                         -1)}))


(let [bean (.. ManagementFactory getMemoryMXBean)]
  (defn finalization-count
    "Return the finalization count for the JVM instance."
    []
    {:pending (. bean getObjectPendingFinalizationCount)}))


(defn- memory-usage
  [^MemoryUsage usage]
  {:committed (.getCommitted usage)
   :init      (.getInit usage)
   :max       (.getMax usage)
   :used      (.getUsed usage)})


(let [mem-bean (.. ManagementFactory getMemoryMXBean)]
  (defn memory
    "Return the memory usage for the JVM instance."
    []
    (let [heap     (memory-usage (. mem-bean getHeapMemoryUsage))
          non-heap (memory-usage (. mem-bean getNonHeapMemoryUsage))
          sum      (util/sum heap non-heap)]
      {:heap     heap
       :non-heap non-heap
       :total    sum    })))


(let [pools (.. ManagementFactory getMemoryPoolMXBeans)]
  (defn memory-pools
    "Return the finalization count for the JVM instance."
    []
    (let [pool-usaage (for [^MemoryPoolMXBean pool pools]
                        [(.getName pool) (memory-usage (.getUsage pool))])
          sum         (reduce util/sum (map second pool-usaage))]
      (into {} pool-usaage))))


(let [beans (.. ManagementFactory getGarbageCollectorMXBeans)
      kws   (reduce
              (fn [res ^GarbageCollectorMXBean bean]
                (let [n (. bean getName)]
                  (assoc res n (keyword (-> n
                                            string/lower-case
                                            (string/replace \space \-))))))
              {}
              beans)]
  (defn garbage-collector-stats
    "Return the garbage collection counts and times for the JVM instance.

    Returns a sequence of maps each with :name, :count, :name keys,
    for each of the system garbace collectors, and for their total,
    with the keyword :total."
    []
    (let [stats (reduce
                  (fn [res ^GarbageCollectorMXBean bean]
                    (assoc res (kws (. bean getName))
                           {:count (. bean getCollectionCount)
                            :time  (. bean getCollectionTime)}))
                  {}
                  beans)
          total (reduce
                  #(merge-with + %1 %2)
                  {}
                  (vals stats))]
      (assoc stats :total total))))


;;; Memory reporting
(defn heap-used
  "Report a (inconsistent) snapshot of the heap memory used."
  []
  (let [runtime (Runtime/getRuntime)]
    (- (.totalMemory runtime) (.freeMemory runtime))))


(defn runtime-memory
  "Report a (inconsistent) snapshot of the memory situation."
  []
  (let [runtime (Runtime/getRuntime)]
    {:free (.freeMemory runtime)
     :total (.totalMemory runtime)
     :max (.maxMemory runtime)}))

(defn verbose-classloading
  "Set whether the classloader is verbose or not."
  [flag]
  (let [bean (.. ManagementFactory getClassLoadingMXBean)]
    (. bean setVerbose (boolean flag))))


(defn jit-name
  "Returns the name of the JIT compiler."
  []
  (let [bean (.. ManagementFactory getCompilationMXBean)]
    (. bean getName)))


(defn os-details
  "Return the operating system details as a hash."
  []
  (let [bean (.. ManagementFactory getOperatingSystemMXBean)]
    {:arch (. bean getArch)
     :available-processors (. bean getAvailableProcessors)
     :name (. bean getName)
     :version (. bean getVersion)}))


(defn runtime-details
  "Return the runtime details as a hash."
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)
        props (. bean getSystemProperties)]
    {:input-arguments (. bean getInputArguments)
     :name (. bean getName)
     :spec-name (. bean getSpecName)
     :spec-vendor (. bean getSpecVendor)
     :spec-version (. bean getSpecVersion)
     :vm-name (. bean getVmName)
     :vm-vendor (. bean getVmVendor)
     :vm-version (. bean getVmVersion)
     :java-version (get props "java.version")
     :java-runtime-version (get props "java.runtime.version")
     :sun-arch-data-model (get props "sun.arch.data.model")
     :clojure-version-string (clojure-version)
     :clojure-version *clojure-version*}))


(defn system-properties
  "Return the operating system details."
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)]
    (. bean getSystemProperties)))


(defn wait
  "Utility function to wait the given time in ns, without releasing the thread.
  Returns the actual time waited, in nanoseconds.
  This is CPU intensive, and is for verifying criterium itself.
  It should have good accuracy down to about 10us."
  [ns]
  (let [start (timestamp)]
    (loop []
      (let [diff (- (timestamp) start)]
        (if (> diff ns)
          diff
          (recur))))))
