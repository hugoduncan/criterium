(ns criterium.jvm.impl
  (:require
   [clojure.string :as str]
   [criterium.util.helpers :as util])
  (:import
   [java.lang.management
    GarbageCollectorMXBean
    ManagementFactory
    MemoryPoolMXBean
    MemoryUsage
    ThreadInfo]))

;;; Utils

(defn- val-sum
  ([] {})
  ([a b] (merge-with + a b)))

(defn- val-diff
  ([] {})
  ([a b] (merge-with - a b)))

(defn- name->keyword [n]
  (-> n
      str/lower-case
      (str/replace \space \-)
      keyword))

;;; ClassLoadingMXBean

(let [bean (.. ManagementFactory getClassLoadingMXBean)]
  (defn class-loader-counts
    []
    {:loaded-count   (. bean getTotalLoadedClassCount)
     :unloaded-count (. bean getUnloadedClassCount)})

  (defn set-verbose-classloading
    "Set whether the classloader is verbose or not."
    [flag]
    (. bean setVerbose (boolean flag))))

(defn class-loader-counts-change
  [first-sample last-sample]
  (val-diff last-sample first-sample))

;;; MemoryMXBean

(defn- memory-usage
  [^MemoryUsage usage]
  {:committed (.getCommitted usage)
   :init      (.getInit usage)
   :max       (.getMax usage)
   :used      (.getUsed usage)})

(defn- memory-usage-change
  [^MemoryUsage first-usage ^MemoryUsage last-usage]
  {:committed (- (.getCommitted last-usage) (.getCommitted last-usage))
   :used      (- (.getUsed last-usage) (.getUsed first-usage))})

(let [mem-bean (.. ManagementFactory getMemoryMXBean)]
  (defn finalization-sample
    ^long []
    (. mem-bean getObjectPendingFinalizationCount))

  (defn memory-sample
    []
    (let [heap     (. mem-bean getHeapMemoryUsage)
          non-heap (. mem-bean getNonHeapMemoryUsage)]
      {:heap     heap
       :non-heap non-heap}))

  (defn set-memory-verbose!
    "Set whether the memory collection system emits verbose output."
    [flag]
    (. mem-bean setVerbose flag)))

(defn memory
  "Return a map of the memory usage for the JVM instance."
  [memory-sample]
  (let [res   (util/update-vals memory-sample memory-usage)
        total (util/sum (:heap res) (:non-heap res))]
    (assoc res :total total)))

(defn memory-change
  "Return a map of the change in memory usage between first and last samples."
  [first-memory-sample last-memory-sample]
  (let [heap     (memory-usage-change
                  (:heap first-memory-sample)
                  (:heap last-memory-sample))
        non-heap (memory-usage-change
                  (:non-heap first-memory-sample)
                  (:non-heap last-memory-sample))]
    {:heap     heap
     :non-heap non-heap
     :total    (val-sum heap non-heap)}))

(defn finalization
  [finalization-sample]
  {:pending finalization-sample})

(defn finalization-change
  [^long first-sample ^long last-sample]
  {:pending (- last-sample first-sample)})

;;; CompilationMXBean

(let [bean (.. ManagementFactory getCompilationMXBean)]
  (if (. bean isCompilationTimeMonitoringSupported)
    (defn compilation-sample
      ^long []
      (. bean getTotalCompilationTime))
    (defn compilation-sample
      ^long []
      -1))

  (defn jit-name
    "Returns the name of the JIT compiler."
    []
    (. bean getName)))

(defn compilation
  [sample]
  {:time-ms sample})

(defn compilation-change
  [^long first-sample ^long last-sample]
  {:time-ms (- last-sample first-sample)})

;;; MemoryPoolMXBeans

(def memory-pool-beans (vec (.. ManagementFactory getMemoryPoolMXBeans)))

(defn memory-pool-names
  []
  (mapv #(.getName ^MemoryPoolMXBean %) memory-pool-beans))

(def memory-pool-keywords
  (mapv name->keyword (memory-pool-names)))

(defmacro memory-pool-beans-samples
  []
  `[~@(for [i (range (count memory-pool-beans))]
        `(.getUsage ^MemoryPoolMXBean (memory-pool-beans ~i)))])

(defn memory-pools-sample
  []
  (memory-pool-beans-samples))

(defn memory-pools
  [sample]
  (let [res (zipmap memory-pool-keywords (map memory-usage sample))]
    (assoc res :total  (reduce val-sum (vals res)))))

(defn memory-pools-change
  [first-sample last-sample]
  (let [change (mapv memory-usage-change first-sample last-sample)
        res    (zipmap memory-pool-keywords change)]
    (assoc res :total  (reduce val-sum (vals res)))))

;;; GarbageCollectorMXBeans

(def garbage-collector-beans
  (vec (.. ManagementFactory getGarbageCollectorMXBeans)))

(defmacro garbage-collector-bean-samples
  []
  `[~@(for [i (range (count garbage-collector-beans))]
        `(garbage-collector-bean-sample
          (garbage-collector-beans ~i)))])

(defn garbage-collector-names
  []
  (mapv #(.getName ^GarbageCollectorMXBean %) garbage-collector-beans))

(def garbage-collector-keywords
  (mapv name->keyword (garbage-collector-names)))

(defn- garbage-collector-bean-sample
  [^GarbageCollectorMXBean bean]
  {:count   (.getCollectionCount bean)
   :time-ms (.getCollectionTime bean)})

(defn garbage-collector-sample
  []
  (garbage-collector-bean-samples))

(defn garbage-collector
  [sample]
  (assoc
   (zipmap garbage-collector-keywords sample)
   :total (reduce val-sum sample)))

(defn garbage-collector-change
  [first-sample last-sample]
  (let [changes (mapv val-diff last-sample first-sample)]
    (assoc
     (zipmap garbage-collector-keywords changes)
     :total (reduce val-sum changes))))

;;; ThreadMXBean

(defn current-thread-id
  ^long []
  (.getId (Thread/currentThread)))

(let [bean (.. ManagementFactory getThreadMXBean)]

  (defn thread-ids
    []
    (vec (.getAllThreadIds bean)))

  (defn thread-info-map
    [^ThreadInfo thread-info]
    {:blocked-count   (.getBlockedCount thread-info)
     :blocked-time-ms (.getBlockedTime thread-info)
     :waited-count    (.getWaitedCount thread-info)
     :waited-time-ms  (.getWaitedTime thread-info)})

  (defn thread-cpu-time ^long [^long id]
    (.getThreadCpuTime bean id))

  (defn thread-user-time ^long [^long id]
    (.getThreadUserTime bean id))

  (defn thread-allocated-bytes
    ^long [^long id]
    (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean id))

  (defn thread-sample
    [^long id]
    {:thread-info (.getThreadInfo bean id)
     :cpu-time    (.getThreadCpuTime bean id)
     :user-time   (.getThreadUserTime bean id)
     :allocated   (.getThreadAllocatedBytes
                   ^com.sun.management.ThreadMXBean bean
                   id)})

  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (defn threads-summary-sample
    []
    {:count         (.getThreadCount bean)
     :daemon-count  (.getDaemonThreadCount bean)
     :started-count (.getTotalStartedThreadCount bean)})

  (defn set-thread-contention-monitoring-enabled [flag]
    (.setThreadContentionMonitoringEnabled bean flag))

  (defn set-thread-cpu-time-enabled [flag]
    (.setThreadCpuTimeEnabled bean flag)))

(defn thread
  [sample]
  (update sample :thread-info thread-info-map))

(defn thread-change
  [first-sample last-sample]
  (assoc
   (val-diff (dissoc last-sample :thread-info)
             (dissoc first-sample :thread-info))
   :thread-info (val-diff (thread-info-map (:thread-info last-sample))
                          (thread-info-map (:thread-info first-sample)))))

;;; OperatingSysteMXBean

(let [bean (.. ManagementFactory getOperatingSystemMXBean)]
  (defn os-details
    "Return the operating system details as a hash."
    []
    {:arch                 (. bean getArch)
     :available-processors (. bean getAvailableProcessors)
     :name                 (. bean getName)
     :version              (. bean getVersion)}))

;;; RuntimeMXBean

(let [bean  (.. ManagementFactory getRuntimeMXBean)
      props (. bean getSystemProperties)]

  (defn runtime-details
    []
    {:input-arguments        (. bean getInputArguments)
     :name                   (. bean getName)
     :spec-name              (. bean getSpecName)
     :spec-vendor            (. bean getSpecVendor)
     :spec-version           (. bean getSpecVersion)
     :vm-name                (. bean getVmName)
     :vm-vendor              (. bean getVmVendor)
     :vm-version             (. bean getVmVersion)
     :java-version           (get props "java.version")
     :java-runtime-version   (get props "java.runtime.version")
     :sun-arch-data-model    (get props "sun.arch.data.model")
     :clojure-version-string (clojure-version)
     :clojure-version        *clojure-version*})

  (defn system-properties
    []
    (. bean getSystemProperties)))
