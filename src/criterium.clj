;;;; Criterium - measures expression computation speed over multiple invocations

;;;; Inspired by Brent Broyer's http://www.ellipticgroup.com/html/benchmarkingArticle.html
;;;; See also Haskell's Criterion

;;;; Unlike java solutions, this can benchmark general expressions rather than just functions.

(ns criterium
  (:import (java.lang.management ManagementFactory)))

;; this is taken from clojure.core
(defmacro add-doc {:private true} [name docstring]
  `(alter-meta! (var ~name)  assoc :doc ~docstring))

(def *use-mxbean-for-times* nil)

(def *final-gc-problem-threshold* 0.01)
(add-doc *final-gc-problem-threshold*
	 "Fraction of excution time allowed for final cleanup before a warning is issued.")

(def *s-to-ns* (* 1000 1000 1000)) ; in ns

(def *warmup-jit-period* (* 10 *s-to-ns*)) ; in ns
(add-doc *warmup-jit-period*
	 "Time period used to let the code run so that jit compiler can do its work.")

(def *sample-count* 60)
(add-doc *sample-count*
	 "Number of executions required")

(def *target-execution-time* (* 1 *s-to-ns*)) ; in ns
(add-doc *target-execution-time*
	 "Target elapsed time for execution for a single measurement")


;;; Java Management interface
(defprotocol StateChanged
  (state-changed? [state] "Check to see if a state delta represents no change")
  (state-delta [state-1 state-2] "Return a state object for the difference between two states"))

(deftype JvmClassLoaderState [loaded-count unloaded-count]
  clojure.lang.IPersistentMap)

(extend-type ::JvmClassLoaderState
 StateChanged
 (state-changed? [state]
		(not (and (zero? (:loaded-count state)) (zero? (:unloaded-count state)))))
 (state-delta [state-1 state-2]
	      (apply JvmClassLoaderState (map - (vals state-1) (vals state-2)))))

(defn jvm-class-loader-state []
  (let [bean (.. ManagementFactory getClassLoadingMXBean)]
    (JvmClassLoaderState (. bean getLoadedClassCount)
			 (. bean getUnloadedClassCount))))


(deftype JvmCompilationState [compilation-time]
  clojure.lang.IPersistentMap)

(extend-type ::JvmCompilationState
 StateChanged
 (state-changed? [state]
		 (not (zero? (:compilation-time state))))
 (state-delta [state-1 state-2]
	      (apply JvmCompilationState (map - (vals state-1) (vals state-2)))))

(defn jvm-compilation-state []
  (let [bean (.. ManagementFactory getCompilationMXBean)]
    (JvmCompilationState (if (. bean isCompilationTimeMonitoringSupported)
			   (. bean getTotalCompilationTime)
			   -1))))

(defn jvm-jit-name
  "Returns the name of the JIT compiler"
  []
  (let [bean (.. ManagementFactory getCompilationMXBean)]
    (. bean getName)))

(defn os-details
  "Return the operating system details"
  []
  (let [bean (.. ManagementFactory getOperatingSystemMXBean)]
    {:arch (. bean getArch)
     :available-processors (. bean getAvailableProcessors)
     :name (. bean getName)
     :version (. bean getVersion)}))

(defn runtime-details
  "Return the operating system details"
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)]
    {:input-arguments (. bean getInputArguments)
     :name (. bean getName)
     :spec-name (. bean getSpecName)
     :spec-vendor (. bean getSpecVendor)
     :spec-version (. bean getSpecVersion)
     :vm-name (. bean getVmName)
     :vm-vendor (. bean getVmVendor)
     :vm-version (. bean getVmVersion)}))

(defn system-properties
  "Return the operating system details"
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)]
    (. bean getSystemProperties)))

;;; Time reporting
(defn timestamp
  "Obtain a timestamp"
  [] (System/nanoTime))

(defn timestamp-2
  "Obtain a timestamp, possibly using MXBean"
  []
  (if *use-mxbean-for-times*
    (.. ManagementFactory getThreadMXBean getCurrentThreadCpuTime)
    (System/nanoTime)))

;;; Execution timing
(defmacro time-body
  "Returns a vector containing execution time and result of specified function"
  [expr]
  `(let [start# (timestamp)
	 ret# ~expr
	 finish# (timestamp)]
     [(- finish# start#) ret#]))

(defmacro time-body-with-jvm-state
  "Returns a vector containing execution time, change in loaded and unloaded class counts, change in compilation time and result of specified function"
  [expr]
  `(let [cl-state# (jvm-class-loader-state)
	 comp-state# (jvm-compilation-state)
	 start# (timestamp)
	 ret# ~expr
	 finish# (timestamp)]
     [(- finish# start#)
      (merge-with - cl-state# (jvm-class-loader-state))
      (merge-with - comp-state# (jvm-compilation-state))
      ret#]))



;;; Memory reporting
(defn heap-used
  "Report a (inconsistent) snapshot of the heap memory used"
  []
  (let [runtime (Runtime/getRuntime)]
    (- (.totalMemory runtime) (.freeMemory runtime))))

(defn memory
  "Report a (inconsistent) snapshot of the memory situation"
  []
  (let [runtime (Runtime/getRuntime)]
    [ (.freeMemory runtime) (.totalMemory runtime) (.maxMemory runtime)]))

;;; Memory management
(defn reclaim-memory
  "Reclaim memory by running garbage collection and finalisers. Up to max-attempts are made to recover memory."
  ([] (reclaim-memory 100))
  ([max-attempts]
     (loop [memory-used (heap-used)
	    attempts 0]
       (System/runFinalization)
       (System/gc)
       (let [new-memory-used (heap-used)]
	 (if (and (or (pos? (.. ManagementFactory getMemoryMXBean getObjectPendingFinalizationCount))
		      (> memory-used new-memory-used))
		  (< attempts max-attempts))
	   (recur new-memory-used (inc attempts)))))))

(defn final-gc
  "Time a final clean up of JVM memory. If this time is significant compared to
  the runtime, then the runtime should maybe include this time."
  [execution-time]
  (let [cleanup-time (first (time-body (reclaim-memory)))
	fractional-time (/ cleanup-time execution-time)]
    [(> fractional-time *final-gc-problem-threshold*) fractional-time cleanup-time]))

(defn final-gc-warn [final-gc-result]
  (when (first final-gc-result)
    (println "WARNING: Final GC required" (* 100.0 (second final-gc-result)) "% of runtime"))
  final-gc-result)

;;; Compilation
(defmacro warmup-for-jit
  "Run expression for the given amount of time to enable JIT compilation."
  [warmup-period expr]
  `(loop [elapsed# 0 count# 0]
     (if (> elapsed# ~warmup-period)
       [elapsed# count#]
       (recur (+ elapsed# (first (time-body ~expr))) (inc count#)))))



;;; Execution parameters
(defmacro estimate-execution-count
  "Estimate the number of executions required in order to have at least the
   specified execution period, check for the jvm to have constant class loader
   and compilation state."
  [execution-period expr]
  `(let [period# ~execution-period]
     (loop [n# 1 cl-state# (jvm-class-loader-state) comp-state# (jvm-compilation-state)]
       (let [t# (first (time-body (dotimes [_# n#] ~expr)))
	     new-cl-state# (jvm-class-loader-state)
	     new-comp-state# (jvm-compilation-state)]
	 (if (and (>= t# period#)
		  (= cl-state# new-cl-state#)
		  (= comp-state# new-comp-state#))
	   n#
	   (recur (if (>= t# period#)
		    n#
		    (min (* 2 n#) (inc (int (* n# (/ period# t#))))))
		  new-cl-state# new-comp-state#))))))




;;; Execution
(defmacro execute-expr
  "A function to execute an expression the given number of times, timing the complete execution."
  [n expr]
  `(fn [_#] (time-body (dotimes [_# ~n] ~expr))))

(defn calc-total-time [a b]
  (+ a (first b)))


(defmacro run-benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   quick test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [iterations warmup-jit-period target-execution-time expr]
  `(let [sample-count# ~iterations
	 warmup-jit-period# ~warmup-jit-period
	 target-execution-time# ~target-execution-time]
     (reclaim-memory)
     (let [first-execution# (time-body ~expr)]
       (warmup-for-jit warmup-jit-period# ~expr)
       (let [n-exec# (estimate-execution-count target-execution-time# ~expr)
	     samples# (map (execute-expr n-exec# ~expr) (range 0 sample-count#))
	     total# (reduce calc-total-time 0 samples#)
	     final-gc-result# (final-gc-warn (final-gc total#))]
	 {:execution-count n-exec#
	  :sample-count sample-count#
	  :samples samples#
	  :total-time (/ total# 1e9)
	  :average-time (/ total# sample-count# n-exec# 1e9)}))))

(defmacro benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   quick test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  ([expr]
     `(run-benchmark *sample-count*
		     *warmup-jit-period*
		     *target-execution-time*
		     ~expr))
  ([iterations warmup-jit-period target-execution-time expr]
     `(run-benchmark ~iterations
		     (* ~warmup-jit-period *s-to-ns*)
		     (* ~target-execution-time *s-to-ns*)
		     ~expr)))

(defmacro quick-bench
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  ([expr]
     `(run-benchmark (/ *sample-count* 10)
		     (/ *warmup-jit-period* 10)
		     (/ *target-execution-time* 10)
		     ~expr)))


(defn report [results & opts]
  (let [verbose (some #(= :verbose %) opts)
	show-os (or verbose (some #(= :os %) opts))
	show-runtime (or verbose (some #(= :runtime %) opts))]
    (when show-os
      (apply println (conj (apply vector (map #(%1 (criterium/os-details)) [:arch :name :version :available-processors])) "cpu(s)")))
    (when show-runtime
      (apply println (map #(%1 (runtime-details)) [:vm-name :vm-version]))
      (apply println "Runtime arguments:" (:input-arguments (runtime-details)))))
  (println "# evaluations :" (* (:execution-count results)
				(:sample-count results)))
  (println "Average time :" (:average-time results)))
