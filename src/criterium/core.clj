;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.


;;;; Criterium - measures expression computation time over multiple invocations

;;;; Inspired by Brent Broyer's http://www.ellipticgroup.com/html/benchmarkingArticle.html
;;;; and also Haskell's Criterion

;;;; Unlike java solutions, this can benchmark general expressions rather than just functions.

(ns #^{:author "Hugo Duncan"
       :doc "Criterium measures the computation time of an expression.  It is
designed to address some of the pitfalls of benchmarking, and benchmarking on
the JVM in particular.

This includes:
  - statistical processing of multiple evaluations
  - inclusion of a warm-up period, designed to allow the JIT compiler to
    optimise its code
  - purging of gc before testing, to isolate timings from GC state prior
    to testing
  - a final forced GC after testing to estimate impact of cleanup on the
    timing results

Usage:
  (use 'criterium.core)
  (bench (Thread/sleep 1000) :verbose)
  (with-progress-reporting (bench (Thread/sleep 1000) :verbose))
  (report-result (benchmark (Thread/sleep 1000)) :verbose)
  (report-result (quick-bench (Thread/sleep 1000)))

References:
See http://www.ellipticgroup.com/html/benchmarkingArticle.html for a Java
benchmarking library.  The accompanying article describes many of the JVM
benchmarking pitfalls.

See http://hackage.haskell.org/package/criterion for a Haskell benchmarking
library that applies many of the same statistical techniques.
"
       :see-also [["http://github.com/hugoduncan/criterium" "Source code"]
		  ["http://hugoduncan.github.com/criterium" "API Documentation"]]}
  criterium.core
  (:use clojure.set
	criterium.stats)
  (:require criterium.well)
  (:import (java.lang.management ManagementFactory)))

;; (set! *warn-on-reflection* true)

;; this is taken from clojure.core
(defmacro add-doc {:private true} [name docstring]
  `(alter-meta! (var ~name)  assoc :doc ~docstring))

(def *use-mxbean-for-times* nil)

(def *final-gc-problem-threshold* 0.01)
(add-doc *final-gc-problem-threshold*
	 "Fraction of excution time allowed for final cleanup before a warning is issued.")

(def *s-to-ns* (* 1000 1000 1000)) ; in ns
(def *ns-to-s* 1e-9) ; in ns

(def *warmup-jit-period* (* 10 *s-to-ns*)) ; in ns
(add-doc *warmup-jit-period*
	 "Time period used to let the code run so that jit compiler can do its work.")

(def *sample-count* 60)
(add-doc *sample-count*
	 "Number of executions required")

(def *target-execution-time* (* 1 *s-to-ns*)) ; in ns
(add-doc *target-execution-time*
	 "Target elapsed time for execution for a single measurement.")

(def *max-gc-attempts* 100)
(add-doc *max-gc-attempts*
	 "Maximum number of attempts to run finalisers and gc.")

(def *default-benchmark-opts*
     {:max-gc-attempts *max-gc-attempts*
      :samples *sample-count*
      :target-execution-time *target-execution-time*
      :warmup-jit-period *warmup-jit-period*
      :confidence-interval 0.95})

(def *default-quick-bench-opts*
     {:max-gc-attempts *max-gc-attempts*
      :samples (/ *sample-count* 10)
      :target-execution-time (/ *target-execution-time*)
      :warmup-jit-period *warmup-jit-period*
      :confidence-interval 0.95})

;;; Progress reporting
(def *report-progress* nil)

(defn #^{:skip-wiki true}
  progress
  "Conditionally report progress to *out*."
  [& message]
  (when *report-progress*
    (apply println message)))

;;; Java Management interface
(defprotocol StateChanged
  "Interrogation of differences in a state."
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

(defn jvm-compilation-state
  "Returns the total compilation time for the JVM instance."
  []
  (let [bean (.. ManagementFactory getCompilationMXBean)]
    (JvmCompilationState (if (. bean isCompilationTimeMonitoringSupported)
			   (. bean getTotalCompilationTime)
			   -1))))

(defn jvm-jit-name
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
  "Return the operating system details."
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)]
    (. bean getSystemProperties)))

;;; OS Specific Code
(defn clear-cache-mac []
  (.. Runtime getRuntime (exec "/usr/bin/purge") waitFor))

(defn clear-cache-linux []
  ;; not sure how to deal with the sudo
  (.. Runtime getRuntime
      (exec "sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'") waitFor))

(defn clear-cache []
  (condp #(re-find %1 %2) (.. System getProperties (getProperty "os.name"))
    #"Mac" (clear-cache-mac)
    :else (println "WARNING: don't know how to clear disk buffer cache for "
		   (.. System getProperties (getProperty "os.name")))))

;;; Time reporting
(defn timestamp
  "Obtain a timestamp"
  [] (System/nanoTime))

(defn timestamp-2
  "Obtain a timestamp, possibly using MXBean."
  []
  (if *use-mxbean-for-times*
    (.. ManagementFactory getThreadMXBean getCurrentThreadCpuTime)
    (System/nanoTime)))

;;; Execution timing
(defmacro time-body
  "Returns a vector containing execution time and result of specified function."
  ([expr pre]
     `(do ~pre
	  (time-body ~expr)))
  ([expr]
     `(let [start# (timestamp)
	    ret# ~expr
	    finish# (timestamp)]
	[(- finish# start#) ret#])))

(defmacro time-body-with-jvm-state
  "Returns a vector containing execution time, change in loaded and unloaded
class counts, change in compilation time and result of specified function."
  ([expr pre]
     `(do ~pre
	  (time-body-with-jvm-state ~expr)))
  ([expr]
  `(let [cl-state# (jvm-class-loader-state)
	 comp-state# (jvm-compilation-state)
	 start# (timestamp)
	 ret# ~expr
	 finish# (timestamp)]
     [(- finish# start#)
      (merge-with - cl-state# (jvm-class-loader-state))
      (merge-with - comp-state# (jvm-compilation-state))
      ret#])))


;;; Memory reporting
(defn heap-used
  "Report a (inconsistent) snapshot of the heap memory used."
  []
  (let [runtime (Runtime/getRuntime)]
    (- (.totalMemory runtime) (.freeMemory runtime))))

(defn memory
  "Report a (inconsistent) snapshot of the memory situation."
  []
  (let [runtime (Runtime/getRuntime)]
    [ (.freeMemory runtime) (.totalMemory runtime) (.maxMemory runtime)]))

;;; Memory management
(defn force-gc
  "Force garbage collection and finalisers so that execution time associated
   with this is not incurred later. Up to max-attempts are made.
"
  ([] (force-gc *max-gc-attempts*))
  ([max-attempts]
     (progress "Cleaning JVM allocations ...")
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
  (progress "Checking GC...")
  (let [cleanup-time (first (time-body (force-gc)))
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
  `(do
     (progress "Warming up for JIT ...")
     (loop [elapsed# 0 count# 0]
       (if (> elapsed# ~warmup-period)
	 [elapsed# count#]
	 (recur (+ elapsed# (first (time-body ~expr))) (inc count#))))))

;;; Execution parameters
(defmacro estimate-execution-count
  "Estimate the number of executions required in order to have at least the
   specified execution period, check for the jvm to have constant class loader
   and compilation state."
  [execution-period expr]
  `(let [period# ~execution-period]
     (progress "Estimating execution count ...")
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
  "A function to execute an expression the given number of times, timing the
  complete execution."
  [n expr]
  `(fn [_#] (time-body (dotimes [_# ~n] ~expr))))

(defmacro collect-samples [sample-count execution-count expr]
  `(let [sample-count# ~sample-count
	 n-exec# ~execution-count]
     (progress "Running ...")
     (map (execute-expr n-exec# ~expr) (range 0 sample-count#))))

(defmacro run-benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   quick test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [iterations warmup-jit-period target-execution-time expr pre-expr]
  `(let [sample-count# ~iterations
	 warmup-jit-period# ~warmup-jit-period
	 target-execution-time# ~target-execution-time]
     (force-gc)
     (let [first-execution# (time-body ~expr)]
       (warmup-for-jit warmup-jit-period# ~expr)
       (let [n-exec# (estimate-execution-count target-execution-time# ~expr)
	     samples# (collect-samples sample-count# n-exec# ~expr)
	     sample-times# (map first samples#)
	     total# (reduce + 0 sample-times#)
	     final-gc-result# (final-gc-warn (final-gc total#))
	     ]
	 {:execution-count n-exec#
	  :sample-count sample-count#
	  :samples sample-times#
	  :results (map second samples#)
	  :total-time (/ total# 1e9)})))) ;; :average-time (/ total# sample-count# n-exec# 1e9)



(defn bootstrap-bca
  "Bootstrap a statistic. Statistic can produce multiple statistics as a vector
   so you can use juxt to pass multiple statistics.
   http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size alpha rng-factory]
  (progress "Bootstrapping ...")
  (let [bca (bca-nonparametric data statistic size alpha rng-factory)]
    (if (vector? bca)
      (bca-to-estimate alpha bca)
      (map (partial bca-to-estimate alpha) bca))))

(defn bootstrap
  "Bootstrap a statistic. Statistic can produce multiple statistics as a vector
   so you can use juxt to pass multiple statistics.
   http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size rng-factory]
  (progress "Bootstrapping ...")
  (let [samples (bootstrap-sample data statistic size rng-factory)
	transpose (fn [data] (apply map vector data))]
    (if (vector? (first samples))
      (map bootstrap-estimate samples)
      (bootstrap-estimate samples))))

;;; Outliers

(defn outlier-effect
  "Return a keyword describing the effect of outliers on the estimate of mean
  runtime."
  [var-out-min]
  (cond
    (< var-out-min 0.01) :unaffected
    (< var-out-min 0.1) :slight
    (< var-out-min 0.5) :moderate
    :else :severe))

(defn point-estimate [estimate]
  (first estimate))

(defn point-estimate-ci [estimate]
  (last estimate))

(defn outlier-significance
  "Find the significance of outliers given boostrapped mean and variance
estimates.
See http://www.ellipticgroup.com/misc/article_supplement.pdf, p17."
  [mean-estimate variance-estimate n]
  (progress "Checking outlier significance")
  (let [mean-block (point-estimate mean-estimate)
	variance-block (point-estimate variance-estimate)
	std-dev-block (Math/sqrt variance-block)
	mean-action (/ mean-block n)
	mean-g-min (/ mean-action 2)
	sigma-g (min (/ mean-g-min 4) (/ std-dev-block (Math/sqrt n)))
	variance-g (* sigma-g sigma-g)
	c-max (fn [t-min]
		(let [j0 (- mean-action t-min)
		      k0 (- (* n n j0 j0))
		      k1 (+ variance-block (- (* n variance-g)) (* n j0 j0))
		      det (- (* k1 k1) (* 4 variance-g k0))]
		  (Math/floor (/ (* -2 k0) (+ k1 (Math/sqrt det))))))
	var-out (fn [c]
		  (let [nmc (- n c)]
		    (* (/ nmc n) (- variance-block (* nmc variance-g)))))
	min-f (fn [f q r]
		(min (f q) (f r)))
	]
    (/ (min-f var-out 1 (min-f c-max 0 mean-g-min)) variance-block)))


(deftype outlier-count [low-severe low-mild high-mild high-severe]
  clojure.lang.IPersistentMap)


(defn add-outlier [low-severe low-mild high-mild high-severe counts x]
  (outlier-count
   (if (<= x low-severe)
     (inc (:low-severe counts))
     (:low-severe counts))
   (if (< low-severe x low-mild)
     (inc (:low-mild counts))
     (:low-mild counts))
   (if (> high-severe x high-mild)
     (inc (:high-mild counts))
     (:high-mild counts))
   (if (>= x high-severe)
     (inc (:high-severe counts))
     (:high-severe counts))))

(defn outliers
  "Find the outliers in the data using a boxplot technique."
  [data]
  (progress "Finding outliers ...")
  (reduce (apply partial add-outlier
		 (apply boxplot-outlier-thresholds
			((juxt first last) (quartiles (sort data)))))
	  (outlier-count 0 0 0 0)
	  data))

;;; options
(defn extract-report-options
  "Extract reporting options from the given options vector.  Returns a two
  element vector containing the reporting options followed by the non-reporting
  options"
  [opts]
  (let [known-options #{:os :runtime :verbose}
	option-set (set opts)]
    [(intersection known-options option-set)
     (remove #(contains? known-options %1) opts)]))

(defn add-default-options [options defaults]
  (let [time-periods #{:warmup-jit-period :target-execution-time}]
    (merge defaults
	   (into {} (map #(if (contains? time-periods (first %1))
			    [(first %1) (* (second %1) *s-to-ns*)]
			    %1)
			 options)))))

;;; User top level functions
(defmacro with-progress-reporting
  "Macro to enable progress reporting during the benchmark."
  [expr]
  `(binding [*report-progress* true]
     ~expr))

(defmacro benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [expr & options]
  `(let [options# (vector ~@options)
	 opts# (add-default-options
		(if (empty? options#) {} (apply assoc {} options#))
		*default-benchmark-opts*)
	 times# (run-benchmark (:samples opts#)
			       (:warmup-jit-period opts#)
			       (:target-execution-time opts#)
			       ~expr
			       (:pre opts#))
	 outliers# (outliers (:samples times#))
	 ci# (/ (:confidence-interval opts#) 2)
	 stats# (bootstrap-bca (:samples times#) (juxt mean variance)
			       1000 [0.5 ci# (- 1 ci#)]
			       criterium.well/well-rng-1024a)
	 analysis# (outlier-significance (first stats#) (second stats#)
					 (:sample-count times#))]
     (merge times#
	    {:outliers outliers#
	     :mean (scale-bootstrap-estimate
		    (first stats#) (/ 1e-9 (:execution-count times#)))
	     :variance (scale-bootstrap-estimate
			(second stats#) (/ 1e-18 (:execution-count times#)))
	     :outlier-variance analysis#
	     :confidence-interval (:confidence-interval opts#)})))



(defmacro quick-bench
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  ([expr & options]
     `(let [options# (vector ~@options)
	    opts# (add-default-options
		   (if (empty? options#) {} (apply assoc {} options#))
		   *default-quick-bench-opts*)
	    times# (run-benchmark (:samples opts#)
				  (:warmup-jit-period opts#)
				  (:target-execution-time opts#)
				  ~expr
				  (:pre opts#))
	    outliers# (outliers (:samples times#))
	    ci# (/ (:confidence-interval opts#) 2)
	    stats# (bootstrap-bca (:samples times#) (juxt mean variance)
				  500 [0.5 ci# (- 1 ci#)]
				  criterium.well/well-rng-1024a)
	    analysis# (outlier-significance (first stats#) (second stats#)
					 (:sample-count times#))]
	(merge times#
	       {:outliers outliers#
		:mean (scale-bootstrap-estimate
		       (first stats#) (/ 1e-9 (:execution-count times#)))
		:variance (scale-bootstrap-estimate
			   (second stats#) (/ 1e-18 (:execution-count times#)))
		:outlier-variance analysis#
		:confidence-interval (:confidence-interval opts#)}))))

(defn report-estimate [estimate unit significance]
  (print (point-estimate estimate) unit " "
	 (str (* significance 100) "% CI:")
	 (point-estimate-ci estimate)))

(defn report-estimate-sqrt [estimate unit significance]
  (print (Math/sqrt (point-estimate estimate)) unit
	 (str (* significance 100) "% CI:")
	 (map #(Math/sqrt %1) (point-estimate-ci estimate))))

(defn report-outliers [results]
  (let [outliers (:outliers results)
	values (vals outliers)
	labels {:unaffected  "unaffected"
		:slight "slightly inflated"
		:moderate "moderately inflated"
		:severe "severely inflated"}
	sample-count (:sample-count results)]
    (when (some pos? values)
      (println "Found" (reduce + values) "outliers in"
	       (:sample-count results) " samples ("
	       (* 100.0 (/ (reduce + values) (:sample-count results))) "%)")
    (dorun
     (map #(when (pos? %1)
	     (println " " %2 ":" %1 "(" (* 100.0 (/ %1 sample-count)) "%)"))
	  values
	  ["low-severe" "low-mild" "high-mild" "high-severe"]))

    (println " variance introduced by outliers:" (* (:outlier-variance results) 100.0) "%")
    (println " variance is" ((outlier-effect (:outlier-variance results)) labels) "by outliers")
    (println))))

(defn report-result [results & opts]
  (let [verbose (some #(= :verbose %) opts)
	show-os (or verbose (some #(= :os %) opts))
	show-runtime (or verbose (some #(= :runtime %) opts))]
    (when show-os
      (apply println
             (conj
              (apply vector (map #(%1 (os-details))
                                 [:arch :name :version :available-processors]))
			   "cpu(s)")))
    (when show-runtime
      (apply println (map #(%1 (runtime-details)) [:vm-name :vm-version]))
      (apply println "Runtime arguments:" (:input-arguments (runtime-details)))))
  (println "# evaluations :" (* (:execution-count results)
				(:sample-count results)))

  (print "Execution time mean : ")
  (report-estimate (:mean results) "sec" (:confidence-interval results))
  (println)

  (print "Execution time variance : ")
  (report-estimate-sqrt (:variance results) "sec" (:confidence-interval results))
  (println)

  (report-outliers results))

(defmacro bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
  to *out* in human readable format."
  [expr & opts]
  (let [[report-options options] (extract-report-options opts)]
    `(report-result (benchmark ~expr ~@options) ~@report-options)))

