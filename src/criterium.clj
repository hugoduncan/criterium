;;;; Criterium - measures expression computation speed over multiple invocations

;;;; Inspired by Brent Broyer's http://www.ellipticgroup.com/html/benchmarkingArticle.html
;;;; and also Haskell's Criterion

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
      :sample *sample-count*
      :target-execution-time *target-execution-time*
      :warmup-jit-period *warmup-jit-period*})

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

;;; OS Specific Code
(defn clear-cache-mac []
  (.. Runtime getRuntime (exec "/usr/bin/purge") waitFor))

(defn clear-cache-linux []
  ;; not sure how to deal with the sudo
  (.. Runtime getRuntime (exec "sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'") waitFor))

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
  "Obtain a timestamp, possibly using MXBean"
  []
  (if *use-mxbean-for-times*
    (.. ManagementFactory getThreadMXBean getCurrentThreadCpuTime)
    (System/nanoTime)))

;;; Execution timing
(defmacro time-body
  "Returns a vector containing execution time and result of specified function"
  ([expr pre]
     `(do ~pre
	  (time-body ~expr)))
  ([expr]
     `(let [start# (timestamp)
	    ret# ~expr
	    finish# (timestamp)]
	[(- finish# start#) ret#])))

(defmacro time-body-with-jvm-state
  "Returns a vector containing execution time, change in loaded and unloaded class counts, change in compilation time and result of specified function"
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
(defn force-gc
  "Force garbage collection and finalisers so that execution time associated
   with this is not incurred later. Up to max-attempts are made.
"
  ([] (force-gc *max-gc-attempts*))
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
  [iterations warmup-jit-period target-execution-time expr pre-expr]
  `(let [sample-count# ~iterations
	 warmup-jit-period# ~warmup-jit-period
	 target-execution-time# ~target-execution-time]
     (force-gc)
     (let [first-execution# (time-body ~expr)]
       (warmup-for-jit warmup-jit-period# ~expr)
       (let [n-exec# (estimate-execution-count target-execution-time# ~expr)
	     samples# (map (execute-expr n-exec# ~expr) (range 0 sample-count#))
	     total# (reduce calc-total-time 0 samples#)
	     final-gc-result# (final-gc-warn (final-gc total#))]
	 {:execution-count n-exec#
	  :sample-count sample-count#
	  :samples (map first samples#)
	  :results (map second samples#)
	  :total-time (/ total# 1e9)})))) ;; :average-time (/ total# sample-count# n-exec# 1e9)


;;; Statistics
(defn sqr
  "Square of argument"
  [x] (* x x))

(defn mean
  "Arithmetic mean of data."
  [data]
  (/ (reduce + data) (count data)))

(defn sum
  "Sum of each data point."
  [data] (reduce + data))

(defn sum-of-squares
  "Sum of the squares of each data point."
  [data] (reduce #(+ %1 (* %2 %2)) 0 data))

(defn variance
  "Sample variance. Returns variance.
   Ref: Chan et al. Algorithms for computing the sample variance: analysis and
        recommendations. American Statistician (1983)."
  ([data] (variance data 1))
  ([data df]
     ;; Uses a single pass, non-pairwise algorithm, without shifting.
     (let [update-estimates (fn [[m q k] x]
			      [(+ m (/ (- x m) (inc k)))
			       (+ q (/ (* k (sqr (- x m))) (inc k)))
			       (inc k)])
	   [m q k] (reduce update-estimates [0 0 0] data)]
       (/ q (- k df)))))

(defn uniform-distribution [max-val rng]
  (map (fn [x] (int (* x max-val))) rng))

(defn sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val"
  [n max-val rng]
  (take n (uniform-distribution max-val rng)))

(defn sample
  "Sample with replacement."
  [x rng]
  (let [n (count x)]
    (map #(nth x %1) (sample-uniform n n rng))))

(defn bootstrap-sample
  "Bootstrap sampling of a statistic, using resampling with replacement."
  [data statistic size rng-factory]
  (for [_ (range size)] (statistic (sample data (rng-factory)))))

(defn confidence-interval
  "Find the significance of outliers gicen boostrapped mean and variance estimates.
   This uses the bootstrapped statistic's variance, but we should use BCa of ABC."
  [mean variance]
  (let [n-sigma 1.96			; use 95% confidence interval
	delta (* n-sigma (Math/sqrt variance))]
    [(- mean delta) (+ mean delta)]))

(defn bootstrap-estimate
  "Mean, variance and confidence interval. This uses the bootstrapped
  statistic's variance for the confidence interval, but we should use BCa of
  ABC."
  [sampled-stat]
  (let [stats ((juxt mean variance) sampled-stat)]
    (conj stats (apply confidence-interval stats))))

(defn scale-bootstrap-estimate [estimate scale]
  [ (* (first estimate) scale) (* (second estimate) scale scale)
    (map #(* scale %1) (last estimate))])

(defn point-estimate [estimate]
  (first estimate))

(defn point-estimate-variance [estimate]
  (second estimate))

(defn point-estimate-ci [estimate]
  (last estimate))

(defn bootstrap
  "Bootstrap a statistic. Statistic can produce multiple statistics as a vector
   so you can use juxt to pass multiple statistics.
   http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size rng-factory]
  (println "Bootstrap samples ...")
  (let [samples (bootstrap-sample data statistic size rng-factory)
	transpose (fn [data] (apply map vector data))]
    (if (vector? (first samples))
      (map bootstrap-estimate (transpose samples))
      (bootstrap-estimate samples))))

(defn outlier-effect
  "Return a keyword describing the effect of outliers on the estimate of mean
  runtime."
  [var-out-min]
  (cond
    (< var-out-min 0.01) :unaffected
    (< var-out-min 0.1) :slight
    (< var-out-min 0.5) :moderate
    :else :severe))

(defn outlier-significance
  "Find the significance of outliers given boostrapped mean and variance estimates."
  [mean-estimate variance-estimate n]
  (let [mean (point-estimate mean-estimate)
	variance (point-estimate variance-estimate)
	std-dev (Math/sqrt variance)
	mean-over-n (/ mean n)
	min-g (/ mean-over-n 2)
	sigma-g (min (/ min-g 4) (/ std-dev (Math/sqrt n)))
	variance-g (sqr sigma-g)
	std-dev2 (sqr std-dev)
	c-max (fn [x]
		(let [k (- mean-over-n x)
		      d (* 2 k)
		      nd (* n d)
		      k0 (* (- n) nd)
		      k1 (+ (- variance (* n variance-g)) nd)
		      det (- (* k1 k1) (* 4 variance-g k0))]
		  (Math/floor (/ (* -2 k0)  (+ k1 (Math/sqrt det))))))
	var-out (fn [c]
		  (let [nmc (- n c)]
		    (* (/ nmc n) (- variance (* nmc variance-g)))))
	min-f (fn [f q r]
		(min (f q) (f r)))
	]
    (/ (min-f var-out 1 (min-f c-max 0 min-g)) variance)))


;; For the moment we take the easy option of sorting samples
(defn median
  "Calculate the median of a sorted data set
   References: http://en.wikipedia.org/wiki/Median"
  [data]
  (let [n (count data)
	i (bit-shift-right n 1)]
    (if (even? n)
      [(/ (+ (nth data (dec i)) (nth data i)) 2)
       (take i data)
       (drop i data)]
      [(nth data (bit-shift-right n 1))
       (take i data)
       (drop (inc i) data)])))


(defn quartiles
  "Calculate the quartiles of a sorted data set
   References: http://en.wikipedia.org/wiki/Quartile"
  [data]
  (let [[m lower upper] (median data)]
    [(first (median lower)) m (first (median upper))]))

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

(defn boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles."
  [q1 q3]
  (let [iqr (- q3 q1)
	severe (* iqr 3)
	mild (* iqr 1.5)]
    [(- q1 severe)
     (- q1 mild)
     (+ q3 mild)
     (+ q3 severe)]))

(defn outliers
  "Find the outliers in the data using a boxplot technique."
  [data]
  (println "Finding outliers...")
  (reduce (apply partial add-outlier
		 (apply boxplot-outlier-thresholds
			((juxt first last) (quartiles (sort data)))))
	  (outlier-count 0 0 0 0)
	  data))

(defmacro benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   quick test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [expr & options]
  `(let [opts# (into {} ~options)
	 times# (run-benchmark  (or (:samples opts#) *sample-count*)
				  (if-let [warmup-jit-period# (:warmup-jit-period opts#)]
				    (* warmup-jit-period# *s-to-ns*)
				    *warmup-jit-period*)
				  (if-let [target-execution-time# (:target-execution-time opts#)]
				    (* target-execution-time# *s-to-ns*)
				    *target-execution-time*)
				  ~expr
				  (:pre opts#))
	 outliers# (outliers (:samples times#))
	 stats# (bootstrap (:samples times#) (juxt mean variance) 20 criterium.well/well-rng-1024a)
	 analysis# (outlier-significance (first stats#) (second stats#) (:sample-count times#))]
     (merge times# {:outliers outliers#
		    :mean (scale-bootstrap-estimate (first stats#) (/ 1e-9 (:execution-count times#)))
		    :variance (scale-bootstrap-estimate (second stats#) (/ 1e-18 (:execution-count times#)))
		    :outlier-variance analysis#})))



(defmacro quick-bench
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  ([expr]
     `(run-benchmark (/ *sample-count* 10)
		     (/ *warmup-jit-period* 10)
		     (/ *target-execution-time* 10)
		     ~expr)))

(defn report-estimate [estimate unit]
  (print (point-estimate estimate) unit
	   " 95% CI:" (point-estimate-ci estimate)))

(defn report-estimate-sqrt [estimate unit]
  (print (Math/sqrt (point-estimate estimate)) unit
	   " 95% CI:" (map #(Math/sqrt %1) (point-estimate-ci estimate))))

(defn report-outliers [outlier-count samples]
  (dorun
   (map #(when (pos? %1) (println " " %2 ":" %1 "(" (* 100.0 (/ %1 samples)) "%)"))
	(vals outlier-count)
	["low-severe" "low-mild" "high-mild" "high-severe"])))

(defn report-result [results & opts]
  (let [verbose (some #(= :verbose %) opts)
	show-os (or verbose (some #(= :os %) opts))
	show-runtime (or verbose (some #(= :runtime %) opts))]
    (when show-os
      (apply println (conj (apply vector (map #(%1 (criterium/os-details))
					      [:arch :name :version :available-processors]))
			   "cpu(s)")))
    (when show-runtime
      (apply println (map #(%1 (runtime-details)) [:vm-name :vm-version]))
      (apply println "Runtime arguments:" (:input-arguments (runtime-details)))))
  (println "# evaluations :" (* (:execution-count results)
				(:sample-count results)))

  (print "Execution time mean : ")
  (report-estimate (:mean results) "sec")
  (println)

  (print "Execution time variance : ")
  (report-estimate-sqrt (:variance results) "sec")
  (println)

  (let [outliers (:outliers results)
	values (vals outliers)
	labels {:unaffected  "unaffected"
		:slight "slightly inflated"
		:moderate "moderately inflated"
		:severe "severely inflated"}]
    (when (some pos? values)
      (println "Found" (reduce + values) "outliers in"
	       (:sample-count results) " samples ("
	       (* 100.0 (/ (reduce + values) (:sample-count results))) "%)")
      (report-outliers outliers (:sample-count results))
      (println " variance introduced by outliers:" (* (:outlier-variance results) 100.0) "%")
      (println " variance is" ((outlier-effect (:outlier-variance results)) labels) "by outliers")
      (println))))

(defmacro bench [expr & opts]
  `(report-result (benchmark ~expr) ~@opts))
