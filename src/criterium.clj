;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.


;;;; Criterium - measures expression computation speed over multiple invocations

;;;; Inspired by Brent Broyer's http://www.ellipticgroup.com/html/benchmarkingArticle.html
;;;; and also Haskell's Criterion

;;;; Unlike java solutions, this can benchmark general expressions rather than just functions.


(ns criterium
  (:use clojure.set)
  (:require criterium.well)
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
      :samples *sample-count*
      :target-execution-time *target-execution-time*
      :warmup-jit-period *warmup-jit-period*
      :confidence-interval 0.95})

;;; Progress reporting
(def *report-progress* nil)

(defn progress [& message]
  (when *report-progress*
    (apply println message)))

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

;;; Reshaping
(defn transpose
  "Transpose a vector of vectors."
  [data]
  (if (vector? (first data))
    (apply map vector data)
    data))

;;; Statistics
(defn sqr
  "Square of argument"
  [x] (* x x))

(defn cube
  "Square of argument"
  [x] (* x x x))

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
  (transpose
   (for [_ (range size)] (statistic (sample data (rng-factory))))))

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
  [(* (first estimate) scale)
   (map #(* scale %1) (last estimate))])

(defn point-estimate [estimate]
  (first estimate))

(defn point-estimate-ci [estimate]
  (last estimate))

(defn polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
descending order (so the last element of coefficients is the constant term)."
  [x coefficients]
  (reduce #(+ (* x %1) %2) (first coefficients) (rest coefficients)))

(defn erf
  "erf polynomial approximation.  Maximum error is 1.5e-7.
  Handbook of Mathematical Functions: with Formulas, Graphs, and Mathematical
  Tables. Milton Abramowitz (Editor), Irene A. Stegun (Editor), 7.1.26"
  [x]
  (let [x (double x)
	sign (Math/signum x)
	x (Math/abs x)
	a [1.061405429 -1.453152027 1.421413741 -0.284496736 0.254829592 0]
	p 0.3275911
	t (/ (+ 1 (* p x)))
	value (- 1 (* (polynomial-value t a) (Math/exp (- (* x x)))))]
    (* sign value)))

(defn normal-cdf
  "Probability p(X<x), for a normal distrubtion.  Uses the polynomial erf
  approximation above, and so is not super accurate."
  [x]
  (* 1/2 (+ 1 (erf (/ x (Math/sqrt 2))))))

(defn normal-quantile
  "Normal quantile function. Given a quantile in (0,1), return the normal value for that quantile.
  Wichura, MJ. 'Algorithm AS241' The Percentage Points of the Normal Distribution. Applied Statistics, 37, 477-484
"
  [x]
  (let [x (double x)
	a [2509.0809287301226727
	   33430.575583588128105
	   67265.770927008700853
	   45921.953931549871457
	   13731.693765509461125
	   1971.5909503065514427
	   133.14166789178437745
	   3.3871328727963666080]
	b [5226.4952788528545610
	   28729.085735721942674
	   39307.895800092710610
	   21213.794301586595867
	   5394.1960214247511077
	   687.18700749205790830
	   42.313330701600911252
	   1]
	c [0.000774545014278341407640
	   0.0227238449892691845833
	   0.241780725177450611770
	   1.27045825245236838258
	   3.64784832476320460504
	   5.76949722146069140550
	   4.63033784615654529590
	   1.42343711074968357734]
	d [1.05075007164441684324e-9
	   0.000547593808499534494600
	   0.0151986665636164571966
	   0.148103976427480074590
	   0.689767334985100004550
	   1.67638483018380384940
	   2.05319162663775882187
	   1]
  	e [
	   2.01033439929228813265e-7
	   0.0000271155556874348757815
	   0.00124266094738807843860
	   0.0265321895265761230930
	   0.296560571828504891230
	   1.78482653991729133580
	   5.46378491116411436990
	   6.65790464350110377720
	   ]
	f [2.04426310338993978564e-15
	   1.42151175831644588870e-7
	   1.84631831751005468180e-5
	   0.000786869131145613259100
	   0.0148753612908506148525
	   0.136929880922735805310
	   0.599832206555887937690
	   1]]
    (if (<= 0.075 x 0.925)
      (let [v (- x 0.5)
	    r (- 180625e-6 (* v v))]
	(* v (/ (polynomial-value r a) (polynomial-value r b))))
      (let [r (if (< x 1/2) x (- 1 x))
	    r (Math/sqrt (- (Math/log r)))]
	(if (<= r 5)
	  (let [r (- r 16/10)]
	    (* (Math/signum (- x 1/2)) (/ (polynomial-value r c) (polynomial-value r d))))
	  (let [r (- r 5)]
	    (* (Math/signum (- x 1/2)) (/ (polynomial-value r e) (polynomial-value r f)))))))))

(defn drop-at [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (concat (take n s) (next (drop n s))))))

(defn trunc
  "Round towards zero to an integeral value."
  [x] (if (pos? x)
	(Math/floor x)
	(Math/ceil x)))

(defn jacknife
  "Jacknife statistics on data."
  [data statistic]
  (transpose
   (map #(statistic (drop-at %1 data)) (range (count data)))))

(defn bca-nonparametric-eval
  "Calculate bootstrap values for given estimate and samples"
  [n size data z-alpha estimate samples jack-samples]
  (let [z0 (normal-quantile (/ (count (filter (partial > estimate) samples)) size))
	jack-mean (mean jack-samples)
	jack-deviation (map #(- jack-mean %1) jack-samples)
	acc (/ (reduce + 0 (map cube jack-deviation))
	       (* 6 (Math/pow (reduce + 0 (map sqr jack-deviation)) 1.5)))
	tt (map #(normal-cdf (+ z0 (/ (+ z0 %1) (- 1 (* acc (+ z0 %1)))))) z-alpha)
	ooo (map #(trunc (* %1 size)) tt)
	sorted-samples (sort samples)
	confpoints (map (partial nth sorted-samples) ooo)]
    [confpoints z0 acc jack-mean jack-samples]))

(defn bca-nonparametric
  "Non-parametric BCa estimate of a statistic on data. Size bootstrap samples
  are used. Confidence values are returned at the alpha normal
  quantiles. rng-factory is a method that returns a random number generator to
  use for the sampling.
  An introduction to the bootstrap.  Efron, B., & Tibshirani, R. J. (1993).
  See http://lib.stat.cmu.edu/S/bootstrap.funs for Efron's original implementation.
"
  [data statistic size alpha rng-factory]
  (let [n (count data)
	estimate (statistic data)
	samples (bootstrap-sample data statistic size rng-factory)
	jack-samples (jacknife data statistic)
	alpha (if (vector? alpha) alpha [alpha])
	z-alpha (map normal-quantile alpha)]
    (if (vector? estimate)
      (map (partial bca-nonparametric-eval n size data z-alpha) estimate samples jack-samples)
      (bca-nonparametric-eval n size data z-alpha estimate samples jack-samples))))

(defn bca-to-estimate [alpha bca-estimate]
  [(first (first bca-estimate)) (next (first bca-estimate))])

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
  (progress "Checking outlier significance")
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

(defn add-default-options [options]
  (let [time-periods #{:warmup-jit-period :target-execution-time}]
    (merge *default-benchmark-opts*
	   (into {} (map #(if (contains? time-periods (first %1))
			    [(first %1) (* (second %1) *s-to-ns*)]
			    %1)
			 options)))))

;;; User top level functions
(defmacro with-progress-reporting [expr]
  `(binding [*report-progress* true]
     ~expr))

(defmacro benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [expr & options]
  `(let [options# (vector ~@options)
	 opts# (add-default-options (if (empty? options#) {} (apply assoc {} options#)))
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
  ([expr]
     `(run-benchmark (/ *sample-count* 10)
		     (/ *warmup-jit-period* 10)
		     (/ *target-execution-time* 10)
		     ~expr)))

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
      (apply println (conj (apply vector (map #(%1 (criterium/os-details))
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
