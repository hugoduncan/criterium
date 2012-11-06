;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.


;;;; Criterium - measures expression computation time over multiple invocations

;;;; Inspired by Brent Broyer's
;;;; http://www.ellipticgroup.com/html/benchmarkingArticle.html
;;;; and also Haskell's Criterion

;;;; Unlike java solutions, this can benchmark general expressions rather than
;;;; just functions.

(ns ^{:author "Hugo Duncan"
      :see-also
      [["http://github.com/hugoduncan/criterium" "Source code"]
       ["http://hugoduncan.github.com/criterium" "API Documentation"]]}
  criterium.core
  "Criterium measures the computation time of an expression.  It is
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
library that applies many of the same statistical techniques."
  (:use clojure.set
         criterium.stats)
  (:require criterium.well)
  (:import (java.lang.management ManagementFactory)))

(def ^{:dynamic true} *use-mxbean-for-times* nil)

(def ^{:doc "Fraction of excution time allowed for final cleanup before a
             warning is issued."
       :dynamic true}
  *final-gc-problem-threshold* 0.01)

(def s-to-ns (* 1000 1000 1000)) ; in ns
(def ns-to-s 1e-9) ; in ns

(def ^{:doc "Time period used to let the code run so that jit compiler can do
             its work."
       :dynamic true}
  *warmup-jit-period* (* 10 s-to-ns)) ; in ns

(def ^{:doc "Number of executions required"
       :dynamic true} *sample-count* 60)

(def ^{:doc "Target elapsed time for execution for a single measurement."
       :dynamic true}
  *target-execution-time* (* 1 s-to-ns)) ; in ns

(def ^{:doc "Maximum number of attempts to run finalisers and gc."
       :dynamic true}
  *max-gc-attempts* 100)

(def ^{:dynamic true}
  *default-benchmark-opts*
  {:max-gc-attempts *max-gc-attempts*
   :samples *sample-count*
   :target-execution-time *target-execution-time*
   :warmup-jit-period *warmup-jit-period*
   :tail-quantile 0.025
   :bootstrap-size 1000})

(def ^{:dynamic true}
  *default-quick-bench-opts*
  {:max-gc-attempts *max-gc-attempts*
   :samples (/ *sample-count* 10)
   :target-execution-time (/ *target-execution-time* 10)
   :warmup-jit-period (/ *warmup-jit-period* 2)
   :tail-quantile 0.025
   :bootstrap-size 500})

;;; Progress reporting
(def ^{:dynamic true} *report-progress* nil)

(defn #^{:skip-wiki true}
  progress
  "Conditionally report progress to *out*."
  [& message]
  (when *report-progress*
    (apply println message)))

;;; Java Management interface
(defprotocol StateChanged
  "Interrogation of differences in a state."
  (state-changed?
   [state]
   "Check to see if a state delta represents no change")
  (state-delta
   [state-1 state-2]
   "Return a state object for the difference between two states"))

(defrecord JvmClassLoaderState [loaded-count unloaded-count]
  StateChanged
  (state-changed?
   [state]
   (not (and (zero? (:loaded-count state)) (zero? (:unloaded-count state)))))
  (state-delta
   [state-1 state-2]
   (let [vals (map - (vals state-1) (vals state-2))]
     (JvmClassLoaderState. (first vals) (second)))))

(defn jvm-class-loader-state []
  (let [bean (.. ManagementFactory getClassLoadingMXBean)]
    (JvmClassLoaderState. (. bean getLoadedClassCount)
                          (. bean getUnloadedClassCount))))


(defrecord JvmCompilationState [compilation-time]
  StateChanged
  (state-changed?
   [state]
   (not (zero? (:compilation-time state))))
  (state-delta
   [state-1 state-2]
   (let [vals (map - (vals state-1) (vals state-2))]
     (JvmCompilationState. (first vals)))))

(defn jvm-compilation-state
  "Returns the total compilation time for the JVM instance."
  []
  (let [bean (.. ManagementFactory getCompilationMXBean)]
    (JvmCompilationState. (if (. bean isCompilationTimeMonitoringSupported)
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
(defmacro timestamp
  "Obtain a timestamp"
  [] `(System/nanoTime))

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

(defn replace-ret-val-in-time-body-result
  [[elapsed-time _] new-ret-val]
  [elapsed-time new-ret-val])

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
         (if (and (or (pos? (.. ManagementFactory
                                getMemoryMXBean
                                getObjectPendingFinalizationCount))
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
    [(> fractional-time *final-gc-problem-threshold*)
     fractional-time
     cleanup-time]))

(defn final-gc-warn [final-gc-result]
  (when (first final-gc-result)
    (println
     "WARNING: Final GC required"
     (* 100.0 (second final-gc-result))
     "% of runtime"))
  final-gc-result)

;; Andy Fingerhut note: I tried using a larger value like 8192 for
;; +max-obj-array-size+, but for a benchmarked expression like (conj
;; coll :foo) (where coll is (into [] (range (+ 32768 32)))), it seems
;; to result in much more GC time required between calls to
;; execute-expr.  I saw it take multiple seconds between calls to
;; execute-expr, even though the calls to execute-expr themselves only
;; took about 1 second.  I'm not quite sure why it does this, since
;; the amount of memory allocated by each call to (conj coll :foo) in
;; that case is not terribly large -- most of the structure is shared
;; with the original coll.
;;
;; By using a small value like 4, it seems that the GC can much more
;; quickly get rid of the garbage.  My guess is that this has
;; something to do with how generational GC works.
;;
;; For expressions that construct brand new large data structures, it
;; also obviously requires significantly more memory during
;; execute-expr to have a large value for +max-obj-array-size+,
;; e.g. when benchmarking an expression like (into [] (range 100000)).
;;
;; TBD: Consider using the value 1.  This would still require keeping
;; around the previous expression's return value while calculating the
;; next one, so 2 total in memory at a time, but it seems like any
;; value that is >= 1 should prevent the JVM from optimizing away the
;; calculation of the expression.
(def ^:const +max-obj-array-size+ 4)

(defn execute-expr-core-timed-part
  "Performs the part of execute-expr where we actually measure the
  elapsed run time.  Evaluates (f) n times, each time saving the
  return value as an Object in the Java Object array ret-vals-arr,
  i.e. ret-vals-arr is mutated.

  The idea is that except for the call to (f), the only things done
  during each iteration are a few arithmetic operations and
  comparisons to 0 on primitive longs, and the aset to store the
  return value.

  The JVM is not free to optimize away the calls to f because the
  return values are saved in ret-vals-arr.

  This array is at most +max-obj-array-size+ elements long, to save
  memory.  An artificially intelligent JVM might be able to determine
  that if n is larger than +max-obj-array-size+, some of the return
  values are overwritten and thus those calls need not be made.  I
  doubt we will see that kind of optimization any time soon, and
  perhaps some JVM rules even prohibit doing so since the writes to
  ret-vals-arr could potentially be read by a different thread."
  [n f ret-vals-arr]
  (let [^objects arr ret-vals-arr
        arr-size-1 (long (dec (count arr)))
        init-j (rem (dec n) +max-obj-array-size+)]
    (time-body
     (loop [i (long (dec n))
            j (long init-j)
            v (f)]
       (aset arr j v)
       (if (pos? i)
         (recur (unchecked-dec i)
                (if (zero? j) arr-size-1 (unchecked-dec j))
                (f)))))))

(defn execute-expr-core
  "See execute-expr-core-timed-part.  Here we create the Java Object
  array in which execute-expr-core-timed-part will store return values
  from calling (f).  Even though storing the return values in that
  array is probably enough not to allow the JVM to optimize away calls
  to (f), go ahead and use reduce-with to combine up to
  +max-obj-array-size+ return values together and return that."
  [n f reduce-with]
  (let [arr-size (int (min +max-obj-array-size+ n))
        arr-size-1 (int (dec arr-size))
        ret-vals-arr (object-array arr-size)
        time-and-ret (execute-expr-core-timed-part n f ret-vals-arr)]
    (loop [i (int arr-size-1)
           v (aget ret-vals-arr i)]
      (if (pos? i)
        (recur (dec i) (reduce-with v (aget ret-vals-arr (dec i))))
        (replace-ret-val-in-time-body-result time-and-ret v)))))

;;; Execution
(defn execute-expr
  "A function to execute a function the given number of times, timing the
  complete execution."
  [n f reduce-with]
  (if reduce-with
    (execute-expr-core n f reduce-with)
    (time-body (doall (for [_ (range 0 n)] (f))))))

(defn collect-samples
  [sample-count execution-count f reduce-with gc-before-sample]
  (doall
   (for [_ (range 0 sample-count)]
     (do
       (when gc-before-sample
         (force-gc))
       (execute-expr execution-count f reduce-with)))))

;;; Compilation
(defn warmup-for-jit
  "Run expression for the given amount of time to enable JIT compilation."
  [warmup-period f]
  (progress "Warming up for JIT optimisations ...")
  (loop [elapsed 0 count 0]
    (if (> elapsed warmup-period)
      [elapsed count]
      (recur (+ elapsed (long (first (time-body (f))))) (inc count)))))

;;; Execution parameters
(defn estimate-execution-count
  "Estimate the number of executions required in order to have at least the
   specified execution period, check for the jvm to have constant class loader
   and compilation state."
  [period f reduce-with gc-before-sample]
  (progress "Estimating execution count ...")
  (loop [n 1
         cl-state (jvm-class-loader-state)
         comp-state (jvm-compilation-state)]
    (let [t (ffirst (collect-samples 1 n f reduce-with gc-before-sample))
          ;; It is possible for small n and a fast expression to get
          ;; t=0 nsec back from collect-samples.  This is likely due
          ;; to how (System/nanoTime) quantizes the time on some
          ;; systems.
          t (max 1 t)
          new-cl-state (jvm-class-loader-state)
          new-comp-state (jvm-compilation-state)]
      (if (and (>= t period)
               (= cl-state new-cl-state)
               (= comp-state new-comp-state))
        n
        (recur (if (>= t period)
                 n
                 (min (* 2 n) (inc (long (* n (/ period t))))))
               new-cl-state new-comp-state)))))


;; benchmark
(defn run-benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   quick test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [sample-count warmup-jit-period target-execution-time f reduce-with
   gc-before-sample]
  (force-gc)
  (let [first-execution (time-body (f))]
    (warmup-for-jit warmup-jit-period f)
    (let [n-exec (estimate-execution-count
                  target-execution-time f reduce-with gc-before-sample)
          _   (progress
               "Running with sample-count" sample-count
               "exec-count" n-exec
               (if reduce-with "reducing results" ""))
          samples (collect-samples
                   sample-count n-exec f reduce-with gc-before-sample)
          sample-times (map first samples)
          total (reduce + 0 sample-times)
          final-gc-result (final-gc-warn (final-gc total))]
      {:execution-count n-exec
       :sample-count sample-count
       :samples sample-times
       :results (map second samples)
       :total-time (/ total 1e9)})))
;; :average-time (/ total# sample-count# n-exec# 1e9)


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


(defrecord OutlierCount [low-severe low-mild high-mild high-severe])

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  (OutlierCount. low-severe low-mild high-mild high-severe))


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
                            [(first %1) (* (second %1) s-to-ns)]
                            %1)
                         options)))))

(defn default-reducer
  "A function that can be used to reduce any function output."
  [^Object a ^Object b]
  (let [a (if (nil? a) 0 (.hashCode a))
        b (if (nil? b) 0 (.hashCode b))]
    (+ a b)))

;;; User top level functions
(defmacro with-progress-reporting
  "Macro to enable progress reporting during the benchmark."
  [expr]
  `(binding [*report-progress* true]
     ~expr))

(defn benchmark*
  "Benchmark a function. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [f & {:as options}]
  (let [opts (merge *default-benchmark-opts* options)
        times (run-benchmark (:samples opts)
                             (:warmup-jit-period opts)
                             (:target-execution-time opts)
                             f
                             (:reduce-with opts default-reducer)
                             (:gc-before-sample opts))
        outliers (outliers (:samples times))
        tail-quantile (:tail-quantile opts)
        stats (bootstrap-bca
               (map double (:samples times))
               (juxt
                mean
                variance
                (partial quantile tail-quantile)
                (partial quantile (- 1.0 tail-quantile)))
               (:bootstrap-size opts) [0.5 tail-quantile (- 1.0 tail-quantile)]
               criterium.well/well-rng-1024a)
        analysis (outlier-significance (first stats) (second stats)
                                       (:sample-count times))
        sqr (fn [x] (* x x))
        m (mean (map double (:samples times)))
        s (Math/sqrt (variance (map double (:samples times))))]
    (merge times
           {:outliers outliers
            :mean (scale-bootstrap-estimate
                   (first stats) (/ 1e-9 (:execution-count times)))
            :sample-mean (scale-bootstrap-estimate
                          [m [(- m (* 3 s)) (+ m (* 3 s))]]
                          (/ 1e-9 (:execution-count times)))
            :variance (scale-bootstrap-estimate
                       (second stats) (sqr (/ 1e-9 (:execution-count times))))
            :sample-variance (scale-bootstrap-estimate
                              [ (sqr s) [0 0]]
                              (sqr (/ 1e-9 (:execution-count times))))
            :lower-q (scale-bootstrap-estimate
                       (nth stats 2) (/ 1e-9 (:execution-count times)))
            :upper-q (scale-bootstrap-estimate
                       (nth stats 3) (/ 1e-9 (:execution-count times)))
            :outlier-variance analysis
            :tail-quantile (:tail-quantile opts)
            :os-details (os-details)
            :options opts
            :runtime-details (->
                              (runtime-details)
                              (update-in [:input-arguments] vec))})))

(defmacro benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [expr & options]
  `(benchmark* (fn [] ~expr) ~@options))

(defn quick-benchmark*
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  [f & {:as options}]
  (apply
   benchmark* f (apply concat (merge *default-quick-bench-opts* options))))

(defmacro quick-benchmark
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  [expr & options]
  `(quick-benchmark* (fn [] ~expr) ~@options))

(defn report
  "Print format output"
  [format-string & values]
  (print (apply format format-string values)))

(defn scale-time
  "Determine a scale factor and unit for displaying a time."
  [measurement]
  (cond
   (> measurement 60) [(/ 60) "min"]
   (< measurement 1e-6) [1e9 "ns"]
   (< measurement 1e-3) [1e6 "us"]
   (< measurement 1) [1e3 "ms"]
   :else [1 "sec"]))

(defn format-value [value scale unit]
  (format "%f %s" (* scale value) unit))

(defn report-estimate
  [msg estimate significance]
  (let [mean (first estimate)
        [factor unit] (scale-time mean)]
    (apply
     report "%32s : %s  %2.1f%% CI: (%s, %s)\n"
     msg
     (format-value mean factor unit)
     (* significance 100)
     (map #(format-value % factor unit) (last estimate)))))

(defn report-point-estimate
  ([msg estimate]
     (let [mean (first estimate)
           [factor unit] (scale-time mean)]
       (report "%32s : %s\n" msg (format-value mean factor unit))))
  ([msg estimate quantile]
     (let [mean (first estimate)
           [factor unit] (scale-time mean)]
       (report
        "%32s : %s (%4.1f%%)\n"
        msg (format-value mean factor unit) (* quantile 100)))))

(defn report-estimate-sqrt
  [msg estimate significance]
  (let [mean (Math/sqrt (first estimate))
        [factor unit] (scale-time mean)]
    (apply
     report "%32s : %s  %2.1f%% CI: (%s, %s)\n"
     msg
     (format-value mean factor unit)
     (* significance 100)
     (map #(format-value (Math/sqrt %) factor unit) (last estimate)))))

(defn report-point-estimate-sqrt
  [msg estimate]
  (let [mean (Math/sqrt (first estimate))
        [factor unit] (scale-time mean)]
    (report "%32s : %s\n" msg (format-value mean factor unit))))

(defn report-outliers [results]
  (let [outliers (:outliers results)
        values (vals outliers)
        labels {:unaffected "unaffected"
                :slight "slightly inflated"
                :moderate "moderately inflated"
                :severe "severely inflated"}
        sample-count (:sample-count results)
        types ["low-severe" "low-mild" "high-mild" "high-severe"]]
    (when (some pos? values)
      (let [sum (reduce + values)]
        (report
         "\nFound %d outliers in %d samples (%2.4f %%)\n"
         sum sample-count (* 100.0 (/ sum sample-count))))
      (doseq [[v c] (partition 2 (interleave (filter pos? values) types))]
        (report "\t%s\t %d (%2.4f %%)\n" c v (* 100.0 (/ v sample-count))))
      (report " Variance from outliers : %2.4f %%"
              (* (:outlier-variance results) 100.0))
      (report " Variance is %s by outliers\n"
              (-> (:outlier-variance results) outlier-effect labels)))))

(defn report-result [results & opts]
  (let [verbose (some #(= :verbose %) opts)
        show-os (or verbose (some #(= :os %) opts))
        show-runtime (or verbose (some #(= :runtime %) opts))]
    (when show-os
      (apply println
             (->  (map
                   #(%1 (:os-details results))
                   [:arch :name :version :available-processors])
                  vec (conj "cpu(s)"))))
    (when show-runtime
      (let [runtime-details (:runtime-details results)]
        (apply println (map #(%1 runtime-details) [:vm-name :vm-version]))
        (apply println "Runtime arguments:"
               (:input-arguments runtime-details))))
    (println "Evaluation count :" (* (:execution-count results)
                                     (:sample-count results))
             "in" (:sample-count results) "samples of"
             (:execution-count results) "calls.")

    (when verbose
      (report-point-estimate
       "Execution time sample mean" (:sample-mean results)))
    (report-point-estimate "Execution time mean" (:mean results))
    (when verbose
      (report-point-estimate-sqrt
       "Execution time sample std-deviation" (:sample-variance results)))
    (report-point-estimate-sqrt
     "Execution time std-deviation" (:variance results))
    (report-point-estimate
     "Execution time lower quantile"
     (:lower-q results) (:tail-quantile results))
    (report-point-estimate
     "Execution time upper quantile"
     (:upper-q results) (- 1.0 (:tail-quantile results)))
    (report-outliers results)))

(defmacro bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
  to *out* in human readable format. Options for report format are: :os,
:runtime, and :verbose."
  [expr & opts]
  (let [[report-options options] (extract-report-options opts)]
    `(report-result (benchmark ~expr ~@options) ~@report-options)))

(defmacro quick-bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
to *out* in human readable format. Options for report format are: :os,
:runtime, and :verbose."
  [expr & opts]
  (let [[report-options options] (extract-report-options opts)]
    `(report-result (quick-benchmark ~expr ~@options) ~@report-options)))
