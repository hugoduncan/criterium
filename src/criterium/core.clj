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

(def ^{:dynamic true} *report-debug* nil)

(defn #^{:skip-wiki true}
  debug
  "Conditionally report debug to *out*."
  [& message]
  (when *report-debug*
    (apply println message)))

(def ^{:dynamic true} *report-warn* nil)

(defn #^{:skip-wiki true}
  warn
  "Conditionally report warn to *out*."
  [& message]
  (when *report-warn*
    (apply println "WARNING:" message)))

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
     (JvmClassLoaderState. (first vals) (second vals)))))

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
    :else (warn "don't know how to clear disk buffer cache for "
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
     (debug "Cleaning JVM allocations ...")
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
  []
  (progress "Final GC...")
  (first (time-body (force-gc))))

(defn final-gc-warn
  [execution-time final-gc-time]
  (progress "Checking GC...")
  (let [fractional-time (/ final-gc-time execution-time)
        final-gc-result [(> fractional-time *final-gc-problem-threshold*)
                         fractional-time
                         final-gc-time]]
    (when (first final-gc-result)
      (warn
       "Final GC required"
       (* 100.0 (second final-gc-result))
       "% of runtime"))
    final-gc-result))

;;; ## Core timing loop

;;; A mutable field is used to store the result of each function call, to
;;; prevent JIT optimising away the expression entirely.

(defprotocol MutablePlace
  "Provides a mutable place"
  (set-place [_ v] "Set mutable field to value.")
  (get-place [_] "Get mutable field value."))

(deftype Unsynchronized [^{:unsynchronized-mutable true :tag Object} v]
  MutablePlace
  (set-place [_ value] (set! v value))
  (get-place [_] v))

(def mutable-place (Unsynchronized. nil))

(defn execute-expr-core-timed-part
 "Performs the part of execute-expr where we actually measure the elapsed run
  time.  Evaluates `(f)` `n` times, each time saving the return value as an
  Object in `mutable-place`.

  The idea is that except for the call to (f), the only things done during each
  iteration are a few arithmetic operations and comparisons to 0 on primitive
  longs, and the storage of the return value.

  The JVM is not free to optimize away the calls to f because the return values
  are saved in `mutable-place`.

  This array is at most +max-obj-array-size+ elements long, to save
  memory.  An artificially intelligent JVM might be able to determine
  that if n is larger than +max-obj-array-size+, some of the return
  values are overwritten and thus those calls need not be made.  I
  doubt we will see that kind of optimization any time soon, and
  perhaps some JVM rules even prohibit doing so since the writes to
  ret-vals-arr could potentially be read by a different thread."
  [n f]
  (time-body
   (loop [i (long (dec n))
          v (f)]
     (set-place mutable-place v)
     (if (pos? i)
       (recur (unchecked-dec i) (f))
       v))))

;;; ## Execution
(defn execute-expr
  "Time the execution of `n` invocations of `f`. See
  `execute-expr-core-timed-part`."
  [n f]
  (let [time-and-ret (execute-expr-core-timed-part n f)]
    (get-place mutable-place) ;; just for good measure, use the mutable value
    time-and-ret))

(defn collect-samples
  [sample-count execution-count f gc-before-sample]
  {:pre [(pos? sample-count)]}
  (let [result (object-array sample-count)]
    (loop [i (long 0)]
      (if (< i sample-count)
        (do
          (when gc-before-sample
            (force-gc))
          (aset result i (execute-expr execution-count f))
          (recur (unchecked-inc i)))
        result))))

;;; Compilation
(defn warmup-for-jit
  "Run expression for the given amount of time to enable JIT compilation."
  [warmup-period f]
  (progress "Warming up for JIT optimisations" warmup-period "...")
  (let [cl-state (jvm-class-loader-state)
        comp-state (jvm-compilation-state)
        t (max 1 (first (time-body (f))))
        _ (debug "  initial t" t)
        [t n] (if (< t 100000)           ; 100us
                (let [n (/ 100000 t)]
                  [(first (execute-expr n f)) n])
                [t 1])
        p (/ warmup-period t)
        c (long (max 1 (* n (/ p 5))))]
    (debug "  using t" t "n" n)
    (debug "  using execution-count" c)
    (loop [elapsed (long t)
           count (long n)
           delta-free (long 0)
           old-cl-state cl-state
           old-comp-state comp-state]
      (let [new-cl-state (jvm-class-loader-state)
            new-comp-state (jvm-compilation-state)]
        (if (not= old-cl-state new-cl-state)
          (progress "  classes loaded before" count "iterations"))
        (if (not= old-comp-state new-comp-state)
          (progress "  compilation occurred before" count "iterations"))
        (debug "  elapsed" elapsed " count" count)
        (if (and (> delta-free 2) (> elapsed warmup-period))
          [elapsed count
           (state-delta new-cl-state cl-state)
           (state-delta new-comp-state comp-state)]
          (recur (+ elapsed (long (first (execute-expr c f))))
                 (+ count c)
                 (if (and (= old-cl-state new-cl-state)
                          (= old-comp-state new-comp-state))
                   (unchecked-inc delta-free)
                   (long 0))
                 new-cl-state
                 new-comp-state))))))

;;; Execution parameters
(defn estimate-execution-count
  "Estimate the number of executions required in order to have at least the
   specified execution period, check for the jvm to have constant class loader
   and compilation state."
  [period f gc-before-sample estimated-fn-time]
  (progress "Estimating execution count ...")
  (debug " estimated-fn-time" estimated-fn-time)
  (loop [n (max 1 (long (/ period (max 1 estimated-fn-time) 5)))
         cl-state (jvm-class-loader-state)
         comp-state (jvm-compilation-state)]
    (let [t (ffirst (collect-samples 1 n f gc-before-sample))
          ;; It is possible for small n and a fast expression to get
          ;; t=0 nsec back from collect-samples.  This is likely due
          ;; to how (System/nanoTime) quantizes the time on some
          ;; systems.
          t (max 1 t)
          new-cl-state (jvm-class-loader-state)
          new-comp-state (jvm-compilation-state)]
      (debug " ..." n)
      (when (not= comp-state new-comp-state)
        (warn "new compilations in execution estimation phase"))
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
  [sample-count warmup-jit-period target-execution-time f gc-before-sample
   overhead]
  (force-gc)
  (let [first-execution (time-body (f))
        [warmup-t warmup-n cl-state comp-state] (warmup-for-jit
                                                 warmup-jit-period f)
        n-exec (estimate-execution-count
                target-execution-time f gc-before-sample
                (long (/ warmup-t warmup-n)))
        total-overhead (long (* (or overhead 0) 1e9 n-exec))
        _   (progress "Sampling ...")
        _   (debug
             "Running with\n sample-count" sample-count \newline
             "exec-count" n-exec \newline
             "overhead[s]" overhead \newline
             "total-overhead[ns]" total-overhead)
        _   (force-gc)
        samples (collect-samples sample-count n-exec f gc-before-sample)
        final-gc-time (final-gc)
        sample-times (->> samples
                          (map first)
                          (map #(- % total-overhead)))
        total (reduce + 0 sample-times)
        final-gc-result (final-gc-warn total final-gc-time)]
    {:execution-count n-exec
     :sample-count sample-count
     :samples sample-times
     :results (map second samples)
     :total-time (/ total 1e9)
     :warmup-time warmup-t
     :warmup-executions warmup-n
     :final-gc-time final-gc-time
     :overhead overhead}))


(defn run-benchmarks-round-robin
  "Benchmark multiple expressions in a 'round robin' fashion.  Very
similar to run-benchmark, except it takes multiple expressions in a
sequence instead of only one (each element of the sequence should be a
map with keys :f and :expr-string).  It runs the following steps in
sequence:

1. Execute each expr once

2. Run expression 1 for at least warmup-jit-period nanoseconds so the
   JIT has an opportunity to optimize it.  Then do the same for each
   of the other expressions.

3. Run expression 1 many times to estimate how many times it must be
   executed to take a total of target-execution-time nanoseconds.  The
   result is a number of iterations n-exec1 for expression 1.  Do the
   same for each of the other expressions, each with the same
   target-execution-time, each resulting in its own independent number
   of executions.

4. Run expression 1 n-exec1 times, measuring the total elapsed time.
   Do the same for the rest of the expressions.

5. Repeat step 4 a total of sample-count times."
  [sample-count warmup-jit-period target-execution-time exprs gc-before-sample]
  (force-gc)
  (let [first-executions (map (fn [{:keys [f]}] (time-body (f))) exprs)
        _ (progress (format "Warming up %d expression for %.2e sec each:"
                          (count exprs) (/ warmup-jit-period 1.0e9)))
        warmup (vec (for [{:keys [f expr-string]} exprs]
                      (do (progress (format "    %s..." expr-string))
                          (warmup-for-jit warmup-jit-period f))))]
    (progress
     (format
      "Estimating execution counts for %d expressions.  Target execution time = %.2e sec:"
                      (count exprs) (/ target-execution-time 1.0e9)))
    (let [exprs (map-indexed
                 (fn [idx {:keys [f expr-string] :as expr}]
                   (progress (format "    %s..." expr-string))
                   (let [ [warmup-t warmup-n cl-state comp-state] (get warmup idx)]
                     (assoc expr :index idx
                       :n-exec (estimate-execution-count
                                target-execution-time f
                                gc-before-sample
                                (long (/ warmup-t warmup-n))))))
                 exprs)
;;          _   (progress
;;               "Running with sample-count" sample-count
;;               "exec-count" n-exec  ; tbd: update)
          all-samples (doall
                       (for [i (range sample-count)]
                         (do
                           (progress
                            (format
                             "    Running sample %d/%d for %d expressions:"
                             (inc i) sample-count (count exprs)))
                           (doall
                            (for [{:keys [f n-exec expr-string] :as expr} exprs]
                              (do
                                (progress (format "        %s..." expr-string))
                                (assoc expr
                                  :sample (first
                                           (collect-samples
                                            1 n-exec f gc-before-sample)))))))))

          ;; 'transpose' all-samples so that all samples for a
          ;; particular expression are in a sequence together, and
          ;; all-samples is a sequence of one map per expression.
          all-samples (group-by :index (apply concat all-samples))
          all-samples
          (map (fn [[idx data-seq]]
                 (let [expr (dissoc (first data-seq) :sample)
                       n-exec (:n-exec expr)
                       samples (map :sample data-seq)
                       final-gc-time (final-gc)
                       sample-times (map first samples)
                       total (reduce + 0 sample-times)
                       ;; TBD: Doesn't make much sense to attach final
                       ;; GC warning to the expression that happened
                       ;; to be first in the sequence, but that is
                       ;; what this probably does right now.  Think
                       ;; what might be better to do.
                       final-gc-result (final-gc-warn total final-gc-time)]
                   {:execution-count n-exec
                    :sample-count sample-count
                    :samples sample-times
                    :results (map second samples)
                    :total-time (/ total 1e9)}))
               all-samples)]
      all-samples)))


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

;;; overhead estimation
(declare benchmark*)

(defn estimate-overhead
  "Calculate a conservative estimate of the timing loop overhead."
  []
  (-> (benchmark*
       (fn [] 0)
       {:warmup-jit-period (* 10 s-to-ns)
        :samples 10
        :target-execution-time (* 0.5 s-to-ns)
        :overhead 0
        :supress-jvm-option-warnings true})
      :lower-q
      first))

(def estimated-overhead-cache nil)

(defn estimated-overhead!
  "Sets the estimated overhead."
  []
  (progress "Estimating sampling overhead")
  (alter-var-root
   #'estimated-overhead-cache (constantly (estimate-overhead))))

(defn estimated-overhead
  []
  (or estimated-overhead-cache
      (estimated-overhead!)))

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

;;; User top level functions
(defmacro with-progress-reporting
  "Macro to enable progress reporting during the benchmark."
  [expr]
  `(binding [*report-progress* true]
     ~expr))

(defn benchmark-stats [times opts]
  (let [outliers (outliers (:samples times))
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

(defn warn-on-suspicious-jvm-options
  "Warn if the JIT options are suspicious looking."
  []
  (let [compiler (jvm-jit-name)
        {:keys [input-arguments]} (runtime-details)]
    (when-let [arg (and (re-find #"Tiered" compiler)
                        (some #(re-find #"TieredStopAtLevel=(.*)" %)
                              input-arguments))]
      (warn
       "JVM argument" (first arg) "is active,"
       "and may lead to unexpected results as JIT C2 compiler may not be active."
       "See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies."))))

(defn benchmark*
  "Benchmark a function. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [f {:keys [samples warmup-jit-period target-execution-time gc-before-sample
             overhead supress-jvm-option-warnings] :as options}]
  (when-not supress-jvm-option-warnings
    (warn-on-suspicious-jvm-options))
  (let [{:keys [samples warmup-jit-period target-execution-time
                gc-before-sample overhead] :as opts}
        (merge *default-benchmark-opts*
               {:overhead (or overhead (estimated-overhead))}
               options)
        times (run-benchmark
               samples warmup-jit-period target-execution-time f opts overhead)]
    (benchmark-stats times opts)))

(defn benchmark-round-robin*
  [exprs options]
  (let [opts (merge *default-benchmark-opts* options)
        times (run-benchmarks-round-robin
               (:samples opts)
               (:warmup-jit-period opts)
               (:target-execution-time opts)
               exprs
               (:gc-before-sample opts))]
    (map #(benchmark-stats % opts) times)))

(defmacro benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [expr options]
  `(benchmark* (fn [] ~expr) ~options))

(defmacro benchmark-round-robin
  [exprs options]
  (let [wrap-exprs (fn [exprs]
                     (cons 'list
                           (map (fn [expr]
                                  {:f `(fn [] ~expr)
                                   :expr-string (str expr)})
                                exprs)))]
    `(benchmark-round-robin* ~(wrap-exprs exprs) ~options)))

(defn quick-benchmark*
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  [f {:as options}]
  (benchmark* f (merge *default-quick-bench-opts* options)))

(defmacro quick-benchmark
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  [expr options]
  `(quick-benchmark* (fn [] ~expr) ~options))

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
   (< measurement 1e-3) [1e6 "µs"]
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
    (when-let [overhead (:overhead results)]
      (when (pos? overhead)
        (report-point-estimate "Overhead used" [overhead])))
    (report-outliers results)))

(defmacro bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
  to *out* in human readable format. Options for report format are: :os,
:runtime, and :verbose."
  [expr & opts]
  (let [[report-options options] (extract-report-options opts)]
    `(report-result
      (benchmark
       ~expr
       ~(when (seq options) (apply hash-map options)))
      ~@report-options)))

(defmacro quick-bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
to *out* in human readable format. Options for report format are: :os,
:runtime, and :verbose."
  [expr & opts]
  (let [[report-options options] (extract-report-options opts)]
    `(report-result
      (quick-benchmark
       ~expr
       ~(when (seq options) (apply hash-map options)))
      ~@report-options)))
