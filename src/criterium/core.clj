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
  (:require [criterium
             [jvm :as jvm]
             [toolkit :as toolkit]
             [util :as util]
             [well :as well]])
  (:import (java.lang.management ManagementFactory)))

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
  {:max-gc-attempts       *max-gc-attempts*
   :num-samples           *sample-count*
   :target-execution-time *target-execution-time*
   :warmup-jit-period     *warmup-jit-period*
   :tail-quantile         0.025
   :bootstrap-size        1000})

(def ^{:dynamic true}
  *default-quick-bench-opts*
  {:max-gc-attempts       *max-gc-attempts*
   :num-samples           (/ *sample-count* 10)
   :target-execution-time (/ *target-execution-time* 10)
   :warmup-jit-period     (/ *warmup-jit-period* 2)
   :tail-quantile         0.025
   :bootstrap-size        500})

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
    :else  (warn "don't know how to clear disk buffer cache for "
                 (.. System getProperties (getProperty "os.name")))))


(defn elapsed-time [data]
  (-> data :time :elapsed))

;;; Execution timing
(defmacro time-expr
  "Returns a map containing execution time and result of specified function."
  [expr]
  `(toolkit/deltas
     (toolkit/instrumented
       (toolkit/with-garbage-collector-stats
         (toolkit/with-time
           (toolkit/with-expr-value
             ~expr))))))


(defmacro time-expr-for-warmup
  "Returns a map containing execution time, change in loaded and unloaded
  class counts, change in compilation time and result of specified function."
  [expr]
  `(toolkit/deltas
     (toolkit/instrumented
       (toolkit/with-class-loader-counts
         (toolkit/with-compilation-time
           (toolkit/with-time
             (toolkit/with-expr
               ~expr)))))))


;;; Memory management
(defn force-gc
  "Force garbage collection and finalisers so that execution time
  associated with this is not incurred at another time. Up to
  max-attempts are run to clear all pending finalizers and free as
  much memory as possible.

  Returns the GC execution time,  total changes in memory, and in
  object finalizers pending."
  ([] (force-gc *max-gc-attempts*))
  ([max-attempts]
   (debug "Cleaning JVM allocations ...")
   (loop [all-deltas [] ; hold onto data we allocate here
          attempts   0]
     (let [deltas (toolkit/deltas
                    (toolkit/instrumented
                      (toolkit/with-memory
                        (toolkit/with-finalization-count
                          (toolkit/with-time
                            (toolkit/with-expr-value
                              (jvm/run-finalizers-and-gc)))))))]

       (let [new-memory-used (-> deltas :memory :total :used)]
         (debug "pending finalizers" (-> deltas :finalization :pending))
         (debug "new-memory-used" new-memory-used)
         (if (and (< attempts max-attempts)
                  (or (pos? (-> deltas :finalization :pending))
                      (< new-memory-used 0)))
           (recur (conj all-deltas (dissoc deltas :expr-value))
                  (inc attempts))
           (reduce util/sum all-deltas)))))))


(defn final-gc-warn
  "If the final GC execution time is significant compared to
  the runtime, then the runtime should maybe include this time."
  [execution-time final-gc-time]
  (let [fractional-time (/ final-gc-time execution-time)
        is-significant? (> fractional-time *final-gc-problem-threshold*)]
    (when is-significant?
      (warn "Final GC required" (* 100.0 fractional-time) "% of runtime"))
    [is-significant?
     fractional-time
     final-gc-time]))

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

(defmacro execute-fn-n-times
  "Evaluates `(f)` `n` times, each time saving the
  return value as an Object in `mutable-place`.

  Except for the call to (f), only a few primitive long arithmetic
  operations and comparisons to 0, and the storage of the return
  value, are done during each iteration.

  The JVM is not free to optimize away the calls to f, as the return
  values are saved in `mutable-place`."
  [f n]
  `(loop [i# (long (dec ~n))
          v# (~f)]
     (set-place mutable-place v#)
     (if (pos? i#)
       (recur (unchecked-dec i#) (~f))
       v#)))

;;; ## Execution
(defn execute-expr
  "Time the execution of `n` invocations of `f`. See `execute-expr*`."
  [n f]
  (let [deltas (time-expr (execute-fn-n-times f n))]
    (get-place mutable-place) ;; just for good measure, use the mutable value
    deltas))

(defn execute-expr-for-warmup
  "Time the execution of `n` invocations of `f`. See `execute-expr*`."
  [n f]
  (let [deltas (time-expr-for-warmup (execute-fn-n-times f n))]
    (get-place mutable-place) ;; just for good measure, use the mutable value
    deltas))

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
  "Run expression for the given amount of time to enable JIT compilation.

  Returns a vector of execution count, deltas and estimated function
  execution time."  [warmup-period f]
  (debug "warmup-for-jit f" f)
  (let [ignore-first (time-expr-for-warmup (f))
        deltas-1     (time-expr-for-warmup (f))
        t            (max 1 (elapsed-time deltas-1))
        _            (debug "  initial t" t)
        [deltas-n n] (if (< t 100000)           ; 100us
                       (let [n (inc (quot 100000 t))]
                         [(execute-expr-for-warmup n f) n])
                       [deltas-1 1])
        t            (elapsed-time deltas-n)
        p            (/ warmup-period t)
        c            (long (max 1 (* n (/ p 5))))]
    (debug "  using t" t "for n" n)
    (debug "  using execution-count" c)
    (loop [count      n
           delta-free 0
           deltas-sum (if (= n 1)
                        deltas-1
                        (util/sum deltas-1 deltas-n))]
      (let [deltas    (execute-expr-for-warmup c f)
            sum       (util/sum deltas-sum deltas)
            count     (+ count c)
            elapsed   (elapsed-time sum)
            cl-counts (-> deltas :class-loader :loaded-count)
            comp-time (-> deltas :compilation :compilation-time)]
        (if (pos? cl-counts)
          (debug "  classes loaded before" count "iterations"))
        (if (pos? comp-time)
          (debug "  compilation occurred before" count "iterations"))
        ;; (debug "elapsed-time" elapsed "count" count)
        (if (and (> delta-free 2) (> elapsed warmup-period))
          (let [estimated-fn-time (max 1 (quot elapsed count))]
            (progress "estimated-fn-time"
                      estimated-fn-time (float estimated-fn-time))
            [count sum estimated-fn-time])
          (recur count
                 (if (and (zero? cl-counts)
                          (zero? comp-time))
                   (inc delta-free)
                   0)
                 sum))))))

;;; Execution parameters
(defn estimate-execution-count
  "Estimate the number of executions required in order to have at least the
   specified execution period, check for the jvm to have constant class loader
   and compilation state."
  [period f gc-before-sample estimated-fn-time]
  (debug " period" period)
  (debug " estimated-fn-time" estimated-fn-time)
  (loop [n (max 1 (long (/ period (max 1 estimated-fn-time) 5)))]
    (when gc-before-sample
      (force-gc))
    (let [deltas    (execute-expr-for-warmup n f)
          t         (elapsed-time deltas)
          ;; It is possible for small n and a fast expression to get
          ;; t=0 nsec.  This is likely due to how (System/nanoTime)
          ;; quantizes the time on some systems.
          t         (max 1 t)
          cl-counts (-> deltas :class-loader :loaded-count)
          comp-time (-> deltas :compilation :compilation-time)]
      (debug " ..." n)
      (when (pos? comp-time)
        (warn "new compilations in execution estimation phase"))
      (if (and (>= t period)
               (zero? cl-counts)
               (zero? comp-time))
        n
        (recur (if (>= t period)
                 n
                 (min (* 2 n) (inc (long (* n (/ period t)))))))))))


;; benchmark
(defn run-benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   quick test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [sample-count warmup-jit-period target-execution-time f gc-before-sample
   overhead]
  (force-gc)
  (let [_                   (progress "Warm up for JIT optimisations ...")
        [warmup-n
         deltas
         estimated-fn-time] (warmup-for-jit warmup-jit-period f)
        _                   (progress "Estimate execution count ...")
        warmup-t            (elapsed-time deltas)
        n-exec              (estimate-execution-count
                              target-execution-time f gc-before-sample
                              estimated-fn-time)
        total-overhead      (long (* (or overhead 0) 1e9 n-exec))
        _                   (progress "Sample ...")
        _                   (debug
                              "Running with\n sample-count" sample-count \newline
                              "exec-count" n-exec \newline
                              "overhead[s]" overhead \newline
                              "total-overhead[ns]" total-overhead)
        _                   (force-gc)
        samples             (collect-samples
                              sample-count
                              n-exec
                              f
                              gc-before-sample)
        _                   (progress "Final GC...")
        gc-deltas           (force-gc)
        final-gc-time       (elapsed-time gc-deltas)
        sample-times        (->> samples
                                 (map elapsed-time)
                                 (map #(- % total-overhead)))
        total               (reduce + 0 sample-times)
        _                   (progress "Checking GC...")
        final-gc-result     (final-gc-warn total final-gc-time)]
    {:execution-count   n-exec
     :sample-count      sample-count
     :sample-times      sample-times
     :samples           samples
     :results           (map :value samples)
     :total-time        (/ total 1e9)
     :warmup-time       warmup-t
     :warmup-executions warmup-n
     :final-gc-time     final-gc-time
     :overhead          overhead}))


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
  (let [_           (progress
                      (format "Warm up %d expressions for %.2e sec each:"
                              (count exprs) (/ warmup-jit-period 1.0e9)))
        warmup      (vec (for [{:keys [f expr-string]} exprs]
                           (do (progress (format "    %s..." expr-string))
                               (warmup-for-jit warmup-jit-period f))))
        _           (progress
                      (format
                        (str "Estimate execution counts for %d expressions.  "
                             "Target execution time = %.2e sec:")
                        (count exprs) (/ target-execution-time 1.0e9)))
        exec-counts (map
                      (fn [{:keys [f expr-string] :as expr}
                           [warmup-n deltas estimated-fn-time]]
                        (progress (format "    %s..." expr-string))
                        (estimate-execution-count
                          target-execution-time f
                          gc-before-sample
                          estimated-fn-time))
                      exprs warmup)
        exprs       (map
                      (fn [idx expr exec-count]
                        (assoc expr :index idx :n-exec exec-count))
                      (range) exprs exec-counts)
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
               (let [expr            (dissoc (first data-seq) :sample)
                     n-exec          (:n-exec expr)
                     samples         (map :sample data-seq)
                     _               (progress "Final GC...")
                     gc-deltas       (force-gc)
                     final-gc-time   (elapsed-time gc-deltas)
                     sample-times    (map elapsed-time samples)
                     total           (reduce + 0 sample-times)
                     ;; TBD: Doesn't make much sense to attach final
                     ;; GC warning to the expression that happened
                     ;; to be first in the sequence, but that is
                     ;; what this probably does right now.  Think
                     ;; what might be better to do.
                     _               (progress "Checking GC..." total final-gc-time)
                     final-gc-result (final-gc-warn total final-gc-time)]
                 {:execution-count n-exec
                  :sample-count    sample-count
                  :sample-times    sample-times
                  :results         (map second samples)
                  :total-time      (/ total 1e9)}))
             all-samples)]
    all-samples))


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
  (debug "mean-estimate" mean-estimate
         "variance-estimate" variance-estimate
         "n" n)
  (let [mean-block     (point-estimate mean-estimate)
        variance-block (point-estimate variance-estimate)
        std-dev-block  (Math/sqrt variance-block)
        mean-action    (/ mean-block n)
        mean-g-min     (/ mean-action 2)
        sigma-g        (min (/ mean-g-min 4) (/ std-dev-block (Math/sqrt n)))
        variance-g     (* sigma-g sigma-g)
        c-max          (fn [t-min]
                         (let [j0  (- mean-action t-min)
                               k0  (- (* n n j0 j0))
                               k1  (+ variance-block (- (* n variance-g)) (* n j0 j0))
                               det (- (* k1 k1) (* 4 variance-g k0))]
                           (Math/floor (/ (* -2 k0) (+ k1 (Math/sqrt det))))))
        var-out        (fn [c]
                         (let [nmc (- n c)]
                           (* (/ nmc n) (- variance-block (* nmc variance-g)))))
        min-f          (fn [f q r]
                         (min (f q) (f r)))
        ]
    (if (zero? variance-block)
      0
      (/ (min-f var-out 1 (min-f c-max 0 mean-g-min)) variance-block))))


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
  (let [bm (benchmark*
             (fn [] 1)
             {:warmup-jit-period           (* 10 s-to-ns)
              :num-samples                 10
              :target-execution-time       (* 0.5 s-to-ns)
              :overhead                    0
              :supress-jvm-option-warnings true})]
    (-> bm :execution-time :lower-q first)))

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

(defn sample-stats
  "Compute sample statistics for the given values."
  [values sample-count execution-count opts]
  (let [outliers      (outliers values)
        tail-quantile (:tail-quantile opts)
        stats         (bootstrap-bca
                        (map double values)
                        (juxt
                          mean
                          variance
                          (partial quantile tail-quantile)
                          (partial quantile (- 1.0 tail-quantile)))
                        (:bootstrap-size opts)
                        [0.5 tail-quantile (- 1.0 tail-quantile)]
                        well/well-rng-1024a)
        analysis      (outlier-significance
                        (first stats)
                        (second stats)
                        sample-count)
        sqr           (fn [x] (* x x))
        m             (mean (map double values))
        s             (Math/sqrt (variance (map double values)))]
    {:outliers         outliers
     :mean             (scale-bootstrap-estimate
                         (first stats) (/ 1e-9 execution-count))
     :sample-mean      (scale-bootstrap-estimate
                         [m [(- m (* 3 s)) (+ m (* 3 s))]]
                         (/ 1e-9 execution-count))
     :variance         (scale-bootstrap-estimate
                         (second stats) (sqr (/ 1e-9 execution-count)))
     :sample-variance  (scale-bootstrap-estimate
                         [ (sqr s) [0 0]]
                         (sqr (/ 1e-9 execution-count)))
     :lower-q          (scale-bootstrap-estimate
                         (nth stats 2) (/ 1e-9 execution-count))
     :upper-q          (scale-bootstrap-estimate
                         (nth stats 3) (/ 1e-9 execution-count))
     :outlier-variance analysis
     :tail-quantile    (:tail-quantile opts)
     :sample-count     sample-count
     :execution-count  execution-count}))


(defn gc-sample-stats
  "Compute sample statistics for the given GC data."
  [counts times sample-count execution-count opts]
  (let [num-with-gc          (->> counts (filter pos?) count)
        total-num-gcs        (reduce + counts)
        frac-samples-with-gc (/ num-with-gc (count counts))
        total-gc-time        (reduce + times)
        mean-gc-time         (/ total-gc-time (count times))]
    {:num-with-gc          num-with-gc
     :total-num-gcs        total-num-gcs
     :frac-samples-with-gc frac-samples-with-gc
     :total-gc-time        total-gc-time
     :mean-gc-time         mean-gc-time
     :sample-count         sample-count
     :execution-count      execution-count}))


(defn benchmark-stats
  [data opts]
  (let [execution-count  (:execution-count data)
        sample-count     (:sample-count data)
        time-samples     (:sample-times data)
        time-stats       (sample-stats
                           time-samples
                           sample-count
                           execution-count
                           opts)
        gc-count-samples (map
                           #(-> % :garbage-collector :total :count)
                           (:samples data))
        gc-time-samples  (map
                           #(-> % :garbage-collector :total :time)
                           (:samples data))
        gc-stats         (gc-sample-stats
                           gc-count-samples
                           gc-time-samples
                           sample-count
                           execution-count
                           opts)]
    (merge data
           {:execution-time  time-stats
            :gc-stats        gc-stats
            :os-details      (jvm/os-details)
            :options         opts
            :runtime-details (->
                               (jvm/runtime-details)
                               (update-in [:input-arguments] vec))
            :sample-count    sample-count
            :execution-count execution-count})))

(defn warn-on-suspicious-jvm-options
  "Warn if the JIT options are suspicious looking."
  []
  (let [compiler (jvm/jit-name)
        {:keys [input-arguments]} (jvm/runtime-details)]
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
  [f {:keys [num-samples
             warmup-jit-period
             target-execution-time
             gc-before-sample
             overhead
             supress-jvm-option-warnings] :as options}]
  (when-not supress-jvm-option-warnings
    (warn-on-suspicious-jvm-options))
  (let [{:keys [num-samples
                warmup-jit-period
                target-execution-time
                gc-before-sample overhead] :as opts}
        (merge *default-benchmark-opts*
               {:overhead (or overhead (estimated-overhead))}
               options)
        data (run-benchmark
               num-samples
               warmup-jit-period
               target-execution-time
               f
               opts
               overhead)]
    (benchmark-stats data opts)))

(defn benchmark-round-robin*
  [exprs options]
  (let [opts  (merge *default-benchmark-opts* options)
        times (run-benchmarks-round-robin
                (:num-samples opts)
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
                                  {:f           `(fn [] ~expr)
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
    (> measurement 60)   [(/ 60) "min"]
    (< measurement 1e-6) [1e9 "ns"]
    (< measurement 1e-3) [1e6 "µs"]
    (< measurement 1)    [1e3 "ms"]
    :else                [1 "sec"]))


(def one-kb 1024)
(def one-mb (* 1024 1024))
(def one-gb (* 1024 1024 1024))


(defn scale-memory
  "Determine a scale factor and unit for displaying a memory."
  [measurement]
  (cond
    (< measurement one-kb) [1 "bytes"]
    (< measurement one-mb) [(/ one-kb) "Kb"]
    (< measurement one-gb) [(/ one-mb) "Mb"]
    :else                  [(/ one-gb) "Gb"]))

(defn format-value [value scale unit]
  (format "%f %s" (* scale value) unit))

(defn report-estimate
  [msg estimate significance scale-fn]
  (let [mean          (first estimate)
        [factor unit] (scale-fn mean)]
    (apply
      report "%32s : %s  %2.1f%% CI: (%s, %s)\n"
      msg
      (format-value mean factor unit)
      (* significance 100)
      (map #(format-value % factor unit) (last estimate)))))

(defn report-point-estimate
  ([msg estimate scale-fn]
   (let [mean          (first estimate)
         [factor unit] (scale-fn mean)]
     (report "%32s : %s\n" msg (format-value mean factor unit))))
  ([msg estimate quantile scale-fn]
   (let [mean          (first estimate)
         [factor unit] (scale-fn mean)]
     (report
       "%32s : %s (%4.1f%%)\n"
       msg (format-value mean factor unit) (* quantile 100)))))

(defn report-estimate-sqrt
  [msg estimate significance scale-fn]
  (let [mean          (Math/sqrt (first estimate))
        [factor unit] (scale-fn mean)]
    (apply
      report "%32s : %s  %2.1f%% CI: (%s, %s)\n"
      msg
      (format-value mean factor unit)
      (* significance 100)
      (map #(format-value (Math/sqrt %) factor unit) (last estimate)))))

(defn report-point-estimate-sqrt
  [msg estimate scale-fn]
  (let [mean          (Math/sqrt (first estimate))
        [factor unit] (scale-fn mean)]
    (report "%32s : %s\n" msg (format-value mean factor unit))))

(defn report-outliers [results]
  (let [outliers     (:outliers results)
        values       (vals outliers)
        labels       {:unaffected "unaffected"
                      :slight     "slightly inflated"
                      :moderate   "moderately inflated"
                      :severe     "severely inflated"}
        sample-count (:sample-count results)
        types        ["low-severe" "low-mild" "high-mild" "high-severe"]]
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


(defn report-sample-stats [results dimension scale-fn verbose]
  (when verbose
    (report-point-estimate
      (str dimension " sample mean")
      (:sample-mean results)
      scale-fn))
  (report-point-estimate (str dimension " mean") (:mean results) scale-fn)
  (when verbose
    (report-point-estimate-sqrt
      (str dimension " sample std-deviation")
      (:sample-variance results)
      scale-fn))
  (report-point-estimate-sqrt
    (str dimension" std-deviation")
    (:variance results)
    scale-fn)
  (report-point-estimate
    (str dimension " lower quantile")
    (:lower-q results) (:tail-quantile results)
    scale-fn)
  (report-point-estimate
    (str dimension " upper quantile")
    (:upper-q results) (- 1.0 (:tail-quantile results))
    scale-fn)
  (when-let [overhead (:overhead results)]
    (when (pos? overhead)
      (report-point-estimate "Overhead used" [overhead] scale-fn)))
  (report-outliers results))

(defn report-gc-sample-stats
  [results verbose]
  (when (pos? (:num-with-gc results))
    (println)
    (println "Samples included" (:total-num-gcs results) "GCs.")
    (println "  Effected samples:"
             (* 100 (:frac-samples-with-gc results))
             "%")))

(defn report-result [{:keys [execution-time gc-stats] :as results} & opts]
  (let [verbose      (some #(= :verbose %) opts)
        show-os      (or verbose (some #(= :os %) opts))
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
    (println)
    (report-sample-stats execution-time "Execution time" scale-time verbose)
    (report-gc-sample-stats gc-stats verbose)))

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
