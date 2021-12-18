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
  (:require
   [clojure.set :as set]
   [criterium.bench :as bench]
   [criterium.benchmark :as benchmark]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

;; Default values controlling behaviour

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ^:dynamic *final-gc-problem-threshold*
  "Fraction of excution time allowed for final cleanup before a
  warning is issued."
  0.01)

(def s-to-ns (* 1000 1000 1000)) ; in ns

(def ^:dynamic *warmup-jit-period*
  "Time period used to let the code run so that jit compiler can do
  its work."
  (* 10 s-to-ns)) ; in ns

(def ^:dynamic *sample-count*
  "Number of executions required"
  60)

(def ^:dynamic *target-execution-time*
  "Target elapsed time for execution for a single measurement."
  (* 1 s-to-ns)) ; in ns

(def ^:dynamic *max-gc-attempts*
  "Maximum number of attempts to run finalisers and gc."
  100)

(def ^:dynamic *default-benchmark-opts*
  {:max-gc-attempts       *max-gc-attempts*
   :num-samples           *sample-count*
   :target-execution-time *target-execution-time*
   :warmup-jit-period     *warmup-jit-period*
   :tail-quantile         0.025
   :bootstrap-size        1000})

(def ^:dynamic *default-quick-bench-opts*
  {:max-gc-attempts       *max-gc-attempts*
   :num-samples           (/ *sample-count* 10)
   :target-execution-time (/ *target-execution-time* 10)
   :warmup-jit-period     (/ *warmup-jit-period* 2)
   :tail-quantile         0.025
   :bootstrap-size        500})

(defn options->time-config
  [{:keys [max-gc-attempts target-execution-time warmup-jit-period]
    :as   options}]
  {:collect-plan
   {:scheme-type      :with-jit-warmup
    :max-gc-attempts  max-gc-attempts
    :batch-time-ns    target-execution-time
    :warmup-period-ns warmup-jit-period}
   :benchmark (benchmark/->benchmark
               {:analyse [:stats
                          :event-stats]
                :view    (into (filterv some?
                                        [(when (:os options)
                                           :os)
                                         (when (:runtime options)
                                           :runtime)])
                               [:stats
                                :event-stats])})})

;;; Progress reporting

(def ^:dynamic *report-progress*
  "Flag to control output of progress messages"
  nil)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:skip-wiki progress
  "Conditionally report progress to *out*."
  [& message]
  (when *report-progress*
    (apply println message)))

(def ^:dynamic *report-debug*
  "Flag to control output of debug messages"
  nil)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:skip-wiki debug
  "Conditionally report debug to *out*."
  [& message]
  (when *report-debug*
    (apply println message)))

(def ^:dynamic *report-warn*
  "Flag to control output of warning messages"
  nil)

(defn ^:skip-wiki warn
  "Conditionally report warn to *out*."
  [& message]
  (when *report-warn*
    (apply println "WARNING:" message)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro with-progress-reporting
  "Macro to enable progress reporting during the benchmark."
  [expr]
  `(binding [*report-progress* true]
     ~expr))

;;; Overhead estimation

(declare benchmark*)

;;; Options

(defn extract-report-options
  "Extract reporting options from the given options vector.  Returns a two
  element vector containing the reporting options followed by the non-reporting
  options"
  [opts]
  (let [known-options #{:os :runtime :verbose}
        option-set    (set opts)]
    (into
     (vec
      (interleave
       (set/intersection known-options option-set)
       (repeat true)))
     (remove #(contains? known-options %1) opts))))

;;; Sample statistic

(defn warn-on-suspicious-jvm-options
  "Warn if the JIT options are suspicious looking."
  []
  (let [compiler                  (jvm/jit-name)
        {:keys [input-arguments]} (jvm/runtime-details)]
    (when-let [arg (and (re-find #"Tiered" compiler)
                        (some #(re-find #"TieredStopAtLevel=(.*)" %)
                              input-arguments))]
      (warn
       "JVM argument" (first arg) "is active,"
       "and may lead to unexpected results as JIT C2 compiler may not be active."
       "See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies."))))

;;; User top level functions

(defn benchmark*
  "Benchmark a function. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [measured
   {:keys [supress-jvm-option-warnings]
    :as   options}]
  {:pre [(measured/measured? measured)]}
  (when-not supress-jvm-option-warnings
    (warn-on-suspicious-jvm-options))
  (let [opts   (merge *default-benchmark-opts* options)
        config (options->time-config opts)]
    (bench/bench-measured measured config)))

(defmacro ^:deprecated benchmark
  "Benchmark an expression. This tries its best to eliminate sources of error.
   This also means that it runs for a while.  It will typically take 70s for a
   fast test expression (less than 1s run time) or 10s plus 60 run times for
   longer running expressions."
  [expr options]
  `(benchmark* (measured/expr ~expr) ~options))

(defn quick-benchmark*
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  [f {:as options}]
  (benchmark* f (merge *default-quick-bench-opts* options)))

(defmacro ^:deprecated quick-benchmark
  "Benchmark an expression. Less rigorous benchmark (higher uncertainty)."
  [expr options]
  `(quick-benchmark* (measured/expr ~expr) ~options))

;;; All in one invocations

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var :deprecated-var]}
(defmacro bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
  to *out* in human readable format. Options for report format are: :os,
  :runtime, and :verbose."
  [expr & opts]
  (let [options (extract-report-options opts)]
    `(benchmark
      ~expr
      ~(when (seq options) (apply hash-map options)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var :deprecated-var]}
(defmacro quick-bench
  "Convenience macro for benchmarking an expression, expr.  Results are reported
  to *out* in human readable format. Options for report format are: :os,
  :runtime, and :verbose."
  [expr & opts]
  (let [options (extract-report-options opts)]
    `(quick-benchmark
      ~expr
      ~(when (seq options) (apply hash-map options)))))
