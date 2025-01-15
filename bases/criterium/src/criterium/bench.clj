(ns criterium.bench
  "Perform sound benchmarking of Clojure code.

  Provides functions and macros for measuring code performance while
  accounting for:

  - JVM warmup periods
  - Garbage collection effects
  - Statistical significance

  Primary API:
  - bench         - Macro for benchmarking expressions
  - bench-measured - Function for benchmarking pre-wrapped measurements
  - last-bench    - Access results from most recent benchmark

  Example:
  (bench (+ 1 1))                 ; Basic usage
  (bench (+ 1 1) :viewer :pprint) ; With pretty-printed output"
  (:require
   [criterium.bench.config :as bench-config]
   [criterium.bench.impl :as impl]
   [criterium.benchmark :as benchmark]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector :as collector]
   [criterium.measured :as measured]
   [criterium.util.output :as output]))

(defn last-bench
  "Returns the complete measurement data from the most recent benchmark.

  The returned data structure contains all metrics, statistical analysis, and
  raw measurements from the last call to bench or bench-measured.

  Returns nil if no benchmarks have been run in the current session.

  Example:
  (bench (+ 1 1))
  (let [results (last-bench)]
    ;; Access detailed metrics from results
    )"
  []
  (impl/last-bench))

(defn collect-data-map
  "Collect metrics according to the sampling plan.

  Parameters:
    collector-config - Configuration for the metric collector
    collect-plan    - Strategy for collecting samples
    measured        - The wrapped code/function to measure

  Returns collected measurements."
  [collector-config collect-plan measured]
  (let [collector (collector/collector collector-config)]
    (collect-plan/collect collect-plan collector measured)))

(defn analyze
  "Apply statistical analysis to collected metrics.

  Parameters:
    analyse-config - Vector of analysis steps to perform
    metrics       - Raw metrics from collect-metrics

  Returns analyzed metrics with statistical computations added."
  [analyse-plan data-map]
  (let [analyze (benchmark/->analyse analyse-plan)]
    (analyze data-map)))

(defn view
  "Format and present analyzed metrics.

  Parameters:
    view-config - Vector of view components to include
    metrics     - Analyzed metrics from analyze-metrics
    options     - Additional view options like :viewer

  Returns the viewed metrics data structure."
  [view-plan viewer data-map]
  (let [view (benchmark/->view view-plan)]
    (view viewer data-map)))

(defn- return-value
  "Extract the returned value for the sampled."
  [config bench-map]
  (get-in bench-map (:return-value config)))

(defn bench-measured
  "Evaluate measured and output the benchmark time.

  By default, return the value of calling the measured's wrapped
  function.

  The timing info is available as a data structure by calling last-time.

  Takes a bench-plan that fully specifies the benchmark behaviour."
  [bench-plan measured]
  (output/with-progress-reporting (:verbose bench-plan)
    (let [data-map (->> (collect-data-map
                         (:collector-config bench-plan)
                         (:collect-plan bench-plan) measured)
                        (analyze (:analyse bench-plan)))]
      (view (:view bench-plan) (:viewer bench-plan) data-map)
      (impl/last-bench! {:bench-plan bench-plan :data data-map})
      (return-value bench-plan data-map))))

#_(defn bench-measured
    "Evaluate and benchmark a pre-wrapped measurement.

  The metrics and output are controlled via parameters.

  Parameters:
    measured - A wrapped function/expression prepared for measurement
    options  - Map of configuration options:
      :viewer      - Output format [:pprint, :portal, or nil(default)]
      :analyse     - Vector of analysis steps [[:outliers] [:stats]]
      :view       - Vector of view components [:stats]
      :metric-ids  - Vector of metrics to collect, from:
                     [:elapsed-time :garbage-collector :finalization
                      :memory :thread-allocation :compilation
                      :measured-args :class-loader]
      :limit-time-s - Time limit in seconds (optional)
      :collect-plan - Sampling strategy (optional)

  Return:
  The value from evaluating the measured expression.
  The complete benchmark data is available via (last-bench).

  Examples:
  ;; Basic usage with a measured expression
  (bench-measured my-measured {})

  ;; With pretty-printed output and specific metrics
  (bench-measured my-measured
    {:viewer :pprint
     :metric-ids [:elapsed-time :memory]})

  Notes:
  - Ensures statistical significance through multiple samples
  - Accounts for JVM warmup
  - Handles GC interference"
    [measured options]
    (bench-measured* (bench-config/config-map options) measured))

(defn options->bench-plan
  "Explicit conversion of `bench` options into a bench-plan."
  [& {:as options}]
  (bench-config/config-map options))


(defmacro bench
  "Main macro for benchmarking Clojure expressions with statistical rigor.

  Intended for simplified use at the REPL.

  Takes an expression to benchmark, and optional configuration options.

  The expression must be free of local references.

  Parameters:
    expr    - Expression to benchmark
    options - Keyword/value pairs for configuration:
      :viewer      - Output format [:pprint, :portal, or nil(default)]
      :analyse     - Vector of analysis steps [[:outliers] [:stats]]
      :view       - Vector of view components [:stats]
      :metric-ids  - Vector of metrics to collect, from:
                     [:elapsed-time :garbage-collector :finalization
                      :memory :thread-allocation :compilation
                      :measured-args :class-loader]
      :limit-time-s - Time limit in seconds (optional)
      :collect-plan - Sampling strategy (optional)
      :time-fn     - Custom timing function (optional)

  Returns:
  The value from evaluating the expression.
  Complete benchmark data available via (last-bench).

  Examples:
  ;; Basic usage
  (bench (+ 1 1))

  ;; With pretty-printed output
  (bench (+ 1 1) :viewer :pprint)

  ;; With specific metrics and time limit
  (bench (my-function)
         :metric-ids [:elapsed-time :memory]
         :limit-time-s 5)

  (bench (+ 1 1)
         :viewer :portal
         :benchmark (criterium.benchmark/->benchmark
                   {:analyse [[:quantiles {:quantiles [0.025 0.5 0.975]}]
                              :outliers
                              :stats]
                   :view [:stats :quantiles :samples :histogram]}))

  For the portal viewer, you will need to have portal connected to tap>.

  Notes:
  - Handles JVM warmup automatically
  - Accounts for GC interference
  - Ensures statistical significance
  - Expression cannot refer to local bindings"
  [expr & options]
  (let [options-map  (apply hash-map options)
        expr-options (select-keys options-map [:time-fn])
        options      (dissoc options-map :time-fn)]
    `(bench-measured
      (options->bench-plan ~options)
      (measured/expr ~expr ~expr-options))))
