(ns criterium.bench
  "REPL based benchmarking."
  (:refer-clojure :exclude [time])
  (:require
   [criterium.bench.config :as bench-config]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector :as collector]
   [criterium.measured :as measured]
   [criterium.util.output :as output]))

(def ^:no-doc last-bench* (volatile! nil))

(defn last-bench
  "Return the data from the last bench invocation."
  []
  @last-bench*)

(defn measure
  "Samples measured and returns evaluated measurement data."
  [collector-config collect-plan benchmark benchmark-options
   measured]
  (let [pipeline        (collector/collector collector-config)
        metrics-configs (:metrics-configs pipeline)
        sampled         (collect-plan/collect
                         collect-plan
                         metrics-configs
                         pipeline
                         measured)]
    (benchmark
     (assoc (merge benchmark-options sampled)
            :metrics-configs metrics-configs))))

(defn- return-value [config sampled]
  (get-in sampled (-> config :return-value)))

(defn bench-measured*
  "Evaluates measured and outputs the time it took.

  By default, return the value of calling the measured's wrapped
  function.

  The timing info is available as a data structure by calling last-time.

  Takes a configuration map that fully specifies the time behaviour."
  [measured config]
  (output/with-progress-reporting (:verbose config)
    (->> (measure
          (:collector-config config)
          (:collect-plan config)
          (:benchmark config)
          {:viewer (:viewer config)}
          measured)
         (vreset! last-bench*)
         (return-value config))))

(defn bench-measured
  "Evaluates a measured and outputs the time it took.

  By default, return the value of calling the measured's wrapped
  function.

  The timing info is available as a data structure by calling last-time.

  Takes a map of options.

  By default the output is printed, but can also be pretty printed or
  sent to portal, by passing either `:pprint` or `:portal` to the
  `:viewer` key.

  The analysis and output can be controlled by passing a benchmark map
  to the `:benchmark` key.  Example benchmark maps can be found in the
  `clojure.benchmarks` namespace.

  The :metric-ids option accepts a sequence of metric keyword
  selectors. Valid metrics are:
     :elapsed-time, :garbage-collector, :finalization, :memory,
     :thread-allocation, :compilation, :measured-args
     and :class-loader."
  [measured options]
  (bench-measured* measured (bench-config/config-map options)))

(defmacro bench
  "Evaluates an expression and outputs benchmarks for it.

  The expression can not refer to locals.

  By default, return the value of calling the measured's wrapped
  function.

  The timing info is available as a data structure by calling last-time.

  Takes optional ketword value option pairs.

  By default the output is printed, but can also be pretty printed or
  sent to portal, by passing either `:pprint` or `:portal` to the
  `:viewer` key.

  The analysis and output can be controlled by passing a benchmark map
  to the `:benchmark` key.  Example benchmark maps can be found in the
  `clojure.benchmarks` namespace.

  The :metric-ids option accepts a sequence of metric keyword
  selectors. Valid metrics are:
     :elapsed-time, :garbage-collector, :finalization, :memory,
     :thread-allocation, :compilation, :measured-args
     and :class-loader."
  [expr & options]
  (let [options-map  (apply hash-map options)
        expr-options (select-keys options-map [:time-fn])
        options      (dissoc options-map :time-fn)]
    `(bench-measured
      (measured/expr ~expr ~expr-options)
      ~options)))
