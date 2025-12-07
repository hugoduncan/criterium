(ns criterium.sampled-fn
  "First-class function instrumentation for aggregated sampling.

  Provides functionality for wrapping functions with instrumentation code
  that collects performance data during execution. The instrumented functions
  are first-class objects that maintain their own sample collection state."
  (:require
   [criterium.collect-plan :as collect-plan]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.metric :as metric]
   [criterium.sampler :as sampler]
   [criterium.util.invariant :refer [have]]
   [criterium.util.t-digest :as t-digest])
  (:import
   [java.util.concurrent Callable]))

(defn- measured [original-fn]
  (measured/measured
   (fn state-f [] (assert false))
   (fn measured-f [args ^long eval-count]
     (have (partial = 1) eval-count)
     (let [start  (jvm/timestamp)
           res    (apply original-fn args)
           finish (jvm/timestamp)]
       [(unchecked-subtract finish start) res]))))

(defmacro ^:private invoke-f
  [collector measured metric-keys args]
  `(let [sample# (collector/collect ~collector ~measured ~args 1)]
     (set! ~'digests (reduce
                      (fn [ds# k#]
                        (update ds# k# t-digest/add-point (get-in sample# k#)))
                      ~'digests
                      ~metric-keys))
     (:expr-value sample#)))

(defn- sample-map
  "Convert t-digests into an analyzable sample map structure.

  Takes collected samples and metrics configurations and produces a map
  in the format expected by criterium's analysis functions."
  [metrics-defs digests]
  {:type           :criterium/digest
   :metric->digest (update-vals digests t-digest/compress)
   :transform      collect-plan/identity-transforms
   :metrics-defs   metrics-defs
   :expr-value     nil
   :source-id      nil})

(deftype SampledFn
         [^clojure.lang.IFn original-fn
          metric-keys
          collector
          measured
          ^:volatile-mutable digests]

  sampler/Sampler
  (samples-map [_] (sample-map (:metrics-defs collector) digests))
  (reset-samples! [_] (set! digests (zipmap metric-keys (repeat (t-digest/new-digest)))))

  clojure.lang.IFn
  (invoke [_]
    (invoke-f collector measured metric-keys []))
  (invoke [_ a1]
    (invoke-f collector measured metric-keys [a1]))
  (invoke [_ a1 a2]
    (invoke-f collector measured metric-keys [a1 a2]))
  (invoke [_ a1 a2 a3]
    (invoke-f collector measured metric-keys [a1 a2 a3]))
  (invoke [_ a1 a2 a3 a4]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4]))
  (invoke [_ a1 a2 a3 a4 a5]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5]))
  (invoke [_ a1 a2 a3 a4 a5 a6]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7 a8]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7 a8 a9]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
    (invoke-f collector measured metric-keys [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]))
  (invoke [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]))
  (invoke
    [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]))
  (invoke
    [_ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20]
    (invoke-f
     collector
     measured
     metric-keys
     [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20]))
  (applyTo [_ args]
    (invoke-f collector measured metric-keys args))

  Runnable
  (run [_]
    (invoke-f collector measured metric-keys []))

  Callable
  (call [_]
    (invoke-f collector measured metric-keys [])))

(defn sample-fn
  "Create an sample wrapper of function f.

  Takes a function and a collector configuration and returns a new
  function that wraps the original while collecting timing samples
  during execution.  The returned function implements IFn, Runnable, and
  Callable interfaces.

  Parameters:
    f                - The function to instrument
    collector-config - A collector configuration that defines how samples
                       are processed

  Returns:
    An InstrumentedFn instance that wraps the original function and maintains
    its own sample collection state."
  [f collector-config]
  (let [collector      (collector/collector collector-config)
        metrics-defs   (-> (:metrics-defs collector)
                           (metric/filter-metrics
                            (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        metric-keys    (mapv :path metric-configs)]
    (->SampledFn
     f
     metric-keys
     collector
     (measured f)
     (zipmap metric-keys (repeat (t-digest/new-digest))))))
