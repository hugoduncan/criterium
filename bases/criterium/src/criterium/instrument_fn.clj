(ns criterium.instrument-fn
  "First-class function instrumentation for performance sampling.

  Provides functionality for wrapping functions with instrumentation code
  that collects performance data during execution. The instrumented functions
  are first-class objects that maintain their own sample collection state."
  (:require
   [criterium.collect :as collect]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.util.invariant :refer [have]])
  (:import
   [java.util.concurrent Callable]))

(defprotocol InstrumentationState
  "Protocol for accessing instrumentation state"
  (samples-map [this] "Get the current samples collected")
  (reset-samples! [this] "Clear all collected samples")  )

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
  [pipeline measured args]
  `(let [sample# (collector/collect ~pipeline ~measured ~args 1)]
     (set! ~'samples (conj ~'samples sample#))
     (:expr-value sample#)))

(defn- sample-map
  "Convert raw samples into an analyzable sample map structure.

  Takes collected samples and metrics configurations and produces a map
  in the format expected by criterium's analysis functions."
  [metrics-defs samples]
  {:type           :criterium/metrics-samples
   :metric->values (collect/sample-maps->map-of-samples
                    samples
                    metrics-defs)
   :transform      collect-plan/identity-transforms
   :batch-size     1
   :eval-count     (count samples)
   :num-samples    (count samples)
   :metrics-defs   (have metrics-defs)
   :expr-value     nil
   :source-id      nil})

(deftype InstrumentedFn
  [original-fn
   pipeline
   measured
   ^:volatile-mutable samples]
  InstrumentationState
  (samples-map [_] (sample-map (:metrics-configs pipeline) samples))
  (reset-samples! [_] (set! samples []) nil)

  clojure.lang.IFn
  (invoke [this]
    (invoke-f pipeline measured []))
  (invoke [this a1]
    (invoke-f pipeline measured [a1]))
  (invoke [this a1 a2]
    (invoke-f pipeline measured [a1 a2]))
  (invoke [this a1 a2 a3]
    (invoke-f pipeline measured [a1 a2 a3]))
  (invoke [this a1 a2 a3 a4]
    (invoke-f pipeline measured [a1 a2 a3 a4]))
  (invoke [this a1 a2 a3 a4 a5]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5]))
  (invoke [this a1 a2 a3 a4 a5 a6]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7 a8]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7 a8 a9]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
    (invoke-f pipeline measured [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]))
  (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]))
  (invoke
    [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]))
  (invoke
    [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20]
    (invoke-f
      pipeline
      measured
      [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20]))
  (applyTo [this args]
    (invoke-f pipeline measured args))

  Runnable
  (run [this]
    (invoke-f pipeline measured []))

  Callable
  (call [this]
    (invoke-f pipeline measured [])))

(defn instrument-fn
  "Create an instrumented wrapper of function f.

  Takes a function and a collector pipeline configuration and returns a
  new function that wraps the original while collecting timing samples
  during execution.  The returned function implements IFn, Runnable, and
  Callable interfaces.

  Parameters:
    f        - The function to instrument
    pipeline - A collector pipeline configuration that defines how samples
               are processed

  Returns:
    An InstrumentedFn instance that wraps the original function and maintains
    its own sample collection state."
  [f pipeline-config]
  (->InstrumentedFn
   f
   (collector/collector pipeline-config)
   (measured f)
   []))
