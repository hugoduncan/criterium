(ns criterium.instrument
  "Instrumentation facilities for collecting performance samples from functions.

  This namespace provides tools for measuring function performance
  during normal execution, outside of criterium's direct control. It
  works by wrapping functions with instrumentation code that collects
  timing data while preserving the original function behavior.

  Key features:
  - Non-intrusive function wrapping that maintains original behavior
  - Automatic sample collection during function execution
  - Safe metadata management for storing/restoring original functions
  - Integration with criterium's analysis pipeline

  Example usage:
  ```clojure
  (with-instrumentation [my-fn collector-config]
    (some-code
      (my-fn args)))
  ```

  The instrumentation can also be manually controlled using
  instrument!/uninstrument!  for more fine-grained control over the
  scope which is sampled."
  (:refer-clojure :exclude [reset!])
  (:require
   [criterium.collect :as collect]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

(def ^:private original-f ::original-f)
(def ^:private samples ::samples)

(defn- wrap [f sample-atom pipeline]
  {:pre [(fn? f)
         (instance? clojure.lang.Atom sample-atom)]}
  (let [measured (measured/measured
                  (fn state-f [] (assert false))
                  (fn measured-f [args eval-count]
                    (assert (= 1 eval-count))
                    (let [start  (jvm/timestamp)
                          res    (apply f args)
                          finish (jvm/timestamp)]
                      [(unchecked-subtract finish start) res])))]
    (assert (:length pipeline) pipeline)
    (fn instrumented-f [& args]
      (let [sample (collector/collect pipeline measured args 1)]
        (swap! sample-atom conj sample)
        (:expr-value sample)))))

(defn instrument!
  "Add instrumentation to the var, v, for performance sampling.

  Takes a var and a pipeline configuration, wraps the function to
  collect timing samples during execution while preserving the original
  function behavior. The instrumentation stores the original function
  and sample data in the var's metadata.

  This function is idempotent - calling it multiple times on the same var
  will only instrument it once.

  You must use uninstrument! to remove the instrumentation and restore
  the original function.

  Parameters:
    v        - The var to instrument (e.g. #'my-namespace/my-function)
    pipeline - A collector pipeline configuration that defines how samples
               are processed

  Side effects:
    - Modifies the var's root binding to install the instrumented function
    - Adds metadata to track the original function and store samples"
  [v pipeline]
  (when-not (original-f (meta v))
    (let [sample-atom (atom [])]
    (alter-meta! v assoc original-f @v samples sample-atom)
    (alter-var-root v wrap sample-atom pipeline))))

(defn uninstrument!
  "Remove instrumentation from the var, v and restore original function.

  Reverses the effects of instrument! by:
  - Restoring the original function as the var's root binding
  - Removing tracking metadata added during instrumentation

  This function is idempotent - calling it multiple times on the same var
  is safe and will only uninstrument once. Safe to call on vars that
  aren't instrumented."
  [v]
  (when-let [f (original-f (meta v))]
    (alter-var-root v (constantly f))
    (alter-meta! v dissoc original-f samples)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn reset!
  "Reset the sample collection on the var, v.

  Clears all collected samples from an instrumented var while
  maintaining the instrumentation. Useful when you want to start a fresh
  sampling session without removing and re-adding instrumentation."
  [v]
  (clojure.core/reset! (some-> v meta samples) []))

(defn sample-map
  "Convert raw samples into an analyzable sample map structure.

  Takes collected samples and metrics configurations and produces a map
  in the format expected by criterium's analysis functions. The
  resulting map includes:

  - batch-size: Always 1 for instrumented functions
  - eval-count: Total number of samples collected
  - samples: A map of metric keys to sample vectors with criterium metadata
  - metrics-configs: The original metrics configuration

  The returned map is compatible with criterium's analysis functions."
  [metrics-configs samples]
  {:batch-size      1
   :eval-count      (count samples)
   :samples         (with-meta
                      ((collect/sample-maps->map-of-samples metrics-configs)
                       samples)
                      {:type      :criterium/samples
                       :transform {:sample-> identity :->sample identity}})
   :metrics-configs metrics-configs})

(defmacro with-instrumentation
  "Provides a scope within which the top level function f is instrumented.

  Creates a controlled environment for collecting performance samples
  from a function during normal execution. The macro:

  1. Instruments the specified function
  2. Executes the body forms
  3. Collects timing samples during execution
  4. Restores the original function
  5. Returns a tuple of [sample-data body-result]

  Parameters:
    f                - Symbol naming the function to instrument
    collector-config - Configuration map for the sample collector
    body            - Forms to execute while collecting samples

  Returns: [sample-map body-result]
    - sample-map: Map of collected performance data ready for analysis
    - body-result: Value from evaluating the body forms"
  [[f collector-config] & body]
  {:pre [f collector-config]}
  `(let [v#                (var ~f)
         collector-config# ~collector-config
         pipeline#         (collector/collector collector-config#)
         metrics-configs#  (:metrics-configs pipeline#)]
     (try
       (instrument! v# pipeline#)
       (let [start#   (jvm/timestamp)
             res#     (do ~@body)
             finsih#  (jvm/timestamp)
             samples# (-> v# meta ~samples deref)]
         [(assoc (sample-map metrics-configs# samples#)
                 :elapsed-time (unchecked-subtract finsih# start#))
          res#])
       (finally
         (uninstrument! v#)))))
