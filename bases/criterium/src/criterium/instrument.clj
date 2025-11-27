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
   [criterium.instrument-fn :as instrument-fn]))

(def ^:private original-f ::original-f)

(defn instrument!
  "Add instrumentation to the var, v, for performance sampling.

  Takes a var and a collector configuration, wraps the function to
  collect timing samples during execution while preserving the original
  function behavior. The instrumentation stores the original function
  and sample data in the var's metadata.

  This function is idempotent - calling it multiple times on the same var
  will only instrument it once.

  You must use uninstrument! to remove the instrumentation and restore
  the original function.

  Parameters:
    v                - The var to instrument (e.g. #'my-namespace/my-function)
    collector-config - A collector pipeline configuration that defines
                       how samples are processed

  Side effects:
    - Modifies the var's root binding to install the instrumented function
    - Adds metadata to track the original function and store samples"
  [v collector-config]
  (when-not (original-f (meta v))
    (let [inst-fn (instrument-fn/instrument-fn @v collector-config)]
      (alter-meta! v assoc original-f @v)
      (alter-var-root v (constantly inst-fn)))))

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
    (alter-meta! v dissoc original-f)))
