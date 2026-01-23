(ns criterium.kaocha-hooks
  "Kaocha hooks for criterium test runs.

  Provides hooks for:
  - Malli instrumentation to validate function inputs and outputs
  - Suppressing warnings from third-party libraries during test loading

  **Load Order:**
  This namespace pre-loads noisy third-party namespaces with warnings
  disabled BEFORE loading criterium.schema.instrument. This prevents
  warning noise from malli.generator and other libraries that are
  pulled in transitively by the instrumentation system."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

;;; Third-party namespaces with known reflection or boxed math warnings
;;
;; Must be loaded before criterium.schema.instrument to prevent warnings.
;; Loaded from build/noisy-namespaces.edn resource (shared with build.check-warnings).

(def ^:private noisy-namespaces
  "Third-party namespaces that emit reflection or boxed math warnings.
  Each require wrapped in try/catch for silent failure when deps aren't on classpath."
  (-> (io/resource "build/noisy-namespaces.edn")
      slurp
      edn/read-string))

;;; Pre-load noisy namespaces at compile time
;;
;; Executes immediately when this namespace loads, BEFORE the subsequent
;; require of criterium.schema.instrument which would otherwise trigger
;; warnings from malli.generator.

(binding [*warn-on-reflection* false
          *unchecked-math* false]
  (doseq [ns-sym noisy-namespaces]
    (try
      (require ns-sym)
      (catch Exception _))))

;;; Load instrumentation support (now warning-free)

(require '[criterium.schema.instrument :as inst])

;;; Hook functions

(defn suppress-warnings-pre-load
  "Pre-load hook that requires noisy third-party namespaces with warnings disabled.

  Pre-loads namespaces that emit reflection or boxed math warnings before
  the test suite's warning flags are in effect. Returns config unchanged.

  Note: Most pre-loading happens when this namespace loads. This hook
  serves as a no-op for test runs that don't use the kaocha-hooks namespace
  during development."
  [config]
  config)

(defn instrument-pre-run
  "Pre-run hook that enables malli instrumentation.

  Returns the test-plan unchanged after enabling instrumentation."
  [test-plan]
  (inst/instrument!)
  test-plan)

(defn unstrument-post-run
  "Post-run hook that disables malli instrumentation.

  Returns the test-plan unchanged after disabling instrumentation."
  [test-plan]
  (inst/unstrument!)
  test-plan)
