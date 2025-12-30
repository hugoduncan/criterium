(ns criterium.kaocha-hooks
  "Kaocha hooks for criterium test runs.

  Enables malli instrumentation before tests run to validate function
  inputs and outputs at runtime.")

;; Pre-require malli.generator with *unchecked-math* bound to nil
;; to suppress boxed math warnings from malli.generator.cljc
(binding [*unchecked-math* nil]
  (try
    (require 'malli.generator)
    (catch Exception _)))

(require '[criterium.schema.instrument :as inst])

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
