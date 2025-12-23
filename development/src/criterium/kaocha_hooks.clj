(ns criterium.kaocha-hooks
  "Kaocha hooks for criterium test runs.

  Enables malli instrumentation before tests run to validate function
  inputs and outputs at runtime."
  (:require
   [criterium.schema.instrument :as inst]))

(defn instrument-pre-run
  "Pre-run hook that enables malli instrumentation.

  Returns the test-plan unchanged after enabling instrumentation."
  [test-plan]
  (inst/instrument!)
  test-plan)
