(ns criterium.kaocha-hooks
  "Kaocha hooks for criterium test runs.

  Provides hooks for:
  - Malli instrumentation to validate function inputs and outputs
  - Suppressing warnings from third-party libraries during test loading")

(require '[criterium.schema.instrument :as inst])

;;; Third-party namespaces with known reflection or boxed math warnings

(def ^:private noisy-namespaces
  "Third-party namespaces that emit reflection or boxed math warnings.
  Each require wrapped in try/catch for silent failure when deps aren't on classpath."
  '[aero.alpha.core
    babashka.fs
    cider.nrepl.inlined.deps.toolsreader.v1v4v1.clojure.tools.reader
    cider.nrepl.middleware.test
    cider.nrepl.middleware.util.instrument
    clojure.test.check
    clojure.test.check.clojure-test
    clojure.tools.cli
    clojure.tools.deps
    clojure.tools.gitlibs
    clojure.tools.reader
    fipp
    kaocha.plugin.profiling
    kaocha.report
    kaocha.runner
    lambdaisland.deep-diff2
    malli.core
    malli.generator
    nextjournal.beholder
    nextjournal.markdown.transform
    nextjournal.markdown.utils
    nrepl.core
    nrepl.middleware
    nrepl.middleware.session
    orchard.inspect
    scicloj.clay.v2.make
    scicloj.clay.v2.notebook
    scicloj.clay.v2.util.image
    scicloj.kindly-render.note.to-hiccup])

(defn suppress-warnings-pre-load
  "Pre-load hook that requires noisy third-party namespaces with warnings disabled.

  Pre-loads namespaces that emit reflection or boxed math warnings before
  the test suite's warning flags are in effect. Returns config unchanged.

  Binds `*warn-on-reflection*` to false and `*unchecked-math*` to false
  to suppress all warnings during loading. Each require is wrapped in
  try/catch for silent failure when dependencies aren't on classpath."
  [config]
  (binding [*warn-on-reflection* false
            *unchecked-math* false]
    (doseq [ns-sym noisy-namespaces]
      (try
        (require ns-sym)
        (catch Exception _))))
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
