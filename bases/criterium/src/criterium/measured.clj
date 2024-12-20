(ns criterium.measured
  "Implements the concept of a measured function for benchmarking.

  Criterium's metric collection works on a Measured instance. A Measured
  represents a benchmarkable unit of code, consisting of:
  - A function to execute and measure
  - An arguments generator to prevent constant folding
  - Optional symbolic representation for debugging

  The Measured implements a timed, batch invocation interface that:
  - Supports multiple evaluations per timing sample for fast expressions
  - Guarantees zero garbage allocation during measurement
  - Prevents constant folding optimization of inputs

  While Criterium automatically creates Measured instances for expressions,
  you can also construct custom ones for special measurement needs."
  (:require
   [criterium.measured.impl :as impl]))

;;; Measured type

(defn measured?
  "Predicate for x being a Measured."
  [x]
  (impl/measured? x))

(defn measured
  "Return a Measured for a function that can be benchmarked.

  The Measured is the basic unit of measurement. A Measured consists of
  an arguments generation funtion and a function to be measured.  The
  function to be measured takes the result of calling the arrguments
  function and an eval-count as arguments, ie. `(f (args-fn)
  eval-count)`, and returns an `[elapsed-time expr-value]` tuple.

  The arguments function is used to prevent constant folding for
  constant inputs.

  The eval-count allows the function to time multiple evaluations of the
  subject expression, so that we can measure expressions that are faster
  than the timer granularity.

  expr-fn, if specified, returns a symbolic representation of the measured,
  for inspection purposes (unused internally)."
  ^criterium.measured.impl.Measured
  [args-fn f & [expr-fn]]
  (impl/measured args-fn f expr-fn))

(defn args
  "Generate the input state for a measured.

  Returns a value suitable for passing to the measured's function.
  The returned value is created fresh each time to prevent constant folding
  optimizations."
  [measured]
  ((:args-fn measured)))

(defn invoke
  "Invoke the given Measured.

  Calls the Measured's function with the given state and eval-count.
  The state can be created with `args`. Returns a tuple of [elapsed-time expr-value]
  where:
  - elapsed-time is a long representing the execution time in nanoseconds
  - expr-value is the result of evaluating the measured expression

  Guarantees zero garbage allocation during measurement."
  [measured state eval-count]
  ;; NOTE eval-count is explicitly not tagged as 'long, since this function is
  ;; invoked non-literally, so the calling value will always be an object.
  ((:f measured) state eval-count))

(defn ^:no-doc symbolic
  "Return a symbolic representation of the measured.

  Used for debugging and introspection to understand how the measured will
  be executed. Returns nil if no symbolic representation was provided."
  [measured]
  (when-let [expr-fn (:expr-fn measured)]
    (expr-fn)))

;;; Build a Measured from an expression

(defmacro expr
  "Return a Measured for the given expression.

  The expression is wrapped in a function.

  When the expression is a list form, it is treated as a function call.
  The function arguments are treated as constant expressions and are
  hoisted into a state function.  The result of the state function is a
  vector that is passed to the function wrapper as a vector and
  destructured."
  ([expr]
   (impl/measured-expr* expr nil))
  ([expr options]
   (impl/measured-expr* expr options)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro callable
  "Return a Measured for the given no arg function."
  ([f]
   (impl/measured-callable f))
  ([sf f]
   (impl/measured-callable sf f)))
