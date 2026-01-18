(ns criterium.measured
  "Implements the concept of a measured function for benchmarking.

  Criterium's metric collection works on a Measured instance. A Measured
  represents a benchmarkable unit of code, consisting of:
  - A function to execute and measure
  - An arguments generator to prevent constant folding
  - Optional symbolic representation for debugging
  - Optional warmup arguments generator for JIT optimization

  The Measured implements a timed, batch invocation interface that:
  - Supports multiple evaluations per timing sample for fast expressions
  - Guarantees zero garbage allocation during measurement
  - Prevents constant folding optimization of inputs

  Warmup Customization:
  Functions may have different complexities based on their inputs. If warmup
  always uses the same arguments, JIT may over-specialize for those inputs.
  The warmup-args-fn field enables using varied inputs during warmup for
  more representative JIT optimization.

  Priority rule for warmup arguments:
  1. Options-level :warmup-args-fn (in bench macro or bench-measured)
  2. Measured-level warmup-args-fn (from measured constructor)
  3. Fall back to regular args-fn

  While Criterium automatically creates Measured instances for expressions,
  you can also construct custom ones for special measurement needs."
  (:require
   [criterium.measured.impl :as impl])
  (:import
   [criterium.measured.impl Measured]))

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
  for inspection purposes (unused internally).

  warmup-args-fn, if specified, provides arguments for warmup phase instead
  of args-fn. This allows warmup with more varied inputs to get more
  representative JIT optimization."
  (^criterium.measured.impl.Measured [args-fn f]
   (impl/measured args-fn f))
  (^criterium.measured.impl.Measured [args-fn f expr-fn]
   (impl/measured args-fn f expr-fn))
  (^criterium.measured.impl.Measured [args-fn f expr-fn warmup-args-fn]
   (impl/measured args-fn f expr-fn warmup-args-fn)))

(defn with-args-fn
  "Return a new Measured with the args-fn replaced.
  Preserves the measurement function, symbolic representation, and warmup-args-fn.

  This is useful for running the same measured expression with different
  input generators, e.g., when benchmarking across a parameter space."
  ^criterium.measured.impl.Measured
  [measured new-args-fn]
  (impl/measured new-args-fn
                 (.-f ^Measured measured)
                 (:expr-fn measured)
                 (:warmup-args-fn measured)))

(defn with-warmup-args-fn
  "Return a new Measured with the warmup-args-fn set or replaced.
  Preserves the measurement function, args-fn, and symbolic representation.

  This is useful for adding varied warmup inputs to an existing Measured,
  for example when running domain analysis where all implementations should
  share the same warmup strategy."
  ^criterium.measured.impl.Measured
  [measured new-warmup-args-fn]
  (impl/measured (:args-fn measured)
                 (.-f ^Measured measured)
                 (:expr-fn measured)
                 new-warmup-args-fn))

(defn args
  "Generate the input state for a measured.

  Returns a value suitable for passing to the measured's function.
  The returned value is created fresh each time to prevent constant folding
  optimizations."
  [measured]
  ((:args-fn measured)))

(defn warmup-args
  "Generate warmup input state for a measured.

  Returns warmup-args-fn result if present, otherwise falls back to args-fn.
  This allows warmup to use more varied inputs for better JIT optimization
  while measurement uses the actual benchmark inputs."
  [measured]
  (if-let [warmup-args-fn (:warmup-args-fn measured)]
    (warmup-args-fn)
    ((:args-fn measured))))

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
  ;; Use direct field access to avoid allocations from keyword lookup
  ((.-f ^Measured measured) state eval-count))

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
  destructured.

  Local bindings from the enclosing scope can be used as arguments in
  the expression."
  ([expr]
   (impl/measured-expr* expr nil &env))
  ([expr options]
   (impl/measured-expr* expr options &env)))

(defmacro callable
  "Return a Measured for the given no arg function."
  ([f]
   (impl/measured-callable f))
  ([sf f]
   (impl/measured-callable sf f)))
