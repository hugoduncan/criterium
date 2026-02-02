(ns criterium.arg-gen
  "Argument generation using test.check generators.

  Provides macros for creating benchmarks with generated arguments and
  for creating zero-arg functions that generate varied inputs.

  Primary use cases:
  - `measured`: Create a Measured with generated arguments for benchmarking
  - `args-fn`: Create a warmup-args-fn for use with :warmup-args-fn option

  The `args-fn` macro is particularly useful for creating warmup functions
  that generate varied inputs, enabling more representative JIT optimization
  during the warmup phase.

  Example:
  (require '[criterium.arg-gen :as arg-gen]
           '[criterium.bench :refer [bench]]
           '[clojure.test.check.generators :as gen])

  ;; Use varied warmup inputs for better JIT optimization
  (let [coll (vec (range 1000))]
    (bench (sort coll)
           :warmup-args-fn (arg-gen/args-fn {:size 200}
                             [v (gen/vector gen/small-integer)]
                             [v])))"
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [criterium.jvm :as jvm]
            [criterium.measured :as measured]
            [criterium.measured.impl :as measured-impl]))

;; from c.t.check (private)

(defn- make-rng
  "Return [seed rng] pair. Uses timestamp if seed is nil."
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (jvm/timestamp)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn args-fn-state
  "Create mutable state for argument generation.
  Returns a volatile containing :created-seed, :rng, and :size-seq."
  [max-size seed]
  (let [[created-seed rng] (make-rng seed)
        size-seq           (gen/make-size-range-seq max-size)]
    (volatile! {:created-seed created-seed
                :rng          rng
                :size-seq     size-seq})))

(defn make-args-fn-from-state
  "Return a zero-arg function that generates arguments from gen.
  Each call advances the RNG and size sequence in args-fn-state.
  Internal helper - used by macros via eval."
  [gen args-fn-state]
  (fn []
    (let [{:keys [rng size-seq]} @args-fn-state
          [size & rest-size-seq] size-seq
          [r1 r2]                (random/split rng)
          result-map-rose        (gen/call-gen gen r1 size)]
      (vswap! args-fn-state assoc :rng r2 :size-seq rest-size-seq)
      (rose/root result-map-rose))))

(defn measured-impl
  "Create a measured from a generator and function.
  Returns a measured that generates arguments from gen and applies f.

  Example:

  (measured-impl
    (gen/tuple gen/large-integer gen/large-integer)
    (fn [[a b]] (+ a b))
    {})"
  [gen f {:keys [size seed] :or {size 100 seed nil}}]
  (let [state (args-fn-state size seed)]
    (measured/measured
     (make-args-fn-from-state gen state)
     f)))

(defn arg-metas-from-example
  "Return a vector of type hints for the generated state elements."
  [binding-gens]
  (let [example-size  2
        example-form  `((make-args-fn-from-state
                         ~binding-gens
                         (args-fn-state ~example-size nil)))
        example-state (eval example-form)
        types         (mapv type example-state)]
    (mapv measured-impl/tag-meta types)))

(defmacro measured*
  "Return a measured with generated arguments.

  Takes a `let`-style bindings vector where each right-hand side is a
  test.check generator. The body expression is measured with generated
  values bound to the symbols.

  Earlier binding pairs are visible to later pairs. Multiple body
  expressions execute in sequence as with `do`.

  Example:

  (measured* {:size 50}
    [a gen/large-integer
     b gen/large-integer]
    (+ a b))"
  [{:keys [size seed arg-metas]
    :or   {size 100 seed nil}
    :as   _options}
   bindings & body]
  (let [pairs        (partition 2 bindings)
        binding-vars (mapv first pairs)
        binding-gens (reduce
                      (fn [curr [sym code]]
                        `(gen/bind ~code (fn [~sym] ~curr)))
                      `(gen/return ~binding-vars)
                      (reverse pairs))
        options      {:arg-metas
                      (or arg-metas
                          (arg-metas-from-example binding-gens))}]
    `(measured-impl
      ~binding-gens
      ~(measured-impl/measured-expr-fn
        binding-vars
        `(do ~@body)
        options)
      ~{:size size
        :seed seed})))

(defmacro measured
  "Return a measured using test.check generators for arguments."
  [bindings & body]
  (if (vector? bindings)
    `(measured* nil ~bindings ~@body)
    (do
      (assert
       (map? bindings)
       "First arg must be options map or bindings vector")
      `(measured* ~bindings ~@body))))

(defmacro args-fn*
  "Return a zero-arg function that generates arguments using test.check.

  Takes a `let`-style bindings vector where each right-hand side is a
  test.check generator. The body expression determines the return value
  shape, typically a vector of arguments.

  Earlier binding pairs are visible to later pairs. Multiple body
  expressions execute in sequence as with `do`.

  Each call to the returned function generates new values, advancing
  the internal RNG and size sequence.

  Example:

  (args-fn* {:size 50 :seed 42}
    [a gen/large-integer
     b gen/large-integer]
    [a b])"
  [{:keys [size seed]
    :or   {size 100 seed nil}
    :as   _options}
   bindings & body]
  (let [pairs        (partition 2 bindings)
        binding-vars (mapv first pairs)
        binding-gens (reduce
                      (fn [curr [sym code]]
                        `(gen/bind ~code (fn [~sym] ~curr)))
                      `(gen/return ~binding-vars)
                      (reverse pairs))]
    `(let [state# (args-fn-state ~size ~seed)
           gen#   ~binding-gens]
       (fn []
         (let [~binding-vars ((make-args-fn-from-state gen# state#))]
           (do ~@body))))))

(defmacro args-fn
  "Return a zero-arg function that generates arguments using test.check.

  Takes an optional options map followed by a `let`-style bindings vector
  where each right-hand side is a test.check generator. The body expression
  determines the return value shape, typically a vector of arguments.

  Options:
    :size - Maximum size for generators (default 100)
    :seed - Random seed for reproducibility (default nil, uses timestamp)

  Earlier binding pairs are visible to later pairs.

  Examples:

  ;; Without options
  (args-fn [a gen/large-integer
            b gen/large-integer]
    [a b])

  ;; With options
  (args-fn {:size 50 :seed 42}
    [a gen/large-integer
     b gen/large-integer]
    [a b])

  ;; Dependent bindings
  (args-fn {:size 100}
    [n (gen/choose 10 100)
     v (gen/vector gen/small-integer n)]
    [v])"
  [bindings-or-options & args]
  (if (vector? bindings-or-options)
    `(args-fn* nil ~bindings-or-options ~@args)
    (do
      (assert (map? bindings-or-options)
              "First arg must be options map or bindings vector")
      `(args-fn* ~bindings-or-options ~@args))))
