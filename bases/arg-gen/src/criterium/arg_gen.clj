(ns criterium.arg-gen
  "Argument generation"
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

(defn args-fn
  "Return a zero-arg function that generates arguments from gen.
  Each call advances the RNG and size sequence in args-fn-state."
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
  (let [args-fn-state (args-fn-state size seed)]
    (measured/measured
     (args-fn gen args-fn-state)
     f)))

(defn arg-metas-from-example
  "Return a vector of type hints for the generated state elements."
  [binding-gens]
  (let [example-size  2
        example-form  `((args-fn
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
      (assert (map? bindings) "First arg must be options map or bindings vector")
      `(measured* ~bindings ~@body))))
