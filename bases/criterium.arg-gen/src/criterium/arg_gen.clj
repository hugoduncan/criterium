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
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (jvm/timestamp)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn state-fn-state [max-size seed]
  (let [[created-seed rng] (make-rng seed)
        size-seq           (gen/make-size-range-seq max-size)]
    (volatile! {:created-seed created-seed
                :rng          rng
                :size-seq     size-seq})))

(defn state-fn   ; TODO make state-fn-state a mutable field on measured?
  [gen state-fn-state]
  (fn []
    (let [{:keys [rng size-seq]} @state-fn-state
          [size & rest-size-seq] size-seq
          [r1 r2]                (random/split rng)
          result-map-rose        (gen/call-gen gen r1 size)]
      (vswap! state-fn-state assoc :rng r2 :size-seq rest-size-seq)
      (rose/root result-map-rose))))

(defn measured-impl
  "A function version of `for-all`. Takes a sequence of N generators and a
  function of N args, and returns a measured function, which can be
  called with generated values, like with `for-all`.

  Example:

  (for-all* [gen/large-integer gen/large-integer]
            (fn [a b] (+ a b) a))"
  [gen f {:keys [size seed] :or {size 100 seed nil}}]
  (let [state-fn-state (state-fn-state size seed)]
    (measured/measured
     (state-fn gen state-fn-state)
     f)))

(defn arg-metas-from-example
  "Return a vector of type hints for the generated state elements."
  [binding-gens]
  (let [example-size  2
        example-form  `((state-fn
                         ~binding-gens
                         (state-fn-state ~example-size nil)))
        example-state (eval example-form)
        types         (mapv type example-state)]
    (mapv measured-impl/tag-meta types)))

(defmacro measured*
  "Returns a measured, which is the combination of some generators and an
  expression that should be measured for all generated values.

  `for-all` takes a `let`-style bindings vector, where the right-hand
  side of each binding is a generator.

  The body should be an expression of the generated values that will be
  measured.

  When there are multiple binding pairs, the earlier pairs are visible
  to the later pairs.

  If there are multiple body expressions, all but the last one are
  executed for side effects, as with `do`.

  Example:

  (time*
    (for-all [a gen/large-integer
              b gen/large-integer]
       (+ a b))
    {})"
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
  "Return a measured using test.check generators for state."
  [bindings & body]
  (if (vector? bindings)
    `(measured* nil ~bindings ~@body)
    (do
      (assert (map? bindings) "options must be passed as a literal map")
      `(measured* ~bindings ~@body))))
