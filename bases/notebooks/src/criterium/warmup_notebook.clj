(ns
 ^{:kindly/options {:kinds-that-hide-code #{:kind/hidden}}}
 criterium.warmup-notebook
  "Understanding and customizing JIT warmup for representative benchmarks."
  (:require
   [clojure.test.check.generators :as gen]
   [criterium.arg-gen :as arg-gen]
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain.builder :as builder]
   [criterium.measured :as measured]
   [scicloj.kindly.v4.kind :as kind]))

(kind/hidden
 (bench/set-default-viewer! :kindly))

;; # JIT Warmup and Varied Inputs
;;
;; The JVM's JIT compiler optimizes frequently executed code paths based on the
;; inputs it observes. If warmup always uses the same arguments, the JIT may
;; over-specialize for those specific inputs, leading to benchmarks that don't
;; reflect real-world performance.
;;
;; The `:warmup-args-fn` feature lets you provide varied inputs during warmup,
;; resulting in more representative JIT optimizations.

;; ## Why Varied Warmup Matters
;;
;; Consider sorting a collection. The optimal sorting strategy can depend on:
;; - Collection size
;; - Pre-sortedness of data
;; - Data distribution (mostly uniform vs many duplicates)
;;
;; If warmup uses only a single input, the JIT may optimize for that specific
;; case, potentially producing misleading benchmark results.

;; ## Basic Usage
;;
;; Use `:warmup-args-fn` with the bench macro to specify a function that
;; generates varied inputs during warmup:

(let [coll (vec (range 1000))]
  (bench/bench (sort coll)
               :warmup-args-fn (fn [] [(vec (shuffle (range 5000)))])))

;; The warmup phase calls `warmup-args-fn` repeatedly to generate fresh inputs.
;; Measurement continues to use the original arguments (`coll`).

;; ## Using arg-gen for Warmup Functions
;;
;; The `arg-gen/args-fn` macro creates warmup functions using test.check
;; generators. This provides varied, reproducible inputs:

(let [coll (vec (range 1000))]
  (bench/bench (sort coll)
               :warmup-args-fn (arg-gen/args-fn {:size 200 :seed 42}
                                                [v (gen/vector gen/small-integer)]
                                                [v])))

;; ### Options
;;
;; - `:size` - Maximum size for generators (default 100)
;; - `:seed` - Random seed for reproducibility (default nil, uses timestamp)

;; ### Multiple Arguments
;;
;; Use dependent bindings for functions with multiple arguments:

(defn lookup-key
  "Look up a key in a map."
  [m k]
  (get m k))

(let [m (zipmap (range 1000) (range 1000))
      k 500]
  (bench/bench (lookup-key m k)
               :warmup-args-fn (arg-gen/args-fn {:size 100}
                                                [n (gen/choose 100 5000)
                                                 m (gen/fmap #(zipmap (range %) (range %))
                                                             (gen/return n))
                                                 k (gen/choose 0 n)]
                                                [m k])))

;; ## Warmup on Measured Instances
;;
;; You can set warmup-args-fn when creating Measured instances directly.

;; ### Using measured/callable

;; The `callable` macro accepts warmup-args-fn as a third argument:

(def sort-measured
  (measured/callable
   (fn [] [(vec (range 1000))])     ; args-fn for measurement
   sort                              ; function to measure
   (fn [] [(vec (shuffle (range 5000)))]))) ; warmup-args-fn

(bench/bench-measured
 (bench/options->bench-plan)
 sort-measured)

;; ### Using arg-gen/measured with :warmup-args-fn option
;;
;; When the measurement itself uses generated arguments, you can override
;; warmup with the bench option:

(bench/bench-measured
 (bench/options->bench-plan :warmup-args-fn (arg-gen/args-fn {:size 200}
                                                             [v (gen/vector gen/small-integer)]
                                                             [v]))
 (arg-gen/measured {:size 100 :seed 42}
                   [v (gen/vector gen/small-integer)]
                   (sort v)))

;; ## Priority Rules
;;
;; Warmup arguments are resolved in this order:
;; 1. Options-level `:warmup-args-fn` (in bench macro or bench-measured)
;; 2. Measured-level warmup-args-fn (from measured constructor)
;; 3. Fall back to regular args-fn
;;
;; This allows overriding warmup behavior at any level.

;; ## Domain Analysis with Shared Warmup
;;
;; When comparing implementations across a parameter space, you often want
;; consistent JIT warmup for all implementations.

;; ### Using :warmup-args-fn in domain-expr
;;
;; Pass options as the third argument to `domain-expr`:

(defn random-seq
  "Generate a random sequence of n integers."
  ^clojure.lang.PersistentVector [n]
  (mapv rand-int (repeat n 1000)))

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 100 1000 3)]
  {:sort    (sort (random-seq n))
   :sort-by (sort-by identity (random-seq n))}
  {:warmup-args-fn (fn [] [(vec (shuffle (range 500)))])})
 :reporter nil)

;; ### Using :bench-options for Domain Warmup
;;
;; Alternatively, use `:bench-options` with `:warmup-args-fn`:

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 100 1000 3)]
  (sort (random-seq n)))
 :bench-options {:warmup-args-fn (arg-gen/args-fn {:size 200}
                                                  [v (gen/vector gen/small-integer)]
                                                  [v])}
 :reporter nil)

;; ## When to Use Varied Warmup
;;
;; Consider using varied warmup inputs when:
;;
;; - **Input-dependent algorithms**: Sorting, searching, compression
;; - **Polymorphic dispatch**: Code handling multiple types
;; - **Branch-heavy code**: Where JIT may specialize on observed branches
;; - **Cache-sensitive operations**: Where data locality matters
;;
;; For simple arithmetic or pure functions with uniform behavior,
;; varied warmup provides less benefit.

;; ## Best Practices
;;
;; 1. **Match warmup complexity**: Warmup inputs should exercise the same
;;    code paths as measurement inputs
;;
;; 2. **Use appropriate generator sizes**: The `:size` option should produce
;;    inputs representative of your workload
;;
;; 3. **Consider reproducibility**: Use `:seed` when comparing benchmark runs
;;    across sessions
;;
;; 4. **Don't over-vary**: Extremely varied warmup may prevent JIT from
;;    finding stable optimizations

(kind/hidden
 (bench/set-default-viewer! :print))
