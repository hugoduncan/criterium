(ns
 ^{:kindly/options {:kinds-that-hide-code #{:kind/hidden}}}
 criterium.arg-gen-notebook
  "Benchmarking with generated arguments using test.check generators."
  (:require
   [clojure.string :as str]
   [clojure.test.check.generators :as gen]
   [criterium.arg-gen :as arg-gen]
   [criterium.bench :as bench]
   [scicloj.kindly.v4.kind :as kind]))

(kind/hidden
 (bench/set-default-viewer! :kindly))

;; # Benchmarking with Generated Arguments
;;
;; The `criterium.arg-gen` namespace enables benchmarking functions with
;; dynamically generated inputs using test.check generators. This solves
;; several common benchmarking challenges:
;;
;; - **Input variety**: Test performance across diverse inputs
;; - **Reproducibility**: Control randomness with seeds
;; - **Size scaling**: Vary input sizes to understand complexity

;; ## The measured Macro
;;
;; The `measured` macro creates a measured instance with generated arguments.
;; It uses a `let`-style binding syntax where right-hand sides are test.check
;; generators:

(arg-gen/measured
 [n gen/small-integer]
 (* n n))

;; Each benchmark iteration receives fresh generated values.

;; ## Basic Usage
;;
;; Benchmark arithmetic with generated integers:

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured
  [a gen/small-integer
   b gen/small-integer]
  (+ a b)))

;; ## Multiple Bindings
;;
;; Earlier bindings are visible to later ones, enabling dependent generation:

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured
  [n (gen/choose 10 100)
   coll (gen/vector gen/small-integer n)]
  (reduce + coll)))

;; ## Controlling Generation
;;
;; ### Size Parameter
;;
;; The `:size` option controls the maximum size passed to generators.
;; Larger sizes produce larger generated values for sized generators:

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured {:size 10}
                   [coll (gen/vector gen/small-integer)]
                   (count coll)))

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured {:size 200}
                   [coll (gen/vector gen/small-integer)]
                   (count coll)))

;; ### Seed Parameter
;;
;; The `:seed` option provides reproducible generation sequences:

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured {:seed 12345}
                   [n gen/small-integer]
                   (* n n)))

;; Running with the same seed produces identical input sequences.

;; ## Common Patterns
;;
;; ### String Processing

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured
  [s gen/string-alphanumeric]
  (str/upper-case s)))

;; ### Collection Operations

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured {:size 50}
                   [v (gen/vector gen/small-integer)]
                   (sort v)))

;; ### Map Operations

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured {:size 20}
                   [m (gen/map gen/keyword gen/small-integer)]
                   (vals m)))

;; ## Understanding Generation
;;
;; The `arg-gen/measured` macro:
;; 1. Creates a generator that produces tuples of bound values
;; 2. Wraps the body in a function that destructures the tuple
;; 3. Returns a `measured` instance that generates fresh args each iteration
;;
;; This ensures each benchmark sample uses different input values,
;; providing a more realistic performance profile.

;; ## Combining with Bench Options
;;
;; Generated arguments work with all standard bench options.
;; Use `:collect-plan :one-shot` for minimal collection:

(bench/bench-measured
 (bench/options->bench-plan :collect-plan :one-shot)
 (arg-gen/measured {:size 30}
                   [coll (gen/vector gen/small-integer)]
                   (vec (reverse coll))))

;; ## Custom Generators
;;
;; Define domain-specific generators for targeted benchmarking:

(def gen-point
  "Generator for 2D points."
  (gen/hash-map :x gen/small-integer
                :y gen/small-integer))

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured
  [p1 gen-point
   p2 gen-point]
  (let [dx (- (long (:x p2)) (long (:x p1)))
        dy (- (long (:y p2)) (long (:y p1)))]
    (Math/sqrt (+ (* dx dx) (* dy dy))))))

;; ## Comparison with Static Data
;;
;; Compare generated vs static input benchmarking:

(def static-vector
  "Static test data."
  (vec (range 100)))

;; Static data - same input every iteration:
(bench/bench (reduce + static-vector))

;; Generated data - varied inputs each iteration:
(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured
  {:size 100}
  [v (gen/vector gen/small-integer)]
  (reduce + v)))

;; Generated inputs reveal performance variance across different data shapes.

;; REPL exploration

;; Create a measured instance
(def m (arg-gen/measured
        [n gen/small-integer]
        (* n n)))

 ;; Benchmark it
(bench/bench-measured
 (bench/options->bench-plan)
 m)

;; With options
(bench/bench-measured
 (bench/options->bench-plan :collect-plan :one-shot)
 (arg-gen/measured
  {:size 50 :seed 42}
  [coll (gen/vector gen/small-integer)]
  (sort coll)))

(kind/hidden
 (bench/set-default-viewer! :print))
