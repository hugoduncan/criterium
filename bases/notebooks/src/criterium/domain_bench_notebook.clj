(ns
 ^{:kindly/options {:kinds-that-hide-code #{:kind/hidden}}}
 criterium.domain-bench-notebook
  "Simplified domain benchmarking with domain/bench."
  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.builder :as builder]
   [scicloj.kindly.v4.kind :as kind]))

(kind/hidden
 (bench/set-default-viewer! :kindly))

;; # Domain Bench
;;
;; The `domain/bench` function provides the simplest way to benchmark
;; across a parameter space. It combines `domain-expr`, `domain-builder`,
;; and `analyse-domain` into a single call.
;;
;; For more control over the process, see:
;; - [Domain Analysis](./criterium.analyse_domain_notebook.html) - Manual domain construction
;; - [Domain Builder](./criterium.domain_builder_notebook.html) - Automated construction with domain-builder

;; ## Basic Usage
;;
;; Use `domain-expr` to define what to benchmark, then pass it to `bench`:

(defn random-seq
  "Generate a random sequence of n integers."
  ^clojure.lang.PersistentVector [n]
  (mapv rand-int (repeat n 10000)))

;; Benchmark sorting across different input sizes:

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 10 100 3)]
  (sort (random-seq n))))

;; The default plan is `extract-metrics`, which discovers and displays
;; all collected metrics.

;; ## Comparing Implementations
;;
;; Use a map in `domain-expr` to compare multiple implementations:

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 10 100 3)]
  {:sort    (sort (random-seq n))
   :sort-by (sort-by identity (random-seq n))})
 :domain-plan domain-plans/implementation-comparison)

;; The `implementation-comparison` plan groups by implementation and
;; shows factors relative to the baseline (first implementation).

;; ## Single-Point Comparison
;;
;; Compare implementations at a single point (no parameter range).
;; Results display as box plots showing median, confidence intervals,
;; and percentile whiskers.

(domain/bench
 (domain/domain-expr
  []
  {;; Intentionally comparing idiomatic vs non-idiomatic forms
   :not-empty #_:clj-kondo/ignore (not (empty? (range 5)))
   :seq       (seq (range 5))})
 :domain-plan domain-plans/implementation-comparison)

;; ## Complexity Analysis
;;
;; Use `complexity-analysis` to fit O(n), O(n log n), etc. models:

(domain/bench
 (domain/domain-expr
  [n (builder/n-log-n-range 10 1000 5)]
  (sort (random-seq n)))
 :domain-plan domain-plans/complexity-analysis)

;; ## Options
;;
;; Control benchmarking behavior with options:
;;
;; - `:domain-plan` - Analysis plan (default: `extract-metrics`)
;; - `:reporter` - Progress reporter (`nil` for silent)
;; - `:bench-options` - Options passed to each benchmark
;; - `:time-axis` - Axis for time estimation

;; Silent benchmarking with custom time limit:

(domain/bench
 (domain/domain-expr
  [n [10 50 100]]
  (sort (random-seq n)))
 :reporter nil
 :bench-options {:limit-time-s 1})

;; ## Collecting Additional Metrics
;;
;; Use `:bench-options` to collect thread allocation data:

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 10 100 3)]
  {:sort    (sort (random-seq n))
   :sort-by (sort-by identity (random-seq n))})
 :bench-options {:metric-ids [:elapsed-time :thread-allocation]}
 :domain-plan domain-plans/implementation-comparison)

(kind/hidden
 (bench/set-default-viewer! :print))
