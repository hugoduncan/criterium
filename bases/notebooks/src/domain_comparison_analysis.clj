(ns domain-comparison-analysis
  "Comparing implementations with criterium."
  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.builder :as builder]))

;; # Domain Comparison Analysis
;;
;; Implementation comparison measures how different implementations perform
;; relative to each other. Use it to compare algorithms, libraries, or
;; optimization strategies.
;;
;; This notebook covers:
;;
;; - Comparing implementations with `domain/bench`
;; - Single-point comparison (fixed input size)
;; - Multi-point comparison (across input sizes)
;; - Understanding the output: tables, charts, and factors
;; - Multi-metric comparison (time and allocation)
;; - Controlling baseline selection

;; ## When to Use Implementation Comparison
;;
;; Implementation comparison is useful when you want to:
;;
;; - **Compare algorithms** - Which sort is faster for your data?
;; - **Evaluate libraries** - How does library A compare to library B?
;; - **Measure optimizations** - Did your optimization actually help?
;; - **Trade-off analysis** - Which implementation is faster but uses more memory?
;;
;; For analyzing how performance scales with input size, see the
;; [Domain Complexity Analysis](./domain_complexity_analysis.html) notebook.

;; ## Single-Point Comparison
;;
;; The simplest comparison benchmarks implementations at a single input size.
;; Use an empty axis binding `[]` when you don't need to vary inputs:

(defn random-vec
  "Generate a random vector of n integers."
  ^clojure.lang.PersistentVector [n]
  (mapv rand-int (repeat n 10000)))

^:kindly/hide-code
(bench/set-default-viewer! :kindly)

;; Compare two sorting approaches at a fixed size:

(domain/bench
 (domain/domain-expr
  []
  {:sort    (sort (random-vec 100))
   :sort-by (sort-by identity (random-vec 100))})
 :domain-plan domain-plans/implementation-comparison
 :bench-options {:limit-time-s 5})

;; The empty binding vector `[]` means no axes vary - we benchmark each
;; implementation once at the fixed input.

;; ## Understanding Single-Point Output
;;
;; The comparison produces two main outputs:
;;
;; ### Comparison Table
;;
;; Shows each implementation's performance:
;;
;; | Implementation | median elapsed-time | elapsed-time CI | elapsed-time × |
;; |----------------|---------------------|-----------------|----------------|
;; | sort           | 45.2 µs             | 44.8 - 45.6     | 1.00           |
;; | sort-by        | 52.1 µs             | 51.5 - 52.7     | 1.15           |
;;
;; Key columns:
;;
;; - **median** - The median execution time with SI units
;; - **CI** - 95% confidence interval bounds
;; - **×** (factor) - Ratio relative to baseline (first implementation)
;;
;; The **baseline** (first implementation listed) shows factor 1.00.
;; Other implementations show how many times slower (>1) or faster (<1)
;; they are compared to the baseline.
;;
;; ### Box Plot
;;
;; Visualizes the distribution for each implementation:
;;
;; - **Box** spans from 10th to 90th percentile
;; - **Line** in box marks the median
;; - **Whiskers** show confidence interval bounds (when available)
;;
;; Overlapping boxes suggest implementations may not differ significantly.

;; ## Multi-Point Comparison
;;
;; To compare implementations across different input sizes, add an axis:

(domain/bench
 (domain/domain-expr
  [n [8 16 32 64 128]]
  {:sort    (sort (random-vec n))
   :sort-by (sort-by identity (random-vec n))})
 :domain-plan domain-plans/implementation-comparison
 :bench-options {:limit-time-s 20})

;; Now `n` varies across the specified values, and each implementation
;; is benchmarked at every point.

;; ## Understanding Multi-Point Output
;;
;; ### Comparison Table
;;
;; Shows factors at each input size:
;;
;; | n    | sort      | sort-by   | sort-by × |
;; |------|-----------|-----------|-----------|
;; | 100  | 4.52 µs   | 5.21 µs   | 1.15      |
;; | 500  | 28.3 µs   | 32.1 µs   | 1.13      |
;; | 1000 | 62.4 µs   | 71.5 µs   | 1.15      |
;;
;; The baseline implementation shows absolute values with SI units.
;; Other implementations show both absolute values and factors.
;;
;; ### Line Chart
;;
;; Shows how each implementation scales:
;;
;; - X-axis: the varying parameter (n)
;; - Y-axis: elapsed time
;; - Lines: one per implementation
;; - Error bars: confidence interval bounds
;;
;; Parallel lines indicate consistent relative performance.
;; Converging or diverging lines show the factor changes with input size.

;; ## Baseline Selection
;;
;; The **first implementation** in your map becomes the baseline.
;; All factors are computed relative to it.
;;
;; Implementation ordering in Clojure maps:
;;
;; - **Array maps** (≤8 entries) preserve insertion order
;; - **Hash maps** (>8 entries) have unpredictable order
;;
;; To control baseline with many implementations, use `array-map`:

;; (domain/domain-expr
;;  []
;;  (array-map
;;   :baseline-impl (baseline-code)
;;   :other-impl-a  (code-a)
;;   :other-impl-b  (code-b)))

;; Or simply list the baseline implementation first in small maps:

(domain/bench
 (domain/domain-expr
  []
  {:reference (reduce + (vec (range 100)))        ; baseline
   :transduce (transduce identity + (vec (range 100)))})
 :domain-plan domain-plans/implementation-comparison
 :bench-options {:limit-time-s 5})

;; Here `:reference` appears first, so it becomes the baseline.

;; ## Multi-Metric Comparison
;;
;; Compare both execution time and memory allocation by specifying
;; multiple metrics in `:bench-options`:

(domain/bench
 (domain/domain-expr
  []
  {:into (into [] (range 1000))
   :mapv (mapv identity (range 1000))})
 :domain-plan domain-plans/implementation-comparison
 :bench-options {:limit-time-s 5
                 :metric-ids   [:elapsed-time :thread-allocation]})

;; The output includes:
;;
;; - Separate columns/charts for each metric
;; - Factors computed independently for time and allocation
;; - Trade-offs become visible (faster but more allocation, etc.)

;; ## Options
;;
;; ### :bench-options
;;
;; Options passed to each individual benchmark:

(domain/bench
 (domain/domain-expr
  []
  {:a (Thread/sleep 1)
   :b (Thread/sleep 2)})
 :domain-plan domain-plans/implementation-comparison
 :bench-options {:limit-time-s 5})

;; Common bench-options:
;;
;; - `:limit-time-s` - Maximum time per benchmark point (default: 10s)
;; - `:metric-ids` - Which metrics to collect (default: `[:elapsed-time]`)
;;
;; ### :reporter
;;
;; Control progress output. Set to `nil` for silent operation:

(domain/bench
 (domain/domain-expr
  []
  {:a (reduce + (vec (range 100)))
   :b (apply + (vec (range 100)))})
 :domain-plan domain-plans/implementation-comparison
 :reporter nil
 :bench-options {:limit-time-s 5})

;; ## Example: Collection Operations
;;
;; Compare different ways to build a collection:

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 8 128 5)]
  {:into-vec  (into [] (range n))
   :vec-range (vec (range n))
   :mapv      (mapv identity (range n))})
 :domain-plan domain-plans/implementation-comparison
 :bench-options {:limit-time-s 5})

;; The results show which collection-building approach is most efficient
;; and whether the relative performance changes with size.

;; ## Next Steps
;;
;; - [Domain Complexity Analysis](./domain_complexity_analysis.html) -
;;   Analyze algorithmic complexity
;; - [Domain Bench](./criterium.domain_bench_notebook.html) - More domain
;;   benchmarking features
;; - [Quickstart](./quickstart.html) - Single-point benchmarking basics

^:kindly/hide-code
(bench/set-default-viewer! :print)
