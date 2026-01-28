(ns domain-complexity-analysis
  "Analyzing algorithmic complexity with criterium."
  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.builder :as builder]
   [criterium.notebook.helpers :refer [bench-display]]))

;; # Domain Complexity Analysis
;;
;; Complexity analysis measures how execution time scales with input size.
;; Use it to verify that your algorithm has the expected O(n), O(n log n),
;; or other complexity class.
;;
;; This notebook covers:
;;
;; - Running complexity analysis with `domain/bench`
;; - Understanding the output: tables, charts, and regression fits
;; - Choosing coordinate ranges for accurate analysis
;; - Configuring options for benchmarking

;; ## When to Use Complexity Analysis
;;
;; Complexity analysis is useful when you want to:
;;
;; - **Verify algorithmic complexity** - Confirm that your implementation
;;   scales as expected (e.g., O(n log n) for a sort)
;; - **Detect performance regressions** - Catch when O(n) code becomes O(n²)
;; - **Compare scaling behavior** - See how different implementations scale
;;   across input sizes
;;
;; For comparing implementations at fixed input sizes, see the
;; [Domain Comparison Analysis](./domain_comparison_analysis.html) notebook.

;; ## Basic Usage
;;
;; Use `domain/bench` with `domain-expr` to define what to benchmark,
;; and the `complexity-analysis` plan to fit scaling models:

(defn random-vec
  "Generate a random vector of n integers."
  ^clojure.lang.PersistentVector [n]
  (mapv rand-int (repeat n 10000)))

^:kindly/hide-code
(bench/set-default-viewer! :kindly)

;; Benchmark sorting across different input sizes:

^:kindly/hide-code
(bench-display
 (domain/bench
  (domain/domain-expr
   [n (builder/log-range 100 10000 5)]
   (sort (random-vec n)))
  :domain-plan domain-plans/complexity-analysis
  :bench-options {:limit-time-s 1}))

;; The `domain-expr` macro defines:
;;
;; - **Axis bindings** `[n (builder/log-range 100 10000 5)]` - The parameter
;;   `n` takes values from a logarithmic range
;; - **Expression** `(sort (random-vec n))` - The code to benchmark,
;;   which can reference axis variables

;; ## Understanding the Output
;;
;; The complexity analysis produces three main outputs:
;;
;; ### Extract Table
;;
;; Shows the measured elapsed time at each input size:
;;
;; | n     | elapsed-time |
;; |-------|--------------|
;; | 100   | 12.3 µs      |
;; | 316   | 45.7 µs      |
;; | 1000  | 156 µs       |
;; | ...   | ...          |
;;
;; The `±` values show error bounds (±3σ) for each measurement.
;;
;; ### Extract Chart
;;
;; Visualizes elapsed time vs input size. The shape reveals the complexity:
;;
;; - **Linear slope** on a log-log plot suggests O(n)
;; - **Steeper than linear** suggests O(n²) or worse
;; - **Flatter than linear** suggests O(log n) or O(1)
;;
;; ### Regression Results
;;
;; Shows fitted models with their R² values:
;;
;; | Model      | R²    | Equation                    |
;; |------------|-------|-----------------------------|
;; | O(n log n) | 0.998 | 1.23e-7 * n*log(n) + 5.6e-6 |
;; | O(n)       | 0.987 | 1.56e-6 * n + 1.2e-5        |
;; | O(n²)      | 0.912 | 2.3e-9 * n² + 4.5e-6        |
;; | ...        | ...   | ...                         |
;;
;; The **best fit** (highest R²) indicates the likely complexity class.
;; For sorting algorithms, expect O(n log n) to have the highest R².

;; ## Log-Log Diagnostic
;;
;; The log-log chart provides an intuitive complexity class estimate.
;; On a log-log plot, power-law relationships become straight lines:
;;
;; - **Slope ≈ 1** → O(n)
;; - **Slope ≈ 2** → O(n²)
;; - **Slope ≈ 0.5** → O(√n)
;;
;; For O(n log n), the slope is slightly greater than 1 and increases
;; slowly with n.

;; ## Coordinate Ranges
;;
;; Choosing the right range is important for accurate analysis.
;; The `criterium.domain.builder` namespace provides several generators:
;;
;; ### log-range
;;
;; Logarithmically-spaced values. Good for covering wide ranges efficiently:

(builder/log-range 10 10000 5)
;; => (10 56 316 1778 10000)

;; The ratio between successive values is constant, so you sample
;; proportionally across orders of magnitude.
;;
;; ### n-log-n-range
;;
;; Values spaced along an n*log(n) curve. Designed for O(n log n) algorithms:

(builder/n-log-n-range 10 10000 5)
;; => (10 69 359 1645 10000)

;; This gives denser sampling at smaller sizes where the log(n) factor
;; changes more rapidly.
;;
;; ### linear-range
;;
;; Evenly-spaced values. Best for detecting linear scaling:

(builder/linear-range 100 1000 5)
;; => (100 325 550 775 1000)

;; ### Choosing a Range
;;
;; Guidelines for effective complexity analysis:
;;
;; - **Start large enough** - Very small inputs may not show true scaling
;;   behavior due to fixed overhead
;; - **End large enough** - The range should span enough to distinguish
;;   O(n) from O(n log n) from O(n²)
;; - **Use enough points** - At least 5 points for reliable regression
;; - **Match the expected complexity** - Use `n-log-n-range` for O(n log n)
;;   algorithms, `log-range` for general analysis

;; ## Options
;;
;; Control benchmarking behavior with options to `domain/bench`:
;;
;; ### :bench-options
;;
;; Options passed to each individual benchmark:

^:kindly/hide-code
(bench-display
 (domain/bench
  (domain/domain-expr
   [n (builder/log-range 100 1000 3)]
   (sort (random-vec n)))
  :domain-plan domain-plans/complexity-analysis
  :bench-options {:limit-time-s 0.5}))

;; Common bench-options:
;;
;; - `:limit-time-s` - Maximum time per benchmark point (default: 10s)
;; - `:metric-ids` - Which metrics to collect (default: `[:elapsed-time]`)
;;
;; ### :reporter
;;
;; Control progress output. Set to `nil` for silent operation:

^:kindly/hide-code
(bench-display
 (domain/bench
  (domain/domain-expr
   [n [100 500 1000]]
   (sort (random-vec n)))
  :domain-plan domain-plans/complexity-analysis
  :reporter nil
  :bench-options {:limit-time-s 0.5}))

;; ### :time-axis
;;
;; Specify which axis represents input size for regression.
;; Defaults to the first axis:

;; (domain/bench
;;  (domain/domain-expr
;;   [m [10 50]
;;    n (builder/log-range 100 1000 3)]
;;   (matrix-op m n))
;;  :time-axis :n  ; Use :n for complexity fitting
;;  :domain-plan domain-plans/complexity-analysis)

;; ## Example: Comparing Sort Complexities
;;
;; Different sorting algorithms have different complexity classes.
;; Here's how to verify the O(n²) complexity of a bubble sort vs
;; the O(n log n) of `sort`:

(defn bubble-sort
  "A deliberately slow O(n²) sort for demonstration."
  [coll]
  (let [v (vec coll)]
    (loop [v v
           swapped? true]
      (if-not swapped?
        v
        (let [[v' swapped?']
              (reduce
               (fn [[v swapped?] i]
                 (if (> (nth v i) (nth v (inc i)))
                   [(assoc v i (nth v (inc i)) (inc i) (nth v i)) true]
                   [v swapped?]))
               [v false]
               (range (dec (count v))))]
          (recur v' swapped?'))))))

;; Compare the two at small sizes (bubble sort is too slow for large n):

^:kindly/hide-code
(bench-display
 (domain/bench
  (domain/domain-expr
   [n (builder/log-range 10 100 4)]
   {:built-in (sort (random-vec n))
    :bubble   (bubble-sort (random-vec n))})
  :domain-plan domain-plans/complexity-analysis
  :bench-options {:limit-time-s 0.5}))

;; The regression results show:
;;
;; - `sort` fits O(n log n) best
;; - `bubble-sort` fits O(n²) best

;; ## Next Steps
;;
;; - [Domain Bench](./criterium.domain_bench_notebook.html) - More domain
;;   benchmarking features
;; - [Quickstart](./quickstart.html) - Single-point benchmarking basics

^:kindly/hide-code
(bench/set-default-viewer! :print)
