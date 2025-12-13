(ns
 ^{:kindly/options {:kinds-that-hide-code #{:kind/hidden}}}
 criterium.domain-notebook
  "Working with multiple related benchmark runs using domains."

  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.measured :as measured]
   [criterium.view :as view]
   [criterium.viewer.print]
   [scicloj.kindly.v4.kind :as kind]))

(kind/hidden
 (bench/set-default-viewer! :kindly))

;; # Domain Analysis with Criterium
;;
;; While single-point benchmarks tell you how fast code runs at one input,
;; domains help you understand performance *across* a parameter space.
;;
;; Domains enable:
;; - **Scaling analysis** — How does time grow with input size?
;; - **Implementation comparison** — Which algorithm is faster?
;; - **Parameter exploration** — Where do performance characteristics change?

;; ## Creating Domains
;;
;; A domain is an immutable collection of benchmark runs indexed by coordinates.
;; Start with an empty domain:

(domain/domain)

;; Or create one with existing runs. Each run has a `:coord` (keyword or map)
;; and `:data` (the benchmark result):

(domain/domain
 {:coord :baseline :data {:mock "result1"}}
 {:coord {:n 100} :data {:mock "result2"}})

;; ## Populating Domains
;;
;; In practice, you build domains by running benchmarks and adding results
;; with `add-run`. Here we benchmark sorting at different sizes:

(def sort-sizes
  "Input sizes for scaling analysis."
  [100 500 1000])

(def sort-inputs
  "Pre-generated random vectors for each size."
  (into {}
        (map (fn [n] [n (mapv rand-int (repeat n 10000))]) sort-sizes)))

;; Run benchmarks and accumulate results:

^:kindly/hide-code
(defn build-sort-domain
  "Build a domain with sort benchmarks at each size."
  []
  (reduce
   (fn [d n]
     (let [input (sort-inputs n)]
       (bench/bench (sort input))
       (domain/add-run d {:n n} (:data (bench/last-bench)))))
   (domain/domain)
   sort-sizes))

^:kindly/hide-code
(kind/md "Building domain with sort benchmarks...")

^:kindly/hide-code
(def sort-domain (build-sort-domain))

;; Check what we collected:

(domain/coords sort-domain)

;; ## Query Functions
;;
;; ### All Runs

(count (domain/runs sort-domain))

;; ### Inferred Axes
;;
;; For map coordinates, `axes` returns the dimension keys:

(domain/axes sort-domain)

;; ### Partial Matching
;;
;; Filter runs by partial coordinate match:

(domain/coords (domain/select sort-domain {:n 500}))

;; ## Analysis Functions
;;
;; ### Extract Metric Values
;;
;; Pull a specific metric across all runs:

(domain/extract sort-domain [:stats :elapsed-time :mean])

;; ### Compare Implementations
;;
;; Build a domain comparing `sort` vs `sort-by`:

(def impl-inputs
  "Shared inputs for implementation comparison."
  (into {} (map (fn [n] [n (mapv rand-int (repeat n 10000))]) [100 500])))

(defn build-impl-domain
  "Build domain comparing sort implementations."
  []
  (reduce
   (fn [d [n input]]
     (bench/bench (sort input))
     (let [d (domain/add-run d {:n n :impl :sort} (:data (bench/last-bench)))]
       (bench/bench (sort-by identity input))
       (domain/add-run d {:n n :impl :sort-by} (:data (bench/last-bench)))))
   (domain/domain)
   impl-inputs))

;;"Building domain comparing implementations..."

(def impl-domain (build-impl-domain))

;; Group by implementation:

(let [grouped (domain/group-by-axis impl-domain :impl)]
  {:axis (:axis grouped)
   :groups (keys (:data grouped))})

;; Compare elapsed time across implementations:

(domain/compare-by impl-domain :impl [:stats :elapsed-time :mean])

;; ## Viewing Results
;;
;; Domain analysis results integrate with criterium's view system.
;; The print viewer formats results for terminal output.

;; ### Extract View

((view/domain-extract {:extract-id :extract})
 :print
 {:extract (domain/extract sort-domain [:stats :elapsed-time :mean])})

;; ### Comparison View
;;
;; The comparison view shows a table with axis values as columns:

((view/domain-comparison {:comparison-id :comparison})
 :print
 {:comparison (domain/compare-by impl-domain :impl [:stats :elapsed-time :mean])})

;; ## Scaling Analysis
;;
;; Use input sequence generators to explore algorithmic complexity.

;; ### Powers of 2
;;
;; Doubling input size reveals O(n), O(n log n), O(n²) patterns:

(domain/powers-of-2 0 10)

;; ### Logarithmic Range
;;
;; Cover wide ranges efficiently:

(domain/log-range 10 10000 5)

;; ### Linear Range
;;
;; Uniform sampling for linear scaling detection:

(domain/linear-range 100 1000 5)

;; ### Example: O(n log n) Scaling
;;
;; Benchmark sort across powers of 2 to observe n log n behavior:

(def scaling-sizes (domain/powers-of-2 4 9))

(def scaling-inputs
  (into {} (map (fn [n] [n (mapv rand-int (repeat n 10000))]) scaling-sizes)))

(defn build-scaling-domain
  "Build domain for scaling analysis."
  []
  (reduce
   (fn [d n]
     (let [input (scaling-inputs n)]
       (bench/bench (sort input))
       (domain/add-run d {:n n} (:data (bench/last-bench)))))
   (domain/domain)
   scaling-sizes))

;; "Building scaling analysis domain..."

(def scaling-domain (build-scaling-domain))

;; Extract times and observe scaling:

(let [extract (domain/extract scaling-domain [:stats :elapsed-time :mean])]
  (kind/table
   {:column-names [:n :time-ns :ratio-to-previous]
    :row-vectors
    (let [data (:data extract)]
      (map-indexed
       (fn [i [coord value]]
         [(:n coord)
          (format "%.1f" value)
          (if (zero? i)
            "-"
            (format "%.2fx" (/ value (second (nth data (dec i))))))])
       data))}))

;; ### Regression Fitting
;;
;; Quantitatively determine algorithmic complexity by fitting models to the data.
;; The `fit-complexity` function fits O(log n), O(n), O(n log n), and O(n²) models
;; and identifies the best fit by R² value:

(let [extract (domain/extract scaling-domain [:stats :elapsed-time :mean])
      regression (domain/fit-complexity extract :n)]
  {:best-fit (:best-fit regression)
   :models (map (fn [{:keys [id label r-squared]}]
                  {:model label :r-squared (format "%.4f" r-squared)})
                (sort-by :r-squared > (:models regression)))})

;; View regression results with the print viewer:

(let [extract (domain/extract scaling-domain [:stats :elapsed-time :mean])]
  ((view/domain-regression {:regression-id :regression})
   :print
   {:regression (domain/fit-complexity extract :n)}))

;; Use the pipeline function for composable analysis:

(-> {:domain scaling-domain}
    ((domain/domain-extract-fn
      {:id :extract
       :metric-path [:stats :elapsed-time :mean]}))
    ((domain/domain-regression-fn
      {:id :scaling
       :axis :n}))
    :scaling
    :best-fit)

;; Custom models can be provided for specific complexity classes:

(domain/fit-complexity
 (domain/extract scaling-domain [:stats :elapsed-time :mean])
 :n
 {:cubic {:transform (fn [n] (* n n n)) :label "O(n³)"}
  :linear {:transform identity :label "O(n)"}})

;; ## Pipeline Composition
;;
;; For complex analyses, use pipeline functions that operate on data maps:

(-> {:domain impl-domain}
    ((domain/domain-extract-fn
      {:id :mean-time
       :metric-path [:stats :elapsed-time :mean]}))
    ((domain/domain-compare-fn
      {:id :impl-comparison
       :axis-key :impl
       :metric-path [:stats :elapsed-time :mean]}))
    keys)

;; Each pipeline function adds its result under a configurable key,
;; enabling multiple analyses on the same domain.

;; ## Domain Plans
;;
;; For common analysis workflows, use pre-defined domain plans with
;; `analyse-domain`. Plans bundle analysis and viewing into a single call.
;;
;; Plans respect the default viewer set via `bench/set-default-viewer!`,
;; so in this notebook they use `:kindly` automatically.

;; ### Pre-defined Plans
;;
;; `extract-elapsed-time` displays metric values across all runs:

(domain/analyse-domain domain-plans/extract-elapsed-time sort-domain)

;; `complexity-analysis` extracts elapsed time and fits regression models:

(domain/analyse-domain domain-plans/complexity-analysis scaling-domain)

;; `implementation-comparison` compares metrics across implementations:

(domain/analyse-domain domain-plans/implementation-comparison impl-domain)

;; ### Explicit Viewer Selection
;;
;; Override the default viewer with `options->domain-plan`:

(do
  (domain/analyse-domain
   (domain/options->domain-plan domain-plans/complexity-analysis
                                :viewer :print)
   scaling-domain)
  nil)

;; ### Custom Plans
;;
;; Build custom plans by specifying `:analyse` and `:view` vectors:

(do
  (domain/analyse-domain
   {:analyse [[:domain-extract-fn {:id :times
                                   :metric-path [:stats :elapsed-time :mean]}]
              [:domain-compare-fn {:id :by-size
                                   :axis-key :n
                                   :metric-path [:stats :elapsed-time :mean]}]]
    :view []
    :viewer :none}
   impl-domain)
  nil)

;; Or customize a pre-defined plan with `options->domain-plan`:

(-> (domain/options->domain-plan domain-plans/complexity-analysis
                                 :viewer :none)
    (domain/analyse-domain scaling-domain)
    :regression
    :best-fit)

;; ## Domain Builder
;;
;; For automated benchmarking across a parameter space, use `domain-builder`.
;; It handles running benchmarks for multiple implementations across all
;; combinations of axis values, with adaptive time estimation.

;; ### Basic Usage
;;
;; Define the parameter space (axes) and implementations:

;; Building domain with domain-builder (this may take a minute)...

(def builder-domain
  (domain/domain-builder
   ;; Axes define the parameter space
   {:n [100 500 1000]}
   ;; Implementations to compare
   {:sort
    {:measured     (measured/expr (vec (range 100)))
     :args-builder (fn [{:keys [n]}]
                     (fn [] [(mapv rand-int (repeat n 10000))]))}
    :sort-by
    {:measured     (measured/expr (sort-by identity (vec (range 100))))
     ;; Must provide both args: identity function AND collection
     :args-builder (fn [{:keys [n]}]
                     (fn []
                       [identity (mapv rand-int (repeat n 10000))]))}}
   ;; Options
   :reporter nil)) ; nil for silent, or use (domain/dot-reporter)

;; Check what was built:

(domain/coords builder-domain)

;; The domain has runs for each implementation × axis combination:

(count (domain/runs builder-domain))

;; ### Analyzing Builder Results
;;
;; Use domain plans to analyze the results:

(domain/analyse-domain domain-plans/implementation-comparison builder-domain)

;; ### How It Works
;;
;; Each implementation spec contains:
;; - `:measured` — A measured created with example args (for type hints)
;; - `:args-builder` — `(fn [axis-map] (fn [] [args...]))` generates args
;;   for each coordinate
;;
;; The builder:
;; 1. Computes the cartesian product of axis values
;; 2. Sorts by `:time-axis` (default: first axis) ascending
;; 3. Runs all coordinates for each implementation before moving to the next
;; 4. After 2+ runs, estimates time limits using regression on previous results

;; ## Summary
;;
;; Domains provide a structured way to:
;; - Accumulate benchmark results with coordinates
;; - Query and filter runs
;; - Extract and compare metrics
;; - Visualize results with print or portal viewers
;; - Analyze scaling behavior with regression fitting
;; - Bundle analysis workflows with domain plans
;; - Automate parameter-space benchmarking with domain-builder
;;
;; The immutable design supports exploratory analysis in the REPL,
;; while pipeline functions and domain plans enable composable,
;; reusable analysis workflows.

(kind/hidden
 (bench/set-default-viewer! :print))
