(ns criterium.domain-notebook
  "Working with multiple related benchmark runs using domains."

  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.view :as view]
   [criterium.viewer.print]
   [scicloj.kindly.v4.kind :as kind]))

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
  (into {} (map (fn [n] [n (vec (repeatedly n #(rand-int 10000)))]) sort-sizes)))

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

^:kindly/hide-code
(def impl-inputs
  "Shared inputs for implementation comparison."
  (into {} (map (fn [n] [n (vec (repeatedly n #(rand-int 10000)))]) [100 500])))

^:kindly/hide-code
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

^:kindly/hide-code
(kind/md "Building domain comparing implementations...")

^:kindly/hide-code
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

^:kindly/hide-code
(kind/code
 (with-out-str
   ((view/domain-extract {:extract-id :extract})
    :print
    {:extract (domain/extract sort-domain [:stats :elapsed-time :mean])})))

;; ### Comparison View
;;
;; The comparison view shows a table with axis values as columns:

^:kindly/hide-code
(kind/code
 (with-out-str
   ((view/domain-comparison {:comparison-id :comparison})
    :print
    {:comparison (domain/compare-by impl-domain :impl [:stats :elapsed-time :mean])})))

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

^:kindly/hide-code
(def scaling-sizes (take 4 (domain/powers-of-2 6 10)))

^:kindly/hide-code
(def scaling-inputs
  (into {} (map (fn [n] [n (vec (repeatedly n #(rand-int 10000)))]) scaling-sizes)))

^:kindly/hide-code
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

^:kindly/hide-code
(kind/md "Building scaling analysis domain...")

^:kindly/hide-code
(def scaling-domain (build-scaling-domain))

;; Extract times and observe scaling:

^:kindly/hide-code
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

;; ## Summary
;;
;; Domains provide a structured way to:
;; - Accumulate benchmark results with coordinates
;; - Query and filter runs
;; - Extract and compare metrics
;; - Visualize results with print or portal viewers
;; - Analyze scaling behavior
;;
;; The immutable design supports exploratory analysis in the REPL,
;; while pipeline functions enable composable data transformations.
