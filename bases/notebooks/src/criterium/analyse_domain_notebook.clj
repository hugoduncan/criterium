(ns
 ^{:kindly/options {:kinds-that-hide-code #{:kind/hidden}}}
 criterium.analyse-domain-notebook
  "Manual domain construction, analysis, and viewing."

  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.builder :as builder]
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
;;
;; This notebook covers the foundational API for manual domain construction
;; and analysis. For automated benchmarking workflows, see the
;; [Domain Builder](./criterium.domain_builder_notebook.html) notebook.

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

;; Building domain with sort benchmarks...

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

(analysis/extract sort-domain [:stats :elapsed-time :mean])

;; Or extract all collected metrics at once (no metric-path):

(keys (:metrics (analysis/extract sort-domain)))

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
   (domain/domain {:impl-axis :impl :implementations [:sort :sort-by]})
   impl-inputs))

;; Building domain comparing implementations...

(def impl-domain (build-impl-domain))

;; Group by implementation:

(let [grouped (analysis/group-by-axis impl-domain :impl)]
  {:axis (:axis grouped)
   :groups (keys (:data grouped))})

;; Compare elapsed time across implementations:

(analysis/compare-by impl-domain :impl [:stats :elapsed-time :mean])

;; ## Input Sequence Generators
;;
;; Use input sequence generators to explore algorithmic complexity.

;; ### Powers of 2
;;
;; Doubling input size reveals O(n), O(n log n), O(n²) patterns:

(builder/powers-of-2 0 10)

;; ### Linear Range
;;
;; Uniform sampling for linear scaling detection:

(builder/linear-range 100 1000 5)

;; ### Logarithmic Range
;;
;; Values evenly spaced over log(n).- useful for testing O(log n)
;; algorithms

(builder/log-range 10 10000 5)

;; ### N Log N Range
;;
;; Values spaced along an n*log(n) curve - useful for testing O(n log n)
;; algorithms like merge sort where you want denser sampling at larger sizes:

(builder/n-log-n-range 10 10000 5)

;; ## Viewing Results
;;
;; Domain analysis results integrate with criterium's view system.
;; The print viewer formats results for terminal output.

;; ### Extract View

((view/domain-extract-table {:extract-id :extract})
 :print
 {:extract (analysis/extract sort-domain [:stats :elapsed-time :mean])})

;; ### Comparison View
;;
;; The comparison view shows a table with axis values as columns:

((view/domain-comparison-table {:comparison-id :comparison})
 :print
 {:comparison (analysis/compare-by impl-domain :impl [:stats :elapsed-time :mean])})

;; ## Scaling Analysis
;;
;; ### Example: O(n log n) Scaling
;;
;; Benchmark sort across powers of 2 to observe n log n behavior:

(def scaling-sizes (builder/powers-of-2 4 9))

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

;; Building scaling analysis domain...

(def scaling-domain (build-scaling-domain))

;; Extract times and observe scaling:

(let [extract (analysis/extract scaling-domain [:stats :elapsed-time :mean])]
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

(let [extract (analysis/extract scaling-domain [:stats :elapsed-time :mean])
      regression (analysis/fit-complexity extract :n)
      elapsed-time-reg (get-in regression [:regressions :elapsed-time])]
  {:best-fit (:best-fit elapsed-time-reg)
   :models (map (fn [{:keys [label r-squared]}]
                  {:model label :r-squared (format "%.4f" r-squared)})
                (sort-by :r-squared > (:models elapsed-time-reg)))})

;; View regression results with the print viewer:

(let [extract (analysis/extract scaling-domain [:stats :elapsed-time :mean])]
  ((view/domain-regression {:regression-id :regression})
   :print
   {:regression (analysis/fit-complexity extract :n)}))

;; Custom models can be provided for specific complexity classes:

(let [result (analysis/fit-complexity
              (analysis/extract scaling-domain [:stats :elapsed-time :mean])
              :n
              {:cubic {:transform (fn [n] (* n n n)) :label "O(n³)"}
               :linear {:transform identity :label "O(n)"}})]
  (get-in result [:regressions :elapsed-time :best-fit]))

;; ## Pipeline Composition
;;
;; For complex analyses, use pipeline functions that operate on data maps:

(-> {:domain impl-domain}
    ((analysis/domain-extract-fn
      {:id :mean-time
       :metric-path [:stats :elapsed-time :mean]}))
    ((analysis/domain-compare-fn
      {:id :impl-comparison
       :axis-key :impl
       :metric-path [:stats :elapsed-time :mean]}))
    keys)

;; Each pipeline function adds its result under a configurable key,
;; enabling multiple analyses on the same domain.

;; Use the pipeline function for composable analysis:

(-> {:domain scaling-domain}
    ((analysis/domain-extract-fn
      {:id :extract
       :metric-path [:stats :elapsed-time :mean]}))
    ((analysis/domain-regression-fn
      {:id :scaling
       :axis :n}))
    (get-in [:scaling :regressions :elapsed-time :best-fit]))

;; ## Summary
;;
;; This notebook covered the foundational domain API:
;; - **Construction** — `domain`, `add-run`, `remove-run`
;; - **Queries** — `runs`, `coords`, `axes`, `select`
;; - **Analysis** — `extract`, `compare-by`, `group-by-axis`
;; - **Generators** — `powers-of-2`, `log-range`, `linear-range`, `n-log-n-range`
;; - **Viewing** — `domain-extract-table`, `domain-extract-chart`, `domain-comparison-table`, `domain-comparison-chart`, `domain-regression`
;; - **Regression** — `fit-complexity` for algorithmic complexity analysis
;; - **Pipelines** — `domain-extract-fn`, `domain-compare-fn`, `domain-regression-fn`
;;
;; For automated benchmarking across parameter spaces, see the
;; [Domain Builder](./criterium.domain_builder_notebook.html) notebook.

(kind/hidden
 (bench/set-default-viewer! :print))
