(ns
 ^{:kindly/options {:kinds-that-hide-code #{:kind/hidden}}}
 criterium.domain-builder-notebook
  "Automated domain construction and analysis workflows."
  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.builder :as builder]
   [criterium.measured :as measured]
   [scicloj.kindly.v4.kind :as kind]))

(kind/hidden
 (bench/set-default-viewer! :kindly))

;; # Domain Builder
;;
;; This notebook covers high-level workflows for automated benchmarking
;; across parameter spaces. For foundational domain concepts and manual
;; construction, see the
;; [Domain Analysis](./criterium.analyse_domain_notebook.html) notebook.
;;
;; Key features:
;; - **Domain Plans** — Pre-defined analysis workflows with `analyse-domain`
;; - **Custom Plans** — Build your own analysis pipelines
;; - **Domain Builder** — Automated parameter-space benchmarking

;; ## Domain Plans
;;
;; Domain plans bundle analysis and viewing into a single call. Plans respect
;; the default viewer set via `bench/set-default-viewer!`, so in this notebook
;; they use `:kindly` automatically.

;; First, let's build a domain manually for demonstration:

(defn random-seq [n]
  (mapv rand-int (repeat n 10000)))

(def sort-sizes [10 50 100])

(def sort-inputs
  (into {}
        (map (fn [n] [n (random-seq n)]) sort-sizes)))

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

;; Building domain for plan demonstrations...

(def sort-domain (build-sort-domain))

;; ### Pre-defined Plans
;;
;; `extract-elapsed-time` displays metric values across all runs:

(analysis/analyse-domain domain-plans/extract-elapsed-time sort-domain)

;; Build a scaling domain for complexity analysis:

(def scaling-sizes (builder/powers-of-2 4 7))

(def scaling-inputs
  (into {} (map (fn [n] [n (random-seq n)]) scaling-sizes)))

(defn build-scaling-domain
  "Build domain for scaling analysis."
  []
  (reduce
   (fn [d n]
     (let [input (scaling-inputs n)]
       (bench/bench (sort input) :viewer :none)
       (domain/add-run d {:n n} (:data (bench/last-bench)))))
   (domain/domain)
   scaling-sizes))

;; Building scaling domain...

(def scaling-domain (build-scaling-domain))

;; `complexity-analysis` extracts all collected metrics and fits regression models:

(analysis/analyse-domain domain-plans/complexity-analysis scaling-domain)

;; Build an implementation comparison domain:

(def impl-inputs
  (into {} (map (fn [n] [n (random-seq n)]) [10 50])))

(defn build-impl-domain
  "Build domain comparing sort implementations."
  []
  (reduce
   (fn [d [n input]]
     (bench/bench (sort input))
     (let [d (domain/add-run d {:n n :impl :sort} (:data (bench/last-bench)))]
       (bench/bench (sort-by identity input) :viewer :none)
       (domain/add-run d {:n n :impl :sort-by} (:data (bench/last-bench)))))
   (domain/domain {:impl-axis :impl :implementations [:sort :sort-by]})
   impl-inputs))

;; Building implementation comparison domain...

(def impl-domain (build-impl-domain))

;; `implementation-comparison` compares metrics across implementations:

(analysis/analyse-domain domain-plans/implementation-comparison impl-domain)

;; ### Explicit Viewer Selection
;;
;; Override the default viewer with `options->domain-plan`:

(do
  (analysis/analyse-domain
   (analysis/options->domain-plan
    domain-plans/complexity-analysis
    :viewer :print)
   scaling-domain)
  nil)

;; ### Custom Plans
;;
;; Build custom plans by specifying `:analyse` and `:view` vectors:

(do
  (analysis/analyse-domain
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

(-> (analysis/options->domain-plan domain-plans/complexity-analysis
                                   :viewer :none)
    (analysis/analyse-domain scaling-domain)
    (get-in [:regression :regressions :elapsed-time :best-fit]))

;; ## Domain Builder
;;
;; For automated benchmarking across a parameter space, use `domain-builder`.
;; It handles running benchmarks for multiple implementations across all
;; combinations of axis values, with adaptive time estimation.

;; ### Simplified Form (No Axes)
;;
;; For quick comparisons without parameter variation, pass a map of
;; implementation names to measured expressions:

;;"Building domain with simplified domain-builder..."

(def simple-comparison-domain
  (let [input (vec (range 64))]
    (builder/domain-builder
     {:sort    (measured/expr (sort input))
      :sort-by (measured/expr (sort-by identity input))}
     :reporter nil)))

;; This produces a domain with one run per implementation:

(domain/coords simple-comparison-domain)

;; Analyze with any domain plan:

(analysis/analyse-domain domain-plans/implementation-comparison simple-comparison-domain)

;; ### Domain Expression Macro
;;
;; The `domain-expr` macro provides concise syntax for defining domain
;; specifications. It generates the `:axes` and `:implementations` map
;; structure required by `domain-builder`.

;; Single expression (uses `:default` as impl key):

(domain/domain-expr [n [10 50 100]]
                    (sort (random-seq n)))

;; Multiple implementations via map:

(domain/domain-expr [n [10 50 100]]
                    {:sort    (sort (random-seq n))
                     :sort-by (sort-by identity (random-seq n))})

;; Multiple axes:

(domain/domain-expr [n [10 50] m [1 2 3]]
                    (take m (sort (random-seq n))))

;; Use with domain-builder via `apply` and `mapcat`:

(def expr-domain
  (let [spec (domain/domain-expr [n (builder/log-range 8 128 3)]
                                 {:sort    (sort (random-seq n))
                                  :sort-by (sort-by identity (random-seq n))})]
    (apply builder/domain-builder
           (concat [(:axes spec) (:implementations spec)]
                   [:reporter nil]))))

(domain/coords expr-domain)

(analysis/analyse-domain domain-plans/implementation-comparison expr-domain)

;; ### Full Form (With Axes)
;;
;; Define the parameter space (axes) and implementations:

;; Building domain with domain-builder (this may take a minute)...

(def builder-domain
  (builder/domain-builder
   ;; Axes define the parameter space
   {:n (builder/n-log-n-range 8 128 3)}
   ;; Implementations to compare
   {:sort    (fn [{:keys [n]}]
               (measured/expr (sort (random-seq n))))
    :sort-by (fn [{:keys [n]}]
               (measured/expr (sort-by identity (random-seq n))))}
   ;; Options
   :reporter nil)) ; nil for silent, or use (builder/dot-reporter)

;; Check what was built:

(domain/coords builder-domain)

;; The domain has runs for each implementation × axis combination:

(count (domain/runs builder-domain))

;; ### Analyzing Builder Results
;;
;; Use domain plans to analyze the results:

(analysis/analyse-domain domain-plans/implementation-comparison builder-domain)

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

;; ### Collecting Additional Metrics
;;
;; Pass `:bench-options` to collect metrics beyond elapsed time.
;; Here we add thread allocation tracking:

;; Building domain with allocation tracking...

(def builder-domain-with-alloc
  (builder/domain-builder
   {:n (builder/n-log-n-range 8 256 4)}
   {:sort    (fn [{:keys [n]}]
               (measured/expr (sort (mapv rand-int (repeat n 10000)))))
    :sort-by (fn [{:keys [n]}]
               (measured/expr
                (sort-by identity (mapv rand-int (repeat n 10000)))))}
   :bench-options {:metric-ids [:elapsed-time :thread-allocation]}
   :reporter nil))

;; Use complexity-analysis to analyze all collected metrics at once.
;; This extracts and fits regression models for both elapsed-time and
;; thread-allocation:

(analysis/analyse-domain
 domain-plans/complexity-analysis
 builder-domain-with-alloc)

;; Compare allocations between implementations using a custom plan:

(analysis/analyse-domain
 {:analyse [[:domain-compare-fn {:id :alloc
                                 :axis-key :impl
                                 :metric-path [:stats :thread-allocation :mean]}]]
  :view [[:domain-comparison-table {:comparison-id :alloc}]
         [:domain-comparison-chart {:comparison-id :alloc}]]}
 builder-domain-with-alloc)

;; ## Summary
;;
;; This notebook covered automated domain workflows:
;; - **Domain Plans** — `analyse-domain` with pre-defined plans
;;   (`extract-elapsed-time`, `complexity-analysis`, `implementation-comparison`)
;; - **Custom Plans** — Build analysis pipelines with `:analyse` and `:view` vectors
;; - **Viewer Selection** — Override defaults with `options->domain-plan`
;; - **Domain Builder** — `domain-builder` for automated parameter-space benchmarking
;;   - **Simplified Form** — `{:impl-a measured-a :impl-b measured-b}` for quick comparisons
;;   - **Full Form** — Axes + implementation specs for parameter-space exploration
;; - **Implementation Specs** — `:measured` and `:args-builder` for defining benchmarks
;; - **Additional Metrics** — `:bench-options` for collecting memory allocations, etc.
;;
;; For foundational concepts and manual domain construction, see the
;; [Domain Analysis](./criterium.analyse_domain_notebook.html) notebook.

(kind/hidden
 (bench/set-default-viewer! :print))
