(ns criterium.bench-options-notebook
  "Explore criterium's predefined bench plans and viewer options."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.notebook.helpers :refer [bench-display]]
   [scicloj.kindly.v4.kind :as kind]))

;; # Bench Options
;;
;; Criterium provides configurable benchmarking through bench plans and viewer
;; options. This notebook demonstrates the predefined options and how to
;; customize benchmark behavior.

;; ## Collect Plans
;;
;; A collect plan determines how samples are gathered. Criterium provides two
;; built-in collect plans:
;;
;; - `:with-jit-warmup` - Full benchmarking with JIT warmup (default)
;; - `:one-shot` - Minimal single execution

;; ### :one-shot - Minimal Collection
;;
;; The `:one-shot` collect plan executes the expression once without warmup.
;; Use this for measuring an expression that will not be evaluated by your code
;;sufficiently to trigger JIT.

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :collect-plan :one-shot))

;; The one-shot output shows:
;; - Single elapsed time measurement
;; - No statistical analysis (only one sample)
;; - GC and memory metrics if applicable

;; ### :with-jit-warmup - Full Benchmarking (Default)
;;
;; The default collect plan performs extensive sampling with JIT warmup:
;; 1. Estimation phase - determines how many executions fit in a sample
;; 2. Warmup phase - runs ~150,000 executions for JIT optimization
;; 3. Measurement phase - collects ~200 samples for statistical analysis

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :collect-plan :with-jit-warmup))

;; The default output includes:
;; - Mean execution time with confidence interval
;; - Standard deviation as percentage of mean
;; - Outlier detection
;; - Sample count and executions per sample

;; ## Bench Plans
;;
;; Bench plans combine collect plans with analysis and view configurations.
;; The `criterium.bench-plans` namespace provides ready-to-use configurations.

;; ### default-one-shot
;;
;; Minimal benchmarking without statistical analysis:

bench-plans/default-one-shot

;; ### default-with-warmup
;;
;; Standard benchmarking with full statistics:

bench-plans/default-with-warmup

;; ### log-histogram
;;
;; Extended analysis with histogram visualization:

bench-plans/log-histogram

;; Using the log-histogram bench plan provides additional output:
;; - Quantiles (p50, p90, p99)
;; - Outlier counts by category
;; - Sample percentiles
;; - Histogram visualization

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :bench-plan bench-plans/log-histogram))

;; ## Viewer Options
;;
;; The `:viewer` option controls how benchmark results are displayed.
;; Criterium provides three viewer modes:
;;
;; - `:print` - Human-readable text output (default)
;; - `:pprint` - Clojure data structure with pretty printing
;; - `:portal` - Interactive visualization (requires Portal)

;; ### :print (Default)
;;
;; The default viewer formats results as readable text:

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :viewer :print))

;; ### :pprint
;;
;; The pprint viewer outputs the raw data structure:

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :viewer :pprint))

;; The pprint output shows the internal data structure, useful for:
;; - Understanding what metrics criterium collects
;; - Debugging custom analysis pipelines
;; - Programmatic access without using last-bench

;; ### :portal
;;
;; The Portal viewer provides interactive charts and visualizations.
;; Requires Portal to be connected to tap>.
;;
;; Setup Portal before using this viewer:

(kind/code "(require '[portal.api :as p])
(def p (p/open))
(add-tap #'p/submit)")

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :viewer :portal))

;; Portal viewer features:
;; - Interactive histograms
;; - Clickable data exploration
;; - Tables for statistical summaries

;; ## Customizing Collection Parameters
;;
;; The `:with-jit-warmup` collect plan accepts configuration options:
;;
;; - `:limit-time-s` - Maximum time for the entire benchmark (default: 10s)
;; - `:num-warmup-samples` - Target warmup executions (default: 150000)
;; - `:num-measure-samples` - Target sample count (default: 200)

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000)) :limit-time-s 2))

;; ## Specifying Metrics
;;
;; The `:metric-ids` option selects which metrics to collect.
;; Available metrics:
;; - `:elapsed-time` - Wall clock timing
;; - `:memory` - Memory usage
;; - `:garbage-collector` - GC statistics
;; - `:thread-allocation` - Per-thread allocation (requires agent)
;; - `:compilation` - JIT compilation events
;; - `:class-loader` - Class loading statistics
;; - `:finalization` - Object finalization counts

^:kindly/hide-code
(bench-display (bench/bench (vec (range 1000)) :metric-ids [:elapsed-time :memory]))

;; ## Combining Options
;;
;; Options can be combined for tailored benchmarking:

^:kindly/hide-code
(bench-display (bench/bench (reduce + (range 1000))
                            :viewer :pprint
                            :limit-time-s 5
                            :metric-ids [:elapsed-time :memory]))
