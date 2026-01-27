(ns quickstart
  "A quick introduction to benchmarking with criterium."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.notebook.helpers :refer [bench-display]]))

;; # Quickstart
;;
;; Criterium is a benchmarking library for Clojure that provides statistically
;; rigorous timing measurements. Unlike simple timing with `(time ...)`,
;; criterium accounts for JVM warmup, measures many samples, and reports
;; statistical summaries.

;; ## Your First Benchmark
;;
;; The `bench` macro is the main entry point. Pass it any expression:

^:kindly/hide-code
(bench-display (bench/bench (reduce + [1 2])))

;; That's it. Criterium automatically:
;;
;; - Warms up the JIT compiler so you measure optimized code
;; - Runs your expression thousands of times
;; - Computes statistics from the samples
;; - Detects and reports outliers
;; - Detects and reports multimodal distributions of execution time
;; - Detects measurement problems caused by external load, turbo boost, etc

;; ## Reading the Output
;;
;; The output has three main sections. Here's what each means:
;;
;; **Elapsed Time Statistics**
;;
;; ```
;; Elapsed Time median: 0.00390 ns CI [0.00390 0.00390] (0.025 0.975)
;; Elapsed Time spread: [0.00376 0.00410] ns (10th-90th percentile)
;; ```
;;
;; Criterium takes the measured samples and bootstraps statistics.  The reported
;; values are the median, the mean and the spread.  The median includes a 95%
;; confidence interval for the statistic.  The spread show the bootstrapped p10
;; and p90 values.
;;
;; ::: {.callout-note}
;; The default output relies mainly on parametric free,
;; distribution free statistics.
;; :::
;;
;; ::: {.callout-note}
;; The statistics are bootstrapped to get most accuracy out of limited samples
;;and to get confidence intervals.
;; :::
;; ```
;; Elapsed Time: 3.21 ns  3σ [2.98 3.44]  min 2.89
;; ```
;;
;; - `3.21 ns` — the mean execution time
;; - `3σ [2.98 3.44]` — the range containing ~99.7% of expected means
;;   (three standard deviations)
;; - `min 2.89` — the fastest sample observed
;;
;; **Outliers**
;;
;; ```
;; Outliers (outliers / samples): low-severe 0 (0.0%), low-mild 1 (0.5%), ...
;; ```
;;
;; Outliers are samples far from the typical range. A few high outliers are
;; normal (GC pauses, OS scheduling). Many outliers suggest unstable
;; measurements.
;;
;; **Sample Scheme**
;;
;; ```
;; Sample Scheme: 200 samples with batch-size 10000 (2000000 evaluations)
;; ```
;;
;; - `200 samples` — number of timing measurements taken
;; - `batch-size 10000` — executions per sample (batched for timer resolution)
;; - `2000000 evaluations` — total executions during measurement

;; ## Benchmarking with Data
;;
;; When benchmarking code that uses data, you'll often define the data
;; outside the benchmark. Criterium handles this correctly:

(let [data (vec (range 1000))]
  (bench/bench (reduce + data)))

;; The `data` binding is captured before timing begins. Only the `reduce`
;; call is measured, not the vector creation.
;;
;; This works because criterium "hoists" local bindings—it captures their
;; values once, then uses those values for every timed execution.

;; Here's a clearer example showing what gets measured:

^:kindly/hide-code
(bench-display
 (let [expensive-setup (vec (range 10000))]  ; Created once, before timing
   (bench/bench (first expensive-setup))))   ; Only this is timed

;; The 10,000-element vector is created once. The benchmark measures only
;; the `first` call, which takes nanoseconds regardless of the setup cost.

;; ## Seeing the Distribution
;;
;; For deeper insight, use the `log-histogram` bench plan to visualize
;; how your samples are distributed:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))
              :bench-plan bench-plans/log-histogram))

;; The histogram shows timing samples on a log scale. Look for:
;;
;; - **Shape**: A tight cluster indicates consistent performance
;; - **Spread**: Wide spread suggests variable execution times
;; - **Distant bars**: Bars far to the right are slow outliers
;;   (often from GC or OS interrupts)
;;
;; The quantiles section shows percentiles:
;;
;; - `p50` — median (half of samples are faster)
;; - `p90` — 90th percentile
;; - `p99` — 99th percentile (tail latency)

;; ## Next Steps
;;
;; Now that you can run benchmarks, read the
;; [Introduction](./introduction.html) to learn how to choose the right
;; benchmarking approach for your goals.
