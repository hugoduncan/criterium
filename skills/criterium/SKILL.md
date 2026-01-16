---
name: criterium
description: Use this skill when users ask about benchmarking Clojure code, measuring performance, profiling execution time, or using the criterium library. Covers the 0.5.x API including bench macro, bench plans, viewers, domain analysis, and argument generation.
---

# Criterium

Statistically rigorous benchmarking for Clojure that accounts for JVM warmup, garbage collection, and measurement overhead.

## Overview

Criterium is the standard benchmarking library for Clojure. Unlike naive timing approaches, it provides:

- **JVM-aware measurement** - Handles JIT warmup and GC interference
- **Statistical rigor** - Bootstrap confidence intervals, outlier detection
- **Multiple output formats** - Text, structured data, interactive charts

**Library:** `criterium/criterium`
**Current Version:** 0.5.x (alpha)
**License:** EPL-1.0

**Note:** The 0.4.x API (`criterium.core/bench`) is deprecated. Use `criterium.bench/bench` for all new code.

## Quick Start

```clojure
(require '[criterium.bench :as bench])

(bench/bench (+ 1 1))
```

Output:
```
      Elapsed Time: 2.15 ns  3σ [2.08 2.22]  min 2.07
Outliers (outliers / samples): low-severe 0 (0.0%), low-mild 0 (0.0%), high-mild 3 (1.5%), high-severe 0 (0.0%)
Sample Scheme: 200 samples with batch-size 4651 (930200 evaluations)
```

The output shows:
- **Mean time** (2.15 ns) with 3-sigma confidence bounds
- **Outlier counts** by category (low/high, mild/severe)
- **Sample scheme** - how measurements were collected

## Core Concepts

Criterium uses a three-stage pipeline:

```
Collection → Analysis → View
```

1. **Collection** - Gather raw timing samples using collectors
2. **Analysis** - Apply statistical computations (mean, bootstrap CI, outliers)
3. **View** - Format and present results through viewers

### The Measured Abstraction

The `bench` macro wraps your expression in a `measured` - a benchmarkable unit that:
- Prevents constant folding by hoisting arguments
- Supports batched evaluation for fast expressions
- Provides zero-allocation measurement

You rarely interact with `measured` directly, but it enables advanced patterns like argument generation.

## Basic Benchmarking

### The bench Macro

```clojure
(bench/bench expr & options)
```

Returns the expression's value. Benchmark data available via `(bench/last-bench)`.

### Common Options

```clojure
;; Change output format
(bench/bench (sort data) :viewer :pprint)

;; Use a specific bench plan
(bench/bench (sort data) :bench-plan criterium.bench-plans/distribution-analysis)

;; Limit benchmark duration
(bench/bench (sort data) :limit-time-s 5)

;; Collect allocation data (requires native agent)
(bench/bench (sort data) :with-allocation-trace true)
```

### Using Local Bindings

The `bench` macro captures local bindings from the enclosing scope:

```clojure
(let [data (vec (range 1000))]
  (bench/bench (reduce + data)))
```

### Reading Output

Default output fields:

| Field | Meaning |
|-------|---------|
| Elapsed Time | Mean with 3σ bounds and minimum |
| Outliers | Count by category (low/high, mild/severe) |
| Sample Scheme | Samples × batch-size = total evaluations |

### Accessing Results Programmatically

```clojure
(bench/bench (reduce + (range 100)))

;; Get full results
(bench/last-bench)

;; Extract specific values
(require '[criterium.util.helpers :as util])
(util/stats-value (:data (bench/last-bench)) :stats :elapsed-time :mean)
```

## Bench Plans

Bench plans configure what analysis and output criterium produces. The default plan handles most cases.

### default-with-warmup (Default)

Used automatically. Provides:
- JIT warmup phase
- Bootstrap confidence intervals
- Outlier detection
- KDE density estimation

### distribution-analysis

Use when you need to understand the shape of your timing distribution:

```clojure
(require '[criterium.bench-plans :as plans])

(bench/bench (my-function)
             :bench-plan plans/distribution-analysis)
```

Adds:
- Distribution fitting (gamma, log-normal, Weibull)
- Shape statistics (skewness, kurtosis)
- Goodness-of-fit tests
- Q-Q plots (with appropriate viewer)

### Other Plans

- `log-histogram` - Histogram visualization
- `kde-histogram` - KDE density estimation with histogram
- `kde-modes` - Mode detection for multimodal distributions

### Custom Plans

Plans are maps with `:analyse` and `:view` vectors:

```clojure
{:collector-config {...}
 :analyse [:transform-log :outliers [:stats {}] :bootstrap-stats]
 :view [:stats :bootstrap-stats :outlier-counts]}
```

## Viewers

Viewers control output format. Set per-call or globally.

### :print (Default)

Human-readable text to stdout:

```clojure
(bench/bench (+ 1 1))  ; uses :print
```

### :pprint

Structured Clojure data, useful for programmatic access:

```clojure
(bench/bench (+ 1 1) :viewer :pprint)
```

### :portal

Interactive charts and tables in Portal:

```clojure
;; Setup: connect Portal to tap>
(require '[portal.api :as p])
(def portal (p/open))
(add-tap #'p/submit)

;; Use portal viewer
(bench/bench (+ 1 1) :viewer :portal)
```

Provides interactive histograms, KDE plots, and tabular data.

### :kindly

For Clay/Clerk notebooks with Vega-Lite charts:

```clojure
(bench/set-default-viewer! :kindly)
(bench/bench (+ 1 1))
```

Outputs Kindly-annotated data structures rendered as tables and charts.

### Setting Default Viewer

```clojure
;; Set for all subsequent bench calls
(bench/set-default-viewer! :kindly)

;; Check current default
(bench/default-viewer)
```

