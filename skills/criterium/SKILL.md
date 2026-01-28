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

You rarely interact with `measured` directly, but it enables advanced patterns like argument generation. See [Argument Generation](#argument-generation) for explicit usage with test.check generators.

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

### default

Used automatically. Provides:
- JIT warmup phase
- Bootstrap confidence intervals
- Outlier detection
- KDE density estimation (for multimodal warnings)

### histogram

Non-parametric distribution analysis with histogram visualization:

```clojure
(require '[criterium.bench-plans :as plans])

(bench/bench (my-function)
             :bench-plan plans/histogram)
```

Includes:
- Histogram with Knuth optimal binning
- KDE density estimation
- Mode detection for multimodal distributions

### distribution-analysis

Parametric distribution fitting for understanding timing variability:

```clojure
(bench/bench (my-function)
             :bench-plan plans/distribution-analysis)
```

Adds:
- Distribution fitting (gamma, log-normal, Weibull)
- Shape statistics (skewness, kurtosis)
- Goodness-of-fit tests
- Q-Q plots (with appropriate viewer)

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

## Domain Analysis

Domain analysis benchmarks across a parameter space rather than at a single point. Use it for:
- Comparing implementations at multiple input sizes
- Analyzing algorithmic complexity (O(n), O(n log n), etc.)
- Understanding scaling behavior

### Basic Usage

```clojure
(require '[criterium.domain :as domain]
         '[criterium.domain.builder :as builder]
         '[criterium.domain-plans :as domain-plans])

;; Benchmark sorting across input sizes
(domain/bench
 (domain/domain-expr
  [n (builder/log-range 10 1000 5)]
  (sort (vec (range n)))))
```

The `domain-expr` macro defines axes (parameter ranges) and expressions to benchmark. The `bench` function runs benchmarks at each coordinate and analyzes results.

### Comparing Implementations

Use a map body in `domain-expr` to compare implementations:

```clojure
(domain/bench
 (domain/domain-expr
  [n (builder/log-range 100 10000 5)]
  {:sort    (sort (vec (range n)))
   :sort-by (sort-by identity (vec (range n)))})
 :domain-plan domain-plans/implementation-comparison)
```

Output shows the baseline (first implementation) in absolute values and others as relative factors.

### Complexity Analysis

Fit O(log n), O(n), O(n log n), O(n²) models:

```clojure
(domain/bench
 (domain/domain-expr
  [n (builder/n-log-n-range 10 10000 7)]
  (sort (vec (range n))))
 :domain-plan domain-plans/complexity-analysis)
```

Use `n-log-n-range` for better sampling when expecting O(n log n) complexity.

### Range Generators

| Function | Use Case |
|----------|----------|
| `log-range` | Wide range coverage (10 to 10000) |
| `linear-range` | Uniform sampling |
| `n-log-n-range` | O(n log n) algorithms |
| `powers-of-2` | Binary scaling patterns |

### Domain Plans

| Plan | Purpose |
|------|---------|
| `extract-metrics` | Default - shows all metrics |
| `implementation-comparison` | Compare implementations with factors |
| `complexity-analysis` | Fit complexity models |

### Options

```clojure
(domain/bench
 (domain/domain-expr ...)
 :domain-plan domain-plans/complexity-analysis
 :reporter nil                        ; Silent (no progress dots)
 :bench-options {:limit-time-s 2})    ; Per-benchmark time limit
```

## Argument Generation

Generate diverse inputs for each benchmark iteration using test.check generators.

**Dependency:** `criterium/arg-gen` (separate artifact)

```clojure
;; deps.edn
{:deps {criterium/arg-gen {:mvn/version "0.5.x"}}}
```

### The measured Macro

```clojure
(require '[criterium.arg-gen :as arg-gen]
         '[clojure.test.check.generators :as gen])

;; Basic usage - each iteration gets fresh generated values
(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured
  [n gen/small-integer]
  (* n n)))
```

### Multiple Bindings

Bindings are processed left-to-right, with earlier bindings available to later generators. This enables dependent generation where one value determines another:

```clojure
(arg-gen/measured
 [n (gen/choose 10 100)                    ; n bound first
  coll (gen/vector gen/small-integer n)]   ; n used to size the vector
 (reduce + coll))
```

### Options

```clojure
;; Control generator size (affects sized generators like gen/vector)
(arg-gen/measured {:size 50}
 [coll (gen/vector gen/small-integer)]
 (sort coll))

;; Reproducible generation with seed
(arg-gen/measured {:seed 12345}
 [n gen/small-integer]
 (* n n))
```

### Common Patterns

```clojure
;; String processing
(arg-gen/measured
 [s gen/string-alphanumeric]
 (clojure.string/upper-case s))

;; Collection operations
(arg-gen/measured {:size 100}
 [v (gen/vector gen/small-integer)]
 (sort v))

;; Map operations
(arg-gen/measured {:size 20}
 [m (gen/map gen/keyword gen/small-integer)]
 (vals m))
```

## Best Practices

### JVM Warmup

The JIT compiler optimizes code during execution. Criterium handles warmup automatically, but be aware:

- First benchmark in a session may be slower (class loading, JIT)
- Run benchmarks multiple times if results seem inconsistent
- The `default` plan includes warmup phases

### Avoiding Measurement Pitfalls

**Dead code elimination:** The JVM may optimize away computations with unused results. Criterium prevents this by consuming return values, but avoid:

```clojure
;; BAD - side-effect only, result discarded
(bench/bench (do (sort data) nil))

;; GOOD - return the result
(bench/bench (sort data))
```

**Side effects:** Benchmarks with side effects (I/O, mutation) may not measure what you intend:

```clojure
;; BAD - file I/O dominates timing
(bench/bench (spit "test.txt" (str data)))

;; GOOD - separate I/O from computation
(bench/bench (str data))
```

**Constant folding:** The compiler may evaluate constant expressions at compile time:

```clojure
;; BAD - may be optimized to constant
(bench/bench (+ 1 2))

;; BETTER - use local bindings
(let [a 1 b 2]
  (bench/bench (+ a b)))
```

### Interpreting Results

**Outliers:** Some outliers are normal (GC, OS scheduling). Concern when:
- High-severe outliers exceed 5% of samples
- Results vary significantly between runs
- Minimum time much lower than mean

**Confidence intervals:** The 3σ bounds show where 99.7% of values fall. Wide bounds suggest high variance—consider longer benchmarks or investigating causes.

### Choosing Bench Plans

| Situation | Plan |
|-----------|------|
| Quick measurement | `default` |
| Understanding distribution shape | `distribution-analysis` |
| Comparing implementations | `implementation-comparison` (domain) |
| Analyzing complexity | `complexity-analysis` (domain) |

## Quick Reference

### Single Expression

```clojure
(require '[criterium.bench :as bench])

(bench/bench (my-function arg1 arg2))
(bench/bench (my-function arg1 arg2) :viewer :pprint)
(bench/last-bench)  ; Access results
```

### With Local Bindings

```clojure
(let [data (vec (range 1000))]
  (bench/bench (reduce + data)))
```

### Compare Implementations

```clojure
(require '[criterium.domain :as domain]
         '[criterium.domain.builder :as builder]
         '[criterium.domain-plans :as domain-plans])

(domain/bench
 (domain/domain-expr
  [n (builder/log-range 100 10000 5)]
  {:impl-a (sort (vec (range n)))
   :impl-b (sort-by identity (vec (range n)))})
 :domain-plan domain-plans/implementation-comparison)
```

### Complexity Analysis

```clojure
(domain/bench
 (domain/domain-expr
  [n (builder/log-range 10 10000 7)]
  (my-algorithm n))
 :domain-plan domain-plans/complexity-analysis)
```

### Generated Arguments

```clojure
(require '[criterium.arg-gen :as arg-gen]
         '[clojure.test.check.generators :as gen])

(bench/bench-measured
 (bench/options->bench-plan)
 (arg-gen/measured {:size 100}
  [coll (gen/vector gen/small-integer)]
  (sort coll)))
```

