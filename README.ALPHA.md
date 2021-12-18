# Criterium 0.5.x

The goal is to make criterium more data driven, and usable in more
contexts.

## Status

Early alpha.  Breaking changes will be made.

## Quick Start

For the moment there will be no jar release.  Please use as a git
dependency, taking a sha from the master branch.

```
{:deps {io.github.hugoduncan/criterium
        {:git/sha "xxxx"
        :deps/root "projects/criterium"}}
```

To time an expression use `criterium.bench/bench`.

```clojure
(require '[criterium.bench :as bench])
(bench/bench (criterium.jvm/wait 10000)) ; 10us busy wait
```

If you are using portal, try:

```clojure
(require 'criterium.viewer.portal)
(bench/bench (criterium.jvm/wait 10000)
  :viewer :portal
  :benchmark criterium.benchmarks/log-histogram)
```

## New features

- improved accuracy on very fast functions.
- charting via portal
- thread memory allocation collection
- agent to allow capture of all allocations during the execution of an
  expression.

## Design

The `bench` cli is a thin layer over the following.

Criterium separates metrics collection from the processing of the metrics
to analyse and display them.

We provide multiple ways to collect metrics; running a collection plan,
instrumenting a function, or triggering collection on events.  You can
also provide metrics by any other means you like.

Benchmarks are pipelines that can be run with different reporting
targets.

### Metrics Collection

Controlled sampling is based on the concept of a "measured".  This
consists of a pair of functions, a state constructor, used to capture
arguments, and a measure function, that takes the output of the state
constructor as an argument, together with an evaluation count, runs the
benchmark subject the given number of times, and returns the total time
taken.

This design addresses both time resolution and argument capture
concerns.

Other ways of sampling are provided.  You can instrument a function to
collect samples, or can use a trigger which collects samples based on
deltas between successive firing of the trigger.

### Benchmark

Once samples have been collected they are passed to the benchmark, which
provides analysis and viewing.

#### Analysis

The analysis is based on a set of analysis functions that read and write
paths in a data map.  Each analysis reads and writes default paths in
the map, but these can be explicitly specified.

#### View

Viewing is based on a set of viewing function that read paths from
the data map.  Each view uses default paths, but these can be
explicitly specified.

The built in views support different viewers.  The default viewer,
`:print`, prints results in human readable format.  There are also
`:pprint` and `:portal` viewers.

The `:portal` viewer is capable of displaying charts.

## Development Plan

Features that will probably get added:

- add KDE to improve estimation of the PDF.
- add bi-modal detection, probably based on KDE.
- add a percentile sampling mode based oh HDRHistogram
- add charting and regressions around change in metrics with a varied parameter
- add charting to compare alternate implementations of expressions
- add fitting of different distributions (e.g. hyperbolic secant)
