# Allocation Tracing for Instrumented and Sampled Functions

## Problem Description

`instrument-fn` (`criterium.instrument-fn`) and `sample-fn`
(`criterium.sampled-fn`) wrap a function so that performance metrics are
collected on every invocation. They currently surface timing and the
JMX-based metrics that the collector pipeline already provides
(`:elapsed-time`, `:memory`, `:thread-allocation`,
`:garbage-collector`, ...).

None of these can report **how many bytes were freed** during a call.
The `:thread-allocation` stage reads `jvm/thread-allocated-bytes`, a
`ThreadMXBean` counter, which is monotonic and reports only *net*
allocation. There is no JMX path to freed-byte information.

We want to make it possible to obtain **total allocated bytes** and
**total freed bytes** (and the corresponding object counts) from both
`instrument-fn` and `sample-fn`, by capturing per-invocation allocation
data through the native agent's `with-allocation-tracing` and
aggregating it across invocations.

## Background

### How the two wrappers work

Both `InstrumentedFn` and `SampledFn` are `deftype`s that:

1. Wrap the user function in a private `measured` whose `measured-f`
   times one evaluation and returns `[elapsed-time expr-value]`.
2. Push each invocation through a **collector pipeline** built from a
   `collector-config` of the shape `{:stages [...] :terminator ...}`.
3. Store the resulting per-invocation sample, differing only in the
   aggregation strategy:
   - `InstrumentedFn` retains every sample map (`:metric->values`,
     arrays per metric path).
   - `SampledFn` folds each sample into a t-digest per *quantitative*
     metric path (`:metric->digest`).

Both expose state through the `criterium.sampler/Sampler` protocol
(`samples-map`, `reset-samples!`).

### The collector pipeline

`criterium.collector` builds a two-phase pipeline:

- a **sample function** that brackets the measured invocation and writes
  raw before/after readings into an `Object[]` (designed to be
  allocation-free), and
- an **xform** that converts the raw array into a metrics map.

Stages are `SampleStage` records (`criterium.collector.fns`) with `:m`
(sample macro fragment) and `:x` (xform) keys. Existing stages such as
`thread-allocation`, `memory`, and `garbage-collector` already follow
the exact pattern we need: capture a start reading, invoke the rest of
the pipeline, capture an end reading, and compute a delta in the xform.
Metric metadata lives in `criterium.collector.metrics`.

### The agent allocation API

`criterium.agent` already provides everything required:

- `with-allocation-tracing` — `[allocation-records result]`, degrades to
  `[nil result]` when the agent is not attached.
- `allocation-on-thread?` / `criterium.allocation/filter-thread` — filter
  records to a thread.
- `allocations-summary` — returns
  `{:num-allocated :num-freed :allocated-bytes :freed-bytes}`.

`bases/criterium` already depends on `criterium.agent` (see
`criterium.allocation`), so no new project dependency is introduced.

## Goals

- Report total allocated bytes and total freed bytes (and object counts)
  from both `instrument-fn` and `sample-fn`.
- Reuse the existing collector / metric / analysis infrastructure rather
  than special-casing the two `deftype`s.
- Degrade gracefully when the native agent is not attached.

## Non-Goals

- Replacing the JMX `:thread-allocation` stage. It remains the
  low-overhead choice for net allocation.
- Making allocation tracing suitable for high-throughput timing
  benchmarks. It is heavyweight and intended for instrumentation.
- Allocation *site* / call-tree attribution. This design covers
  aggregate totals only.

## Solution Options

### Option A — New `allocation-tracing` collector stage (recommended)

Add a `SampleStage` to `criterium.collector.fns`, modelled on
`thread-allocation`:

- **sample-m**: capture `(jvm/current-thread-id)`, wrap the
  `invoke-next` expansion in `agent/with-allocation-tracing`, and store
  `[thread-id records]` in the sample slot.
- **xform**: filter records to the captured thread and call
  `agent/allocations-summary`, emitting

  ```clojure
  {:allocation {:allocated-bytes … :freed-bytes …
                :num-allocated …   :num-freed …}}
  ```

Add an `:allocation` entry to `criterium.collector.metrics` with
`:quantitative` value paths `[:allocation :allocated-bytes]` and
`[:allocation :freed-bytes]` (and optionally the counts).

Both wrappers then pick the metric up automatically when the stage is
present in their `collector-config`:

- `instrument-fn` retains the per-invocation values; totals are a
  `reduce +` over the `:metric->values` arrays.
- `sample-fn` filters to `:quantitative` metrics, so the allocation
  paths become t-digests; a total is recoverable as
  `mean × sample-count`.

**Pros**: idiomatic; one new stage plus a metric def; both wrappers and
the downstream analysis/view code inherit the capability with no changes
to the `deftype`s.

**Cons**: routes through `measured/invoke`, which allocates ≈208 B inside
the trace window (measured; see Measurement Precision), so it does **not**
give precise garbage measurement; sampled-fn totals are approximate. Use
only where the fixed in-window offset is tolerable.

### Option B — Wrap inside the wrappers' private `measured`

Change `measured-f` in each `deftype` to run the user function inside
`with-allocation-tracing` and stash the summary in a mutable field or
dynamic var.

**Pros**: keeps allocation tracking out of the collector pipeline.

**Cons**: `measured-f` must return `[elapsed-time expr-value]` — there is
no channel for allocation data, so it must be smuggled out of band. It
bypasses the metric/analysis machinery, duplicates aggregation logic in
two places, and does not compose with metrics-defs. Rejected.

### Option C — Totals helper over `samples-map` (complement, not alternative)

Regardless of A or B, provide a function that reads `sampler/samples-map`
and returns totals:

- instrument-fn: sum the allocation arrays in `:metric->values`.
- sample-fn: `(* (t-digest/mean d) (t-digest/sample-count d))` per
  allocation digest.

This is the explicit "aggregate its data" surface the user asked for and
pairs with Option A.

## Decision

For **precise garbage measurement**, adopt a **dedicated
allocation-aware invocation path** (direct-arity call, primitive timing,
summary built after `stop!` — see Measurement Precision and the
Implementation Sketch), complemented by **Option C** (a totals helper)
as the user-facing aggregation. The Option A collector stage remains
available as a convenience for non-precise use where its ≈208 B/invoke
in-window offset is acceptable. Option B is rejected.

## Design Detail

### Invocation path and trace-window scope

The trace window must enclose **only the bare `original-fn` call**, via
direct-arity dispatch (no `apply`), with primitive `jvm/timestamp`
readings and all tuple/summary boxing performed after
`allocation-tracing-stop!`. This is the precision invariant established
empirically: harness allocation inside the window is attributed to the
wrapped code. A generic collector stage that brackets `measured/invoke`
cannot satisfy this because `measured/invoke` allocates inside the
window.

### Thread filtering

The agent records allocations from *all* threads. The invocation thread
id is captured in the sample-m and used by the xform to filter via
`allocation-on-thread?`. For these wrappers the xform runs synchronously
on the invoking thread, but capturing the id explicitly keeps the stage
correct regardless of where the transform executes.

### Agent-absent behaviour

When the agent is not attached, `with-allocation-tracing` yields `nil`
records. The xform must emit zeros or `:criterium/not-collected` rather
than throwing, mirroring how the pipeline already tolerates optional
data. Tests must gate the live path on `agent/attached?`.

### Aggregation semantics

- **instrument-fn**: exact totals — the raw per-invocation values are
  retained.
- **sample-fn**: approximate totals — t-digest preserves `mean` and
  `sample-count`, so the sum is `mean × count`. If exact totals are
  required for sampled-fn, add a running-sum accumulator alongside the
  digest for the allocation paths, or direct users to `instrument-fn`.

## Measurement Precision: Empirical Findings

The purpose of `with-allocation-tracing` here is to **precisely measure
the garbage generation of the wrapped code**. Because the agent records
actual allocations *and* frees inside the trace window, any object the
*harness* allocates inside that window is falsely attributed to the
wrapped code as measurement-specific memory pressure. Precision
therefore depends entirely on keeping harness allocation out of the
window.

The behaviour was measured directly (native agent attached, records
filtered to the calling thread; OpenJDK 25). Bytes are `:object_size`
totals on the calling thread:

| traced body                                   | records | bytes | source                                   |
|-----------------------------------------------|--------:|------:|------------------------------------------|
| `nil`                                         |       0 |     0 | harness floor                            |
| `(jvm/timestamp)` (returns a `long`)          |       0 |     0 | primitive timestamps are free            |
| `(do ts ts nil)`                              |       0 |     0 | timing reads cost nothing                |
| `(f 3 4)` direct call                         |       0 |     0 | direct arity dispatch is clean           |
| `(apply f args)`                              |       3 |   120 | 3× `PersistentVector$ChunkedSeq`         |
| `[elapsed result]` tuple                      |       2 |    64 | `Object[]` + `PersistentVector`          |
| `measured-f` shape (timed `apply` + tuple)    |       6 |   208 | 120 (apply) + boxed `Long` + 64 (tuple)  |

Key conclusions:

1. **The `with-allocation-tracing` harness floor is genuinely zero.**
   `attached?`, `allocation-tracing-start!`, `allocation-tracing-stop!`,
   the macro's `let` bindings, and primitive `jvm/timestamp` reads
   allocate nothing inside the window. The macro itself injects no
   measurement-specific memory pressure.

2. **The `records` container is filled outside the window**, so its
   choice (vector / list / transient) is irrelevant to *precision*.
   `data-fn`'s `swap! records conj …` runs only in response to
   `:report-allocation-tracing`, which is sent *after*
   `allocation-tracing-stop!`. Confirmed: a `nil` body yields 0 in-window
   records. Container choice affects only the harness's own
   *post-measurement* GC pressure (where, measured separately at 100k
   elements: persistent vector ≈ 130 B/elem, list = 40 B/elem, transient
   vector ≈ 5.5 B/elem).

3. **The real contamination is measurement plumbing executed inside the
   window** — specifically the `measured-f` path used today by both
   wrappers: `(apply original-fn args)` (≈120 B of `ChunkedSeq`) plus
   boxing the elapsed `Long` and the `[elapsed result]` tuple (≈88 B).
   That is ≈208 B per invocation that a naive stage wrapping
   `measured/invoke` would mis-attribute to the wrapped code.

These measurements supersede the earlier assumption that the records
container or the pipeline's general garbage-free invariant were the
relevant concerns. The relevant requirement is narrower and achievable:
**zero harness allocation inside the trace window.**

### Precision requirement for the allocation path

To measure the wrapped code's garbage generation without contamination,
the trace window must enclose **only the bare function call**, and all
boxing must happen outside it:

- **Do not wrap `measured/invoke`** — its `apply` and tuple are in-window
  garbage.
- **Invoke `original-fn` by direct arity** instead of `apply args`,
  removing the ≈120 B `ChunkedSeq` cost. The args still arrive as a
  vector (built in the deftype invoke method, *before* the window, so it
  does not contaminate); the fix is to **unroll the `apply`** inside
  `measured-f` by dispatching on the arg count to an arity-explicit
  `.invoke`. Measured: this drops the in-window cost from 3 records /
  120 B to **0 records / 0 B**. `case` dispatch, `.nth` on a
  `PersistentVector`, and arity-explicit `.invoke` are all
  allocation-free. Sketch:

  ```clojure
  (defn- invoke-unrolled [^clojure.lang.IFn f ^clojure.lang.IPersistentVector args]
    (case (.count args)
      0  (.invoke f)
      1  (.invoke f (.nth args 0))
      2  (.invoke f (.nth args 0) (.nth args 1))
      ;; ... through arity 20, mirroring the deftype invoke arities ...
      (.applyTo f (seq args))))  ; fallback for >20 args
  ```

  (A small macro can generate the 0..20 cases.)
- **Take timing with primitive `jvm/timestamp`** (free), and **construct
  the `[elapsed result]` tuple and the allocation summary after
  `stop!`**, outside the window.

Clean per-call shape:

```clojure
(let [start            (jvm/timestamp)
      [records result] (agent/with-allocation-tracing (original-fn a1 a2)) ; direct, no apply
      finish           (jvm/timestamp)]
  ;; box/tuple/summary all out here, outside the traced region
  ...)
```

This implies a **dedicated allocation-aware invocation path** rather than
reusing the generic collector terminal (which routes through
`measured/invoke` and therefore allocates inside the window). See the
revised Implementation Sketch.

## Risks and Trade-offs

1. **In-window harness allocation must be zero (precision invariant).**
   The danger is not the `records` container or the pipeline's general
   garbage-free design, but any harness object allocated *inside* the
   trace window, which the agent attributes to the wrapped code. The
   measured offenders are `apply` (`ChunkedSeq`) and the elapsed-time
   `Long`/tuple boxing in `measured-f`. The allocation path must use
   direct-arity invocation and construct all boxed/tuple/summary values
   after `stop!`. A stage that simply wraps `measured/invoke` would add
   ≈208 B/invocation of measurement-specific memory pressure and is
   therefore not acceptable for precise garbage measurement.

2. **Overhead.** Allocation tracing has high per-call cost. Acceptable
   for instrumentation (`eval-count = 1` per invoke) but not for timing
   benchmarks.

3. **Freed-byte semantics depend on GC timing.** An object counts as
   `:freed` only if GC ran during the trace window. For a single fast
   invocation `freed-bytes` is usually ~0; "total free" becomes
   meaningful only aggregated across a window in which GC actually fired.
   Consumers must interpret the freed totals with this in mind.

4. **Sampled-fn approximation.** As above, sampled-fn totals are
   mean × count unless a running sum is added.

## Implementation Sketch

The empirical findings rule out the naive "stage wrapping
`measured/invoke`" approach for the precise-garbage path, because
`measured/invoke` allocates inside the trace window. Two compatible
routes:

1. **Dedicated allocation-aware invocation path (precise).** Add a
   measured variant (or a small invocation helper) whose function calls
   `original-fn` by **direct arity**, wraps *only that call* in
   `agent/with-allocation-tracing`, reads primitive `jvm/timestamp`s, and
   builds the `[elapsed result]` tuple plus the `allocations-summary`
   **after** `stop!`. This is what `instrument-fn`/`sample-fn` use when
   precise garbage generation is required.

2. **Collector stage (convenient, contaminated).** A `SampleStage` that
   wraps `invoke-next` in `agent/with-allocation-tracing` is simple and
   composes with the metric/analysis machinery, but adds ≈208 B/invoke of
   in-window harness garbage (it routes through `measured/invoke`). Only
   acceptable where that fixed offset is tolerable / can be subtracted,
   not for precise measurement.

Supporting work, common to both:

3. `criterium.collector.metrics`: add the `:allocation` metric def with
   `:quantitative` value paths (`[:allocation :allocated-bytes]`,
   `[:allocation :freed-bytes]`, optionally the counts).
4. Totals helper (Option C) on top of `sampler/samples-map`, for both
   wrappers.
5. Tests mirroring `instrument_fn_test.clj` and
   `array_allocation_test.clj`, gated on `agent/attached?`. Include a
   regression test asserting **zero in-window harness allocation** for a
   non-allocating wrapped fn (records filtered to the calling thread).

## Effort

Moderate, low risk. The two `deftype`s are unchanged; they inherit the
capability through `collector-config`. The bulk of the work is the
single new stage, its metric metadata, the totals helper, and
agent-gated tests.

## Open Questions

- Should object counts (`:num-allocated`, `:num-freed`) be exposed as
  metrics, or only the byte totals?
- For sample-fn, is the t-digest `mean × count` approximation acceptable,
  or do we add an exact running-sum accumulator for allocation paths?
- Should the new stage be added to any default `collector-config`, or
  remain strictly opt-in given its overhead? (Recommended: opt-in.)
