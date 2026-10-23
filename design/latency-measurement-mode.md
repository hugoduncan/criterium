# Latency Measurement Mode for `measured`

Architecture description for a loop-carried-dependency measurement mode in
the Criterium measurement harness, following the ramora model
(`Purpose + Context -> Architecture -> Embodiment -> Observed qualities`).

Scope: `criterium.measured` and `criterium.measured.impl` code generation.
A sibling of the throughput path described in
`design/opaque-input-barrier.md`; this document does not change that path.

Predecessor: the opaque-input barrier (P-001, D-1..D-5). That design makes
a cheap pure expression report its real *throughput* cost. This design adds
the complementary *latency* measurement. The need was recorded as explicitly
out of scope in `design/opaque-input-barrier.md` ("a loop-carried-dependency
measurement mode (latency rather than throughput) ... is a separate,
unscoped concern") and in the task-002 design.

Related: `mementum/knowledge/jit-anti-hoisting-input-barrier.md`,
`mementum/memories/reference-depth-oracle-needs-compute.md`.

---

## 1. Purpose

- `Mission`: measure the cost of a Clojure expression accurately.
- `Capability` (new): measure the **latency** of an expression — the time of
  one evaluation on its dependency critical path — in addition to the
  existing throughput measurement.
- `Outcome`: a reported time that answers "how long does one call take",
  not "how many calls fit in a unit of time". The two differ whenever the
  subject is cheap enough for the superscalar core to overlap independent
  iterations.
- `SuccessCriterion`: a subject with a known critical path reports a per-call
  time that (a) is at least its throughput time, (b) is not sub-cycle for a
  subject whose single-evaluation latency exceeds one cycle, and (c) scales
  with the length of a dependency chain placed on the critical path.
- `Priority`: it must be unambiguous which quantity — latency or throughput —
  a given reading represents. Reporting the wrong quantity silently is the
  failure this capability removes.

```text
id: P-002
status: intended
subject: measured latency mode
predicate: supports
object: capability "measure the critical-path latency of an expression"
scope: criterium.measured public surface
basis: task-002 follow-up; design/opaque-input-barrier.md out-of-scope note
confidence: high
```

Relationship to P-001:

```text
id: P-003
status: inferred
subject: latency measurement (P-002)
predicate: serves
object: outcome "report the measured quantity the user asked for"
scope: criterium.measured
basis: throughput (P-001) and latency are distinct quantities; a cheap subject's throughput is not its latency
confidence: high
```

---

## 2. Context

### Operating environment

- `OperatingEnvironment`: HotSpot C2 JIT on an out-of-order, superscalar
  core. The core executes several independent instructions per cycle and
  keeps many iterations of a dependency-free loop in flight at once
  (instruction-level parallelism, ILP). HotSpot also unrolls counted loops.
- Measured this session on JDK 25, arm64 (Apple M5 Max).

### How the throughput path produces sub-cycle readings

The opaque-input barrier (P-001) deliberately makes every loop iteration
**independent**: each reads its argument opaquely from a cell and the result
is consumed; no value flows from one iteration to the next. Independence is
exactly what lets the core overlap iterations, so a cheap subject reports a
sub-cycle *throughput*. This is correct for throughput and is not a defect of
the barrier.

Observed this session (single thread, `main`):

- `(bench 1)` / `(measured/expr 1)` reports ~0.073 ns/call under the compiler
  blackhole — the empty-loop/consume floor, sub-cycle at any plausible core
  frequency. `1` is a hoisted constant with no cell, so the loop body is just
  the counter plus a no-op `consume`; the number is the harness floor, not a
  per-call cost of evaluating `1`.
- A genuine cheap op, `(unchecked-inc ^long x)`, reports the *same* ~0.074 ns
  floor — its throughput is hidden under loop overhead and ILP overlap.

### Constraints

- `Constraint`: `measured/expr` loop iterations are independent by
  construction (the opaque read yields the same stored value every
  iteration, and nothing carries forward). ILP overlap across iterations is
  then inherent to the hardware — physics, not a harness bug. A latency
  reading therefore cannot be recovered by analysis of a throughput run; the
  loop shape itself must change.
- `Constraint`: a Clojure `loop` accumulator must be primitively typed
  (`long`/`int`/`double`/`float`) or every iteration boxes and allocates,
  which would dominate and invalidate a sub-ns measurement.
- `Constraint` (endomorphism): feeding a result back as the next input
  requires the result type to map into the argument type. Self-chaining works
  directly only for endomorphic subjects (`Math/sqrt`, `+`, `*`, `inc`,
  pointer walk `(.next node)`); other subjects need an explicit mixing
  function that maps the result back into the argument domain.
- `Constraint`: some subjects admit no sensible feedback — `void` returns,
  `boolean`/comparison results, non-endomorphic reference returns — and
  cannot be auto-chained.
- `Constraint` (nuance): a memory-fencing output sink (a volatile store, as in
  the runtime blackhole) *reduces* cross-iteration overlap but does not convert
  a throughput reading into a latency. Observed this session: the same empty
  loop floors at ~0.073 ns under the no-memory-effect compiler blackhole and
  higher under the fencing runtime blackhole, yet neither is the critical-path
  latency. Only a loop-carried dependency yields latency (see "Related work:
  JMH").

### Failure mode being addressed

- `FailureMode`: a throughput reading read as a latency. A sub-cycle number
  for a cheap subject invites the interpretation "one call costs 0.07 ns",
  which is false on the critical path; the true single-evaluation latency can
  be an order of magnitude larger.

```text
id: C-101
status: observed
subject: measured/expr throughput loop
predicate: exposedTo
object: cross-iteration ILP overlap (independent iterations pipelined)
scope: HotSpot C2, out-of-order core
basis: this session — (bench 1) and (unchecked-inc) both pinned to the ~0.073 ns empty-loop floor under the compiler blackhole
confidence: high
```

```text
id: C-102
status: observed
subject: a primitively-untyped loop accumulator
predicate: constrainedBy
object: per-iteration boxing/allocation
scope: Clojure loop/recur codegen
basis: Clojure numeric boxing rules
confidence: high
```

```text
id: C-103
status: inferred
subject: non-endomorphic / void / boolean subjects
predicate: constrainedBy
object: absence of a result-to-argument feedback
scope: latency-mode codegen
basis: a loop-carried dependency requires the result to become the next input
confidence: high
```

### Related work: JMH

JMH (`openjdk/jmh`) is the reference JVM microbenchmark harness. It does **not**
offer latency and throughput as distinct measurement modes. Its four
`BenchmarkMode` values (`jmh-core/.../annotations/Mode.java`) are all
throughput-family — they differ only in how whole-invocation times are counted
and aggregated, not in whether operations form a serial critical path:

| JMH mode                | meaning                                   | latency? |
|-------------------------|-------------------------------------------|----------|
| `Throughput` (`thrpt`)  | operations per unit time                  | no       |
| `AverageTime` (`avgt`)  | time per op — its Javadoc: "the inverse of `Throughput`, but with different aggregation policy" | no (reciprocal of throughput) |
| `SampleTime` (`sample`) | samples individual whole-invocation times | no (per-invocation wall time; body still overlaps) |
| `SingleShotTime` (`ss`) | one invocation, no warmup ("cold")        | no (cold-start, not pipeline isolation) |

The generated measurement loop confirms independence across invocations
(`BenchmarkGenerator.emitCall` plus the per-mode `do { … } while(!control.isDone)`):

```java
do {
    blackhole.consume( bench.method(args) );   // result consumed…
    operations++;
} while (!control.isDone);                      // …never fed into the next call
```

`args` come from `@State` fields (identical every iteration) and each result is
`consume`d, not threaded forward, so consecutive invocations are independent —
throughput, overlappable by the core. `avgt` merely reports its reciprocal.

JMH documents the *same* sub-cycle phenomenon this design addresses, and its
remedy is a sink, not a latency mode:

- `JMHSample_11_Loops`: with in-method loops the "perceived" cost falls to
  "1/20 ns, well beyond what hardware can actually do … the loop is heavily
  unrolled/pipelined" — the `(bench 1)` finding verbatim.
- `JMHSample_34_SafeLooping`: the prescription is to sink each result to a
  `Blackhole`, and JMH notes the defense is only the sink's memory effects:
  "we would normally like to care about several concurrent `work()`
  computations at once, but the memory effects from `Blackhole.consume()`
  prevent those optimizations on most runtimes." This is a partial,
  runtime-dependent overlap brake (the C-101 nuance), not a latency.

Consequences for this design:

- The latency capability (P-002) is a genuine gap: the reference tool does not
  provide it. Automating the loop-carried dependency for endomorphic primitives
  (D-2, D-6) is the novelty; JMH leaves the dependency entirely to the author.
- The "author supplies the data dependency" practice is the established way to
  measure latency in JMH (hand-built `x = f(x)` chains, pointer walks). This
  precedent backs D-2's explicit mixing-function surface for non-endomorphic
  subjects.
- JMH's newer compiler/intrinsic blackhole has no memory effects, so even JMH
  can overlap independent invocations on a modern JVM — reinforcing that a sink
  cannot substitute for a loop-carried dependency when latency is the goal.

```text
id: C-104
status: observed
subject: JMH benchmark modes and generated loop
predicate: exposedTo
object: no latency mode — all modes measure independent-invocation throughput, author must hand-build any dependency chain
scope: openjdk/jmh (Mode.java; BenchmarkGenerator.emitCall; samples 11, 34)
basis: source inspection this session
confidence: high
```

---

## 3. Mechanism

Latency and throughput differ by exactly one structural property: a
**loop-carried dependency**. The throughput loop keeps iterations
independent; the latency loop threads each result into the next input so the
critical path is serial and the core cannot overlap iterations.

```clojure
;; throughput (today, P-001): iterations independent, overlappable
(loop [i n]
  (when (pos? i)
    (blackhole/consume (f (opaque-read cell)))
    (recur (unchecked-dec i))))

;; latency (this design): acc carries the critical path, no overlap possible
(loop [i   n
       acc seed]          ; seed is opaque; acc is primitively typed
  (if (pos? i)
    (recur (unchecked-dec i) (f acc))   ; acc_{n+1} = f(acc_n)
    (blackhole/consume acc)))           ; one consume keeps the chain live
```

In the latency loop the next `(f acc)` cannot start until the previous one
retires, so the reported time per iteration is the critical-path latency of
`f`, multiplied by the (runtime, unknown) count so the JIT cannot precompute
it. No per-iteration opaque read is needed on the carried value: the
dependency itself defeats folding and hoisting. The seed is read opaquely
once, and a single `consume` after the loop prevents dead-code elimination of
the whole chain at zero per-iteration cost.

The within-iteration depth-scaling already observed for the throughput path
(`Math/sqrt` depth 1 vs depth 3) is a *miniature* latency chain inside one
iteration; this design promotes that idea to the loop itself.

---

## 4. Decisions

Decision form: `d = <subject, chosen, rationale, affected qualities,
assumptions, scope, commitment>`.

### D-1 Add a loop-carried-dependency latency mode

- subject: `measured` code generation.
- chosen: add a new generated measurement-function variant that threads each
  result into the next iteration's input via a primitively-typed accumulator,
  forming a serial critical path. Keep the existing throughput variant
  unchanged and default.
- rejects:
  - deriving latency from a throughput run (impossible — ILP overlap is a
    hardware property of the independent loop, C-101);
  - a single loop that serves both modes (the independence that throughput
    needs is exactly what latency must remove);
  - forcing latency by disabling ILP globally (no portable knob exists).
- rationale: latency and throughput are different quantities; only a
  loop-carried dependency yields the critical-path number. A separate codegen
  variant keeps each path unconditional and simple.
- affected qualities: accuracy (adds a quantity that was previously
  unreachable), simplicity (small cost — a second codegen path), applicability
  (latency mode covers fewer subjects than throughput — see D-2).
- assumptions: A-101, A-102.
- scope: the expr path of `measured`.
- commitment: firm in intent, pending task review.

```text
id: D-101
status: intended
subject: measured latency codegen
predicate: chooses
object: loop-carried-dependency accumulator loop
scope: criterium.measured.impl
basis: this session — latency 3.27 ns vs throughput 0.57 ns for the same Math/sqrt subject (section 8)
confidence: high
```

### D-2 Require a feedback (injection) function; auto for endomorphic primitives

- subject: how the result becomes the next input.
- chosen: for endomorphic primitive subjects (result type maps to argument
  type — `Math/sqrt`, `+`, `*`, `inc`, and similar), derive the feedback
  automatically (the result *is* the next `acc`, possibly through a cheap
  fixed coercion that avoids a fixpoint, e.g. `+ 1.0`). For every other
  subject, require an explicit, user-supplied mixing function mapping the
  result back into the argument domain.
- rejects: silently fabricating a feedback for non-endomorphic subjects
  (would measure the fabricated glue, not the subject); refusing all
  non-endomorphic subjects (too restrictive — a supplied mixing function
  makes them measurable).
- rationale: the feedback is on the critical path and is therefore part of
  the measurement; it must be either trivial-and-derivable or explicit so the
  user knows it is included. This mirrors JMH, where the author supplies the
  data dependency.
- affected qualities: accuracy (the included feedback cost is explicit, not
  hidden), applicability (explicit mixing extends coverage), simplicity (a
  new surface concept — the feedback function).
- assumptions: A-103.
- scope: latency codegen and its public surface.
- commitment: firm for the endomorphic-primitive subset; the explicit
  mixing-function surface is a follow-up (D-6).

### D-3 Primitive accumulator, opaque seed, end-of-loop consume

- subject: the carried state and its liveness.
- chosen: carry the critical path in a primitively-typed `loop` binding whose
  type matches the subject (reusing the arg-type/tag machinery of the
  throughput barrier, D-3 of `opaque-input-barrier.md`). Read the seed
  opaquely once before the loop; `consume` the final accumulator once after
  the loop.
- rejects: a boxed accumulator (per-iteration allocation, C-102); a constant
  seed with no opaque read (lets the JIT precompute a short chain); consuming
  the accumulator every iteration (adds the sink cost to the critical path).
- rationale: a primitive accumulator keeps the chain allocation-free; the
  opaque seed keeps the chain start unknown so a finite chain is not
  precomputed; one terminal `consume` prevents dead-code elimination at zero
  per-iteration cost.
- affected qualities: accuracy (no boxing noise), per-call overhead (feedback
  only, no sink per iteration), simplicity (reuses existing typed-cell
  machinery).
- scope: latency codegen.
- commitment: firm.

```text
id: D-103
status: intended
subject: latency accumulator
predicate: chooses
object: primitively-typed loop binding, opaque seed, single terminal consume
scope: criterium.measured.impl
basis: C-102 (boxing), reuse of opaque-input-barrier.md D-3 typing
confidence: high
```

### D-4 No per-iteration opaque barrier on the carried value

- subject: the relationship to the throughput barrier.
- chosen: do not apply the opaque read-barrier to the carried argument — the
  loop-carried dependency already defeats folding and hoisting because the
  value genuinely changes each iteration and the count is runtime-unknown.
  Any *other* (non-carried) arguments remain loop-invariant and keep the full
  throughput barrier (opaque cell + one-time escape, P-001 D-1/D-5).
- rejects: applying the opaque barrier to the carried value too (redundant
  per-iteration cost on the critical path, inflating the latency reading).
- rationale: the barrier exists to make a loop-invariant input look unknown;
  a carried value is not loop-invariant, so the barrier adds cost without
  changing the optimiser's conclusion.
- affected qualities: accuracy (no spurious barrier cost on the critical
  path), simplicity (the two barriers compose cleanly — invariant args
  barriered, carried arg not).
- scope: latency codegen.
- commitment: firm.

### D-5 Expose as a distinct, explicitly named mode; report latency un-subtracted

- subject: the public surface and the reported number's meaning.
- chosen: expose latency as a distinct, explicitly named entry point — a
  `measured/latency-expr` macro (or an explicit `:mode :latency` on `bench`),
  never as a silent change to the default. Report the critical-path latency as
  measured, including the feedback-op cost, without subtracting a baseline
  (consistent with the throughput path's D-4).
- rejects: making latency the default (would change every existing reading);
  a boolean flag that reads as a performance knob (does not convey the "which
  quantity" intent); subtracting the feedback cost (a noisy baseline of the
  same order, which can drive a result negative — the #19 lesson from the
  throughput design).
- rationale: latency and throughput answer different questions; the surface
  must name which one. D-1's "no knob" principle in `opaque-input-barrier.md`
  concerned the throughput *barrier*, not the choice of measured quantity; a
  second, clearly named mode is the right shape.
- affected qualities: interpretability (the name carries the quantity),
  accuracy (honest, non-subtracted), simplicity (one more named entry point).
- scope: `criterium.measured` and `criterium.bench` public surface,
  docstrings.
- commitment: firm.

```text
id: D-105
status: intended
subject: latency measurement surface
predicate: chooses
object: distinct named entry point (measured/latency-expr or bench :mode :latency), latency reported un-subtracted
scope: criterium.measured public surface
basis: parallels opaque-input-barrier.md D-4 (honest, non-subtracted) and the named-escape-hatch note in D-1
confidence: high
```

### D-6 Minimal viable scope first

- subject: initial coverage.
- chosen: ship first for self-chaining primitive endomorphic subjects
  (double/long/float/int arithmetic, `Math/*`), with a typed primitive
  accumulator, opaque seed, terminal consume, and a latency-vs-throughput
  oracle test. Defer the explicit mixing-function surface, reference /
  pointer-chasing latency, and multi-carried-argument forms to follow-ups.
- rejects: attempting the full general mixing-function design in one step.
- rationale: the endomorphic-primitive subset is where the mechanism is
  cleanest and the regression oracle is strongest (section 8), and it
  delivers the capability for the common arithmetic subjects without the
  surface cost of a general injection API.
- affected qualities: simplicity (small first step), applicability (partial
  first, extended later).
- scope: first task slice.
- commitment: firm for the slice boundary.

---

## 5. Principles and patterns

- `Principle`: measure the quantity you name. A benchmark that does not
  declare latency vs throughput invites the sub-cycle misreading (C-101,
  FailureMode).
- `Principle`: a latency measurement needs a critical path; create it with a
  loop-carried dependency, not by trying to suppress the hardware's overlap.
- `Principle`: the feedback is part of the measurement. Keep it trivial and
  derivable, or make it explicit — never hidden.
- `Pattern`: reuse the typed-cell/tag machinery of the throughput barrier for
  accumulator typing; do not duplicate it.
- `Pattern`: source/sink separation (from the throughput design) still holds —
  the opaque seed is the source, the terminal `consume` the sink; the
  loop-carried dependency is a third, orthogonal concern on the critical path.

---

## 6. Tradeoffs

```text
tradesOff(latency-measurability, applicability)
```

A loop-carried dependency is only expressible for endomorphic subjects or
subjects given a mixing function. Latency mode therefore covers fewer subjects
than throughput mode. We accept this: latency is undefined for a `void` or
boolean subject without a declared critical path, so refusing it there is
honest.

```text
tradesOff(accuracy, simplicity)
```

Including the feedback-op cost on the critical path makes the number slightly
larger than the subject's intrinsic latency, but keeps the mechanism simple
and the cost explicit (D-2, D-5). Subtracting it would reintroduce the noisy-
baseline problem the throughput design rejected (#19).

```text
tradesOff(capability-coverage, surface-area)
```

A second named mode (D-5) adds a public concept. We accept it because the
alternative — overloading one surface to mean two quantities — is the exact
ambiguity this design exists to remove.

- `Assumption` A-101: a primitively-typed loop-carried accumulator defeats
  cross-iteration overlap, so the reported per-iteration time is the subject's
  critical-path latency. Status: observed this session for a `Math/sqrt`
  chain (latency 3.27 ns vs throughput 0.57 ns, ~5.8x; section 8). Each new
  subject class should be spot-checked.
- `Assumption` A-102: HotSpot does not speculatively break the carried
  dependency (e.g. value-speculate the accumulator to a constant). Status:
  assumed; not observed to occur for FP chains this session (the chain does
  not collapse to a constant time).
- `Assumption` A-103: a cheap derived feedback coercion (e.g. `+ 1.0` to avoid
  a fixpoint) adds a small, constant, explainable cost to the critical path.
  Status: assumed; to be measured per subject class.

---

## 7. Embodiment

### Elements

- `Component` `criterium.measured.impl` latency codegen (new function, sibling
  to `measured-expr-fn`) — generates the accumulator loop: an opaque seed
  read, a primitively-typed `loop` binding carrying the critical path, the
  subject applied as `(f acc)` (via the derived or supplied feedback), and one
  terminal `consume`. Non-carried arguments are emitted through the existing
  throughput barrier.
- `Abstraction` "accumulator" — the primitively-typed carried value that
  `represents` the dependency critical path across iterations. `embodiedBy` a
  `loop` binding.
- `Abstraction` "feedback function" — the mapping from a result to the next
  input. `represents` the data dependency the user declares (or that is
  derived for endomorphic primitives).

### Interfaces

- `Interface` `measured/latency-expr` (or `bench :mode :latency`) — new named
  entry point selecting latency codegen. Provides the "which quantity" choice.
- `Interface` feedback-function contract — `result -> next-arg`, same
  primitive type; derived for endomorphic subjects, explicit otherwise.

### State

- `State` the accumulator. `owns`: the generated measurement function.
  `mayMutate`: each iteration, as the carried `recur` value (this is the
  measured work). `mayRead`: each iteration, as the input to `f`.
  Seeded once from an opaque read; consumed once after the loop. Lifetime: one
  measurement invocation.

### Invariants

- `Invariant` L-1: a loop-carried dependency is present — each iteration's
  input is the previous iteration's result (possibly through the feedback
  function). Enforcement point: code generation. Rationale: without it the
  loop is a throughput loop and the number is not a latency (C-101).
- `Invariant` L-2: no boxing on the carried path — the accumulator is
  primitively typed. Enforcement point: typed `loop` binding (reusing the
  throughput barrier's tag machinery); verified by absence of boxed-math
  warnings. Rationale: C-102.
- `Invariant` L-3: the seed is read opaquely once and the accumulator is
  consumed once after the loop. Enforcement point: code generation. Rationale:
  keep a finite chain from being precomputed, and keep the chain from being
  dead-code-eliminated, at zero per-iteration cost.
- `Invariant` L-4: exactly one argument is carried; all other arguments are
  loop-invariant and keep the throughput barrier (opaque cell + one-time
  escape, P-001). Enforcement point: code generation. Rationale: D-4.

### Boundary

- `Boundary` latency path vs throughput path. `separates` the carried-
  dependency codegen from the independent-iteration codegen. `permits` sharing
  the typed-cell/tag machinery. `forbids` applying the per-iteration opaque
  read-barrier to the carried value (L-4, D-4).

```text
id: E-101
status: intended
subject: latency codegen function
predicate: realizedBy
object: opaque-seed + primitive accumulator loop + terminal consume
scope: criterium.measured.impl
basis: D-101, D-103
confidence: high
```

```text
id: E-102
status: intended
subject: latency accumulator
predicate: enforces
object: invariant L-1 (loop-carried dependency present)
scope: generated measurement function
basis: C-101 (independent loop is throughput, not latency)
confidence: high
```

```text
id: E-103
status: intended
subject: latency accumulator
predicate: enforces
object: invariant L-2 (primitive, no boxing on the carried path)
scope: generated measurement function
basis: C-102
confidence: high
```

---

## 8. Observed qualities

Metric: per-op time, min of 7 trials; hand-rolled latency vs throughput loops
over the same `Math/sqrt` subject; JDK 25, arm64 (Apple M5 Max), single
thread. The latency/throughput comparison ran under the runtime blackhole in
the session REPL; the `(bench 1)` floor ran under the compiler blackhole.
These are *prototype* measurements that motivate the decisions; they are not
from a shipped latency path.

### Latency vs throughput, same subject

Subject: `acc' = Math/sqrt(acc + 1.0)` (latency, carried) vs
`Math/sqrt(seed + 1.0)` consumed per iteration (throughput, independent),
5e7 iterations.

| mode                                | ns/op | approx cycles* |
|-------------------------------------|-------|----------------|
| latency (loop-carried dependency)   | 3.27  | ~15            |
| throughput (independent iterations) | 0.57  | ~2.6           |

\* ~0.22 ns/cycle at an assumed ~4.6 GHz P-core; used only for order-of-
magnitude reading, not asserted precisely.

The only structural difference is whether the result feeds the next input.
Latency is ~5.8x throughput and is firmly not sub-cycle; throughput overlaps
~6 iterations deep.

### Throughput floor (why the question arose)

Subject-independent empty-loop floor, 1e8 iterations, compiler blackhole:

| subject                     | ns/call | note                               |
|-----------------------------|---------|------------------------------------|
| `(expr 1)` constant         | 0.073   | hoisted constant, no cell          |
| `(unchecked-inc ^long x)`   | 0.074   | real op pinned to the same floor   |
| `(Math/sqrt x)` depth 1     | 0.44    | within-iteration latency chain     |
| `(Math/sqrt^3 x)` depth 3   | 1.31    | chain scales (mini latency)        |

A genuine op (`unchecked-inc`) reports the same floor as the constant — the
ILP signature (C-101). The `Math/sqrt` depth scaling is a within-iteration
critical path and is the seed of the latency-loop idea.

### Quality assertions

```text
id: Q-101
status: observed
subject: loop-carried-dependency latency loop (prototype)
predicate: improves
object: measurement of critical-path latency for a cheap pure subject
scope: JDK 25 arm64 (M5 Max), Math/sqrt chain, runtime blackhole
basis: section 8 — latency 3.27 ns vs throughput 0.57 ns (~5.8x)
confidence: high
```

```text
id: Q-102
status: observed
subject: measured/expr throughput loop for a cheap subject
predicate: violates
object: fitness condition "reported number is a per-call latency"
scope: JDK 25 arm64, (bench 1) and (unchecked-inc), compiler blackhole
basis: section 8 — ~0.073 ns sub-cycle floor; a real op pinned to the same floor
confidence: high
```

```text
id: Q-103
status: inferred
subject: latency mode restricted to endomorphic / mixing-function subjects
predicate: degrades
object: applicability (fewer subjects than throughput mode)
scope: latency-mode public surface
basis: C-103, D-2 (void/boolean/non-endomorphic subjects have no auto feedback)
confidence: high
```

```text
id: Q-104
status: inferred
subject: included feedback-op cost on the critical path
predicate: degrades
object: interpretability (latency reading includes a documented, non-subtracted feedback cost)
scope: cheap subjects where the feedback cost is a large fraction of the subject latency
basis: D-2, D-5; parallels opaque-input-barrier.md D-4 / Q-005
confidence: medium
```

### Verification expectation

```text
id: V-latency-oracle
status: intended
subject: latency-vs-throughput oracle test
predicate: verifiedBy
object: assertion "latency(chain) >> throughput(chain) for a known subject"
scope: criterium.measured tests
basis: section 8 (~5.8x gap is a ready regression oracle)
confidence: high
```

The ~5.8x latency/throughput gap for a known chain is a ready-made,
machine-independent regression oracle (a ratio, not an absolute ns
threshold), mirroring the depth-scaling oracle of the throughput design.

---

## 9. Evolution

- `Change`: add latency mode as a new, explicitly named surface
  (`measured/latency-expr` or `bench :mode :latency`). `CompatibilityRule`:
  purely additive — the default throughput path and every existing reading are
  unchanged.
- `Migration`: none required; opt-in by the new surface.
- `EvolutionExpectation`: extend from endomorphic primitives to an explicit
  mixing-function surface for non-endomorphic and reference subjects
  (`result -> next-arg`), then to pointer-chasing latency via a self-
  referential reference accumulator (the classic cache-latency benchmark).
- `EvolutionExpectation`: multi-carried-argument forms (more than one critical
  path) are a later concern; the first slice carries exactly one argument
  (L-4).
- `ErosionRisk`: a refactor that lets the JIT break the carried dependency
  (e.g. proving the accumulator a constant, or a feedback that collapses to a
  fixpoint the optimiser can fold) would silently restore cross-iteration
  overlap and turn a latency reading back into a throughput reading. Guard
  with the latency-vs-throughput oracle (V-latency-oracle): a known chain must
  measure latency >> throughput.
- `ErosionRisk`: a boxed accumulator introduced by a future change (L-2) would
  add per-iteration allocation and dominate a sub-ns reading. Guard with an
  allocation check on the carried path, as the throughput design does for its
  read path.

```text
id: V-101
status: intended
subject: latency measurement mode
predicate: changes
object: criterium.measured capability set (adds critical-path latency)
scope: criterium.measured
basis: D-101, D-105
confidence: high
```

---

## 10. Traceability summary

- Purpose P-002 (measure latency) is served by decisions D-101..D-6, and is
  distinct from P-001 (measure throughput) per P-003.
- D-101 (carried-dependency loop) is realized by E-101 (latency codegen) and
  enforced by E-102 (L-1); D-103 (primitive accumulator) by E-103 (L-2).
- Context C-101 (ILP overlap makes the throughput loop sub-cycle) is the
  reason latency cannot be derived from a throughput run and motivates D-101;
  Q-102 records the observed sub-cycle throughput floor.
- D-2 (feedback function) responds to C-103 (non-endomorphic subjects have no
  auto feedback); Q-103 records the resulting applicability limit. Context
  C-104 (JMH has no latency mode; its author hand-builds any dependency chain)
  both confirms P-002 as a real gap and backs D-2's explicit mixing-function
  surface as established practice.
- D-4/L-4 compose the latency path with the throughput barrier: carried
  argument un-barriered, invariant arguments barriered (P-001 D-1/D-5).
- D-5 keeps the reported quantity named and honest (parallels
  `opaque-input-barrier.md` D-4); Q-104 records the included, non-subtracted
  feedback cost.
- Quality Q-101 provides scoped prototype evidence (the ~5.8x gap); the same
  gap is the intended regression oracle (V-latency-oracle).
- Open follow-ups: explicit mixing-function surface, reference/pointer-chasing
  latency, multi-carried-argument forms (section 9).
