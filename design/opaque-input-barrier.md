# Opaque Input Barrier for `measured/expr`

Architecture description for an anti-hoist input barrier in the
Criterium measurement harness, following the ramora model
(`Purpose + Context -> Architecture -> Embodiment -> Observed qualities`).

Scope: `criterium.measured` and `criterium.measured.impl` code generation,
and the blackhole support in the `blackhole` base.

Basis issue: #155. Related: #17, #19, #156, #157, #158.

---

## 1. Purpose

- `Mission`: measure the cost of a Clojure expression accurately.
- `Capability` (new): measure expressions whose per-call cost is below
  about 1 nanosecond, where the JIT would otherwise remove the work.
- `Outcome`: a reported time that reflects the subject expression, not an
  optimised-away residue of it.
- `SuccessCriterion`: a cheap pure expression (for example a field read)
  reports a non-zero, repeatable per-call cost that matches an independent
  non-hoistable control, within the harness noise floor.
- `Priority`: correctness of the reported number ranks above minimum
  per-call overhead. A small, constant, measurable overhead is acceptable;
  a silently wrong number is not.

```text
id: P-001
status: intended
subject: measured/expr
predicate: supports
object: capability "measure sub-nanosecond pure expressions"
scope: criterium.measured public surface
basis: issue #155
confidence: high
```

---

## 2. Context

### Operating environment

- `OperatingEnvironment`: HotSpot C2 JIT, JDK 9 or later (`VarHandle`
  present). Compiler blackhole requires JDK 17 or later plus the
  `-XX:CompileCommand=blackhole,...` flag.
- Measured on JDK 25, arm64 (Apple M-series).

### Constraints

- `Constraint`: Clojure cannot call `VarHandle` access-mode methods
  (`getOpaque`) without hitting the `@PolymorphicSignature` varargs form,
  which boxes primitives and allocates an `Object[]` per call. Any design
  that calls `VarHandle.getOpaque` directly from Clojure reintroduces
  allocation into the measured loop.
- `Constraint`: `defrecord` fields are `final`. The JMM lets the JIT treat
  a `final` field of a loop-invariant object as a compile-time constant, so
  no memory barrier can keep a `final`-field read inside the loop.
- `Constraint` (project): no backward-compatibility obligation
  (`AGENTS.md`), but the existing non-opaque path must remain the default
  until the new path is validated.
- `Constraint`: HotSpot C2 escape analysis. A cell that never escapes the
  measurement function and is never written inside the loop has no possible
  concurrent writer. C2 then treats the opaque load value as the
  loop-invariant constant it provably is, and folds or hoists any pure
  computation built on it. Opaque access mode promises only "assume memory
  may change"; escape analysis can prove it cannot. See C-004.

### Failure mode being addressed

- `FailureMode`: loop-invariant code motion. The measured loop evaluates the
  same expression on the same captured inputs every iteration, so the JIT
  lifts the pure computation out of the loop. The reported time is the loop
  cost alone.

```text
id: C-001
status: observed
subject: measured-expr-fn generated loop
predicate: exposedTo
object: loop-invariant code motion
scope: criterium.measured.impl
basis: #155 reproduction; design/opaque-input-barrier experiments
confidence: high
```

```text
id: C-002
status: observed
subject: Clojure call site of VarHandle.getOpaque
predicate: constrainedBy
object: @PolymorphicSignature varargs boxing
scope: measured loop code generation
basis: experiment; reflection/boxing warnings when called from Clojure
confidence: high
```

```text
id: C-004
status: observed
subject: non-escaping opaque cell (thread-local, never written in loop)
predicate: exposedTo
object: escape-analysis constant folding of the opaque load value
scope: HotSpot C2, default escape analysis on
basis: post-implementation experiment; -XX:-DoEscapeAnalysis toggle and one-time cell escape both restore execution (section 8)
confidence: high
```

---

## 3. Root cause

The defect is more specific than "loop-invariant code motion". The harness
binds the arguments once, before the loop, into typed locals:

```clojure
(let [~@(mapcat binding-with-hint-or-cast arg-syms arg-metas) ...]
  (loop [i# n#]
    (when (pos? i#)
      (blackhole/consume ~expr)
      (recur (long (unchecked-dec i#))))))
```

The compiler blackhole keeps the result alive but has no memory effects, so
it cannot prevent hoisting. More importantly, when the subject reads a
`final` field of a loop-invariant object, the JIT folds the read to a
constant regardless of any barrier, because it relies on immutability, not
on "memory has not changed". A memory barrier (such as the runtime
blackhole) therefore cannot fix this case even in principle.

The only effective point of attack is the **input reference**: make the
object (or primitive) that feeds the expression come from a read the JIT
cannot prove is stable across iterations. Then it cannot prove it is the
same object, so it cannot fold the `final`-field read, and it cannot hoist
dependent work.

```text
id: C-003
status: observed
subject: final-field read of loop-invariant object
predicate: exposedTo
object: constant folding across any memory barrier
scope: HotSpot C2
basis: experiment (runtime blackhole does not keep the read in the loop)
confidence: high
```

### Refinement: opaque read necessary, not sufficient (escape analysis)

Post-implementation measurement falsified assumption A-2. Making the input
reference opaque is *necessary* but not *sufficient*. The opaque cell the
harness builds is a thread-local `Atomic*` that never escapes the measurement
function and is never written inside the loop. HotSpot C2 escape analysis
proves no other thread can reach it, hence no concurrent writer can exist,
hence the opaque load value is a provable constant — so C2 folds or hoists the
pure computation built on it, exactly the hoisting the barrier was meant to
stop. The generated code is correct (opaque read and subject both present in
the loop); C2 removes the work anyway.

The missing fact is **escape**. The input read is treated as genuinely unknown
only when C2 cannot prove the cell is thread-local. The barrier must therefore
make each cell escape (C-004, D-5, I-4). This refines, not reverses, C-003: the
point of attack is still the input, but the input must be both opaque *and*
escaped.

---

## 4. Decisions

Decision form: `d = <subject, chosen, rationale, affected qualities,
assumptions, scope, commitment>`.

### D-1 Add an opaque input barrier

- subject: `measured-expr-fn` code generation.
- chosen: read each argument through an **opaque** access
  (`VarHandle` access mode `getOpaque`) on every iteration, from a mutable
  cell initialised once before the loop. Apply this **unconditionally**, with
  an **automatic per-argument fallback** to the existing non-opaque binding
  when an argument cannot be given an opaque cell cleanly (unknown type that
  would otherwise box, array-typed tag per #156, the callable `:as` path).
- rejects:
  - runtime blackhole on the output (too costly, and cannot defeat
    `final`-field folding); compiler blackhole on the input (a `void` sink
    with no memory effects, cannot launder a value); recompute-via-args-fn
    each iteration (allocation and call overhead).
  - a user-facing tuning flag such as `:opaque-args true/false`. A
    correctness fix behind an off-by-default flag only protects users who
    already know about the defect; a flag defaulting to on adds a knob whose
    sole remaining purpose is the "as-compiled" measurement intent (below),
    which a boolean named like a performance option would not convey.
- rationale: opacity forbids the JIT from merging two reads of the same
  location, so the input reference is treated as newly unknown each
  iteration; opaque has no acquire or release fence, so it is far cheaper
  than a volatile or the runtime blackhole. The non-opaque number for a
  cheap pure expression is misleading (#155), so it must not be the default;
  the barrier cost is small, constant, and documented (D-4), so applying it
  always is cheap for subjects where it does not matter. Edge cases are
  handled locally by per-argument fallback, not by a global switch.
- affected qualities: accuracy (improves), robustness (improves — no silent
  wrong default, local fallback), per-call overhead (small, constant
  degrade), simplicity (improves — one path, no tuning knob), portability
  (requires JDK 9+).
- assumptions: A-1, A-2 below.
- scope: the expr path of `measured`; the callable path falls back to the
  existing non-opaque behaviour.
- commitment: firm. Unconditional opaque with automatic per-argument
  fallback; no tuning flag.
- note: the two measurement intents — isolated cost (opaque) and
  as-compiled cost (non-opaque, today's number) — are both legitimate, but
  the as-compiled intent is deliberately **not** exposed on the common path.
  If it is ever needed it should be a distinct, explicitly named escape
  hatch (for example an `:inputs :constant` mode or a separate
  `measured/expr-raw`) whose name carries the "give me the optimiser's
  number" intent, not a boolean that reads as a performance knob.

- amendedBy: D-5. The opaque read alone is insufficient: HotSpot escape
  analysis folds the dependent computation when the cell does not escape (see
  §3 refinement, C-004, A-2). D-5 adds the required one-time cell escape; D-1
  otherwise stands.

### D-2 Keep the barrier scalar, one cell per argument

- subject: the mutable cell.
- chosen: one scalar cell per argument.
- rejects: a shared N-element array indexed inside the loop.
- rationale: the pool/rotation concern (#158) is independent. Measurement
  shows an indexed atomic read (`AtomicReferenceArray`) costs several times a
  scalar read, so fusing the barrier with the pool taxes every
  benchmark for a feature most do not use.
- affected qualities: per-call overhead (improves), simplicity (improves).
- scope: barrier only.
- commitment: firm.

### D-3 Use `java.util.concurrent.atomic` as the mutable place

- subject: the mutable cell type.
- chosen: one atomic per argument — `AtomicReference` for references,
  `AtomicLong`/`AtomicInteger` for integral primitives, `AtomicLong` plus
  `Double` bit reinterpretation for `double`, and `AtomicInteger` plus
  `Float` bit reinterpretation for `float` (`Float/floatToRawIntBits` yields
  an `int`, so an `int`-sized cell is the natural unboxed fit, parallel to
  the `int` mapping). The small primitive tags
  (`boolean`/`byte`/`short`/`char`) also take an unboxed `AtomicInteger`:
  each widens into an `int` on store and narrows back on read
  (`byte`/`short`/`char` by `unchecked-byte`/`unchecked-short`/
  `unchecked-char`; `boolean` by `(if b 1 0)` on store and `(not (zero? x))`
  on read), so the round-trip is exact across the whole value range and the
  read path stays unboxed, parallel to the `float` bit-reinterpretation case.
- rejects:
  - a hand-written Java shim over `arrayElementVarHandle` — works and is
    equally fast, but adds a Java artifact to build and ship;
  - a `deftype` with a mutable field — Clojure offers only plain
    (`^:unsynchronized-mutable`, hoistable) or volatile
    (`^:volatile-mutable`, over-strong and costly) mutability, never
    opaque, and its mutable fields are private so an external `VarHandle`
    cannot reach them.
- rationale: `getOpaque`/`setOpaque` on the atomics are ordinary typed
  methods, not `@PolymorphicSignature`, so Clojure calls them with no
  varargs boxing and no custom Java. A controlled differential shows they
  equal the hand-written shim per read for every type (section 8) and stay
  unboxed for `long` and `double`.
- affected qualities: simplicity (improves), portability (improves, pure
  JDK), build complexity (improves, no extra artifact).
- assumptions: A-1.
- scope: barrier implementation.
- commitment: firm, pending task review.
- amendedBy: task 003. The original rationale — that
  `boolean`/`byte`/`short`/`char` "have no unboxed atomic cell" and must take
  the non-opaque fallback — was wrong: these tags widen unboxed into an
  `AtomicInteger`, exactly as `float` widens via `int` bit reinterpretation.
  Task 003 corrects the rationale (above), reclassifies the four tags as
  opaque-eligible, and moves them onto the opaque path. After this amendment
  the non-opaque fallback holds only array-typed tags (pending #156), not
  "small primitives and arrays".

```text
id: D-003
status: intended
subject: opaque input barrier cell
predicate: chooses
object: java.util.concurrent.atomic Atomic* with getOpaque
scope: criterium.measured.impl code generation
basis: experiments in this document
confidence: high
```

### D-4 Treat the barrier cost as a documented measurement artifact, not a silent correction

- subject: reported per-call time and its interpretation.
- chosen: expose and document the per-argument opaque-read cost as a known
  measurement artifact. Rely on the existing platform baseline facility
  (`criterium.platform/exec-main`, the `:platform` exec, which already
  measures `latency`, `granularity`, `constant-long/double/object/nil`) to
  let a user measure the barrier overhead, and document that a reported time
  for a sub-barrier subject includes the barrier.
- rejects: dynamically computing the barrier baseline per run and
  subtracting it from the result. The baseline is noisy; subtracting a noisy
  estimate from a result of the same order produces negative effective
  measurements (see #19), which are worse than an honestly inflated but
  positive number.
- rationale: the barrier adds roughly 0.07 ns per primitive argument and
  ~0.11 ns per reference argument per iteration (section 8 differential),
  and that figure itself varies ~25-30% between JVM runs. For very cheap
  subjects this is a large fraction of the reading, but it is an explainable
  component. Making it visible and documented preserves trust; silently
  subtracting a per-run-noisy estimate trades a visible artifact for an
  invisible instability and can produce negative results (#19).
- affected qualities: accuracy (improves via honesty, not via correction),
  robustness (improves — no negative results), interpretability (improves
  via documentation).
- scope: documentation, the platform baseline facility, and the
  `measured/expr` docstring.
- commitment: firm. The barrier cost is reported as measured; it is not
  subtracted.

### D-5 Make each opaque cell escape once, at setup

- subject: the opaque cell lifetime and escape state.
- chosen: force each cell to escape exactly once, before timing starts, so
  HotSpot escape analysis cannot prove it thread-local. Concretely, publish
  the cell reference once through the active blackhole (`blackhole/consume` of
  the cell as an `Object`) after the init store and before `start#`, or hold
  the cells in a retained object the harness keeps live (the JMH `@State`
  pattern). One escape point per cell is enough; the loop body is unchanged.
- rejects:
  - relying on the opaque read alone (the as-built D-1 barrier): escape
    analysis folds the dependent computation for non-escaping cells (C-004,
    §3 refinement).
  - disabling escape analysis globally (`-XX:-DoEscapeAnalysis`): a coarse,
    whole-JVM flag that pessimises all user code and is not portable to the
    common invocation path.
  - blackholing the loaded *value* each iteration: that blocks dead-code
    elimination of the load, not constant propagation of its value, so the
    computation still folds. Only escaping the cell *object* changes the EA
    conclusion.
- rationale: escape analysis defeats opaque semantics by proving the cell
  cannot be written concurrently. A single escape point removes that proof, so
  the opaque load becomes genuinely unknown and the dependent work stays in the
  loop. Confirmed flavor-independent: the compiler blackhole marks reference
  arguments as globally escaping (intrinsic), and the runtime blackhole
  publishes the reference through a volatile field; both restore execution
  (section 8). The one-time escape adds no per-iteration cost.
- affected qualities: accuracy (restores — subject work is measured),
  robustness (improves — barrier no longer silently defeated by a default JIT),
  per-call overhead (unchanged — escape is once, outside the loop), simplicity
  (small cost — one extra setup step per cell).
- assumptions: A-3 below.
- scope: cell setup in the generated measurement function; the loop body and
  the opaque read are unchanged.
- commitment: firm, pending task review. This decision post-dates the original
  task-001 implementation; the as-built harness implements D-1..D-4 but not
  D-5, so it is subject to C-004 on a default JVM.

```text
id: D-005
status: intended
subject: opaque input barrier cell
predicate: requires
object: one-time escape of each cell at setup (defeat escape analysis)
scope: criterium.measured.impl code generation
basis: section 8 post-implementation experiments (EA toggle; one-time escape; compiler and runtime blackhole)
confidence: high
```

---

## 5. Principles and patterns

- `Principle`: attack the optimisation at the point it is enabled. The
  enabling fact is a stable input reference, so the barrier belongs on the
  input, not on the output.
- `Principle`: a measurement barrier must be cheaper than the smallest thing
  it is meant to measure, or it measures itself.
- `Principle` (project): minimal design, open to extension. The barrier is a
  scalar primitive; the pool (#158) extends it without changing its
  contract.
- `Pattern`: source/sink separation. Output DCE is handled by the blackhole
  sink; input hoisting is handled by the opaque source. They are
  complementary, at opposite ends of the expression, and must not be fused.

---

## 6. Tradeoffs

```text
tradesOff(accuracy, per-call-overhead)
```

The opaque read adds a constant per-argument cost. We accept it because the
alternative is a number that is silently wrong, and because the cost is
measurable and explainable as a documented artifact (D-4). We do not
subtract it, because subtracting a noisy baseline can drive a cheap result
negative (#19).

```text
tradesOff(simplicity, pool-performance)
```

Choosing scalar atomics (D-2, D-3) keeps the barrier simple and cheapest for
the common case. A future pool will pay its own, higher, indexed-access cost
only when used.

- `Assumption` A-1: `getOpaque` on `Atomic*` is intrinsified by HotSpot and
  stays unboxed for primitive variants. Status: observed on JDK 25.
- `Assumption` A-2: making the input reference opaque is sufficient to keep
  the dependent expression in the loop. Status: **falsified**
  (post-implementation). True only when the cell escapes. For a non-escaping
  cell, HotSpot escape analysis proves the opaque load constant and folds the
  dependent computation (C-004, §3 refinement, section 8). The opaque read is
  necessary but not sufficient; see D-5 and A-3. Earlier "observed" evidence
  for the field read predates this finding and must be re-validated with escape
  analysis on (the non-escaping `AtomicReference` cell could be folded the same
  way).
- `Assumption` A-3: forcing each cell to escape once, at setup, is sufficient
  to keep the dependent expression in the loop under default escape analysis.
  Status: observed on JDK 25 arm64 for `Math/sqrt` chains (depth-independent
  0.055 ns when local; ~0.44 ns and 3x scaling when escaped) and for FP
  division, with both the compiler and runtime blackhole (section 8). Each new
  subject class should still be spot-checked.

---

## 7. Embodiment

### Elements

- `Component` `criterium.measured.impl/measured-expr-fn` — generates the
  measurement function. New responsibility: emit, per argument, either an
  opaque cell (init binding created and filled once, plus a per-iteration
  opaque read) or, when the argument cannot be given an opaque cell cleanly,
  the existing non-opaque binding. The opaque path is unconditional; the
  choice is made per argument, not by a caller option.
- `Abstraction` "opaque cell" — a mutable place supporting an opaque read.
  `represents` the input slot whose stability must be hidden from the JIT.
  `embodiedBy` an `Atomic*` instance.

### Interfaces

- `Interface` `measured`/`bench` public surface — unchanged. No new option
  is added; opaque input handling is internal behaviour.
- `Interface` opaque-cell contract: `init(value) -> cell`;
  `read(cell) -> value` with opaque semantics; per argument type.
- `Interface` per-argument fallback predicate — decides, at code-generation
  time, whether an argument gets an opaque cell or the non-opaque binding.

### State

- `State` the opaque cell. `owns`: the generated measurement function.
  `mayMutate`: only the init step, once, before the loop.
  `mayRead`: the loop body, opaquely. `mustEscape`: once, at setup, before the
  loop (I-4). Lifetime: one measurement invocation.

### Invariants

- `Invariant` I-1: the store into the cell happens exactly once, outside the
  loop; the loop only reads. Enforcement point: code generation.
  Rationale: a store inside the loop adds cost and may enable
  store-to-load forwarding that cancels the opacity.
- `Invariant` I-2: no boxing on the read path for primitive arguments.
  Enforcement point: typed atomics (`AtomicLong`/`AtomicInteger`) and bit
  reinterpretation for floating point; verified by absence of boxed-math
  warnings in the generated loop.
- `Invariant` I-3: the opaque barrier is orthogonal to blackhole mode. The
  input barrier is identical whether the output sink is the compiler or the
  runtime blackhole.
- `Invariant` I-4: each cell escapes exactly once, at setup, before the loop.
  Enforcement point: code generation (a one-time publication of the cell
  reference through the blackhole, or retention in an escaping object).
  Rationale: without escape, HotSpot escape analysis proves the cell
  thread-local and constant and folds the dependent computation (C-004, D-5).
  The escape is one-time and outside the loop, so it adds no per-iteration cost
  and does not interact with I-1.

### Boundary

- `Boundary` input-source vs output-sink. `separates` the opaque read from
  the blackhole consume. `forbids` fusing them into one call.

```text
id: E-001
status: intended
subject: measured-expr-fn
predicate: realizedBy
object: per-argument opaque-cell init and opaque read
scope: criterium.measured.impl
basis: D-1, D-2, D-3
confidence: high
```

```text
id: E-002
status: intended
subject: opaque cell
predicate: enforces
object: invariant I-1 (store once, read in loop)
scope: generated measurement function
basis: experiment (store-in-loop risk)
confidence: high
```

```text
id: E-003
status: intended
subject: opaque cell
predicate: enforces
object: invariant I-4 (escape once at setup)
scope: generated measurement function
basis: section 8 (escape analysis folds a non-escaping cell)
confidence: high
```

---

## 8. Observed qualities

Metric: per-call time, best of 9 trials, 1e9 iterations, JDK 25, arm64,
compiler blackhole active. Subject: `(.a ^R x)` for the reference case and
`(* a b)` for primitives.

Note on sessions: the first four tables below come from earlier JVM runs
that happened to run ~25-30% faster than the later controlled differential
(see "Run-to-run variance"). Read them for the *relative* comparisons
(hoisted vs opaque vs runtime blackhole; shim vs atomic vs indexed). For the
authoritative *absolute per-read* cost, use the controlled differential.

### Reference subject (record field read)

| Variant                                   | ns/call |
|-------------------------------------------|---------|
| compiler blackhole, invariant local       | 0.0274  |
| runtime blackhole, invariant local        | 0.9551  |
| opaque read + compiler blackhole          | 0.1099  |
| opaque read baseline (no field read)      | 0.0549  |
| control: rotate index, then read          | 0.5000  |
| control: rotate index only                | 0.3444  |

Field-read cost, each sink minus its own baseline:

| Sink               | with read | baseline | field read |
|--------------------|-----------|----------|------------|
| runtime blackhole  | 0.982     | 0.985    | ~0.00      |
| opaque + compiler  | 0.113     | 0.059    | ~0.055     |

### Mutable-place comparison (reference)

| Place                                      | read+barrier | baseline |
|--------------------------------------------|--------------|----------|
| Java shim `Object[]` + arrayElementVarHandle | 0.1109     | 0.0549   |
| `AtomicReference.getOpaque`                | 0.1107       | 0.0549   |
| `AtomicReferenceArray.getOpaque(i)`        | 0.3294       | 0.1076   |

### Primitive subjects

| Variant                          | ns/call |
|----------------------------------|---------|
| long: compiler bh (hoisted)      | 0.0274  |
| long: shim `long[]`              | 0.1098  |
| long: `AtomicLong.getOpaque`     | 0.1098  |
| double: compiler bh (hoisted)    | 0.0274  |
| double: shim `double[]`          | 0.1098  |
| double: `AtomicLong` + bits      | 0.1099  |

No boxed-math or reflection warnings occurred in any measurement loop.

### Barrier cost, controlled differential

The subtraction figures above isolate the barrier with single-point
baselines, which are noisy. A controlled differential — measure `k` opaque
reads for `k` in 1..4 in one JVM run, take the slope — isolates the
per-read cost cleanly (it cancels the loop and consume overhead) and shows
whether reads pipeline. Each read is individually consumed, so the compiler
blackhole adds ~0.

Distinct `AtomicLong` cells:

| k | ns/call | increment |
|---|---------|-----------|
| 1 | 0.0727  | —         |
| 2 | 0.1455  | 0.0728    |
| 3 | 0.2180  | 0.0725    |
| 4 | 0.2928  | 0.0748    |

- strictly linear, ~0 intercept: cost is additive at ~0.073 ns per read, so
  an n-argument expression pays ~n x the barrier;
- reads do not pipeline (otherwise the slope would go sub-linear), so the
  cost scales with arity and cannot hide behind instruction-level
  parallelism;
- reading the *same* cell 4 times costs 4x one read (0.295 vs 0.073), not
  1x — direct evidence that `getOpaque` forbids merging/CSE, which is the
  mechanism the design relies on.

Per-read by mutable place, one differential harness, (k4-k1)/3:

| Place                        | per-read ns |
|------------------------------|-------------|
| `long[]` shim                | 0.0727      |
| `AtomicLong`                 | 0.0727      |
| `double[]` shim              | 0.0727      |
| `AtomicLong` + bits (double) | 0.0714      |
| `Object[]` shim              | 0.1111      |
| `AtomicReference`            | 0.1114      |

- primitive reads ~0.072 ns, reference reads ~0.111 ns (refs ~1.5x,
  plausibly compressed-oop decode on arm64);
- shim and `Atomic*` are equal for every type, so `Atomic*` (no custom Java)
  is chosen (D-3);
- `double` via `AtomicLong` + bit reinterpretation equals the `double[]`
  shim (the bit move is free).

### Run-to-run variance

The per-read cost is stable and linear *within* a JVM run but varies across
runs: earlier sessions measured ~0.055 ns for a primitive read, this
differential harness ~0.072 ns — roughly 25-30% between launches (CPU
frequency, thermal, code layout). This instability is itself the reason the
barrier cost must be measured per run and documented, not subtracted as a
fixed constant (D-4): a hardcoded subtraction would be wrong by this margin
and can drive cheap results negative (#19).

### Escape analysis defeats the non-escaping cell (post-implementation)

A later investigation found that the as-built barrier does not keep pure
computation downstream of the opaque read in the loop on a default JVM. The
cell is a thread-local `AtomicLong` that never escapes and is never written in
the loop, so escape analysis proves its value constant and folds the subject.
The generated code is correct in every case (opaque read and subject present in
the loop body); C2 removes the work.

Subject `(Math/sqrt x)` and its 3-deep nesting, JDK 25 arm64, compiler
blackhole, best of several 1e9-iteration runs:

| subject                                 | EA on (default) | EA off |
|-----------------------------------------|-----------------|--------|
| `(Math/sqrt x)`                         | 0.0542          | 0.442  |
| `(Math/sqrt (Math/sqrt (Math/sqrt x)))` | 0.0542          | 1.33   |
| `(/ 1.0 x)`                             | 0.108           | 0.217  |

With EA on, the sqrt result is depth-independent (1 and 3 nested both 0.054 ns)
— the chain is folded away. With EA off it scales ~3x (0.44 -> 1.33), proving
the sqrts execute, throughput-bound on the FSQRT unit. The toggle is
`-XX:-DoEscapeAnalysis`.

One-time cell escape reproduces the EA-off result surgically, with no
per-iteration cost, and is flavor-independent:

| variant (1e9 iters, min of 7)        | compiler bh | runtime bh |
|--------------------------------------|-------------|------------|
| cell stays local, 1 sqrt             | 0.0549      | 0.5685     |
| cell stays local, 3 nested sqrt      | 0.0549      | 0.5680     |
| cell escaped once pre-loop, 1 sqrt   | 0.4359      | 0.6405     |
| cell escaped once pre-loop, 3 nested | 1.3085      | 1.3079     |

- local cell: folded (depth-independent) under both blackholes;
- escaped cell: executes, 3-nested ~3x the single sqrt;
- the compiler blackhole escapes via the HotSpot blackhole intrinsic (reference
  args marked globally escaping); the runtime blackhole escapes via volatile
  publication (`this.obj1 = new WeakReference<>(obj)`);
- blackholing the loaded *value* each iteration does not help — only escaping
  the cell *object* changes the EA conclusion;
- the 3-nested-escaped figure converges to ~1.308 ns under both flavors because
  the FSQRT-unit throughput then dominates and swamps the per-consume overhead
  (compiler ~0.055 floor vs runtime ~0.568 floor).

Caveat on the "reads do not pipeline" claim above: that linear per-read
additivity is a *within-iteration* arity effect, consistent with load-port
throughput saturation. It does not mean iterations cannot overlap. Across
iterations the loop is throughput-bound: a dependency-free subject (same cell
value every iteration, result consumed) overlaps freely, so a
single-read-per-iteration loop can report a sub-cycle median. Honest latency
requires a loop-carried dependency, which the read-only barrier does not
provide.

### Quality assertions

```text
id: Q-001
status: disputed
subject: opaque read + compiler blackhole (non-escaping cell)
predicate: improves
object: measurement accuracy for sub-ns pure expressions
scope: JDK 25 arm64, record field read and primitive arithmetic
basis: section 8 measurements; disputed by C-004 — escape analysis folds downstream computation for a non-escaping cell; the field-read case needs re-validation with escape analysis on
confidence: low
```

```text
id: Q-002
status: observed
subject: runtime blackhole
predicate: violates
object: success criterion P-001 (does not expose the field read; costs ~0.95 ns)
scope: JDK 25 arm64, final-field read
basis: section 8; field read ~0.00 ns under runtime blackhole
confidence: high
```

```text
id: Q-003
status: observed
subject: AtomicReference.getOpaque
predicate: conformsTo
object: fitness condition "no custom Java, unboxed, equal to shim"
scope: reference and primitive subjects
basis: section 8 differential (per-read equal to shim for every type; no boxing warnings)
confidence: high
```

```text
id: Q-004
status: observed
subject: AtomicReferenceArray.getOpaque(i)
predicate: degrades
object: per-call overhead several times a scalar atomic read
scope: reference subject
basis: section 8 (AtomicReferenceArray read vs scalar)
confidence: high
```

### Honest limitation

For the smallest subjects the barrier dominates the reading. `shim *` and
`shim base` for `long` were identical to four decimals, so the multiply
itself was hidden under opaque-load latency. The reported number for such
subjects is mostly the barrier. This is documented as a measurement
artifact (D-4); it is not silently subtracted, because subtracting a noisy
baseline estimate of the same order drives the effective result negative
(#19).

```text
id: Q-005
status: observed
subject: reported time for very cheap subjects
predicate: degrades
object: interpretability (includes a documented, non-subtracted barrier cost)
scope: sub-0.1 ns subjects
basis: section 8 (multiply added ~0 over the two opaque reads)
confidence: high
```

```text
id: Q-006
status: inferred
subject: dynamic baseline subtraction
predicate: mayDegrade
object: robustness (negative effective measurements)
scope: cheap subjects where subject cost ~ barrier noise
basis: D-4 rationale; #19
confidence: high
```

```text
id: Q-007
status: observed
subject: as-built opaque barrier (non-escaping cell), default escape analysis
predicate: violates
object: success criterion P-001 (folds pure computation downstream of the read; reports a sub-cycle residue)
scope: JDK 25 arm64, Math/sqrt chains and FP division
basis: section 8 EA-toggle and one-time-escape experiments
confidence: high
```

```text
id: Q-008
status: observed
subject: one-time cell escape (D-5, I-4)
predicate: improves
object: measurement accuracy for sub-ns pure computation (restores in-loop execution)
scope: JDK 25 arm64, compiler and runtime blackhole
basis: section 8 (local folded vs escaped 3x-scaling)
confidence: high
```

---

## 9. Evolution

- `Change`: apply the opaque input barrier unconditionally on the expr path,
  with automatic per-argument fallback to the existing non-opaque binding.
  No new public option is added. `CompatibilityRule`: reported numbers for
  cheap subjects will change (become larger and correct); document this as
  intended, not a regression. The number includes the barrier cost by
  design; it is not auto-corrected.
- `Migration`: stage the rollout behind validation (tests across subject
  classes and types) rather than behind a user flag. The public surface does
  not change, so the staging is internal.
- `EvolutionExpectation`: the scalar opaque cell generalises to a pool
  (#158) by replacing the scalar atomic with an indexed read and advancing
  the index, without changing the barrier contract.
- `EvolutionExpectation`: if the "as-compiled" measurement intent is ever
  required, add it as a distinct, explicitly named escape hatch (for example
  `:inputs :constant` or `measured/expr-raw`), not as a boolean tuning flag.
- `ErosionRisk`: a future change that stores into the cell inside the loop
  would violate I-1 and silently restore hoisting. Guard with a test that
  asserts a known-cheap subject reports a non-zero per-call cost.
- `Change`: make each cell escape once at setup (D-5, I-4). The as-built
  task-001 harness implements D-1..D-4 but not D-5, so on a default JVM it is
  subject to C-004. `CompatibilityRule`: fixing this will again raise the
  reported number for cheap pure subjects (from a folded sub-cycle residue to
  the real throughput-bound cost); document as intended.
- `ErosionRisk`: a future change that makes a cell non-escaping again (for
  example dropping the setup publication, or a refactor that lets escape
  analysis prove thread-locality) would silently restore folding. Guard with a
  test that a sub-ns pure subject with a known internal dependency chain (for
  example nested `Math/sqrt`) scales with chain depth; depth-independence
  signals folding.
- Falls back automatically (not out of scope, but non-opaque): the callable
  path (`[:as args]`), which uses `:as` destructuring and indirect
  invocation that already resists hoisting; array-typed argument tags
  (blocked by #156); any argument whose type cannot be captured.

```text
id: V-001
status: intended
subject: unconditional opaque input barrier
predicate: changes
object: measured/expr reported time for cheap subjects
scope: criterium.measured
basis: D-1, D-4
confidence: medium
```

```text
id: V-002
status: intended
subject: one-time cell escape (D-5)
predicate: changes
object: measured/expr reported time for cheap pure subjects (from folded residue to real cost)
scope: criterium.measured.impl
basis: D-5, C-004, section 8
confidence: high
```

---

## 10. Traceability summary

- Purpose P-001 is served by decisions D-1..D-5.
- D-1 and D-3 are realized by E-001 (code generation) and the opaque-cell
  abstraction; D-5 is realized by E-003 (one-time cell escape, I-4).
- Qualities Q-001..Q-008 provide scoped evidence from section 8 and the D-4
  rationale.
- D-4 keeps accuracy honest through documentation plus the existing platform
  baseline facility (`criterium.platform`), and explicitly rejects dynamic
  subtraction; Q-005 records the resulting documented artifact and Q-006 the
  robustness reason for not subtracting (#19).
- Post-implementation finding: constraint C-004 (escape analysis) falsified
  assumption A-2 and showed the as-built barrier (D-1..D-4) folds pure
  computation downstream of the read on a default JVM (Q-007). D-5 / I-4
  (one-time cell escape) restore in-loop execution (Q-008, A-3); the task-001
  code does not yet implement D-5.
- Open dependencies: #158 (pool, optional extension), #156 (array tags, blocks
  a subset of inputs). Follow-up: implement D-5 and re-validate the field-read
  success (Q-001) with escape analysis on.
