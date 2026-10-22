---
title: JIT anti-hoisting input barrier for cheap pure expressions
status: active
category: measurement
tags: [jit, microbenchmark, measured, opaque, varhandle, final-field, boxing]
related:
  - bases/criterium/src/criterium/measured/impl.clj
  - design/opaque-input-barrier.md
  - "issues: #155 #156 #157 #158 #159 #19 #17"
depends-on: []
---

# Problem

A pure subject whose inputs are bound once before the measured loop lets the
JIT lift the work out of the loop, so `measured/expr` reports empty-loop cost,
not the expression. The defect is sharper than generic loop-invariant code
motion: when the subject reads a `final` field of a loop-invariant object
(every `defrecord` field is `final`), the JIT folds the read to a constant
because it relies on immutability, not on "memory may have changed".

# Key fact

No output-side memory barrier (compiler or runtime blackhole) can fix
final-field folding. The only effective intervention is the input: make the
value feeding the expression come from a read the compiler can not prove is
stable.

# Solution

Read each argument through an opaque cell inside the loop (`VarHandle`
`getOpaque` access mode). The JIT may not merge two opaque reads of the same
location, so it can not prove the object is the same as last iteration and can
not hoist. Opaque has no acquire/release fence, so it is far cheaper than a
volatile read or the runtime blackhole.

Implementation choices (criterium `measured.impl`):

- Use `java.util.concurrent.atomic` types as the mutable place, NOT raw
  `VarHandle`. Their `getOpaque`/`setOpaque` are ordinary typed methods, so
  Clojure calls them with no `@PolymorphicSignature` varargs boxing and no
  custom Java shim/build step. `deftype` offers only plain (hoistable) or
  volatile (over-strong) mutability, never opaque.
- Cell selection is a pure code-gen-time predicate over the captured `:tag`.
  Opaque-eligible iff tag ∈ `#{long int double float}` (mirror `cast-fn`'s
  set), a reference-class symbol, or absent. Fallback to the plain binding for
  `boolean`/`byte`/`short`/`char`, array tags (`long[]` ...), and anything
  else — no unboxed atomic cell exists for them, so a cell would box.
- Unboxed cells: `AtomicLong` for `long`/`double` (`double` via
  `Double/doubleToRawLongBits`/`longBitsToDouble`); `AtomicInteger` for
  `int`/`float` (`float` via `Float/floatToRawIntBits`/`intBitsToFloat`, the
  int-sized-bits → int-sized-cell rule).
- Store exactly once before timing starts (I-1); the loop only reads opaquely.
  A store inside the loop adds cost and enables store-to-load forwarding that
  cancels opacity.

# Barrier cost is an artifact, never subtracted

Per-argument opaque-read cost: ~0.07 ns primitive, ~0.11 ns reference; reads
do not pipeline, so cost scales with arity; run-to-run variance ~25-30%. It is
documented, not subtracted (subtracting a noisy same-order baseline produces
negative times — #19).

# DEFECT: escape analysis folds downstream computation (default JVM)

Confirmed empirically (JDK 25, arm64, compiler blackhole): the barrier keeps
the opaque *read* in the loop but does NOT keep pure computation *downstream*
of the read in the loop, because C2 escape analysis defeats opaque semantics.

The `AtomicLong`/`AtomicReference` cell is a thread-local object that never
escapes the measured function and is never written inside the loop. Opaque
access mode only promises "treat this location as if another thread may change
it." But escape analysis proves no other thread can ever reach this object, so
there is no possible concurrent writer — and C2 then treats the `getOpaque`
value as the loop-invariant constant it provably is, re-emitting the cheap load
but constant-folding and hoisting any pure computation built on it.

Proof (toggle `-XX:-DoEscapeAnalysis`):

| subject | EA on (default) | EA off |
|---|---|---|
| `(Math/sqrt x)` | 0.054 ns | 0.442 ns |
| `(Math/sqrt (Math/sqrt (Math/sqrt x)))` | 0.054 ns | 1.33 ns |
| `(/ 1.0 x)` | 0.108 ns | 0.217 ns |

With EA on the sqrt result is depth-independent (0.054 ns for 1 and for 3
nested) — the chain is folded away. With EA off it scales ~3× (0.442 → 1.33),
proving the sqrts execute, throughput-bound on the FSQRT unit (~0.44 ns each).
The macroexpansion is correct in both cases (opaque read + sqrt present in the
loop body); the difference is purely C2's EA decision.

Consequences:

- The barrier only reliably measures subjects where **the read is the work**
  (the canonical record field read). For arithmetic / `sqrt` / `/` / `inc`
  downstream of the read, the default JVM folds it to a sub-cycle floor and
  the reported number is meaningless.
- The canonical field-read success case is itself suspect under EA: a
  non-escaping `AtomicReference` cell could be folded the same way. Needs a
  dedicated check (field read with EA on vs off).
- Honest sub-ns numbers require one of: run with `-XX:-DoEscapeAnalysis`; make
  the cell escape (store it where EA cannot prove thread-locality); or impose a
  loop-carried dependency (the current read-only barrier provides none).
- This is a candidate issue against the task-001 design: its stated goal
  ("report a per-call time that reflects the subject expression, including for
  pure expressions whose cost is below about one nanosecond") is not met on a
  default JVM for computation downstream of the barrier read.

# Throughput vs latency: the barrier does not serialize iterations

The barrier defeats *hoisting* (lifting work out of the loop). It does NOT
defeat *cross-iteration instruction-level parallelism*. The opaque cell holds
the same value every iteration and there is no loop-carried dependency, so the
out-of-order core overlaps many iterations: `getOpaque` is a plain `ldr` with
no fence, so independent loads to the same address run concurrently, bounded
only by load-port throughput.

Consequence: for a near-degenerate subject the reported median can go
sub-cycle. Observed `(c/bench 1)` ≈ 0.054 ns/call (JDK 25, arm64) — the `1` is
a captured double argument, so the loop does one opaque read + consume and
nothing else, and the median is just the per-argument barrier-read cost
(~0.055–0.073 ns, matching the authority's own figures). At ~5 GHz that is
~0.27 cycle; a single opaque L1 load has ~3–4 cycle latency (~0.6–0.8 ns), so
a ~0.054 ns median is only possible because iterations overlap.

Caveat on the authority text: `design/opaque-input-barrier.md` says the
per-read cost "cannot hide behind instruction-level parallelism." That holds
only *within* one iteration (reading k cells costs ~k× because the loads
saturate load-port throughput, which is itself the throughput-bound, not
latency-bound, signature). *Across* iterations ILP dominates, which is exactly
why a single-read-per-iteration loop lands far below single-load latency.

Rule of thumb: a sub-~0.2 ns median means "iterations overlapped / subject is
near-degenerate," not "the expression is that fast." Forcing a loop-carried
dependency (feed each iteration's read into the next) would turn the loop
latency-bound, but that is a different measurement contract and is NOT in
scope of any current issue (#156 array tags, #157 callable path, #158
`:args-pool`, #159 reporting baselines all concern other axes).

# Scope boundaries

Scalar, one cell per argument; the input pool (`:args-pool`, #158) is a later
extension that reuses the cell abstraction. The callable path (`[:as args]`)
is not per-argument decomposed, so it takes the whole-binding fallback (#157).
Array-typed tags take the fallback and are a pre-existing defect (#156).
