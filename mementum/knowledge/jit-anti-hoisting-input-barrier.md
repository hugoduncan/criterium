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
  Opaque-eligible iff tag ∈ `#{long int double float}` (the direct-cast
  primitives, mirror `cast-fn`'s set) OR tag ∈ `#{boolean byte short char}`
  (the small primitives, task 003) OR a reference-class symbol OR absent.
  Fallback to the plain binding is now **array tags only** (`long[]` ...,
  pending #156); the `measured/callable` whole-binding path (#157) is separate.
- Unboxed cells: `AtomicLong` for `long`/`double` (`double` via
  `Double/doubleToRawLongBits`/`longBitsToDouble`); `AtomicInteger` for
  `int`/`float` (`float` via `Float/floatToRawIntBits`/`intBitsToFloat`, the
  int-sized-bits → int-sized-cell rule), and for the small primitives
  `boolean`/`byte`/`short`/`char` (task 003) by **widening into the
  `AtomicInteger` on store and narrowing on read**: `byte`/`short`/`char`
  store `(int x)` and read `(unchecked-byte|short|char (.getOpaque cell))`;
  `boolean` stores `(if b 1 0)` and reads `(not (zero? (.getOpaque cell)))`.
  CORRECTION: the earlier "a cell would box, so these fall back" claim was
  wrong — it holds only for `AtomicReference`, NOT for the widen-into-a-
  primitive-cell pattern. Any primitive narrower than `int` fits unboxed in an
  `AtomicInteger`; `(int ^byte x)` widening, the `AtomicInteger(int)` ctor,
  `getOpaque`, and the `unchecked-*`/`zero?` reads all stay unboxed (verified
  under `*warn-on-reflection*` + `*unchecked-math* :warn-on-boxed`, and no
  per-iteration allocation with the native agent — the boolean read's
  `Boolean/TRUE`/`FALSE` are canonical, not allocated). Only array tags still
  lack an unboxed cell.
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
  non-escaping `AtomicReference` cell is folded the same way. Confirmed in task
  002: a pure reference field-read chain reads ~0.072 ns without escape (folded)
  and ~0.22 ns escaped — and does NOT scale with chain depth even escaped,
  because independent iterations overlap the loads (see memory
  `reference-depth-oracle-needs-compute`). Escape makes a field-read→compute
  subject measurable, but a pure pointer-chase is not a usable folding oracle.
- Honest sub-ns numbers require one of: run with `-XX:-DoEscapeAnalysis`; make
  the cell escape (store it where EA cannot prove thread-locality); or impose a
  loop-carried dependency (the current read-only barrier provides none).

## CONFIRMED FIX: escape each cell once at setup

Blackholing the cell *object* once, before the loop, defeats EA with zero
per-iteration cost. HotSpot's compiler-blackhole intrinsic marks reference
arguments as globally escaping, so a single `Blackhole/consume ^Object cell`
before timing taints the object's escape state; EA can then no longer prove
thread-locality, the opaque load is no longer a provable constant, and
downstream pure computation stays in the loop.

Manual experiment (JDK 25, arm64, compiler blackhole, min of 7 × 1e9 iters):

| variant                             | ns/iter |
|-------------------------------------|---------|
| cell stays local, 1 sqrt            | 0.0549  |
| cell stays local, 3 nested sqrt     | 0.0549  |
| cell blackholed once pre-loop, 1 sqrt   | 0.4359  |
| cell blackholed once pre-loop, 3 nested | 1.3085  |

Escaping the cell reproduces the `-XX:-DoEscapeAnalysis` numbers (0.442 /
1.33) surgically: local cells stay folded (depth-independent 0.055 ns);
escaped cells execute and scale ~3× with chain depth. Blackholing the loaded
*value* each iteration does NOT help — only escaping the *object* changes the
EA conclusion (value-blackhole blocks DCE, not constant propagation).

Flavor-independent (confirmed both ways):

- Compiler blackhole (`-XX:CompileCommand=blackhole,...`): escape forced by the
  HotSpot blackhole *intrinsic* marking reference args as globally escaping.
- Runtime blackhole (volatile publication in `consumeRuntime(Object)`:
  `this.obj1 = new WeakReference<>(obj)` under a volatile-guarded branch):
  genuine escape. Rerun (no compiler flag): local cell folded (depth-
  independent 0.568 ns), escaped cell executes (1-sqrt 0.640, 3-nested 1.308).

In BOTH flavors the in-loop *value* sink leaves the local cell folded; only
escaping the cell *reference* defeats EA. The 3-nested-escaped figure converges
to ~1.308 ns in both (compiler 1.3085, runtime 1.3079) because the FSQRT-unit
throughput then dominates and swamps the per-consume overhead (compiler ~0.055
floor vs runtime ~0.568 floor).

LANDED (task 002, D-5 / I-4): the opaque cells now escape — codegen emits one
`[_ (blackhole/consume cell-sym)]` setup binding pair per opaque-eligible cell,
after its init store and before `start#`, outside the loop (zero per-iteration
cost). Reuses the existing blackhole (no new Java artifact / namespace); the
cell object is already an `Object`, so `consume` adds no boxing (I-2). Confirmed
green in-suite under BOTH blackhole modes: `double` nested-`sqrt` depth oracle
scales ~3× with escape and goes depth-flat when the escape is removed (verified
teeth). This closes the task-001 gap (sub-ns pure subjects downstream of the
read now report genuine, non-folded time).

Testing-infra fact: blackhole mode is a LOAD-TIME / process-level property
(`detected-mode` is a `delay` baked into the `measured` fn at macroexpansion);
there is NO in-process switch. Write mode-agnostic behavioural tests (do not
`with-redefs` `detected-mode`); get both-mode coverage by running the suite
under two process configs (default-JVM runtime + `:blackhole`-alias compiler on
JVM 17+). A mode-agnostic depth oracle must be its own always-running deftest,
NOT nested in a compiler-only-guarded test (that would drop the runtime half).

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
Array-typed tags take the fallback and are a pre-existing defect (#156). Small primitives (`boolean`/`byte`/`short`/`char`) no longer fall back — task 003 moved them onto the opaque path via `AtomicInteger` widen/narrow, so `:fallback` now means array tags only.
