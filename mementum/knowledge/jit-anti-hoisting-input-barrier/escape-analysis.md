---
title: Input barrier escape analysis experiments and setup escape
status: active
category: measurement
related:
  - ../jit-anti-hoisting-input-barrier.md
depends-on: []
---

# Historical defect and confirmed fix

The experiments below describe the original non-escaping barrier and its fix.
Current cells escape once at setup; the defect is not the current contract.

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

