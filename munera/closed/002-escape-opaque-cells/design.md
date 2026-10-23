# 002 — Escape opaque cells to survive escape analysis

Design authority: `design/opaque-input-barrier.md` (decision D-5, invariant
I-4, constraint C-004, assumption A-3). Source finding recorded in
`mementum/knowledge/jit-anti-hoisting-input-barrier.md`. Predecessor: task
001 (closed) implemented D-1..D-4 but not D-5. Related issues: #155, #19,
#156, #157, #158, #159.

---

## Intent

The opaque input barrier from task 001 must actually keep pure computation in
the measured loop. Today it does not, on a default JVM. Each argument is read
through a thread-local `Atomic*` cell that never escapes the measurement
function and is never written inside the loop. HotSpot C2 escape analysis
proves the cell has no possible concurrent writer, treats the opaque load as
the loop-invariant constant it provably is, and folds or hoists the subject
computation. The reported time for a cheap pure expression is again an
optimised-away residue, not the expression.

This task makes each opaque cell **escape once, at setup**, so escape
analysis can no longer prove it thread-local. Then the opaque read is a
genuinely unknown value and the dependent work stays in the loop.

This restores success criterion P-001 (measure sub-nanosecond pure
expressions) that task 001 was meant to deliver but did not, for any subject
whose work is downstream of the read (arithmetic, `Math/sqrt`, `/`, field
reads reached through the cell).

## Problem

The opaque read is necessary but not sufficient (design authority §3
refinement; A-2 falsified). The missing fact is escape. Measured evidence
(JDK 25, arm64; design authority §8):

- `(Math/sqrt x)` and `(Math/sqrt (Math/sqrt (Math/sqrt x)))` both report
  ~0.054 ns with escape analysis on — depth-independent, so the chain is
  folded. With `-XX:-DoEscapeAnalysis` they report 0.44 ns and 1.33 ns (~3x),
  so the work executes.
- Publishing the cell reference once before the loop (one `blackhole/consume`
  of the cell as an `Object`) reproduces the escape-analysis-off result with
  no per-iteration cost: local cell folded (0.055 ns), escaped cell executes
  (0.44 ns single, 1.31 ns 3-nested). Confirmed with both the compiler
  blackhole (intrinsic escape marking) and the runtime blackhole (volatile
  publication).
- Blackholing the loaded *value* each iteration does not help; only escaping
  the cell *object* changes the escape-analysis conclusion.

## Goal

Generate, for each opaque cell, a one-time escape at setup (after the init
store, before `start#`), so that the dependent expression is measured, not
folded. The loop body and the opaque read are unchanged. No change to the
public `measured`/`bench`/`measured/expr` surface.

---

## Scope (in)

- Code generation in `criterium.measured.impl` (the function that builds the
  opaque cells and the measurement loop): emit one escape point per opaque
  cell, at setup, outside the loop.
- The escape mechanism uses the already-present blackhole. The escape works
  under both the compiler blackhole (intrinsic escape marking) and the runtime
  blackhole (volatile publication) — a D-5 / I-4 property. (I-3 separately
  keeps the opaque read-barrier orthogonal to blackhole mode.)
- Preserve invariants I-1 (store once, read-only loop), I-2 (no boxing on the
  primitive read path), I-3 (blackhole-mode orthogonal). Add I-4 (escape
  once, at setup).
- Tests: a structural escape-point check for every opaque-eligible cell kind
  (`long`/`int`/`double`/`float`, reference), plus a behavioural depth-scaling
  guard for the kinds whose dependency op is costly enough to clear the
  barrier-noise floor. Depth-independence is the folding signal (design
  authority ErosionRisk). The behavioural oracle covers `double` (nested
  `Math/sqrt`) and `reference` (a record field read through the reference
  cell feeding a nested `Math/sqrt` chain; a pure field-read chain does not
  scale measurably — independent iterations overlap the loads). The remaining primitive kinds (`long`/`int`/`float`),
  whose cheapest dependency ops are sub-cycle and sit below the barrier read's
  run-to-run variance, are covered by the structural check only.
- Documentation: update the `measured`/`measured/expr` docstrings and README
  only if the observed per-call numbers for cheap subjects change materially;
  the barrier-cost framing (D-4) is otherwise unchanged.

## Scope (out)

- Re-validating or changing the fallback cases (array tags #156, callable
  path #157, other primitive tags) beyond confirming they still work. They do
  not use an opaque cell, so D-5 does not apply to them.
- The input pool / rotation (#158). If the pool lands, each pooled cell must
  also escape, but that is the pool task's concern.
- Dynamic baseline subtraction (rejected, D-4; #19). Numbers stay honest and
  non-subtracted.
- Harness baseline reporting (#159).
- A loop-carried-dependency measurement mode (latency rather than
  throughput). The barrier is throughput-oriented; latency measurement is a
  separate, unscoped concern.
- Any new public option or tuning flag (D-1: unconditional, no knob).

## Adjacent task-like work (not this task)

- #158 input pool (`:args-pool`) — pooled cells will also need escape.
- #156 array-typed local tags.
- #157 callable-path loop retention.
- #159 surfacing harness baselines in reporting.
- Re-validation of the task-001 field-read quality claim (design authority
  Q-001, now disputed) is covered by this task's depth-scaling tests for the
  reference cell kind.

---

## Concepts

- **Cell escape** — a one-time publication of a cell reference that prevents
  HotSpot escape analysis from proving the cell thread-local. Performed once,
  at setup, before timing starts.
- **Escape point** — the generated expression that publishes the cell (one
  `blackhole/consume` of the cell as an `Object`, reusing the active
  blackhole; or retention in an object the harness keeps live).
- **Folding signal** — a sub-ns pure subject whose reported time does not
  change with internal dependency-chain depth. Depth-independence means the
  chain is folded; linear scaling with depth means it executes. Used as the
  test oracle because it is machine-independent (a ratio, not an absolute ns
  threshold).

### Invariants

- I-1 (unchanged): the store into a cell happens exactly once, outside the
  loop; the loop only reads opaquely.
- I-2 (unchanged): no boxing on the read path for primitive arguments.
- I-3 (unchanged): the opaque read-barrier is orthogonal to blackhole mode;
  the read barrier is identical whether the output sink is the compiler or the
  runtime blackhole. (Escape-mode-independence — the escape works under both
  modes — is a D-5 / I-4 property, see acceptance criterion 3; it is not part
  of I-3.)
- I-4 (new): each cell escapes exactly once, at setup, before the loop. The
  escape is outside the loop, so it adds no per-iteration cost and does not
  interact with I-1.

### Ownership / abstraction decisions

- The escape reuses the existing blackhole abstraction (source/sink
  separation is preserved: the blackhole still owns publication/sink; this
  task adds one setup-time use of it on the input side). No new Java
  artifact, no new namespace.
- One escape point per cell is sufficient; escape is a per-object property, so
  it need not be repeated per iteration.

---

## Acceptance criteria

1. For the expr path, the generated measurement function escapes every
   opaque-eligible cell exactly once, after its init store and before
   `start#`, and never inside the loop (verifiable by inspecting the
   generated / `macroexpand`ed form). (I-1, I-4)
2. A cheap pure subject with an internal dependency chain measures a per-call
   time that scales with chain depth, not a depth-independent folded residue.
   Concretely, a 3-deep nested `Math/sqrt` measures meaningfully more than a
   single `Math/sqrt` (relative check, tolerant of machine/noise variation,
   no absolute ns threshold). The behavioural depth oracle applies to the
   kinds with a dependency op costly enough to clear the barrier-noise floor —
   `double` (nested `Math/sqrt`) and `reference` (a record field read through
   the reference cell feeding nested `Math/sqrt`; a pure field-read chain
   is too cheap per load to scale measurably). The remaining primitive kinds
   (`long`/`int`/`float`), whose cheapest dependency ops are sub-cycle and fall
   below the barrier read's run-to-run variance, are covered by the structural
   escape-point check (criterion 1) instead; no depth ratio is asserted for
   them.
3. The escape works under both the compiler blackhole and the runtime
   blackhole. (D-5, I-4)
4. Primitive-valued arguments still incur no boxing in the measured loop; the
   escape does not reintroduce boxing or per-iteration allocation. (I-2)
5. Existing `measured` tests pass unchanged; non-hoistable subjects measure as
   before within noise. Fallback cases (array tags, callable path, other
   primitive tags) are unaffected.
6. The depth-scaling guard is added as a durable regression test, so a future
   change that makes a cell non-escaping again (dropping the escape, or a
   refactor that lets escape analysis prove thread-locality) fails the suite.
   (design authority ErosionRisk)

## Verification expectations

- Structural/code-generation test: macroexpand the expr form and assert each
  eligible cell has exactly one escape point at setup and none in the loop.
  This is the deterministic, non-flaky signal.
- Behavioural guard (folding oracle): for the `double` kind, measure a nested
  `Math/sqrt` chain at depth 1 and depth 3 and assert depth-3 is meaningfully
  greater (ratio, tolerant). For the `reference` kind, measure a record field
  read through the reference cell feeding a `Math/sqrt` chain at depth 1 and
  depth 3 and assert the same. No behavioural depth assertion
  for `long`/`int`/`float` — their cheapest dependency ops fall below the noise
  floor; the structural escape-point check is their guard.
- Boxing/allocation check on the setup escape and the read path for each
  primitive cell kind.
- Full existing `measured` suite green.
- Tests must be proven to fail when the mechanism is broken (remove the escape
  → depth-scaling test must fail; mementum memory
  `jit-behavioural-test-teeth`).

## Risks

- The escape mechanism may interact with specific blackhole modes differently;
  mitigate by testing the escape under both blackhole modes (D-5 / I-4) and
  asserting the folding oracle, not absolute timings.
- Over-escaping (escaping inside the loop, or publishing per iteration) would
  add cost and could enable store-to-load forwarding; mitigate with the
  structural test that the escape is exactly once, at setup (I-4), and the
  existing I-1 guard.
- Behavioural timing assertions are noisy; mitigate with the depth-ratio
  oracle and the structural test as the primary signal.
- The compiler blackhole relies on the HotSpot intrinsic escape marking; if a
  future JDK changes that, the runtime-blackhole publication (a genuine
  volatile store) remains a robust fallback. Document the dependency.
