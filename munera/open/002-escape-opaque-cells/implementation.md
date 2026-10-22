# Implementation notes

## Review context (architecture sources)

- Governing architecture authority is `design/opaque-input-barrier.md`
  (D-5, I-4, C-004, A-3) plus
  `mementum/knowledge/jit-anti-hoisting-input-barrier.md`.
- `ramora/META.md` and `doc/architecture.md` named in the review prompt do
  not exist in this repo; use the design authority above instead.
- Escape mechanism confirmed against the blackhole abstraction:
  `criterium.blackhole/consume` has an `Object` overload; runtime mode
  publishes via a volatile store (genuine escape), compiler mode relies on
  the HotSpot intrinsic escape marking (acknowledged in the design Risks).

## Notes for addressing the design-steps

Principles to hold:
- Keep the folding oracle machine-independent: a ratio/relative check, no
  absolute ns threshold (design decision, not an ambiguity to "fix" with a
  hardcoded ns bound).
- Preserve I-1..I-4, reuse the existing blackhole (no new Java artifact or
  namespace), escape once at setup, no public-API/knob change.
- `¬backward-compat` (AGENTS.md): freely rename/retag invariants when
  reconciling the I-3 divergence — no compatibility cost.

Per-kind folding-oracle step (feasibility context from authority §8):
- The `double` chain works because `Math/sqrt` is ~0.44 ns/op (throughput-
  bound), well above the barrier read (~0.072 ns primitive, ~0.111 ns
  reference) and its ~25-30% run-to-run variance. A `long`/`int`/`float`
  chain of cheap arithmetic (add/multiply, sub-cycle) may not clear that
  noise floor, so the depth ratio can be unobservable — pick a costly-enough
  op per kind or fall back to the structural escape-point check for those.
- `reference` kind: authority/knowledge flag the single field read as itself
  suspect under EA; a depth-scaling reference test needs a linked object
  graph (record whose field points to the next record), not one field read.
- `cell-kind` in impl.clj is the single source of truth for the tag taxonomy
  (opaque-eligible kinds vs `:fallback`); reuse it, do not re-enumerate.

Relevant project (non-task) files:
- `bases/criterium/src/criterium/measured/impl.clj`
  (`measured-expr-fn`, `cell-kind`, `opaque-cell`, `arg-bindings` — the
  escape point is emitted here, after each cell `:init` and before `start#`).
- `bases/blackhole/src/clj/criterium/blackhole.clj` and
  `bases/blackhole/src/java/criterium/blackhole/Blackhole.java`
  (`consume(Object)`; runtime publishes via volatile `obj1`, `evaporate`
  clears it).
- `design/opaque-input-barrier.md` §8 (per-read costs, variance, escape
  experiments); `mementum/knowledge/jit-anti-hoisting-input-barrier.md`.

## Review notes

- architectural review added no design steps (design fits the architecture;
  escape-via-blackhole reuse, source/sink separation, one-time setup escape,
  and no-new-artifact are all consistent with project principles)
- ambiguity review added 1 new design step (per-kind depth-scaling folding
  oracle specified only for the `double` kind; non-double kinds, esp.
  `reference`, left open)
- plan-review ambiguity pass added 2 new design steps (plan Slice 3 "run under
  both blackhole modes" has no in-process switch — `blackhole/consume` is a
  macro reading a load-time `detected-mode` delay keyed on JVM version + the
  `:blackhole` `-XX:CompileCommand` flag, so both-mode coverage is process/CI
  level, not a single test; and the `:escape` entries are described as forms but
  must become `let` binding-pairs to splice before `start#`, which the Slice 2
  structural test keys on). Both are in-scope clarifications, no scope change.
- inconsistency review added 1 new design step (I-3 restated as "unchanged"
  but widened to cover escape-in-both-modes, diverging from authority where
  escape is I-4/D-5; acceptance 3 mis-cites I-3). Over-escaping risk's
  "store-to-load forwarding" wording considered but left unfiled (imprecise
  rationale, not build/verify-affecting).
- plan-review ambiguity pass (re-run) added 1 new design step: the plan's
  Slice 3 "CI wiring" presents "add the depth tests under `opaque-behaviour-test`"
  and "widen the CI `--focus`" as equivalent, but the existing
  `opaque-behaviour-test` (measured_test.clj ≈line 1184) is wrapped in
  `(if (blackhole/compiler-blackhole-enabled?) <assertions> <skip>)` and runs
  only the skip branch under the default-JVM runtime config. Nesting the
  mode-agnostic depth oracle there would suppress it under runtime mode and
  drop the runtime half of the both-mode coverage — so the new depth deftest
  must be standalone (always-running) with the CI `--focus` widened to name it.
- plan-review inconsistency pass added no new design steps: plan.md and
  steps.md are mutually consistent and consistent with the converged design —
  the I-3 vs D-5/I-4 citation is applied uniformly across design/plan/steps,
  depth values (1 vs 3), kind taxonomy, slice order, and escape placement all
  agree. The two open plan issues (both-modes test mechanism, `:escape`
  form-vs-binding shape) are ambiguity-classified and already filed; they are
  not cross-file contradictions, so they are not re-filed here.
- plan-review inconsistency pass (re-run) added no new design steps: the only
  cross-file tension — plan.md keeping "add under `opaque-behaviour-test`" as
  equivalent while steps.md already commits to a standalone named deftest —
  resolves to the same fix as the CI-placement ambiguity step filed this
  session, so it is not re-filed (would duplicate). All other depth/kind/
  taxonomy/escape-shape/citation/slice-order facts agree across design.md,
  plan.md, and steps.md.
- plan-review ambiguity pass (shared plan-review session, first turn) added no
  new design steps: plan.md/steps.md are converged. All five prior
  ambiguities (per-kind oracle, I-3 traceability, both-modes mechanism, escape
  binding-pair shape, CI placement) are filed and resolved. Grounding
  reconfirmed against code: `opaque-behaviour-test` compiler guard at
  measured_test.clj:1199, CI `--focus` step at tests.yml:130-135, and the
  `agent/with-allocation-tracing` pattern (`zero-garbage-test`) grounds the
  Slice 4 boxing/allocation check. Remaining soft spots (relative-ratio
  tolerance, "materially changed" README gate) are deliberate design
  decisions, already flagged not-to-file.
- plan-review inconsistency pass (shared plan-review session, second turn)
  added no new design steps: design.md/plan.md/steps.md are mutually
  consistent — depth values (1 vs 3), kind taxonomy, escape binding-pair
  shape, CI standalone-deftest placement, I-3 vs D-5/I-4 citations (acceptance
  3 cites D-5,I-4), and the 5-slice order all agree across files. The
  store-to-load-forwarding wording stays deliberately-not-filed (imprecise
  rationale, not build/verify-affecting).

## Notes for addressing the plan-ambiguity design-steps

Principles to hold:
- Both-modes step: do NOT build an in-process mode switch. `blackhole/mode` is
  a load-time property — `detected-mode` is a private `delay` keyed on JVM
  version + the compiler-blackhole flag, and `consume` is a macro that bakes
  the choice in at the `measured` fn's expansion. Write the depth-scaling tests
  mode-agnostic (no mode-specific assertions, no `with-redefs` of
  `detected-mode`); both-mode coverage comes from running the suite under two
  process configs. Compiler mode additionally requires the flag, not just JVM
  17+.
- Escape-shape step: keep each escape inside the same `let` as a `_
  (blackhole/consume cell-sym)` binding pair placed after the cell's `:init`
  and before `start#` (not a bare form — that is odd-count in a binding
  vector). This keeps it before the warm `val#` and the loop, and gives the
  Slice 2 structural test a stable "consume of cell-sym in setup" to match. The
  cell object (`AtomicLong`/`AtomicInteger`/`AtomicReference`) is already an
  Object, so `consume` of it adds no boxing (I-2).

Relevant project (non-task) files for these steps:
- `deps.edn` `:blackhole` alias — supplies
  `-XX:+UnlockExperimentalVMOptions -XX:CompileCommand=blackhole,criterium.blackhole.Blackhole::consume`;
  this is what flips `detected-mode` to `:compiler` on JVM 17+.
- `.github/workflows/tests.yml` — default-JVM run gives runtime-mode coverage;
  the "opaque barrier under compiler blackhole" step runs the suite via
  `clojure -M:kaocha:dev:test:blackhole` on JVM 21 for compiler-mode coverage.
  New both-mode tests are covered automatically only if they live in the suite
  those runs execute; confirm, else add an alias/CI entry.
- `bases/blackhole/src/clj/criterium/blackhole.clj` — `detected-mode` delay,
  `consume` macro, `mode`/`compiler-blackhole-enabled?` helpers.

## Design-step resolutions (for implementers / later reviews)

- Per-kind folding oracle resolved by split: behavioural depth-scaling oracle
  only for `double` (nested `Math/sqrt`) and `reference` (linked record chain,
  depth = chain length); `long`/`int`/`float` are covered by the structural
  escape-point check only (their cheapest dependency ops are sub-cycle, below
  the barrier read's ~25-30% variance — a depth ratio would be a flaky test,
  not a signal). Implementer: build the `reference` subject as a linked chain
  of records (each record's field holds the next record), reached through the
  opaque cell; do not reuse a single field read (authority flags it as
  EA-suspect). The structural test (macroexpand: one setup escape per eligible
  cell, none in the loop) is the kind-agnostic guard and the primary signal.
- I-3 vs D-5/I-4 traceability settled against `design/opaque-input-barrier.md`:
  I-3 = opaque read-barrier orthogonality to blackhole mode only (authority
  line 452); escape-working-under-both-blackhole-modes is a D-5 / I-4 property
  (D-5 "Confirmed flavor-independent", line 305; I-4 line 455). Acceptance 3
  and the blackhole-mode risk now cite D-5 / I-4, not I-3. Keep this mapping
  when writing or citing invariants in code/tests.
- No `SCOPE_QUESTION:` items were present; both design steps were in-scope
  (clarification + traceability) and changed no scope boundary.

## Plan-ambiguity design-step resolutions (applied to plan.md + steps.md)

- Escape shape settled: each `:escape` is a `let` **binding pair**
  `[_ (blackhole/consume cell-sym)]` (ignored name → even binding count),
  spliced into the measured fn's `let` binding vector after `:init` and
  before `start#`. `:escape` is a flat binding-pair vector, same shape as
  `:init`/`:read`. Slice 2 structural test still matches "one `consume` of
  each cell symbol in setup, before `start#`, none in the loop".
- Both-modes mechanism settled: tests are mode-agnostic (no mode-specific
  assertions, no `with-redefs` of `detected-mode`); both-mode coverage is
  process/CI-level. Confirmed CI fact: `.github/workflows/tests.yml`
  compiler-blackhole step is narrowly
  `--focus criterium.measured-test/opaque-behaviour-test` on JVM 21 — the
  new depth-scaling deftest(s) are NOT auto-covered. Resolution: put the
  depth-scaling tests in a named deftest and widen that step's `--focus` to
  include it (or add them under `opaque-behaviour-test`). No new alias
  needed; `:blackhole` already supplies the `-XX:CompileCommand` flag that
  (with JVM 17+) flips `detected-mode` to `:compiler`.

## Notes for addressing the CI-placement design-step

Principles to hold:
- The depth-scaling deftest must always run (no `compiler-blackhole-enabled?`
  guard, no mode-specific skip). Mode-agnostic means: same assertions in both
  the runtime (default-JVM) and compiler (`:blackhole`) runs. Do NOT fold it
  into `opaque-behaviour-test`, whose assertions are compiler-only by design
  (runtime consume cost ~3 ns swamps its subject/control separation — a
  deliberate guard, not a bug to copy).
- Keep the runtime half real: the one-time setup escape makes per-iteration
  cost mode-independent (just the opaque read), so the depth ratio is
  observable under runtime mode too — that is why the standalone deftest can
  stay unguarded where `opaque-behaviour-test` could not.

Practical details not yet in plan.md/steps.md:
- CI step to widen: `.github/workflows/tests.yml` lines ~130-135, step
  "opaque barrier under compiler blackhole":
  `clojure -M:kaocha:dev:test:blackhole :all --focus
  criterium.measured-test/opaque-behaviour-test --reporter dots` (JVM 21,
  `if: matrix.with-agent == false`). Add the new deftest id to that run —
  confirm kaocha's multi-focus syntax (repeat `--focus <id>`) before editing.
- `opaque-behaviour-test` guard pattern to mirror-but-invert lives at
  `measured_test.clj` ≈line 1184 (`(if (blackhole/compiler-blackhole-enabled?)
  <assertions> <skip asserting :runtime>)`); the new deftest wants the
  assertion branch only, unconditionally.

## CI-placement design-step resolution (applied to plan.md + steps.md)

- CI-placement settled: the false equivalence ("add under
  `opaque-behaviour-test`" OR "widen `--focus`") is removed from plan.md
  Slice 3 CI wiring and steps.md Slice 3. Both now state the depth-scaling
  tests MUST live in their own standalone, always-running deftest — not
  inside `opaque-behaviour-test` — and compiler-mode coverage is obtained by
  widening the compiler-blackhole step's `--focus` to also name that
  standalone deftest.
- Implementer, before editing `.github/workflows/tests.yml`: the step to
  widen is "opaque barrier under compiler blackhole" (lines ≈130-135,
  `if: matrix.with-agent == false`, JVM 21):
  `clojure -M:kaocha:dev:test:blackhole :all --focus
  criterium.measured-test/opaque-behaviour-test --reporter dots`. Confirm
  kaocha's multi-focus syntax (repeat `--focus <id>`) before editing, then
  add the new standalone deftest id. The default-JVM suite run already gives
  the runtime half with no CI change (the deftest is unguarded).

## Design-review convergence (second pass)

- A second full design-review session (architecture → ambiguity →
  inconsistency) was run and found no new actionable feedback: the review set
  is stable/converged. The two existing design-steps (per-kind oracle,
  I-3 traceability) already capture every architecture/ambiguity/inconsistency
  finding and are resolved. The design-step-addressing task should not expect
  further review churn from this design as written.
- Deliberately-not-filed items (confirmed again, do not re-file): the relative
  depth-ratio oracle's tolerance is a design decision, not an ambiguity to fix
  with a hardcoded bound; the over-escaping Risk's "store-to-load forwarding"
  wording is an imprecise rationale, not build/verify-affecting.

## Implementation pass 1 (all slices)

- Codegen: `opaque-cell` returns `:escape ['_ (criterium.util.blackhole/consume cell)]`;
  `arg-bindings` accumulates `:escape` (empty-metas path returns
  `{:init [] :read [] :escape []}`); `measured-expr-fn` splices `~@escape`
  right after `~@init`, before `start#`.
- Tests added (measured_test.clj): `opaque-escape-structure-test`
  (structural, kinds filtered via `cell-kind`), `opaque-depth-scaling-test`
  (standalone, unguarded, ratio > 1.5), `opaque-no-per-iteration-allocation-test`
  (agent; n=1 vs n=10000 count delta < 100 — tracing shows a few objects of
  constant noise, so exact equality is flaky). Compile-time boxing of the
  escape is covered by existing `opaque-no-boxing-test` (compiles whole form).
- CI: kaocha accepts repeated `--focus`; compiler-blackhole step now also
  focuses `opaque-depth-scaling-test` (verified locally: 2 tests run).
- Deviation (reference oracle): a pure linked field-read chain does NOT scale
  with depth even when escaped (JDK 25 arm64, compiler mode: depth 1/3/8 all
  ~0.22 ns; runtime ~0.59 ns). Independent iterations overlap the loads.
  Without escape it is ~0.072 ns (folded), so escape works but chain depth is
  not an oracle. Replaced with `DoubleNode` field read → `Math/sqrt` chain.
- Teeth proof (escape emission removed): double d1/d3 = 0.0547/0.0547 ns
  (compiler), 0.579/0.578 ns (runtime); reference same pattern; test fails in
  both modes. With escape: double 0.43/1.31 (compiler), 0.63/1.31 (runtime).
- Numbers changed: reference-arg per-call cost now ~0.2-0.35 ns (was quoted
  0.11 ns, a folded residue). Primitive ~0.07-0.15 ns. Updated `measured/expr`
  docstring and README; both also mention the one-time cell publication.
- Mode dependency: compiler mode relies on HotSpot's blackhole intrinsic
  treating the argument as escaped; runtime mode stores into a volatile field
  (genuine publication, cleared by `evaporate`). Both confirmed by teeth run.
- Verification: full suite 1091 tests green (runtime); measured suite green
  with agent and under `:blackhole`.

## Implementation review

- added 0 steps: implementation matches the design and architecture. Escape
  emitted once, after init, before `start#`, none in the loop. Verified green:
  structural + fallback + mixed-args + boxing + reference-no-reflection
  (runtime); depth-scaling oracle under both runtime and compiler blackhole;
  per-iteration allocation under the native agent (real teeth). No new-pattern,
  abstraction, or structural-performance issue found.

## Test-shaper review

- added 3 steps (test shaping): redundant per-call warmup around
  `best-ns-per-call`, duplicated depth-oracle structure across the two
  `opaque-depth-scaling-test` sub-tests, and bare magic-number gates (`1.5`
  ratio, `100`-allocation tolerance) whose failure messages omit the
  violated contract. All are clarity/economy shaping; no correctness or
  coverage defect — the behavioural oracle, teeth proof, structural escape
  check and allocation guard are sound.

## Test-shaper follow-up (addressed)

- addressed 3 test-shaper review steps:
  - dropped the redundant discarded `_ (best-ns-per-call ...)` warmup lines
    at the `opaque-depth-scaling-test`/`opaque-behaviour-test` call sites;
    `best-ns-per-call` warms up internally (noted in its docstring).
  - extracted `assert-depth-scales` (depth-1/depth-3 measureds + n), so the
    double-cell and reference-cell sub-tests differ only in their subjects.
  - named the magic numbers `depth-scaling-min-ratio` (1.5) and
    `max-constant-allocation-noise` (100); both failure messages now name
    the violated gate (required ratio / tolerance), not just raw numbers.
  - added `(double ...)`/`(long ...)` hints on the new named constants to
    keep the boxed-math-warning-free compile.
- verification: full measured suite 37 tests / 450 assertions green
  (runtime mode); focused depth/allocation/behaviour tests green. No
  correctness or coverage change — pure clarity/economy shaping.

## Test-shaper review (second pass)

- added 3 steps: the clarity/economy shaping applied to
  `opaque-depth-scaling-test` in the first pass was not carried over to the
  sibling `opaque-behaviour-test` — it still has five verbatim-duplicated
  arrange/act/assert sub-tests, a bare `0.5` hoisting-floor whose failure
  message omits the violated ratio, and a `n 10000000` literal repeated six
  times across the two behavioural tests. No correctness or coverage
  defect — the behavioural guard, teeth proof, structural check and
  allocation guard remain sound.

## Test-shaper review (second pass) — addressed

- addressed 3 review steps: extracted `assert-not-hoisted` so the five
  `opaque-behaviour-test` sub-tests differ only in subject/control;
  named `hoisting-floor-ratio` (0.5) with a gate-stating failure message;
  hoisted `behaviour-eval-count` (10000000) used by both behavioural tests.
- verification: full measured suite 37 tests / 450 assertions green
  (runtime mode). Pure clarity/economy shaping; no correctness or
  coverage change.

## Test-shaper review (third pass)

- added 2 steps: the allocation guard
  (`opaque-no-per-iteration-allocation-test`), untouched by the first two
  passes, still carries bare eval-count literals (`1`/`10000`) that give
  `max-constant-allocation-noise` its teeth yet can silently drift from the
  hard-coded "n=1 → n=10000" failure-message prose, and bare `1000` warmup
  literals with no stated intent — the same magic-number and
  explicit-warmup shaping the earlier passes applied to the sibling
  behavioural tests but not here. No correctness or coverage defect.

## Test-shaper review (third pass) — addressed

- addressed 2 review steps: named the allocation-probe eval counts
  `alloc-baseline-count` (1) and `alloc-probe-count` (10000) — the noise-
  constant comment and the `is` failure message now derive from them (no
  prose/measurement drift); named the two warmup `1000` literals
  `alloc-warmup-iterations` and `alloc-warmup-eval-count` (both documented)
  so the warmup's intent is visible at the call site.
- verification: full measured suite 37 tests / 450 assertions green
  (runtime mode); focused allocation test green. Pure clarity/economy
  shaping; no correctness or coverage change.

## Test-shaper review (fourth pass)

- added 1 step: `opaque-escape-structure-test` re-enumerates the full tag
  literal and only filters it with `cell-kind`, so its covered opaque-
  eligible kinds are not derived from the taxonomy the design names as the
  single source of truth — a new eligible kind would be silently uncovered
  with the test still green. Non-compliance with the design/plan/steps
  "driven from `cell-kind`, not a re-enumerated tag list" instruction. No
  correctness defect in current behaviour; the structural/behavioural/teeth/
  allocation guards remain sound.

## Test-shaper review (fourth pass) — addressed

- addressed 1 review step: made `opaque-escape-structure-test` taxonomy-
  driven instead of re-enumerating tags. Introduced
  `PRIMITIVE-CELL-TAGS` (named set, single source for `cast-fn`) and derived
  `impl/opaque-eligible-tags-by-kind` (map of each non-`:fallback` kind to a
  representative tag, built from `PRIMITIVE-CELL-TAGS` + `java.lang.String`
  via `cell-kind`). The structural test loops that map, so adding an opaque-
  eligible kind extends coverage automatically; fallback tags stay an
  explicit negative list.
- verification: focused structural + cell-kind tests green (1 test, 59
  assertions); full measured suite 37 tests / 440 assertions green (runtime
  mode). Pure test-coverage/clarity shaping; no behaviour change.

## Test-shaper review (fifth pass)

- added 1 step: the fourth-pass taxonomy-drive fix landed on
  `opaque-escape-structure-test` but not its sibling
  `opaque-structure-test`, which still re-enumerates
  `'[long int double float java.lang.String nil]`. The two parallel
  structural tests now disagree on their kind source — one derived, one
  hard-coded — so a new opaque-eligible primitive gains escape-structure
  coverage automatically yet keeps no ctor/read-structure coverage, with the
  test still green: the same false-confidence gap, left on the sibling. No
  correctness or coverage defect in current behaviour.

## Test-shaper review (fifth pass) — addressed

- addressed 1 review step: drove `opaque-structure-test` from
  `impl/opaque-eligible-tags-by-kind` (one representative tag per non-
  `:fallback` kind) instead of the re-enumerated
  `'[long int double float java.lang.String nil]` literal, matching the
  fourth-pass fix on the sibling `opaque-escape-structure-test`. The two
  structural tests now share one taxonomy source, so a new opaque-eligible
  kind extends both ctor/read-structure and escape-structure coverage
  automatically.
- verification: focused `opaque-structure-test` + `opaque-escape-structure-test`
  green (2 tests, 139 assertions, runtime mode). Pure test-coverage/clarity
  shaping; no behaviour change.

## Test-shaper review (sixth pass)

- added 0 steps: the suite is converged. Reviewed the whole test file against
  the test-shaper skill — duplication is extracted (`assert-depth-scales`,
  `assert-not-hoisted`, `assert-opaque-structure`), gates are named with
  gate-stating failure messages, warmup intent is documented, eval-counts are
  hoisted, and both structural tests are taxonomy-driven. The only couplings
  scrutinized (`cell-escape?`'s `-cell\d+$` regex and the hard-coded FQ
  `consume` symbol) are cross-validated against the `cell-ctor?`-based
  `cell-syms`, so they carry no signal gap. 37 tests / 424 assertions green
  (runtime mode).

## review-task-docs review

- added 2 steps (user-facing docs): the `CHANGELOG.md` `[Unreleased]`
  opaque-barrier entry was not updated by Slice 5 — it keeps the stale
  pre-escape reference cost (0.11 ns, now 0.2-0.35 ns in README + docstring)
  and omits the one-time cell publication (escape) that is this task's
  user-visible change. README.md and the `measured/expr` docstring are
  accurate and mutually consistent.

## review-task-docs follow-up (addressed)

- addressed 2 review steps: updated the `CHANGELOG.md` `[Unreleased]`
  opaque-barrier entry to quote the post-escape reference cost
  (0.2-0.35 ns, was 0.11 ns), keeping the ~25-30% variance note, and added
  the one-time cell publication + sub-ns non-folded-time behaviour change.
  CHANGELOG now matches README and the `measured/expr` docstring.

## review-task-docs review (second pass)

- added 0 steps: user-facing docs are converged. README, `CHANGELOG.md`
  `[Unreleased]`, and the `measured/expr` docstring agree on the one-time
  cell publication / escape-analysis framing, the per-argument costs
  (0.07 ns primitive, 0.2-0.35 ns reference, ~25-30% variance), and the
  fallback types (array, `boolean`/`byte`/`short`/`char`, `measured/callable`).
  No stale pre-escape 0.11 ns figure remains. `measured` defers cost to
  `expr`. No `doc/` or `ramora/` directory exists.

## Code-shaper review

- added 0 steps: the escape codegen is simple, consistent, and robust.
  `opaque-cell` factors the single `:escape` pair once via `assoc` across all
  `case` branches (no per-kind duplication); `:init`/`:read`/`:escape` share
  one flat binding-pair shape; `arg-bindings` and `measured-expr-fn` keep
  computation and flow separate. The eligibility⟺cell invariant is structural
  (single `cell-kind` taxonomy, local `assert` on the fallback contract) and
  the I-4 setup-escape placement is enforced by the structural tests. No new
  simplicity/consistency/robustness defect in the code this task introduced.
