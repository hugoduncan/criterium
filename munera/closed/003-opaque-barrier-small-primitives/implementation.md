# Implementation notes

- Architectural re-review (design-review, post-follow-up): no new
  architectural-fit feedback. The prior D-3 contradiction is resolved — the
  authority `design/opaque-input-barrier.md` carries the `amendedBy: task 003`
  note and the corrected small-primitive rationale, and the current design's
  taxonomy/union approach aligns with the single-source-of-truth and
  minimal/open-to-extension principles.
- Ambiguity re-review (design-review, post-follow-up): no new ambiguity
  feedback. The prior taxonomy source-of-truth ambiguity is resolved — the
  design names `SMALL-PRIMITIVE-CELL-TAGS` and specifies both `cell-kind` and
  `opaque-eligible-tags-by-kind` read the union of the two primitive source
  sets; the per-tag store/read forms are fully specified.
- Inconsistency re-review (design-review, post-follow-up): no new
  inconsistency feedback. Verified against `impl.clj`: `OTHER-PRIMITIVE-TAGS`
  is exactly `#{boolean byte short char}`, `cast-fn` returns `nil` for them
  (routed through `binding-with-hint-or-cast`'s type-hint branch), and
  `opaque-eligible-tags-by-kind` = `(conj PRIMITIVE-CELL-TAGS 'java.lang.String)`
  — all matching the design's description of present and target state. The
  prior `cast-fn` wording inconsistency is resolved.
- Design-review re-run closed with no new design-steps: all three turns
  (architecture, ambiguity, inconsistency) confirmed the first-cycle
  follow-ups (the three `[x]` items in design-steps.md) fully resolved their
  findings. Those three items remain the complete, actionable set for the
  implementation task; no further design follow-ups arose. The "For the task
  addressing the design-steps" section below stays authoritative for relevant
  project-file paths and the principles to hold while implementing.
  Architecture authority is `design/opaque-input-barrier.md`; the named
  `ramora/META.md` and `doc/architecture.md` do not exist in this repo.
  Finding: the task moves `boolean`/`byte`/`short`/`char` onto the opaque
  path, contradicting authority decision D-3 ("no unboxed atomic cell …
  non-opaque fallback"), but does not provide for amending D-3, so the
  architecture record would contradict the shipped code. Classified
  actionable because D-3 is named firm authority and the ramora model relies
  on decisions staying truthful; amending it is not excluded by the design's
  "Out of scope" list.
- Ambiguity review (design-review turn 2): added 1 design step. The design's
  taxonomy claims (new kinds in `cell-kind`; `opaque-eligible-tags-by-kind`
  extends "automatically from the taxonomy"; `cast-fn` excludes these tags)
  leave the single-source-of-truth set for the four new kinds unspecified,
  because the existing taxonomy derives kinds through `cast-fn`/
  `PRIMITIVE-CELL-TAGS` direct casts these types do not use. Actionable:
  implementers would build divergent structures.
- Inconsistency review (design-review turn 3): added 1 design step. The
  Approach's `cast-fn` claim misdescribes `impl.clj` — `cast-fn` is driven by
  `PRIMITIVE-CELL-TAGS` and never matched these four tags; the fallback that
  handles them today is `binding-with-hint-or-cast`. The taxonomy
  source-of-truth contradiction is already covered by the turn-2 ambiguity
  item, so it was not re-filed.

## For the task addressing the design-steps

- Relevant project (non-task) files:
  - `bases/criterium/src/criterium/measured/impl.clj` — taxonomy
    (`PRIMITIVE-CELL-TAGS`, `cast-fn`, `cell-kind`, `opaque-tag?`,
    `opaque-eligible-tags-by-kind`, `OTHER-PRIMITIVE-TAGS`), `opaque-cell`
    `case`, `arg-bindings`, `binding-with-hint-or-cast`, `measured-expr-fn`.
  - `bases/criterium/test/criterium/measured_test.clj` — existing anti-hoist
    and structural tests to mirror for the new kinds.
  - `design/opaque-input-barrier.md` — architecture authority (D-3 is the one
    to amend).
- The turn-2 (taxonomy source-of-truth) and turn-3 (`cast-fn` wording) steps
  are coupled: deciding the single source set for the new kinds settles the
  `cast-fn` wording too. Resolve them together, not independently.
- Principles to keep while resolving: no boxing/reflection on the read path
  (I-2); store-once-outside-loop / read-in-loop (I-1); each cell escapes once
  at setup (I-4); keep eligibility ⟺ cell invariant structural (single source
  of truth, do not re-enumerate tags across functions); minimal design, open
  to extension; no backward-compat obligation.

## Design-steps execution (design-review follow-up)

- All three design-steps were actionable (no `SCOPE_QUESTION:` items); none
  deferred.
- D-3 amendment (step 1) was recorded in the authority file
  `design/opaque-input-barrier.md` (an `amendedBy: task 003` note plus the
  corrected "chosen" rationale), not in this task's design.md. The authority
  still asserts the `Atomic*`/`getOpaque` choice (D-003 triple unchanged);
  only the small-primitive rationale changed.
- Steps 2+3 settled in design.md's Approach as one decision: name the new
  source set `SMALL-PRIMITIVE-CELL-TAGS` (`boolean`/`byte`/`short`/`char`),
  renaming/repurposing the current `OTHER-PRIMITIVE-TAGS`. Both `cell-kind`
  and `opaque-eligible-tags-by-kind` read the union of `PRIMITIVE-CELL-TAGS`
  and `SMALL-PRIMITIVE-CELL-TAGS`. These tags deliberately stay out of
  `PRIMITIVE-CELL-TAGS` so `cast-fn` keeps returning `nil` for them.
- Implementation detail to carry over: `OTHER-PRIMITIVE-TAGS` is currently
  `^:private`, while `PRIMITIVE-CELL-TAGS` is `^:no-doc`. The renamed
  `SMALL-PRIMITIVE-CELL-TAGS` should parallel `PRIMITIVE-CELL-TAGS`'
  visibility so `cell-kind`/`opaque-eligible-tags-by-kind` (and structural
  tests driving from them) can read it uniformly.
- `opaque-eligible-tags-by-kind` is built today as
  `(conj PRIMITIVE-CELL-TAGS 'java.lang.String)`; the implementer must union
  in `SMALL-PRIMITIVE-CELL-TAGS` so all four new kinds appear.

## Plan-review (ambiguity turn)

- Ambiguity review (plan-review turn 1): no ambiguity review feedback. The
  plan and steps specify each slice concretely (taxonomy rename/visibility,
  `cell-kind`/`opaque-eligible-tags-by-kind` union, the four `opaque-cell`
  clauses, dispatch). Remaining per-kind test-subject choices (no-boxing and
  anti-hoisting exprs for the four kinds) are normal implementer discretion
  within the established int/float mirror pattern and the ratio-based
  `assert-not-hoisted`/acceptance gate, not actionable ambiguities.
- Verified (loaded during review) the plan's Risks are non-issues: compiling
  the specified boolean store `(AtomicInteger. (if b 1 0))` and read
  `(not (zero? (.getOpaque ^AtomicInteger c)))` under
  `*warn-on-reflection* true` + `*unchecked-math* :warn-on-boxed` emits no
  reflection or boxed-math warnings. So the boolean store needs no extra
  `(int ...)` wrapper and the read boxes nothing; the forms in design.md /
  steps.md are correct as written.
- Inconsistency review (plan-review turn 2): no inconsistency review
  feedback. design.md ↔ plan.md ↔ steps.md agree on cell store/read forms,
  the `opaque-eligible-tags-by-kind` union, the rename+visibility change, the
  `:fallback`→arrays reclassification, and dispatch; all match `impl.clj`.
  Lone non-actionable nuance: plan.md's round-trip bullet says "negative
  shorts" where steps.md lists `Short/MIN_VALUE`/`MAX_VALUE` — same intent,
  steps.md is the fuller boundary list to follow.

## For the implementation task (post plan-review)

- Plan-review added no new design-steps: the actionable set stays the three
  `[x]` items in design-steps.md, already resolved into design.md/plan.md/
  steps.md. Implement against plan.md's slice order; the "For the task
  addressing the design-steps" principles and file paths above still hold.
- Carry-over from this review (no re-verification needed): the slice-2 cell
  forms are usable verbatim — boolean store `(AtomicInteger. (if arg 1 0))`
  needs no `(int …)` wrapper and read `(not (zero? (.getOpaque …)))` emits no
  boxed-math/reflection warning (checked under `*warn-on-reflection*` +
  `*unchecked-math* :warn-on-boxed`). Spend the slice-2 warning check on
  confirming the generated read path, not on reworking these forms.

## 2026-10-21 — slices 1–3 implemented

- `OTHER-PRIMITIVE-TAGS` → `^:no-doc SMALL-PRIMITIVE-CELL-TAGS`; `cell-kind`
  maps it to `(keyword tag)`; `:fallback` is arrays only. Four `opaque-cell`
  clauses added verbatim from design.md. No stragglers in code/docs;
  `design/opaque-input-barrier.md` already names the added types.
- Tests: `opaque-tag-test` driven by the two tag sets; fallback tag lists in
  escape/fallback/rejection tests now array tags only; exact round-trip over
  boundary values; no-boxing check extended to the four kinds; four
  anti-hoisting cases in `opaque-behaviour-test`.
- Discovery: `measured/expr` factors a `(long x)` subform into an Object
  argument (boxed-math warning at the call site). Small-primitive subjects use
  `(unchecked-inc-int (int x))` instead.
- Discovery: the anti-hoisting tests do NOT discriminate on this JVM. With
  `cell-kind` reverted to `:fallback` for the small set, both cheap
  (`unchecked-inc-int`) and sqrt-chain subjects cost the same as with the
  barrier. The fallback binds a boxed arg from the args vector, unboxed per
  iteration, and C2 does not hoist it. Structural tests (opaque-tag,
  opaque-structure, opaque-escape) do fail on that revert, so they carry the
  regression signal. Behavioural tests remain as floor guards.
- Verified: `criterium.measured-test` 37 tests / 560 assertions green under
  default and `:blackhole` aliases.

## Implementation review

- Added 1 follow-up step: the per-iteration allocation test is not extended
  to the four new small-primitive kinds (I-2 / boolean-boxing risk).

## 2026-10-21 — addressed 1 implementation-review follow-up

- Extended `opaque-no-per-iteration-allocation-test` to `:byte`/`:short`/
  `:char`/`:boolean`. Byte/short/char subjects use `(unchecked-inc-int (int
  x))` (same mirror as the behavioural/no-boxing tests); the `:boolean`
  subject is `(if ^boolean bl 1 2)`, exercising the `(not (zero? ...))` read.
- Verified with the native agent attached (`:with-agent-mac`): all 8
  allocation assertions pass, so the `:boolean` read's canonical
  `Boolean/TRUE`/`FALSE` adds no per-iteration allocation (plan Risks
  resolved at runtime, not only compile time).
- Full `criterium.measured-test` suite green (37 tests / 556 assertions).
- The "Follow-up (discovered)" item stays unchecked: it is optional, not
  acceptance-required, and was added in-session (not by a review pass), so
  out of scope for this follow-up run.

## 2026-10-21 — implementation review (second pass)

- Reviewed code, tests, and authority against design/plan/acceptance; tests
  re-run green (37/556). No new actionable issues; added no steps.

## 2026-10-21 — test review (task-test-review)

- Added 2 steps: the `byte`/`short`/`char` subjects in both
  `opaque-behaviour-test` and `opaque-no-per-iteration-allocation-test`
  route through the reference cell, not their small-primitive cells (verified
  the generated expr-fn), so those named per-kind tests do not cover the
  paths they name.

## 2026-10-21 — addressed 2 test-review follow-ups

- Rewrote the `byte`/`short`/`char` subjects (and controls) in both
  `opaque-behaviour-test` and `opaque-no-per-iteration-allocation-test` to put
  the bare hinted symbol directly as the op argument:
  `(unchecked-inc-int ^byte x)`, `(unchecked-inc-int ^short x)`,
  `(int ^char x)`. A `(int x)` subform was factored into a separate captured
  arg that `capture-arg-types` could not widen (only `Long`/`Double`),
  defaulting to `:reference`.
- Verified the generated expr-fn now routes each through its `AtomicInteger`
  cell with the matching `unchecked-*` narrow (AInt=1, u-byte/u-short/
  u-char=2), no boxing/reflection warnings on any subject or control.
- Full `criterium.measured-test` green under `:blackhole` + `:with-agent-mac`
  (37 tests / 564 assertions): behaviour tests ran on the compiler-blackhole
  path (not the skip branch) and allocation tests ran with the native agent.

## 2026-10-21 — test review (task-test-review, pass 2)

- Added 1 step: the `:boolean` behaviour-test control routes through the
  reference cell (unhinted `deref`), not the boolean cell, unlike the
  byte/short/char controls — a test-symmetry gap, not a correctness defect.

## 2026-10-21 — addressed 1 test-review follow-up (pass 2)

- Hinted the `:boolean` behaviour-test control
  `(if ^boolean (deref ^clojure.lang.Volatile v) 1 2)` so subject and control
  both traverse the boolean `AtomicInteger` cell; symmetric with the
  byte/short/char sub-tests.
- Full `criterium.measured-test` green under `:blackhole` + `:with-agent-mac`
  (37 tests / 564 assertions).

## 2026-10-21 — test review (task-test-review, pass 3)

- Added 2 steps: no deterministic `capture-arg-types` assertion for
  small-primitive hint preservation (the contract this task made load-bearing,
  mis-routed twice); and the `opaque-mixed-args-test` round-trip block does not
  discriminate the small-primitive cell from the reference fallback.

## 2026-10-21 — addressed 2 test-review follow-ups (pass 3)

- `capture-arg-types-test`: added a deterministic sub-test driving
  `boolean`/`byte`/`short`/`char` user hints on a local symbol, mirroring the
  existing `long`/`String` cases. Pins the hint-preservation contract this
  task made load-bearing, without depending on the timing-/agent-gated
  behaviour and allocation tests. Var green (9 assertions).
- `opaque-mixed-args-test`: the small-primitive round-trip block is a
  round-trip guard, not a routing guard (value preservation does not
  discriminate the `AtomicInteger` cell from the `AtomicReference` fallback).
  Added a comment pointing at the routing guards (`capture-arg-types-test`,
  `opaque-tag-test`, `opaque-structure-test`) so the block is not mistaken for
  one. Var green (20 assertions).

## 2026-10-21 — test review (task-test-review, pass 4)

- No new steps. Verified the pass-1..3 follow-ups are all resolved in the
  current tests and the suite is green (37/560). Checked the five
  task-test-review criteria: taxonomy-derived structural coverage, routing
  guards (`capture-arg-types-test`, `opaque-tag-test`, `opaque-structure-*`),
  round-trip boundary values, no-boxing (the compiled boolean read
  `(not (zero? (.getOpaque cell)))` is in the generated fn and emits no
  boxed/reflection warning), and no-per-iteration-allocation — all injected
  state, no mocks, no test-only production paths. The transient
  `Numbers.isZero(Object)` boxed-math line on a cold run is a first-compile
  `*err*`-capture quirk, not a production defect (direct compile of the real
  boolean read is clean).

## 2026-10-21 — test review (test-shaper, pass 5)

- No new steps. Re-verified suite green (37/560) and confirmed the pass-1..4
  follow-ups remain resolved. Against the test-shaper lens (simple ∧
  consistent ∧ robust ∧ economical): single-concern tests, taxonomy-derived
  structural coverage, routing guards, boundary round-trip, no-boxing and
  no-per-iteration-allocation probes, meaningful failure messages, and the
  explicit compiler-blackhole skip for the timing-gated behaviour test — all
  in good shape. The literal tag/expr enumeration in `opaque-no-boxing-test`
  and the subject enumeration in `opaque-no-per-iteration-allocation-test` are
  per-kind payload (the differing subject/read form is irreducible, like the
  `opaque-cell` `case`), not avoidable re-enumeration; the eligibility ⟺ cell
  invariant is still driven from the taxonomy elsewhere. No non-compliance.

## 2026-10-21 — docs review (review-task-docs)

- Added 2 steps. README.md and CHANGELOG.md both still state the four
  small-primitive types use a plain binding without the opaque read; the task
  moved them onto the opaque path but updated only the architecture authority
  (`design/opaque-input-barrier.md`), not these user-facing docs. No
  `ramora/` directory exists; `docs/quickstart.qmd` is accurate (does not
  enumerate the excluded types).

## 2026-10-21 — docs review follow-ups addressed

- addressed 2 review steps (docs review): corrected the stale "plain binding"
  claim for `boolean`/`byte`/`short`/`char` in `README.md` and `CHANGELOG.md`.
  Both now state every primitive argument reads through the opaque barrier,
  leaving only array-typed and `measured/callable` arguments on the plain
  binding.

## 2026-10-21 — docs review (review-task-docs, pass 2)

- No new steps. Re-verified user-facing docs against `impl.clj`: `README.md`
  and `CHANGELOG.md` correctly state every primitive argument (incl.
  `boolean`/`byte`/`short`/`char`) reads through the opaque barrier, with only
  array-typed and `measured/callable` arguments on the plain binding — matching
  `cell-kind` (`:fallback` = arrays only) and the separate `measured-callable`
  path. `docs/quickstart.qmd` describes the mechanism generically (no excluded-
  type enumeration), so it is accurate; no `ramora/` directory. No other stale
  references (`ReleaseNotes.md`, `README.ALPHA.md` clean).

## 2026-10-21 — code-shaper review

- Added 2 steps: `cast-fn` docstring typo ("givent"); `cell-kind` uses two
  idioms (`(keyword cast)` vs `(keyword tag)`) for one keyword-from-tag
  operation. Both cosmetic; the four-clause `opaque-cell` `case`, the
  taxonomy union, and dispatch are otherwise well-shaped (single source of
  truth, per-kind payload irreducible).

## 2026-10-21 — code-shaper follow-up

- addressed 2 code-shaper review steps: fixed `cast-fn` docstring typo
  ("givent" → "given"); unified `cell-kind` to use `(keyword tag)` in both
  the primitive and small-primitive branches (`cast` ≡ `tag`), so the
  like-named-kind invariant reads uniformly. Measured suite green (37 tests /
  560 assertions).
