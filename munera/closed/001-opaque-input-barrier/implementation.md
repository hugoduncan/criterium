# Implementation notes — 001-opaque-input-barrier

Provenance:
- issue: #155
- design authority: `design/opaque-input-barrier.md` (ramora D-1..D-4)
- evidence commits on `develop`: 27264e9, d6d1354

Design/refinement notes:
- Opaque is unconditional with per-argument fallback; no `:opaque-args` flag
  (D-1). The non-opaque number for cheap pure exprs is misleading, so it must
  not be the default.
- Mutable place is `java.util.concurrent.atomic` (D-3): no Java shim, no
  varargs boxing; `AtomicLong`+bits covers `double`/`float`.
- Barrier stays scalar, one cell per argument (D-2); pool is #158.
- Barrier cost is a documented artifact, never subtracted (D-4; #19).
- Root cause is final-field constant folding, not just generic LICM — an
  output barrier cannot fix it; the input reference must be made opaque.
- Only `criterium.measured.impl` changes; no new namespace/base/Java source.

Review notes:
- no architectural review feedback (design fits minimal/open-extension,
  source-sink separation, and the single-base boundary; architecture
  authority is `design/opaque-input-barrier.md` — `ramora/META.md` and
  `doc/architecture.md` do not exist in this repo).
- ambiguity review added 1 design step: cell-selection table omits the
  reachable primitive tags `boolean`/`byte`/`short`/`char` (producible by
  `tag-meta`), leaving the predicate's reference-vs-fallback boundary
  under-specified for them.
- inconsistency review added 1 design step: design.md maps `float` to
  `AtomicInteger`, but authority D-3 says floating point uses `AtomicLong`;
  authority evidence never measures `float`/`int`, so the choice is unresolved.

Notes for addressing the design-steps:
- Relevant code: `bases/criterium/src/criterium/measured/impl.clj` —
  `cast-fn` (only `#{long int double float}`), `binding-with-hint-or-cast`
  (the non-opaque fallback), `tag-meta`/`TYPE-NAME-CONVERSIONS` (only the
  four boxed types remap; other primitive classes pass through as their
  canonical name, so `:tag` can be `byte`/`short`/`char`/`boolean`),
  `capture-arg-types`, `measured-expr-fn`.
- Authority D-3 text: `design/opaque-input-barrier.md` lines 200-201.
- The ambiguity step likely resolves to non-opaque fallback: authority D-1
  already lists "unknown type that would otherwise box" as a fallback case,
  which is exactly the non-{long,int,double,float} primitive tags (they would
  box in an `AtomicReference`). Align the predicate with that intent.
- The float inconsistency is a design choice, not empirical (section 8 never
  measured `float`); `floatToRawIntBits` yields an `int`, so `AtomicInteger`
  is the natural fit, but reconcile the chosen document either way.
- Principles to hold: keep cell selection a code-gen-time `:tag` predicate in
  `measured.impl` only; preserve I-1 (store once, read in loop) and I-2 (no
  boxing on primitive reads); do not widen scope. `AGENTS.md` `¬compat(backward)`
  means either design.md or the authority may be changed to reconcile.

Design-step resolutions (both design-steps now checked):
- Opaque-eligibility predicate boundary is now explicit in design.md: opaque
  iff `:tag` ∈ {long,int,double,float} ∨ reference-class ∨ no-tag; non-opaque
  fallback for {boolean,byte,short,char} and array tags. Implementation point:
  the predicate must mirror `cast-fn`'s `#{long int double float}` set for the
  primitive branch; the extra primitive tags need no `cast-fn` change (they
  already fall through to `binding-with-hint-or-cast`).
- `float` reconciled to `AtomicInteger` + `Float/floatToRawIntBits`/
  `intBitsToFloat` in BOTH design.md and the authority. The authority file
  `design/opaque-input-barrier.md` D-3 was edited (not only design.md); a
  reviewer comparing the two documents will find them consistent now. Choice
  is by unboxed-fit rule (int-sized bits → int-sized cell), not a new section
  8 measurement.

Design-review session (architecture turn):
- architectural re-review: no new feedback — design stays aligned (minimal/
  open-extension, source/sink separation, criterium-base-only boundary, pure
  JDK, reuses arg-metas/:tag + binding-with-hint-or-cast fallback). Authority
  remains `design/opaque-input-barrier.md`; `ramora/META.md` and
  `doc/architecture.md` still absent. No scope boundary misfit found.

Design-review session (ambiguity turn):
- ambiguity re-review: no new feedback. The two real ambiguities (primitive-tag
  predicate boundary; `float` cell type) are already captured and resolved in
  design-steps.md. Remaining open choices are implementation-level (array-tag
  detection, cell-symbol naming, cast-then-bits order) or intentionally tolerant
  (criterion 2 behavioural floor), not actionable design ambiguities.

Plan-review session (ambiguity turn):
- plan-review ambiguity turn: no new ambiguity feedback.
- Non-obvious mechanism for the implementation slice: the callable path stays
  non-opaque because `measured-callable` passes empty `arg-metas` (options `{}`),
  so pairing per-argument iteration with `arg-metas` (as today's
  `(mapcat binding-with-hint-or-cast arg-syms arg-metas)` does) produces zero
  per-arg bindings and leaves the `[:as args]` destructure intact. Do NOT drive
  the predicate off `arg-syms` alone: the untagged whole-args symbol would
  classify as opaque (`no tag → AtomicReference`), wrapping the callable binding
  wrongly. Plan decision 5's "the predicate already yields fallback there"
  rationale is imprecise about this; steps.md's "adjust the predicate or call
  site" guard already covers the action, so no new design-step was added.

Plan-review session (inconsistency turn):
- plan-review inconsistency turn: no new inconsistency feedback.
- Considered but dismissed (non-actionable): plan.md's testing prose says boxing
  is "verified structurally AND via warn-on-reflection/unchecked-math", whereas
  design.md criterion 3 and steps.md say "AND/OR". Not flagged: steps.md (the
  operative checklist) and the design authority agree on "and/or", structural
  verification is mandatory everywhere and is the primary signal, and the compile
  check is listed in steps.md regardless — the plan's stricter prose changes
  nothing in the executable path. Recorded to avoid re-litigation.

Design-review session (inconsistency turn):
- inconsistency re-review: no new feedback. Verified authority D-3 now reads
  `AtomicInteger`+`Float` bits for `float` (matches design.md) — the only real
  inconsistency, already resolved. Cross-checked costs (0.07/0.11 ns, ~4x indexed
  = net-of-baseline ratio vs authority "several times"), invariants (design I-4 =
  authority Boundary), predicate boundary, and platform-facility scope (both rely
  on existing facility, no behavioural change): all consistent.

Design-review session outcome (for the next slice):
- All three turns (architecture, ambiguity, inconsistency) added zero new
  design-steps. Both design-steps.md items remain resolved (checked). Design is
  review-clean — the next lifecycle step is implementation, not further design
  clarification.
- Implementation principle to hold: keep cell-selection a pure code-gen-time
  `:tag` predicate living only in `criterium.measured.impl`
  (`bases/criterium/src/criterium/measured/impl.clj`); the predicate's opaque set
  must mirror `cast-fn`'s `#{long int double float}`, with array + other-primitive
  tags routed to the existing `binding-with-hint-or-cast`. Preserve I-1 (store
  once before `start#`, loop reads only) and I-2 (no boxing on primitive reads);
  do not widen scope. `AGENTS.md ¬compat(backward)` permits editing either
  design.md or the authority to keep them in sync.

Plan-review session outcome (for the next slice):
- Both plan-review turns (ambiguity, inconsistency) added zero new design-steps;
  plan.md and steps.md are review-clean. No open design-steps remain. The next
  step is implementation per plan.md's slice order, not further plan clarification.
- Principle to hold while implementing: drive per-argument cell selection by
  pairing `arg-syms` with `arg-metas` (preserving today's empty-`arg-metas`
  callable fallback); do not classify the whole-vector `[:as args]` binding
  through the predicate.

Implementation pass (slices 1-4, all steps done):
- New in `criterium.measured.impl`: `opaque-tag?` (tag predicate),
  `opaque-cell` (returns `{:init [cell ctor] :read [arg read]}`),
  `arg-bindings` (pairs `arg-syms` with `arg-metas`; empty `arg-metas` on
  the callable path ⇒ no cells, `[:as args]` destructure untouched).
- `measured-expr-fn`: inits in the outer `let` before `start#`; reads
  bound in `val#` via `(let [reads] expr)` and in a fresh `let` per loop
  iteration. Fallback args keep `binding-with-hint-or-cast` in the init.
- `int` read is re-wrapped in `(int ...)`; `long` read is bare
  (`AtomicLong.getOpaque` already returns `long`).
- Array detection: tag `name` ends in `"[]"`. Note `'long[]` in source
  reads as two forms; tests use `(symbol "long[]")`.
- Boxing test evals generated forms with `*unchecked-math* :warn-on-boxed`
  and captures `*err*`; sanity-checked that an untagged arg does warn.
- Behavioural test: record field read vs volatile-deref control, asserts
  subject > 0.1 × control (tolerant). With the runtime blackhole both are
  ≥1 ns, so the test is only discriminating under the compiler blackhole.
- Verified: measured/bench/benchmark/core/platform tests green (47 tests);
  cljfmt clean; the one kondo warning (`forms/unrolled-apply-form`) is
  pre-existing.

Implementation-review session:
- added 2 follow-up steps: the behavioural test passes vacuously under the
  default runtime blackhole, so acceptance criterion 2 is only verified under
  the `:blackhole` alias.

Implementation-review follow-up pass:
- addressed 2 review steps.
- opaque-behaviour-test now guards on `criterium.blackhole/compiler-blackhole-enabled?`:
  runs the discriminating criterion-2 assertion under `:compiler`, skips
  explicitly (asserting `mode` = `:runtime`) otherwise, instead of passing
  vacuously under the runtime blackhole.
- `.github/workflows/tests.yml` adds a JDK-21 step running the focused
  behavioural test under the `:blackhole` alias
  (`clojure -M:kaocha:dev:test:blackhole :all
  --focus criterium.measured-test/opaque-behaviour-test`), then restores
  JDK 11, so criterion 2 is exercised repeatably and a hoisting regression
  fails CI. Verified both the `:runtime` skip path and the `:compiler` run
  path locally (JDK 25).

Implementation-review session (second pass):
- no new steps added. Acceptance criteria 1-7 verified: generated structure
  (cell built once outside, two `getOpaque` reads, no store in loop),
  no-boxing compile, fallback + mixed-arg cases, docstrings/README, and the
  compiler-blackhole CI step are all present and green (32 tests, 267
  assertions; `check-warnings` clean for the namespace).

Test-review session:
- added 1 step: `opaque-no-boxing-test` matches the whole `*err*` buffer, so
  the blackhole JVM-17 fallback warning can flip it to a spurious,
  order-dependent failure.

Test-review follow-up pass:
- addressed 1 review step.
- `opaque-no-boxing-test` now filters `*err*` lines for
  `Boxed math warning|Reflection warning` and asserts that set is empty,
  instead of `(= "" (str err))`. The JVM-17 compiler-blackhole fallback
  warning (emitted to `*err*`) can no longer flip the test. Verified the
  old form fails on that warning while the new form passes; `measured-test`
  green (32 tests, 267 assertions); cljfmt clean.

Test-review session (second pass):
- added 1 step: the reference-cell read re-hint (criterion 4) is never
  exercised end-to-end — only untagged references run, and the tagged case
  is inspected structurally, so a dropped re-hint would go uncaught.

Test-review follow-up pass (second pass):
- addressed 1 review step.
- added `opaque-reference-no-reflection-test`: a `(.length a)` subject with
  `a` tagged `^String` compiles under `*warn-on-reflection*` with no
  reflection warnings, plus a `(.length ^String s)` behavioural round-trip.
  Extracted the warning capture shared with `opaque-no-boxing-test` into
  `boxing-reflection-warnings`. Verified teeth: stripping the re-hint from
  the generated form reintroduces the reflection warning. `measured-test`
  green (33 tests, 269 assertions); cljfmt clean.

Test-review session (third pass):
- added 2 steps: criterion 2's primitive-arithmetic behavioural subject is
  missing (only the reference-path record read is exercised); and
  `boolean`/`byte`/`short` fallback tags lack a correct-measurement
  round-trip (only `char` runs).

- addressed 2 third-pass review steps: added a primitive-cell behavioural
  subject (`(unchecked-inc ^long x)`) to `opaque-behaviour-test` and
  `boolean`/`byte`/`short` fallback round-trips to `opaque-mixed-args-test`.
  Suite green (33 tests, 272 assertions); primitive case verified under
  `:blackhole`.

Test-review session (fourth pass):
- added 1 step: the `double`/`float` bit-reinterpretation cell path lacks a
  behavioural anti-hoisting check (only value correctness is tested),
  inconsistent with the third-pass distinct-path standard.

Test-review follow-up pass (fourth pass):
- addressed 1 review step: added a `double` bit-reinterpretation behavioural
  subject (`(unchecked-add 1.0 ^double x)`, AtomicLong +
  `Double/longBitsToDouble`) to `opaque-behaviour-test`, mirroring the long
  case, under the compiler-blackhole guard and tolerant floor check.
  Verified passing under `:blackhole`; full suite green (33 tests, 273
  assertions); cljfmt clean.

Test-review session (fifth pass):
- added 1 step: `int` and `float` cell paths lack a behavioural
  anti-hoisting case, so the distinct-path standard is applied
  inconsistently (fourth pass covered `double` only, deferring `float`;
  `int` was never flagged).

Test-review follow-up pass (fifth pass):
- addressed 1 review step: added `int` (`(unchecked-inc-int ^int x)`,
  AtomicInteger + `(int (.getOpaque cell))`) and `float`
  (`(unchecked-add 1.0 ^float x)`, AtomicInteger + `Float/intBitsToFloat`)
  behavioural subjects to `opaque-behaviour-test`, each with a
  volatile-read control under the compiler-blackhole guard and tolerant
  floor check. Verified under `:blackhole` (focused suite runs the new
  subtests, 276 assertions, all pass); default run skips via the guard
  (33 tests, 272 assertions); cljfmt clean. Every distinct cell code path
  (long/int/double/float/reference) now has a criterion-2 check.

Test-review session (sixth pass):
- added 1 step: the `opaque-behaviour-test` floor `(< (* 0.1 c) s)` has no
  teeth — measured under `:blackhole`, a hoisted subject (~0.030 ns) still
  clears the ~0.0078 ns floor, so all five criterion-2 subtests pass even
  with the barrier broken. The prior passes' "a regression would fail the
  assertion" claims are empirically false; the five distinct-path subtests
  added across passes 3-5 are all vacuous at the current floor.

Test-review follow-up pass (sixth pass):
- addressed 1 review step: tightened the `opaque-behaviour-test` floor from
  `(< (* 0.1 c) s)` to `(< (* 0.5 c) s)` on all five cell-path subtests
  (reference/long/double/int/float), and documented the regime separation
  in the test comment. The loose floor had no teeth: a hoisted subject
  (~0.4x control) cleared `0.1*c` just as a working barrier (~1.0x) did.
  Verified teeth on this machine under `:blackhole`: a faithfully hoisted
  reference subject measured ratio ~0.28 (fails `0.5*c`, passes `0.1*c`),
  the working barrier ratio ~1.02 (passes `0.5*c`). Focused test green
  under `:blackhole` (5 assertions, all paths pass the tightened floor);
  default run skips via the guard (33 tests, 272 assertions); cljfmt clean.

Test-review session (seventh pass):
- added 1 step: `opaque-structure-test` guards only the loop half of
  I-1; it does not assert the cell init precedes `start#`, so a move of
  the init into the timed region (after `start#`, before the loop) would
  pass every current structural assertion.

Test-review follow-up pass (seventh pass):
- addressed 1 review step: `opaque-structure-test` now asserts the cell
  ctor binding precedes the `start#` timestamp binding in the outer
  `let` binding order (`outer-let-bindings` + `binding-value-index` +
  `start-timestamp?`), guarding the timed-region half of I-1 across all
  six opaque-eligible tags. Verified teeth (real form ctor-idx 1 <
  start-idx 7 passes; a synthetic ctor-after-start# vector fails). Full
  `measured-test` suite green (33 tests, 290 assertions); cljfmt clean.

Test-review session (eighth pass):
- added 2 steps: the seventh-pass `opaque-structure-test` emits
  boxed-math warnings on namespace load (lines 973/979; boxed `pos?`/`<`
  on the index arithmetic), undermining the no-boxing signal and matching
  the `boxing-reflection-warnings` capture filter (latent flakiness of the
  class the first pass fixed); and the custom-`time-fn` start-form branch
  of the structural test (and of `start-timestamp?`'s `invokePrim` arm) is
  never exercised, so the ctor-before-start# invariant is unverified there.

Test-review follow-up pass (eighth pass):
- addressed 2 review steps.
- Unboxed the `opaque-structure-test` index arithmetic (ctor-index
  predicate `(< 0 (long (count-matching ...)))`, ordering
  `(< (long ctor-idx) (long start-idx))`): loading the test namespace no
  longer emits `Boxed math warning` (lines 973/979), closing the latent
  `boxing-reflection-warnings` flakiness class.
- Added a custom-`time-fn` structural case: extracted the invariants into
  `assert-opaque-structure`, run per tag for the default and
  `:time-fn` branches, so the `(. time-fn invokePrim)` start-form shape
  and the `invokePrim` arm of `start-timestamp?` are now live. Fixed a
  latent bug in that arm — it compared `(= 'invokePrim (last x))` but the
  generated symbol is namespace-qualified, so the arm was dead; now
  matches on `(name (last x))`. Verified ctor-before-start# has teeth on
  the custom-timer branch. Full `measured-test` suite green (33 tests,
  338 assertions); cljfmt clean.

Test-review session (ninth pass):
- added 2 steps: criterion 1's "every opaque-eligible argument" clause is
  only verified structurally for a single argument (N=1 counts are hard-
  coded; a 2-arg form yields 2 ctors / 4 reads, so dropping a cell for any
  argument but the first passes every current structural and correctness
  test); and the custom-`time-fn` path is never run end-to-end together
  with a live opaque cell (lower risk — the timer and cell codegen are
  orthogonal).

Test-review follow-up pass (ninth pass):
- addressed 2 review steps.
- added `opaque-structure-multi-arg-test`: builds 2- and 3-opaque-arg
  `measured-expr-fn` forms, asserts ctor/in-loop/total `getOpaque` counts
  scale with N (N ctors, N in-loop reads, 2N total), no ctor in the loop,
  and all N ctors precede `start#` — so dropping a cell for any argument
  but the first fails (verified 2 ctors/4 reads at N=2, 3/6 at N=3). Index
  arithmetic kept unboxed to hold the eighth-pass boxed-math-clean load.
- added a custom-`time-fn` + opaque-cell round-trip to `expr-test`
  (`(unchecked-inc ^long x)` with `:time-fn`), the only end-to-end run of
  the custom-timer forms together with a live cell.
- Full `measured-test` suite green (34 tests, 352 assertions); test ns
  loads boxed-math/reflection clean; cljfmt clean.

Test-review session (tenth pass):
- no steps added. All seven acceptance criteria are now covered: every
  cell path (long/int/double/float/reference) has structural (criterion 1,
  single + multi-arg, default + custom timer) and behavioural
  anti-hoisting (criterion 2, floor with teeth) coverage; no-boxing
  (criterion 3) compiles init + read under warn-on-boxed/warn-on-reflection;
  array + callable + boolean/byte/short/char fallback and mixed args
  (criterion 4) round trip end-to-end. Tests inject state (volatile!,
  with-args-fn), use no mocks/stubs, and add no test-only production path,
  argument, or option. No actionable test-quality defect found.

Test-review session (eleventh pass):
- no steps added. Independent test-shaper pass over the opaque tests
  confirms the tenth-pass conclusion: simple (explicit arrange/act/assert),
  consistent (uniform subtest shape, shared `best-ns-per-call` /
  `boxing-reflection-warnings` helpers), robust (compiler-blackhole guard
  removes vacuity; `0.5*c` floor has verified teeth; volatile-read controls
  keep the anti-hoisting signal deterministic), economical (every distinct
  cell path and acceptance criterion covered, no case explosion). The five
  behavioural subtests share a near-identical skeleton, but the subject /
  control are literal `measured/expr` macro forms with embedded tags, so a
  compressing helper would hide intent rather than compress ceremony;
  retaining the explicit AAA is the better trade. No actionable defect.

Docs-review session:
- added 2 follow-up steps: CHANGELOG.md has no entry for the (user-visible)
  opaque barrier; docs/quickstart.qmd (+ source quickstart.clj) still
  describes argument "hoisting ... used for every timed execution", now
  contradicting the opaque-read-every-iteration behaviour.

Docs-review follow-up pass:
- addressed 2 docs-review steps.
- CHANGELOG.md: added an `[Unreleased]` `### Performance` entry for the
  opaque input barrier (store once before timing, `getOpaque` per
  iteration, anti-hoisting / no final-field folding, documented
  non-subtracted per-argument barrier cost, array/boolean/byte/short/char/
  callable plain-binding fallback), worded to match the README section.
- Rewrote the quickstart "Benchmarking with Data" hoisting paragraph in
  both `bases/notebooks/src/quickstart.clj` and the rendered
  `docs/quickstart.qmd`: argument values are stored once before timing and
  read opaquely each iteration, so the JIT can not hoist the subject or
  fold a final-field read; setup still runs once. Remaining "hoist" text is
  the corrected "JIT can not hoist" framing, consistent with the README.
- Blocked (recorded): the cached "expensive-setup" ~11 ns example output in
  the `.qmd` was not regenerated — that requires a live notebook render
  (`-m notebook.render`, benchmarks in a controlled environment), out of
  scope for a prose-consistency fix; the prose edit stands independently.

Docs-review session (second pass):
- added 1 step: the cached ~11.2 ns quickstart "expensive-setup" example
  output is a pre-barrier artifact rendered beside barrier-describing
  prose, never regenerated; the prior pass left it as a "Blocked" note
  inside a checked step with no open tracking item.

Docs-review follow-up pass (second pass):
- addressed 1 review step (the open second-pass docs item).
- Took the step's escape clause: confirmed the cached quickstart
  "expensive-setup" figure is still representative under the barrier
  rather than regenerating all 12 live notebooks (uncontrolled env would
  churn unrelated figures). Re-ran the exact example
  (`(let [expensive-setup (vec (range 10000))] (bench/bench (first
  expensive-setup)))`) on this machine: median 10.6 ns, identical sample
  scheme (batch-size 1250, 250000 evaluations) to the cached 11.2 ns. Same
  order/scheme; the ~0.6 ns delta is within run-to-run/machine variance and
  the documented per-reference barrier cost (~0.11 ns) is negligible against
  an ~11 ns `first`-over-vector read. The cached number is accurate for the
  barrier and does not mislead against the barrier prose; left the `.qmd`
  figure as-is (hand-editing a cached live-render block would itself be an
  unverified artifact) and recorded the verification. Source/rendered `.qmd`
  stay in sync.

Docs-review session (third pass):
- added 1 step: the quickstart "Reading the Output" hand-authored example
  shows `Elapsed Time median: 0.00390 ns`, a sub-nanosecond hoisted figure
  below the per-argument barrier floor the README/quickstart now guarantee.

Docs-review follow-up pass (third pass):
- addressed 1 review step: replaced the fabricated sub-nanosecond
  `0.00390 ns` "Reading the Output" illustrative figure with an
  above-floor plausible figure (median `3.90 ns`, spread/extremes
  `[3.76 4.10]`) and a note that the block illustrates the output format
  only, so it no longer contradicts the documented per-argument barrier
  floor. Applied identically to source
  (`bases/notebooks/src/quickstart.clj`) and rendered
  (`docs/quickstart.qmd`); hand-authored figure, no live render needed.

Docs-review session (fourth pass):
- added 1 step: the `measured/expr` macro docstring still describes
  arguments as "hoisted into a state function", colliding with the
  barrier paragraph's anti-hoist guarantee below it; the first docs pass
  reworded this same "hoist" framing out of the quickstart but left the
  docstring unchanged.

- addressed 1 review step: reworded the `expr` docstring first paragraph
  to drop the "hoisted into a state function" framing (now "evaluated
  once by an arguments function ... destructured"), removing the
  collision with the barrier anti-hoist paragraph below it.

Docs-review session (fifth pass):
- no steps added; no new actionable docs defect. The `criterium.measured`
  namespace docstring still names only the arguments generator for constant
  folding and omits the opaque barrier, but it is accurate (the generator
  does prevent input folding), non-contradictory, and outside design
  criterion 6's enumerated `measured`/`expr` docstring + README scope, so
  not flagged.

Code-shape review session:
- added 3 follow-up steps (first code-shape pass over the production
  `impl.clj`; prior reviews covered design, plan, tests, docs only). The
  tag taxonomy is encoded twice (`opaque-tag?` eligibility vs `opaque-cell`
  construction), so the eligibility⟺cell invariant is edit-maintained, not
  structural; `opaque-tag?`'s reference branch is a double negative; array
  detection uses raw interop instead of the `str/ends-with?` idiom.

Code-shape review follow-up pass:
- addressed 3 review steps.
- Introduced a single `cell-kind` classifier (returns the `cast-fn` tag
  symbols `long`/`int`/`double`/`float`, `:reference` for reference-class
  symbol or no tag, `:fallback` for `boolean`/`byte`/`short`/`char`, array
  tags, and any non-symbol tag). It is the single source of truth for the
  tag taxonomy: `arg-bindings` dispatches opaque-vs-fallback on it and
  `opaque-cell` dispatches per-kind construction on it (former `default`
  branch now an explicit `:reference` arm), so the eligibility ⟺ cell
  invariant is structural, not edit-maintained across two functions.
- `opaque-tag?` is now derived (`(not= :fallback (cell-kind tag))`),
  removing the double negative while preserving its public/test surface.
- Array detection folded into a named `array-tag?` helper using
  `str/ends-with?` (added `[clojure.string :as str]`), replacing the raw
  `.endsWith` interop outlier.
- Verified: `cell-kind` truth table correct for every tag class;
  `opaque-tag?` unchanged; full `measured-test` suite green (34 tests, 352
  assertions); reload under `*warn-on-reflection*`/`*unchecked-math*
  :warn-on-boxed` clean; the two cljfmt and one kondo findings are
  pre-existing (present on the pre-change baseline / noted earlier).

Code-shape review session (second pass):
- added 1 follow-up step: `opaque-cell` is partial over `cell-kind`'s
  codomain — it has no `:fallback` arm or `case` default, so a fallback
  tag throws `No matching clause: :fallback`; the eligibility⟺cell
  invariant is enforced only by `arg-bindings`' call-site guard, not
  locally at `opaque-cell`.

Code-shape review follow-up pass (second pass):
- addressed 1 review step: made `opaque-cell` reject a `:fallback` tag
  with an explicit named precondition (`(assert (not= :fallback kind)
  ...)`) before the `case`, so the eligibility⟺cell invariant is now
  enforced locally at the construction side (not only by the
  `arg-bindings` call-site guard). A misuse now throws a named
  `AssertionError` instead of an opaque `No matching clause: :fallback`.
  Added a contract test to `opaque-tag-test` (each fallback tag throws
  the named message). Suite green (34 tests, 356 assertions); test ns
  loads boxed-math/reflection clean; the cljfmt `FnCallExpr` finding is
  pre-existing (present on the pre-change baseline).

Code-shape review session (third pass):
- added 1 step: `cell-kind` returns a heterogeneous kind representation
  (bare cast symbols for the four primitive kinds, keywords for
  `:reference`/`:fallback`), so the single-source-of-truth classifier
  lacks a consistent data shape across its codomain.

Code-shape review follow-up pass (third pass):
- addressed 1 review step: made `cell-kind`'s codomain a homogeneous
  keyword set. The primitive branch now wraps the `cast-fn` symbol in
  `keyword`, so it returns `:long`/`:int`/`:double`/`:float` alongside
  `:reference`/`:fallback` (was bare symbols for the primitives). Updated
  `opaque-cell`'s `case` arms to the keyword kinds. `opaque-tag?`/
  `arg-bindings` compare only against `:fallback`, unaffected; no
  `cell-kind` truth-table test exists. Verified: expected keyword per tag
  class; reload boxed-math/reflection clean; `measured-test` green (34
  tests, 356 assertions); cljfmt `FnCallExpr` finding pre-existing.

Code-shape review session (fourth pass):
- added 1 step: `cell-kind`'s `cond` has overlapping non-exclusive clauses
  (`array-tag?` and the generic `symbol?` both match array-tag symbols), so
  correctness depends on unsignalled clause order; reordering would silently
  reclassify array tags as opaque references (the #156 defect the fallback
  avoids).

Code-shape review follow-up pass (fourth pass):
- addressed 1 review step: made `cell-kind`'s array-vs-reference decision
  order-independent. The `:reference` `cond` clause is now self-contained
  (`(and (symbol? tag) (not (array-tag? tag)))`), so array tags fall to
  `:else :fallback` regardless of clause order and a reorder can no longer
  silently reclassify an array tag as an opaque `AtomicReference` (the #156
  defect). Pinned the invariant with a `cell-kind` truth-table test in
  `opaque-tag-test` (every tag class → expected keyword; `long[]`/`double[]`/
  `java.lang.String[]` → `:fallback`, not `:reference`). Suite green (34
  tests, 370 assertions); reload boxed-math/reflection clean; the lone cljfmt
  `FnCallExpr` finding is pre-existing (unmodified baseline).

Code-shape review session (fifth pass):
- added 1 follow-up step: `arg-bindings`' `(map vector arg-syms arg-metas)`
  pairing silently truncates to the shorter sequence, so a shorter
  `arg-metas` leaves trailing arguments with no binding at all (no cell,
  no fallback) — bypassing the barrier (criterion 1) rather than falling
  back. The equal-length invariant is a caller-side coincidence, not
  enforced at `arg-bindings`; make the pairing total.

Code-shape review follow-up pass (fifth pass):
- addressed 1 review step: made `arg-bindings`' arg-sym⟺meta pairing total.
  Pairing is now `(map vector arg-syms (concat arg-metas (repeat {})))`, so a
  shorter `arg-metas` defaults each missing meta to `{}` (classified
  `:reference`, still barriered) instead of truncating and leaving trailing
  arguments unbound. The empty-`arg-metas` callable path is preserved by an
  `(if (empty? arg-metas) {:init [] :read []} ...)` guard so `[:as args]` with
  no metas still produces no cells and leaves the destructure intact.
  Verified: `(arg-bindings '[a b] [{:tag long}])` now binds both args (long +
  reference cells); callable `[:as args]`/`[]`/`nil` yield no bindings. Pinned
  with `opaque-tag-test` assertions. Full `measured-test` suite green (34
  tests, 375 assertions); test ns loads boxed-math/reflection clean; the lone
  cljfmt `FnCallExpr` finding is pre-existing.

Code-shape review session (sixth pass):
- no steps added. Independent code-shape pass over the task-scoped
  `impl.clj` functions (`cell-kind`, `opaque-tag?`, `opaque-cell`,
  `arg-bindings`, `measured-expr-fn`) found no new actionable defect:
  single-source-of-truth taxonomy, order-independent classification,
  local + call-site invariant enforcement, and total arg-sym⟺meta pairing
  are all in place from passes 1-5. The `~@read` fragment is emitted twice
  (primed `val#` and loop body) by design (two I-1 read sites), and
  `opaque-cell`'s `al`/`ai`/`ar` local aliases form a deliberate per-arm
  table, not a defect.
