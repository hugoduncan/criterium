# Steps — 001-opaque-input-barrier

Execution checklist. Design is the authority; plan.md records approach.

## Slice 1 — cell selection + codegen

- [x] Add an opaque-eligibility predicate over `:tag` in
      `criterium.measured.impl`: opaque for `#{long int double float}`
      (mirror `cast-fn`), reference-class symbols, and no tag; fallback
      for `boolean`/`byte`/`short`/`char`, array tags (name ends `[]`),
      and anything unrecognised.
- [x] Add per-tag cell-spec generation: given `arg-sym` + `arg-meta`,
      produce the init fragment (cell construction from the destructured
      value with cast / bits conversion, e.g. `AtomicLong.` for `long`,
      `AtomicLong.` of `Double/doubleToRawLongBits` for `double`,
      `AtomicInteger.` of `Float/floatToRawIntBits` for `float`,
      `AtomicReference.` otherwise) and the read-binding fragment
      (`.getOpaque` + re-cast / bits-back / tag re-hint).
- [x] Rework `measured-expr-fn`: emit cell inits after argument
      destructuring and before `start#`; bind the read fragments in the
      primed `val#` computation and freshly at the top of each loop
      iteration around `(blackhole/consume ~expr)`; keep
      `binding-with-hint-or-cast` for fallback arguments; leave the
      sink, timing, and return shape unchanged (I-1, I-3, I-4).
- [x] Confirm the callable path (`measured-callable`, `[:as args]`
      binding) takes the non-opaque fallback whole; adjust the predicate
      or call site if the vector binding would otherwise be treated as
      opaque-eligible.
- [x] REPL sanity check: `macroexpand` a mixed-arg `measured/expr` form
      and eyeball init-outside / read-inside structure; compile with
      `*warn-on-reflection*` on, no warnings.

## Slice 2 — tests

- [x] Structural macroexpand test: for each opaque-eligible tag class
      (`long`, `int`, `double`, `float`, reference, untagged), assert
      the generated form constructs the cell exactly once outside the
      loop and reads via `getOpaque` inside the loop and in the primed
      `val#`; assert no `set`/store inside the loop (I-1 guard).
- [x] Structural fallback test: array-tagged argument and
      `boolean`/`byte`/`short`/`char` tags use the current non-opaque
      binding; mixed opaque + fallback arguments in one expression
      generate correctly and measure.
- [x] Boxing test for each primitive cell type (`long`, `int`,
      `double`, `float`): no reflection/boxed math in the generated
      loop, verified structurally and/or via `*warn-on-reflection*` /
      `*unchecked-math* :warn-on-boxed` focused compile (I-2).
- [x] Behavioural smoke test: a record field read and a primitive
      arithmetic expression measure clearly above the hoisted/empty-loop
      value, using a tolerant relative comparison or a floor relative to
      timer granularity (no absolute ns threshold).
- [x] Callable-path test: `measured-callable` still produces correct
      measurements with the non-opaque binding.

## Slice 3 — docs

- [x] Update `measured` and `measured/expr` docstrings: the harness
      stops constant folding and DCE, keeps work in the loop via opaque
      input reads, and the reported number for very cheap subjects
      includes a small constant per-argument barrier cost (~0.07 ns
      primitive, ~0.11 ns reference) that is documented, not subtracted.
- [x] Update the README section covering what the harness stops/does
      with the same statements and the barrier-cost artifact.

## Slice 4 — verification

- [x] Run the full existing `measured` test suite via `scry`; confirm
      green and that non-hoistable subjects measure as before within
      noise (I-3, I-4).
- [x] Format and lint per `clojure-coding-standards`; fix any findings.

## Follow-ups discovered

- Array-tagged arguments fail to compile today ("Unable to resolve
  classname: long[]") before and after this task; the structural test
  covers the fallback, end-to-end measurement waits on #156.
- `scry` is not on the project classpath; tests ran with
  `clojure.test/run-tests` under `clojure -M:dev:test`.
- [x] `opaque-behaviour-test` passes vacuously under the runtime
      blackhole (the default `:dev:test` alias): with the runtime
      blackhole both subject and control are >=1 ns, so the
      `(< (* 0.1 c) s)` assertion holds regardless of whether the
      barrier works. Acceptance criterion 2 (the core defect: a `final`
      field read must not be hoisted) is therefore only really verified
      under the `:blackhole` alias. Guard the test on
      `(= :compiler (criterium.util.blackhole/mode))` (or the
      `blackhole/compiler-blackhole-enabled?` predicate) so it either
      runs meaningfully or skips explicitly, instead of masking a
      barrier regression in the default run.
      Done: guarded on `criterium.blackhole/compiler-blackhole-enabled?`;
      runs the assertion under `:compiler`, skips explicitly under
      `:runtime` (asserting `mode` = `:runtime`). Verified both paths.
- [x] Ensure acceptance criterion 2 is exercised in a repeatable run:
      add a test alias / CI invocation (or a step note) that runs the
      behavioural test under the `:blackhole` alias
      (`-XX:+UnlockExperimentalVMOptions
      -XX:CompileCommand=blackhole,...`). Today a regression that
      re-enables hoisting would pass the default suite unnoticed.
      Done: `.github/workflows/tests.yml` installs JDK 21 and runs
      `clojure -M:kaocha:dev:test:blackhole :all
      --focus criterium.measured-test/opaque-behaviour-test`, then
      restores JDK 11. Verified the focused invocation runs the
      assertion under the compiler blackhole locally.

## Follow-ups from test review

- [x] `opaque-no-boxing-test` asserts `(= "" (str err))` over the whole
      `*err*` buffer captured during `eval`, so it treats *any* stderr
      output as a boxing failure, not only boxed-math / reflection
      warnings. On JVM 17+ without the compiler-blackhole flag (the
      default `:dev:test` run on most machines and in CI), the blackhole
      namespace emits a one-time "JVM 17+ detected but compiler blackhole
      flag not present ... Falling back to runtime blackhole" warning to
      `*err*`. The test passes today only because that warning is emitted
      during namespace load, before `*err*` is rebound — i.e. it depends
      on test/load order. If `opaque-no-boxing-test` is the first code to
      force the blackhole-mode warning while `*err*` is bound to the
      `StringWriter` (e.g. run in isolation under kaocha focus on a fresh
      JVM, or if load order changes), the assertion fails on the fallback
      warning rather than on boxing — a spurious, environment-dependent
      failure. Verified locally: evaluating the `:tag long` form with a
      freshly loaded `criterium.util.blackhole` captures the fallback
      warning into `err`, and `(= "" (str err))` is false.
      Fix: isolate the boxing/reflection signal instead of matching the
      raw buffer — e.g. assert no line contains "Boxed math warning" /
      "Reflection warning" (or force blackhole-mode resolution before
      rebinding `*err*`), so the test can not be flipped by unrelated
      stderr noise or test ordering.
      Done: `opaque-no-boxing-test` now splits `*err*` into lines and
      filters for `Boxed math warning|Reflection warning`, asserting that
      set is empty. The JVM-17 blackhole fallback warning no longer flips
      the test. Verified: the old `(= "" ...)` form fails on the fallback
      warning line, the new filter passes; full `measured-test` suite
      green (32 tests, 267 assertions); cljfmt clean.

## Follow-ups from test review (second pass)

- [x] The reference-cell read re-hint is never exercised end-to-end. For
      a class-tagged reference argument, `opaque-cell` emits the read as
      `(with-meta arg-sym arg-meta)` so the subject re-hints off the
      `AtomicReference.getOpaque` `Object` result (acceptance criterion
      4: "reference class ... read re-hinted with the tag"; design cell
      table "reference class → read re-hinted with the tag"). Today this
      path is only inspected structurally (`opaque-structure-test` with
      `java.lang.String`, subject `(identity a)` which needs no hint) and
      exercised behaviourally only with *untagged* references
      (`opaque-mixed-args-test`). No test uses a class-tagged reference
      whose subject *requires* the hint to resolve a method/field without
      reflection (for example a `measured/expr` over `(.length s)` with
      `s` captured as `^String`). A regression that dropped the re-hint
      would turn the reference read into a reflective call and no test
      would fail. Add a no-boxing/no-reflection assertion (and/or a
      behavioural round-trip) for a class-tagged reference argument,
      mirroring `opaque-no-boxing-test` but on the reference path.
      Done: added `opaque-reference-no-reflection-test` — a
      `(.length a)` subject with `a` tagged `^String` compiles under
      `*warn-on-reflection*` with no reflection warnings (extracted the
      warning capture into a shared `boxing-reflection-warnings` helper
      with `opaque-no-boxing-test`), plus a `(.length ^String s)`
      round-trip. Verified it has teeth: stripping the re-hint from the
      generated form reintroduces the reflection warning. Full
      `measured-test` suite green (33 tests, 269 assertions); cljfmt
      clean.

## Follow-ups from test review (third pass)

- [x] Acceptance criterion 2 and the design "Verification expectations"
      both require a behavioural check on TWO subjects: "a record field
      read AND a primitive arithmetic expression". Only the record-field
      case exists (`opaque-behaviour-test` subject `(.-x ^OpaqueRec r)`),
      which runs through the `AtomicReference` cell path. There is no
      behavioural (anti-hoisting) assertion for a primitive-cell subject
      (for example `(measured/expr (unchecked-inc ^long x))` with `x` a
      captured `long`). The primitive path (`AtomicLong`/`AtomicInteger`
      + `getOpaque`) is distinct code from the reference path, so a
      regression that re-enabled hoisting for primitive arguments only —
      while the reference path stayed opaque — would pass the whole
      suite. Add a primitive-arithmetic behavioural case to
      `opaque-behaviour-test` (same compiler-blackhole guard and tolerant
      relative/floor check as the record-read case) so criterion 2's
      primitive subject is actually exercised.
      Done: added a `(unchecked-inc ^long x)` subject to
      `opaque-behaviour-test` (AtomicLong cell path) with a volatile-read
      control, under the same compiler-blackhole guard and
      `(< (* 0.1 c) s)` floor check as the record-read case. Verified
      under `:blackhole`: the opaque primitive subject (~0.056 ns/call)
      matches the volatile control (~0.057 ns/call), so it is not hoisted;
      a regression dropping `s` to the empty-loop value would fail the
      assertion. Full `measured-test` suite green (33 tests, 272
      assertions); cljfmt clean.

- [x] Fallback behavioural coverage is asymmetric. Of the four
      non-opaque primitive tags (`boolean`/`byte`/`short`/`char`), only
      `char` runs through a real measurement (`opaque-mixed-args-test`
      via `(.charAt "abc" 1)`); `boolean`/`byte`/`short` fallback
      arguments are asserted only structurally (`opaque-tag?`,
      `opaque-fallback-test`), never invoked to confirm acceptance
      criterion 4's "fall back ... and still produce correct
      measurements". Add a `boolean`/`byte`/`short` fallback argument to
      a round-trip/measurement assertion (lower risk than the primitive
      gap above, since the fallback reuses the pre-existing
      `binding-with-hint-or-cast` path, but the criterion explicitly
      claims correct measurement for these tags).
      Done: added `boolean`/`byte`/`short` fallback round-trips to
      `opaque-mixed-args-test` — `(not ^boolean b)`,
      `(unchecked-inc ^byte by)`, `(unchecked-inc ^short sh)` — each
      invoked and asserted to produce the correct value. Full
      `measured-test` suite green (33 tests, 272 assertions); cljfmt
      clean.

## Follow-ups from test review (fourth pass)

- [x] The `double`/`float` bit-reinterpretation cell path has no
      behavioural (anti-hoisting) assertion. The third-pass review
      established the operative standard that a *distinct* cell code path
      needs its own criterion-2 behavioural check, and added the `long`
      case (`AtomicLong` + bare `getOpaque`) on exactly that reasoning.
      The `double`/`float` cells are a further distinct code path: the
      read is `(Double/longBitsToDouble (.getOpaque cell))` /
      `(Float/intBitsToFloat (.getOpaque cell))`, a bit-reinterpretation
      wrapper around the opaque read that `long`/`int` do not have.
      Today `double`/`float` are exercised only for *value* correctness
      (`opaque-mixed-args-test` round-trips `d` and `f`), never for
      anti-hoisting under the compiler blackhole. A regression that
      re-enabled hoisting on the bit-reinterpretation path specifically
      (for example folding the `longBitsToDouble` of a now-provably-stable
      read) would pass the `long` behavioural case and every structural
      test. Add a `double` (and/or `float`) subject to
      `opaque-behaviour-test` under the same compiler-blackhole guard and
      tolerant floor check, mirroring the `long` case, so criterion 2 is
      exercised on the bit-reinterpretation cell path. (Lowest-risk of the
      behavioural gaps found, since the opaque-read mechanism is shared and
      the bits conversion is a pure post-read function; flag it only to
      keep the distinct-path behavioural standard consistent.)
      Done: added a `(unchecked-add 1.0 ^double x)` subject to
      `opaque-behaviour-test` (AtomicLong + `Double/longBitsToDouble` bit-
      reinterpretation cell path) with a volatile-read control, under the
      same compiler-blackhole guard and `(< (* 0.1 c) s)` floor check as
      the long/record cases. Verified under `:blackhole`: the double
      subject passes the anti-hoisting assertion. Full `measured-test`
      suite green (33 tests, 273 assertions); cljfmt clean.

## Follow-ups from test review (fifth pass)

- [x] The `int` and `float` cell paths still have no behavioural
      (anti-hoisting) assertion in `opaque-behaviour-test`, leaving the
      distinct-path standard the third and fourth passes established and
      acted on applied inconsistently. Both are distinct code from the
      `long`/`double`/reference cases already covered: `int` uses an
      `AtomicInteger` cell with the read re-wrapped `(int (.getOpaque
      cell))` (a narrower atomic type than `AtomicLong`, plus an `int`
      cast the `long` path does not have); `float` uses an
      `AtomicInteger` cell whose read is `(Float/intBitsToFloat
      (.getOpaque cell))` — its own bit-reinterpretation wrapper,
      distinct from the `double` path's `Double/longBitsToDouble` and the
      only `AtomicInteger`-based bits path. The fourth-pass step was
      titled `double`/`float` but its "Done" note added only `double`,
      explicitly deferring `float` ("and/or"); `int` was never flagged.
      Today `int`/`float` are exercised only for value correctness
      (`opaque-mixed-args-test` round-trips `i` and `f`), never for
      anti-hoisting under the compiler blackhole, so a regression that
      re-enabled hoisting on the `AtomicInteger` or `intBitsToFloat`
      paths specifically would pass every existing behavioural case and
      all structural tests. Add `int` and `float` subjects to
      `opaque-behaviour-test` under the same compiler-blackhole guard and
      tolerant floor check as the `long`/`double`/record cases, so
      criterion 2 is exercised on every distinct cell code path. (Lowest
      remaining risk — the opaque-read mechanism and the floor check are
      shared, and the conversions are pure post-read functions — flagged
      only to complete the distinct-path behavioural standard the prior
      passes set.)
      Done: added `int` and `float` subjects to `opaque-behaviour-test`
      under the same compiler-blackhole guard and `(< (* 0.1 c) s)` floor
      check as the long/double/record cases. `int`:
      `(unchecked-inc-int ^int x)` (AtomicInteger + `(int (.getOpaque
      cell))`), `float`: `(unchecked-add 1.0 ^float x)` (AtomicInteger +
      `Float/intBitsToFloat`), each with a volatile-read control.
      Verified under `:blackhole`: both new subtests run (focused suite
      276 assertions vs 272 under runtime) and pass the anti-hoisting
      floor check; the default `:dev:test` run still skips them via the
      guard (33 tests, 272 assertions). cljfmt clean. Every distinct cell
      code path (long, int, double, float, reference) now has a
      criterion-2 behavioural check.

## Follow-ups from test review (sixth pass)

- [x] The `opaque-behaviour-test` floor check `(< (* 0.1 c) s)` has no
      teeth: it does not fail when the barrier is broken and the subject
      is hoisted, so every criterion-2 subtest (the whole point of the
      test) passes vacuously even with no opaque read at all. The floor
      is 10% of the control, but a fully hoisted final-field read — the
      exact defect this task exists to prevent — does not cost zero; it
      costs the empty-loop/consume cost, which is well above 10% of the
      control. Measured on JDK 25 under the `:blackhole` alias:
      working barrier subject ≈ 0.078 ns/call, control (volatile deref)
      ≈ 0.078 ns/call, floor `0.1*c` ≈ 0.0078 ns; a hoisted subject
      (record bound once, `(.-x r)` read in the loop, no opaque cell)
      ≈ 0.030 ns/call. Since 0.030 > 0.0078, `(< (* 0.1 c) s)` is TRUE
      for the hoisted case — the assertion passes whether or not the
      barrier works. The prior-pass "Done" notes claim "a regression
      dropping `s` to the empty-loop value would fail the assertion";
      that claim is false at the measured magnitudes. The test therefore
      does not verify acceptance criterion 2 for ANY cell path; it only
      asserts the subject is non-negative-ish. Tighten the check so it
      separates a working barrier from a hoisted subject: compare the
      subject to the control directly with a floor between the two
      regimes (a working barrier makes subject ≈ control, ratio ~1.0; a
      hoisted subject is ~0.4× control at these magnitudes), e.g.
      `(< (* 0.5 c) s)` (empirically: working 1.0 passes, hoisted 0.39
      fails) rather than `(* 0.1 c)`. Verify teeth by temporarily
      stripping the opaque read (or binding the arg once before the loop)
      and confirming the assertion fails. Apply to all five cell-path
      subtests, since the loose floor masks a regression on every one of
      them.
      Done: tightened the floor to `(< (* 0.5 (double c)) s)` on all five
      cell-path subtests (reference/long/double/int/float) and documented
      the regime separation in the test comment. Verified teeth on this
      machine under `:blackhole`: a faithfully hoisted reference subject
      (record bound once outside the loop, `(.-x r)` read inside, no
      opaque cell) measured ≈ 0.030 ns/call vs control ≈ 0.106 ns/call
      (ratio 0.28) — `0.1*c` is TRUE (no teeth), `0.5*c` is FALSE (fails
      as required); the working barrier measured subject ≈ control (ratio
      ≈ 1.02), `0.5*c` passes. Focused `opaque-behaviour-test` under
      `:blackhole` green (1 test, 5 assertions, all paths run and pass the
      tightened floor); default `:dev:test` run still skips via the guard
      (33 tests, 272 assertions); cljfmt clean.

## Follow-ups from test review (seventh pass)

- [x] `opaque-structure-test` verifies the cell is built "exactly once"
      and "not inside the loop", but it does NOT verify the cell init is
      built *before* `start#` (the timing timestamp). Acceptance
      criterion 1 states the cell is constructed "exactly once before
      `start#`", and I-1's intent is that construction sits outside the
      *timed* region, not merely outside the loop. The generated form
      today is correct (the `*-cell` ctor is bound in the outer `let`
      ahead of `start__..__ (jvm/timestamp)`), but a regression that
      moved the cell init into the `val#` binding — i.e. after `start#`
      and before the loop — would still be "once, outside the loop" and
      would pass every current assertion (`cell-ctor?` count 1 in the
      whole form, 0 in `loop-form`), while silently placing cell
      allocation inside the timed window and inflating the reported time
      by the construction cost. This is a plausible refactor: a change
      that merged each argument's init+read into a single per-arg
      emission at the `val#` site would trip exactly this gap. The
      structural test is the design's "deterministic, non-flaky signal"
      guarding I-1 erosion, but it currently guards only the loop half of
      I-1, not the timed-region half. Add an assertion that the cell
      ctor appears before the timing `start#` binding — e.g. locate the
      `jvm/timestamp` (or `. time-fn invokePrim`) start form in the outer
      `let` binding vector and assert every `cell-ctor?` form precedes it
      in binding order (or assert the ctor is a sibling of `start#` in
      the outer `let`, not nested under the `val#` binding), so a move of
      the init past `start#` fails the test.
      Done: `opaque-structure-test` now locates the outer `let` binding
      vector (`outer-let-bindings`), finds the binding index of the
      cell-ctor init value and of the `start#` timestamp value
      (`binding-value-index` + `start-timestamp?` matching
      `jvm/timestamp` or `(. time-fn invokePrim)`), and asserts the ctor
      index is less than the start index for all six opaque-eligible
      tags. Verified teeth: the real form reports ctor-idx 1 < start-idx
      7 (passes); a synthetic regression binding the ctor after `start#`
      reports ctor-idx > start-idx (fails). Full `measured-test` suite
      green (33 tests, 290 assertions); cljfmt clean.

## Follow-ups from test review (eighth pass)

- [x] `opaque-structure-test` (added in the seventh pass, commit
      `cf98e3e`) introduced test code that emits boxed-math warnings on
      every namespace load. Lines 973 and 979 —
      `#(pos? (count-matching cell-ctor? %))` and
      `(< ctor-idx start-idx)` — compute on boxed `Object` results
      (`count-matching` returns a boxed count; `binding-value-index`
      returns a boxed index via `ffirst`), so `pos?`/`<` compile to
      `Numbers.isPos(Object)` / `Numbers.lt(Object,Object)`. Verified: a
      plain `(require 'criterium.measured-test)` prints
      `Boxed math warning, criterium/measured_test.clj:973:38` and
      `:979:41`. This matters on two counts. (1) It directly undermines
      the "clean compile / no boxing" signal this task relies on: the
      no-boxing tests assert the *generated* code is boxing-free, yet the
      test suite that guards them now emits boxed-math warnings of its
      own on load. (2) It re-creates the exact flakiness class the
      first-pass review fixed: `boxing-reflection-warnings` captures
      `*err*` and filters for `Boxed math warning|Reflection warning`;
      these new warnings match that filter. They are latent today because
      they fire at test-ns compile time (before `*err*` is rebound), but
      a kaocha isolated/fresh-JVM load order, an AOT step, or any future
      move of these forms to eval-time would let them land in the
      captured buffer and flip `opaque-no-boxing-test` /
      `opaque-reference-no-reflection-test` into spurious failures — the
      same order-dependent trap the first pass removed for the blackhole
      fallback warning. Fix: make the structural-test index arithmetic
      unboxed (e.g. hint `ctor-idx`/`start-idx` as `^long` or compare via
      `compare`/explicit `long` casts, and avoid `pos?` on the boxed
      count — e.g. `(< 0 (long (count-matching ...)))` or return a
      primitive count), so loading the test namespace emits no
      boxed-math warning.
      Done: the index arithmetic is now unboxed — the ctor-index
      predicate is `#(< 0 (long (count-matching cell-ctor? %)))` (was
      `(pos? (count-matching ...))`) and the ordering compares
      `(< (long ctor-idx) (long start-idx))` (was `(< ctor-idx
      start-idx)`). Verified: `(require 'criterium.measured-test
      :reload)` now emits no `Boxed math warning` lines (lines 973/979
      previously printed `Numbers.isPos(Object)` /
      `Numbers.lt(Object,Object)`). Full `measured-test` suite green
      (33 tests, 338 assertions); cljfmt clean.

- [x] The structural-test helper `start-timestamp?` has a branch
      (`(and (= '. (first x)) (= 'invokePrim (last x)))`) that matches a
      custom-timer start form `(. time-fn invokePrim)`, but every caller
      (`opaque-structure-test` via `expr-fn-form`) builds the form with
      no `:time-fn`, so the generated `start#` is always
      `(jvm/timestamp)`. The `invokePrim` branch is therefore never
      exercised, and the seventh-pass "cell ctor precedes start#"
      assertion is only verified for the default-timer code path, not for
      the custom-`time-fn` path (`measured-expr-fn` emits a different
      outer-`let` shape when `time-fn` is supplied — the timer symbol is
      re-tagged and `start#` becomes `(. time-fn invokePrim)`). A
      regression that moved the cell init after `start#` only on the
      custom-`time-fn` branch would pass every current structural
      assertion. Add a structural case that passes a `:time-fn` and
      asserts the same ctor-before-start# ordering (and the once-outside /
      read-inside invariants) on that branch, so both start-form shapes
      are covered and the `invokePrim` half of `start-timestamp?` is live.
      Done: extracted the once-outside / read-inside / ctor-before-start#
      assertions into `assert-opaque-structure`, and `opaque-structure-
      test` now runs it twice per tag — default timer and custom
      `:time-fn` (`(expr-fn-form tag 'time-fn)`), exercising the
      `(. time-fn invokePrim)` start-form shape. Also fixed a latent bug:
      `start-timestamp?`'s `invokePrim` arm compared `(= 'invokePrim (last
      x))`, but the generated form carries a namespace-qualified
      `criterium.measured.impl/invokePrim`, so the arm was dead and the
      new custom-`time-fn` case would have failed `(some? start-idx)`;
      the predicate now matches on `(= "invokePrim" (name (last x)))`.
      Verified the custom-`time-fn` form matches two start-timestamp
      forms and reports ctor-idx 1 < start-idx 7 (the ordering assertion
      has teeth on that branch). Full `measured-test` suite green (33
      tests, 338 assertions); cljfmt clean.

## Follow-ups from test review (ninth pass)

- [x] The structural coverage of acceptance criterion 1 ("the generated
      measurement function reads *every* opaque-eligible argument through
      an atomic cell") is only exercised for a single argument.
      `opaque-structure-test` (via `expr-fn-form`/`assert-opaque-structure`)
      always builds a one-argument form (`'[a]`), so every assertion is
      fixed at the N=1 counts: exactly 1 `cell-ctor?` and exactly 2
      `get-opaque?` in the whole form, 1 `get-opaque?` in the loop. The
      per-argument multiplicity is never checked. Verified: a two-opaque-
      argument form `(measured-expr-fn '[a b] '(+ (long a) (long b))
      {:arg-metas [{:tag long} {:tag long}]})` produces 2 ctors and 4
      `getOpaque` reads — but a regression that gave a cell to only the
      first argument and silently dropped the rest to the plain binding
      would produce 1 ctor / 2 reads and pass every current structural
      assertion. The correctness tests (`opaque-mixed-args-test`) would
      also pass, because a non-opaque fallback binding still computes the
      right value. So the "every argument" clause of criterion 1 (and the
      anti-hoisting intent for the 2nd..Nth argument) has no test teeth.
      Add a multi-opaque-argument structural case that asserts the cell
      ctor and in-loop `getOpaque` counts scale with the number of
      opaque-eligible arguments (e.g. for two opaque args: 2 ctors, 4
      reads total, 2 reads inside the loop, each still constructed before
      `start#`), so dropping a cell for any argument but the first fails.
      Done: added `opaque-structure-multi-arg-test`, which builds 2- and
      3-opaque-argument `measured-expr-fn` forms and asserts the cell-ctor
      count, the in-loop `getOpaque` count, and the total `getOpaque`
      count all scale with N (N ctors, N in-loop reads, 2N total), with no
      ctor inside the loop, and that all N cell ctors precede `start#` in
      the outer-`let` binding order. Verified teeth: a 2-arg form yields
      2 ctors / 4 reads and a 3-arg form 3 ctors / 6 reads, so dropping a
      cell for any argument but the first (→ 1 ctor / 2 reads) fails the
      scaled assertion. Index arithmetic kept unboxed (`(long n')`,
      `(long i)`, `(long start-idx)`) so the test namespace stays
      boxed-math clean (eighth-pass invariant). Full `measured-test`
      suite green (34 tests, 352 assertions); cljfmt clean.

- [x] The custom-`time-fn` code path is never exercised end-to-end
      together with an opaque cell. `opaque-structure-test` inspects the
      `:time-fn` branch structurally (eighth pass), and `expr-test` runs a
      custom `time-fn` end-to-end (line 195) but only over the constant
      subject `1`, which has no opaque-eligible argument and therefore no
      cell. So no test actually invokes a generated measurement that
      combines the custom-timer `start#`/`finish#` forms with a live
      opaque cell read. The two codegen concerns are orthogonal today
      (cell handling does not touch the timer forms), so this is lower
      risk than the multi-argument gap above; flag it only to close the
      "custom timer + opaque cell" interaction with a round-trip
      assertion (invoke a `measured/expr` over a tagged argument with a
      `:time-fn` and assert the returned value is correct).
      Done: added a round-trip to `expr-test`'s `:time-fn` block —
      `(measured/expr (unchecked-inc ^long x) {:time-fn f})` over a
      captured `long x`, asserting the captured arg (`[41]`) and the
      invoked result (`42`) are correct. This is the only end-to-end
      invocation combining the custom-timer `start#`/`finish#` forms with
      a live opaque cell (the pre-existing constant-`1` case has no cell).
      Full `measured-test` suite green (34 tests, 352 assertions); cljfmt
      clean.

## Follow-ups from docs review

- [x] `CHANGELOG.md` has no entry for the opaque input barrier, although
      the change is user-visible (design criterion 6): the harness now
      prevents hoisting / `final`-field constant folding of
      `measured/expr` subjects, and the reported time for very cheap pure
      expressions now includes a small, documented, non-subtracted
      per-argument barrier cost (~0.07 ns primitive, ~0.11 ns reference).
      The `[Unreleased]` section already lists other `measured` changes
      (callable argument fix, invocation unroll) but omits this one. Add
      an `[Unreleased]` entry (likely under a behaviour/Performance
      heading) describing the opaque input read, the anti-hoisting
      guarantee, the per-argument barrier cost artifact, and the
      array/`boolean`/`byte`/`short`/`char`/`callable` plain-binding
      fallback, consistent with the README "What the harness stops, and
      its cost" section and the `measured`/`expr` docstrings.
      Done: added an `[Unreleased]` `### Performance` entry (first bullet,
      ahead of the callable-unroll entry) describing the opaque input read
      (store once before timing, `getOpaque` per iteration), the
      anti-hoisting / no-`final`-field-folding guarantee, the documented
      non-subtracted per-argument barrier cost (~0.07 ns primitive,
      ~0.11 ns reference, ~25-30% run variance), and the
      array/`boolean`/`byte`/`short`/`char`/`callable` plain-binding
      fallback, worded consistently with the README "What the harness
      stops, and its cost" section.
- [x] `docs/quickstart.qmd` "Benchmarking with Data" (and its source
      `bases/notebooks/src/quickstart.clj`, lines 90-91) explain that
      criterium `"hoists" local bindings — it captures their values once,
      then uses those values for every timed execution`. This is now
      inconsistent with the implemented barrier: each captured argument is
      stored once in an atomic cell and re-read with `getOpaque` on every
      iteration precisely so the JIT can NOT treat the value as stable and
      hoist/constant-fold the subject. The "hoists ... uses those values
      for every timed execution" wording describes the pre-task behaviour
      the task removed and contradicts the README/docstring statements.
      Correct the quickstart source (and regenerate the `.qmd`) to say the
      argument values are captured once before timing but read opaquely
      each iteration (work stays in the loop), avoiding the "hoist" framing
      for the measured subject. Note the "first expensive-setup" example
      output (~11 ns) may also warrant a re-check / regeneration under the
      new barrier, since that example is exactly the cheap loop-invariant
      read the barrier changes.
      Done: replaced the "criterium 'hoists' local bindings ... uses those
      values for every timed execution" paragraph in both the source
      (`bases/notebooks/src/quickstart.clj`) and the rendered
      `docs/quickstart.qmd` with the barrier-accurate framing — each
      captured argument value is stored once before timing and read
      opaquely each iteration, so the JIT can not prove the input stable,
      hoist the measured expression, or fold a `final` field read; the
      setup still runs once. The only remaining "hoist" occurrences are the
      corrected "JIT can not hoist" wording, matching the README.
      Blocked (recorded, not actioned): re-checking the cached
      "expensive-setup" ~11 ns output requires regenerating the notebook
      (`clojure -M:...:notebook -m notebook.render`), which runs live
      benchmarks in a controlled environment; the prose edit does not
      depend on it. See implementation.md.

## Follow-ups from docs review (second pass)

- [x] `docs/quickstart.qmd` renders a full cached benchmark for the
      `(let [expensive-setup (vec (range 10000))] (bench/bench (first
      expensive-setup)))` example (lines ~230-260: Sample Scheme, median
      11.2 ns, extremes, outliers, autocorrelation). This output was
      generated before the opaque input barrier existed. `(first v)` over
      a loop-invariant vector is precisely the cheap loop-invariant read
      the barrier now changes (no `final`-field folding / no hoisting,
      plus the documented per-reference-argument read cost). The prose
      around it was updated to describe the barrier (first docs pass), but
      the rendered figure beside it was not regenerated, so the example
      output no longer demonstrably reflects current behaviour — a reader
      comparing the "11.2 ns" number against the new barrier prose gets an
      unverified, possibly stale artifact (docs-review checklist items 4
      "examples accurate" and 5 "consistency with implementation"). The
      prior pass logged this as "Blocked (recorded, not actioned)" inside
      a checked step, so it had no open tracking item. Regenerate the
      quickstart notebook (`-m notebook.render` under the notebook alias)
      so the cached example output reflects the barrier, or, if a live
      render is undesirable in this task, confirm the number is still
      representative and note the verification. Keep the source/rendered
      `.qmd` in sync.
      Done: took the escape clause — confirmed the cached figure is still
      representative rather than regenerating all 12 live notebooks in an
      uncontrolled environment. Re-ran the exact example under the barrier
      (`(let [expensive-setup (vec (range 10000))] (bench/bench (first
      expensive-setup)))`, this machine): median 10.6 ns with the identical
      sample scheme (batch-size 1250, 250000 evaluations) as the cached
      11.2 ns figure. Same order of magnitude, same scheme; the ~0.6 ns
      delta is within run-to-run/machine variance and the barrier's
      documented per-reference cost (~0.11 ns) is negligible against an
      ~11 ns `first`-over-vector read, so the cached number is accurate for
      the barrier and does not mislead a reader against the barrier prose.
      No `.qmd` figure edit needed (hand-editing a cached live-render block
      would itself be an unverified artifact); source/rendered stay in sync.
      See implementation.md.

## Follow-ups from docs review (third pass)

- [x] The "Reading the Output" section of the quickstart
      (`bases/notebooks/src/quickstart.clj` lines 44-52, rendered to
      `docs/quickstart.qmd` lines ~115-124) shows a hand-authored
      illustrative "Metric Statistics" block with `Elapsed Time median:
      0.00390 ns` (spread `[0.00376 0.00410]`, batch-size 10000 /
      2000000 evaluations). Unlike the other quickstart figures this
      block is not a live `bench-display` render — it is a fabricated
      example embedded in the comment prose to show the output format,
      and its numbers match none of the real rendered examples (the first
      benchmark is 4.82 ns at batch-size 2500). A median of ~0.0039 ns is
      a near-zero, sub-nanosecond *hoisted* result: it is below the
      documented per-argument barrier floor the same file and the README
      now state the harness guarantees (~0.07 ns primitive, ~0.11 ns
      reference, never near zero for a real subject). So the illustrative
      output number now contradicts this task's own barrier
      documentation — a reader learning to read criterium output is shown
      a reported time the new behaviour makes impossible for an actual
      measured subject (docs-review checklist item 4 "examples accurate",
      item 5 "consistency with implementation"). This is distinct from
      the first/second docs passes, which addressed the "hoist" prose and
      the live-rendered "expensive-setup" figure; this is a hand-authored
      figure, so it can be corrected without a live notebook render.
      Replace the fabricated `0.00390 ns` figure with a representative
      value consistent with the barrier (for example a small-but-plausible
      ns figure, or reuse one of the real rendered examples' numbers), or
      reword so the block clearly reads as a format illustration whose
      magnitude is not a current measurement. Keep source
      (`quickstart.clj`) and rendered (`quickstart.qmd`) in sync.
      Done: replaced the fabricated sub-nanosecond `0.00390 ns` figure
      (median/spread/extremes) with a plausible, above-floor illustrative
      figure (median `3.90 ns`, spread/extremes `[3.76 4.10]`), and added a
      one-line note that the block illustrates the output format only and
      its magnitudes are not a current measurement. The figure no longer
      contradicts the barrier floor the README/quickstart guarantee. Applied
      identically to source (`bases/notebooks/src/quickstart.clj`) and
      rendered (`docs/quickstart.qmd`); the only remaining `0.0039*`
      occurrences in docs are unrelated tail-analysis vega data.

## Follow-ups from docs review (fourth pass)

- [x] The `measured/expr` macro docstring
      (`bases/criterium/src/criterium/measured.clj`, the `expr` defmacro,
      lines ~155-158) still opens with the pre-barrier framing: "The
      function arguments are treated as constant expressions and are
      *hoisted into a state function*. The result of the state function is
      a vector that is passed to the function wrapper ...". The very next
      paragraph (lines ~164-171, added by this task) guarantees the
      opposite for the measured subject — "the JIT keeps the work in the
      loop", arguments are "read with `getOpaque` on every loop
      iteration". A user reading the docstring top-to-bottom sees
      "arguments are hoisted" immediately followed by the anti-hoist
      guarantee, which reads as self-contradictory (the word "hoisted"
      here means argument *evaluation* is moved into the args/state
      function, not the JIT loop-hoist the barrier prevents, but nothing
      in the text disambiguates the two senses). The docs-review first
      pass deliberately reworded exactly this "hoist" framing out of the
      quickstart prose (`quickstart.clj` / `quickstart.qmd`) for the same
      collision, but the docstring — the other surface design criterion 6
      requires updating — was left with the old wording. This is a docs
      accuracy/consistency gap (checklist items 1 "new behaviours
      reflected" and 5 "documentation language matches implementation"),
      distinct from the three prior docs passes (CHANGELOG entry;
      quickstart "hoist" prose; expensive-setup figure; Reading-the-Output
      figure), none of which touched the `expr` docstring body. Reword the
      first paragraph so it does not describe arguments as "hoisted" (for
      example: argument values are evaluated once by the arguments/state
      function and passed to the measured as a destructured vector), or
      otherwise disambiguate the two senses of "hoist", so the docstring
      is internally consistent with the barrier paragraph below it and
      with the reworded quickstart prose.
      Done: reworded the `expr` docstring first paragraph to drop the
      "treated as constant expressions and hoisted into a state function"
      framing — it now reads "The function arguments are evaluated once by
      an arguments function, which returns a vector. That vector is passed
      to the function wrapper and destructured into the individual
      arguments." The word "hoisted" no longer appears in the paragraph,
      so it no longer collides with the barrier paragraph's anti-hoist
      guarantee below it, and it matches the reworded quickstart prose. No
      remaining `hoisted into a state function`/`treated as constant
      expressions` occurrences in `bases` or `README.md`; cljfmt clean.

## Follow-ups from code-shape review

- [x] Single source of truth for the tag taxonomy. `opaque-tag?` and
      `opaque-cell` independently re-derive the same argument-tag
      classification: `opaque-tag?` decides eligibility (four unboxed
      primitives via `cast-fn`, reference-class symbol, no tag; array +
      other-primitive excluded), and `opaque-cell`'s `case (cast-fn tag)`
      re-splits the same primitives from a `default` branch that silently
      stands in for "reference or no tag". The invariant the barrier
      depends on — `opaque-tag?` true ⟺ `opaque-cell` builds a cell — is
      maintained only by keeping two parallel decisions in step; a future
      tag added to one but not the other (for example a new unboxed
      primitive, or narrowing the reference default) breaks it silently
      (eligible tag → wrong/boxing cell, or ineligible tag → cell built
      anyway). Shape the taxonomy into one classifier (for example a pure
      `cell-kind` returning `:long`/`:int`/`:double`/`:float`/
      `:reference`/`:fallback`) that both `arg-bindings` (opaque-vs-
      fallback choice) and `opaque-cell` (per-kind construction) dispatch
      on, so the eligibility⟺cell invariant is structural
      (`shaped_by(code, formalisms) → enforceable(invariants)`) rather than
      maintained by matching edits in two functions.
      Done: added a pure `cell-kind` classifier returning
      `long`/`int`/`double`/`float` (the `cast-fn` tag symbols),
      `:reference` (reference-class symbol or no tag), or `:fallback`
      (`boolean`/`byte`/`short`/`char`, array tags, any non-symbol tag).
      It is now the single source of truth: `arg-bindings` dispatches
      opaque-vs-fallback on `(= :fallback (cell-kind tag))`, and
      `opaque-cell` dispatches per-kind construction on `(cell-kind tag)`
      (the former `default` branch is now an explicit `:reference` arm).
      `opaque-tag?` is derived (`(not= :fallback (cell-kind tag))`),
      preserving its public/test surface. The eligibility ⟺ cell invariant
      is now structural — a new tag classified once in `cell-kind` can not
      be eligible in one function and absent in the other. Verified:
      `cell-kind` returns the expected kind for every tag class;
      `opaque-tag?` unchanged truth table; full `measured-test` suite green
      (34 tests, 352 assertions); reload under
      `*warn-on-reflection*`/`*unchecked-math* :warn-on-boxed` clean.

- [x] `opaque-tag?` encodes "reference class" as a double negative —
      `(and (symbol? tag) (not (OTHER-PRIMITIVE-TAGS tag)) (not array?))`
      — so the positive design rule ("opaque iff one of four primitives, a
      reference class, or absent") reads in the code as "a symbol that is
      not one of these primitives and not an array", which is not locally
      comprehensible: a reader must enumerate the exclusions to recover the
      intent, and the branch also silently swallows an unresolvable/typo
      class symbol into the reference path. Prefer a positive predicate
      (classify the tag into named kinds, or name an `array-tag?` helper
      and express reference as the residual explicitly) so the eligibility
      rule is readable top-to-bottom. Subsumed by the single-classifier
      step above if that is taken.
      Done: subsumed by the `cell-kind` classifier. The double negative is
      gone; `cell-kind` names every outcome positively via a `cond` that
      handles `nil`, other-primitives, arrays, and reference symbols in
      order, with `:reference` the explicit residual for a symbol tag.
      `opaque-tag?` is now a positive derivation off that classifier.

- [x] Array detection uses raw interop `(.endsWith (name tag) "[]")`,
      inconsistent with the project idiom `clojure.string/ends-with?`
      (the `str` alias is used throughout the viewer and jvm namespaces).
      It is correct and reflection-free today (`(name tag)` is a
      `String`), but the raw interop form is an idiom outlier in the base;
      switch to `str/ends-with?` (or fold the check into a named
      `array-tag?` helper as part of the classifier step) for consistency.
      Done: folded array detection into a named `array-tag?` helper that
      uses `str/ends-with?` (added the `[clojure.string :as str]` require,
      matching the viewer/jvm idiom). `cell-kind` calls `array-tag?`; the
      raw `.endsWith` interop is removed. Reflection-free (confirmed clean
      under `*warn-on-reflection*`).

## Follow-ups from code-shape review (second pass)

- [x] `opaque-cell` is partial over `cell-kind`'s codomain. The first
      code-shape pass made `cell-kind` the single source of truth,
      returning `long`/`int`/`double`/`float`/`:reference`/`:fallback`,
      but `opaque-cell`'s `(case (cell-kind (:tag arg-meta)) ...)` has
      arms only for the five opaque kinds — there is no `:fallback` arm
      and no `case` default. Calling `opaque-cell` with a fallback tag
      therefore throws `IllegalArgumentException: No matching clause:
      :fallback` (verified: `(opaque-cell 'x {:tag 'boolean})` throws).
      The "never pass a fallback tag to `opaque-cell`" invariant is
      enforced only by `arg-bindings`' `(if (= :fallback (cell-kind ...)))`
      guard at the single call site, not locally at `opaque-cell` itself.
      So the eligibility⟺cell invariant the first pass made structural in
      `cell-kind` is only half-enforced at the construction end:
      `opaque-cell` is not `locally_comprehensible` as a total function of
      its documented input (its docstring and name promise "the cell for
      an argument", but it is silently undefined for a whole class of
      `cell-kind` outputs), and a second caller — or a future refactor
      that dropped the `arg-bindings` guard — would get an opaque
      `No matching clause` failure rather than an intentional signal. This
      is distinct from the first pass's three steps (taxonomy duplication,
      double-negative predicate, raw-interop array check), which addressed
      classification; this is the construction side's robustness to a
      classified-but-ineligible input. Either make `opaque-cell` reject a
      `:fallback` kind with an explicit, named precondition / assertion
      (so the contract is stated at the function, not inferred from the
      lone caller), or restructure so `opaque-cell` is only ever reachable
      for an opaque kind by construction (for example fold the
      opaque-vs-fallback dispatch and the per-kind construction into one
      `cell-kind`-driven form so there is no partial function to misuse).
      Done: took the explicit-precondition option (keeps `opaque-cell`'s
      `case` dispatch and the structural `cell-kind` single-source-of-truth
      from the first pass intact, smaller change than refolding the
      dispatch). `opaque-cell` now binds `kind (cell-kind (:tag arg-meta))`
      and `(assert (not= :fallback kind) ...)` with a named message
      ("opaque-cell requires an opaque-eligible tag; got fallback tag ...")
      before the `case`, so the contract is stated locally at the function
      and a caller that skips the `arg-bindings` guard gets an intentional
      `AssertionError` rather than an opaque `No matching clause:
      :fallback`. Added a test to `opaque-tag-test`: `opaque-cell` with
      each fallback tag (`boolean`/`byte`/`short`/`char`) throws
      `AssertionError` matching the named message. Verified teeth: the
      pre-change form threw `IllegalArgumentException: No matching clause`;
      the new form throws the named `AssertionError`. Full `measured-test`
      suite green (34 tests, 356 assertions, +4 for the new contract
      assertions); test ns loads boxed-math/reflection clean; the lone
      cljfmt `FnCallExpr` finding is pre-existing (present on the
      unmodified baseline).

## Follow-ups from code-shape review (third pass)

- [x] `cell-kind` returns a heterogeneous kind representation — bare
      symbols (`long`/`int`/`double`/`float`) for the primitive-cell kinds
      and keywords (`:reference`/`:fallback`) for the rest — so the one
      classifier the second pass made "the single source of truth for the
      tag taxonomy" does not have a `consistent(data_shapes)`: the closed
      set of named kinds is split across two Clojure types with no
      semantic distinction between them (both groups are just "which cell
      to build"). The `case` in `opaque-cell` then dispatches on that mix
      (`long`/`int`/`double`/`float`/`:reference`), and `arg-bindings`
      compares against the keyword `:fallback`, so a reader must hold two
      kind vocabularies at once and can not tell from a kind value's type
      whether it is a cast symbol or a kind tag. The symbol-valued kinds
      coincide with `cast-fn`'s return symbols but are not consumed as
      casts in `opaque-cell` (each arm emits its own literal
      `(long ...)`/`(int ...)` etc.), so the symbol choice is incidental,
      not load-bearing — it reads as "the kind happens to equal the cast
      symbol" rather than an intentional reuse. Make the taxonomy uniform:
      return keywords for every kind (for example
      `:long`/`:int`/`:double`/`:float`/`:reference`/`:fallback`), so the
      classifier's codomain is one data shape, the `opaque-cell` `case`
      dispatches on a single kind vocabulary, and the eligibility⟺cell
      invariant the second pass localised is expressed over a homogeneous
      set. Update `opaque-cell`'s `case` arms and any `cell-kind` truth-
      table test accordingly; `opaque-tag?`/`arg-bindings` already compare
      only against `:fallback` and are unaffected.
      Done: `cell-kind` now returns keywords for every kind — the
      primitive branch wraps the `cast-fn` symbol in `keyword`
      (`:long`/`:int`/`:double`/`:float`), so the codomain is a
      homogeneous keyword set. `opaque-cell`'s `case` arms updated to
      `:long`/`:int`/`:double`/`:float`/`:reference` (one kind
      vocabulary). `opaque-tag?` and `arg-bindings` compare only against
      `:fallback`, unaffected. No `cell-kind` truth-table test exists
      (only `opaque-tag?` is asserted), so none needed updating. Verified:
      `cell-kind` returns the expected keyword for every tag class;
      reload clean under `*warn-on-reflection*`/`*unchecked-math*
      :warn-on-boxed` (no boxed-math/reflection warnings); full
      `measured-test` suite green (34 tests, 356 assertions); the lone
      cljfmt `FnCallExpr` finding is pre-existing (outside the edited
      region).

## Follow-ups from code-shape review (fourth pass)

- [x] `cell-kind`'s inner `cond` encodes an unsignalled precedence
      invariant that is not `locally_comprehensible` and is one reordering
      away from a silent misclassification. The branches
      `(array-tag? tag) :fallback` and `(symbol? tag) :reference` are not
      mutually exclusive — an array tag (`long[]`, `String[]`) is itself a
      symbol — so `cell-kind`'s correctness depends on `array-tag?`
      appearing *before* the generic `(symbol? tag) :reference` catch.
      Swapping those two `cond` clauses (a plausible future edit, e.g. to
      group the two `:reference` cases `nil?`/`symbol?` together) would
      silently reclassify every array tag as `:reference`/opaque, so
      `opaque-cell` would build an `AtomicReference` for a `long[]`
      argument — re-hinting the read with the array tag and feeding it to
      the subject — which is exactly the #156 array-tag defect the
      `:fallback` branch exists to avoid. The invariant (array tags must
      be excluded before the generic symbol catch) is load-bearing for
      correctness but is implicit in clause order, not structural. Make
      the array-vs-reference decision order-independent: either test array
      tags as part of a positive reference predicate
      (`(and (symbol? tag) (not (array-tag? tag))) :reference`) so each
      clause is self-contained regardless of position, or classify the
      symbol case once and branch array-vs-reference inside it, so no two
      `cond` clauses overlap on the same input and a reorder can not change
      the result. (No current defect — the present order is correct and
      all classifications verified; this is a robustness/comprehensibility
      follow-up, so a `cell-kind` truth-table test over `long[]`/`String[]`
      vs reference tags would also pin the invariant.)
      Done: made the array-vs-reference decision order-independent — the
      `:reference` clause is now self-contained
      (`(and (symbol? tag) (not (array-tag? tag))) :reference`) and array
      tags fall to the `:else :fallback` branch, so no two `cond` clauses
      overlap on the same input and reordering can not reclassify an array
      tag as an opaque reference (the #156 defect). Pinned the invariant
      with a `cell-kind` truth-table test in `opaque-tag-test`: asserts the
      keyword kind for every tag class and specifically that `long[]`/
      `double[]`/`java.lang.String[]` map to `:fallback`, not `:reference`.
      Verified: reload clean under `*warn-on-reflection*`/`*unchecked-math*
      :warn-on-boxed`; full `measured-test` suite green (34 tests, 370
      assertions, +14 for the truth-table test); the lone cljfmt
      `FnCallExpr` finding is pre-existing (present on the unmodified
      baseline, outside the edited region).

## Follow-ups from code-shape review (fifth pass)

- [x] `arg-bindings` pairs arguments with metas via
      `(map vector arg-syms arg-metas)`, which silently truncates to the
      shorter sequence. So when `arg-metas` is shorter than `arg-syms`
      the trailing arguments get NO binding at all — neither an opaque
      cell init/read nor the `binding-with-hint-or-cast` fallback — rather
      than falling back safely. Such an argument is then referenced in the
      subject only through the raw destructured parameter, bypassing the
      barrier entirely (acceptance criterion 1: "reads *every*
      opaque-eligible argument through an atomic cell"), and without even
      the fallback hint. The "every argument is bound" invariant is held
      only by an unstated caller-side length coincidence
      (`capture-arg-types` returns one meta per arg-sym, so the expr path
      is equal-length today), not structurally at `arg-bindings`; the
      function is not `locally_comprehensible` without knowing that
      external coincidence, and `merge-metas` (which pads to
      `max(len1,len2)`) means a user-supplied `:arg-metas` shorter than the
      captured metas is already a reachable way to feed unequal-length
      inputs. Verified: `(arg-bindings '[a b] [{:tag long}])` emits a cell
      for `a` only; `b` gets no `:init` and no `:read`. This is distinct
      from the four prior code-shape passes (taxonomy duplication,
      double-negative predicate, raw-interop array check, partial
      `opaque-cell`, heterogeneous codomain, cond-order overlap), all of
      which addressed the per-tag classification of a single argument, not
      the arg-sym⟺meta pairing across arguments. Make the pairing
      total/robust: either state and enforce the equal-length precondition
      at `arg-bindings` (an explicit `assert`/named signal on
      `(= (count arg-syms) (count arg-metas))`, excluding the empty-metas
      callable path), or default a missing meta to `{}` (which classifies
      `:reference` → still barriered, or choose the fallback) via
      `(map vector arg-syms (concat arg-metas (repeat {})))` so no
      argument is ever left unbound. Keep the empty-`arg-metas` callable
      path (`[:as args]`, no metas → no cells, destructure intact) working.
      A structural test over an N-sym / (N-1)-meta input would pin the
      invariant.
      Done: took the default-missing-meta-to-`{}` option. `arg-bindings`
      now pairs via `(map vector arg-syms (concat arg-metas (repeat {})))`,
      so a shorter `arg-metas` binds every arg-sym — a missing meta
      defaults to `{}`, classified `:reference`, so the trailing argument
      is still barriered (not silently dropped, not even the plain
      fallback). The empty-`arg-metas` callable path is preserved
      explicitly with an `(if (empty? arg-metas) {:init [] :read []} ...)`
      guard, so `[:as args]` with no metas still produces no cells and
      leaves the destructure intact rather than building `:reference` cells
      for the whole-args symbols. Verified: `(arg-bindings '[a b]
      [{:tag long}])` now binds both `a` (long cell) and `b` (reference
      cell) — the old truncating form left `b` with no `:init`/`:read`;
      the callable `[:as args]`/`[]` and `[:as args]`/`nil` cases both
      return `{:init [] :read []}`. Pinned with assertions in
      `opaque-tag-test` (N-sym/(N-1)-meta binds every arg-sym; empty/nil
      metas produce no bindings). Full `measured-test` suite green (34
      tests, 375 assertions); test ns loads boxed-math/reflection clean;
      the lone cljfmt `FnCallExpr` finding is pre-existing (present on the
      unmodified baseline, outside the edited region).
