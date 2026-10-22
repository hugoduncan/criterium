# Steps — 002 escape opaque cells

## Slice 1 — codegen: escape point per opaque cell

- [x] In `impl.clj`, extend `opaque-cell` to return an `:escape` entry
      alongside `:init`/`:read` for every opaque-eligible kind, shaped as
      a `let` **binding pair** `[_ (blackhole/consume cell-sym)]` (ignored
      binding name, so it splices into a binding vector with even count);
      not a bare form.
- [x] Extend `arg-bindings` to accumulate `:escape` binding pairs (one
      `_ (blackhole/consume cell-sym)` pair per opaque-eligible arg; none
      for `:fallback` args or the empty-`arg-metas` callable path) and
      return `{:init [...] :read [...] :escape [...]}` with `:escape` a
      flat binding-pair vector (same shape as `:init`/`:read`).
- [x] In `measured-expr-fn`, splice the `:escape` binding pairs into the
      measured fn's `let` binding vector after the `init` bindings and
      before `start#`, outside the loop; leave the loop body and read
      bindings unchanged.
- [x] Update `opaque-cell` / `arg-bindings` / `measured-expr-fn`
      docstrings to state I-4 (escape once, at setup) alongside I-1..I-3,
      citing D-5 / I-4 (not I-3) for escape-mode independence.
- [x] REPL-check: macroexpand a `measured/expr` form with a `double` arg
      and visually confirm one escape, placed between init and `start#`.

## Slice 2 — structural escape-point test

- [x] Add a structural test in `measured_test.clj` that macroexpands the
      expr form for each opaque-eligible kind (`long`, `int`, `double`,
      `float`, `reference`; driven from `cell-kind`, not a re-enumerated
      tag list) and asserts exactly one `blackhole/consume` of each cell
      symbol in the setup section (before `start#`).
- [x] In the same test, assert no `consume` of any cell symbol appears
      inside the generated `loop` form.
- [x] Assert a `:fallback`-tagged arg and the callable path produce no
      cell escape.
- [x] Run the structural test and confirm green.

## Slice 3 — behavioural folding oracle + teeth

- [x] Add a `double` depth-scaling test: measure nested `Math/sqrt` at
      depth 1 and depth 3 through `measured/expr`; assert depth-3 time is
      meaningfully greater than depth-1 (relative ratio, tolerant, no
      absolute ns threshold).
- [x] ~~Linked record chain fixture~~ — replaced: a pure field-read chain
      does not scale measurably even when escaped (see implementation.md).
      `reference` depth test uses `DoubleNode` record field read through
      the reference cell feeding a `Math/sqrt` chain, depth 1 vs 3.
- [x] Write the depth-scaling tests mode-agnostic: no mode-specific
      assertions, no `with-redefs` of `detected-mode` (blackhole mode is a
      load-time, process-level property baked in at `measured`-fn
      expansion; there is no in-process switch). Both-mode coverage comes
      from running the same tests under two process configs, not from a
      single run.
- [x] Deliver both-mode coverage: the default-JVM suite run gives
      runtime-mode coverage; add compiler-mode coverage by having the
      JVM-17+ `:blackhole`-alias CI run reach the new tests. Place the
      depth-scaling tests in their own standalone, always-running deftest —
      **not** inside `opaque-behaviour-test`. `opaque-behaviour-test`
      (measured_test.clj ≈line 1184) guards its assertions with
      `(if (blackhole/compiler-blackhole-enabled?) <assertions> <skip>)` and
      runs only the skip branch under the default-JVM runtime config;
      nesting the mode-agnostic depth oracle there would suppress it under
      runtime mode and drop the runtime half of the both-mode coverage.
      Obtain compiler-mode coverage by widening the compiler-blackhole step
      in `.github/workflows/tests.yml` (`--focus
      criterium.measured-test/opaque-behaviour-test`) to also name the new
      standalone deftest. No new alias is needed — `:blackhole` already
      supplies the `-XX:CompileCommand` flag; compiler mode needs that flag
      plus JVM 17+, not JVM 17+ alone. Both runs must pass (D-5 / I-4).
- [x] Teeth proof: temporarily disable the escape emission, confirm the
      `double` depth-scaling test fails (depth-independent folded residue),
      restore the escape, and record the observed numbers in
      implementation.md.

## Slice 4 — boxing/allocation + regression

- [x] Add/extend a check that the setup escape and the per-iteration read
      path introduce no boxing and no per-iteration allocation for each
      primitive cell kind (I-2).
- [x] Run the full existing `measured` test suite; all green, unchanged.
- [x] Confirm fallback cases (array tags, callable path, other primitive
      tags) still work and are unaffected by the change.

## Slice 5 — documentation (conditional)

- [x] Compare observed per-call numbers for cheap subjects against the
      values quoted in `measured`/`measured/expr` docstrings and README;
      update them only if materially changed (barrier-cost framing D-4
      unchanged).
- [x] Note in implementation.md the compiler-blackhole dependency on the
      HotSpot intrinsic escape marking and the runtime-blackhole volatile
      fallback.

## Follow-up — test shaping (test-shaper review)

- [x] `best-ns-per-call` already warms up internally (the leading
      `(invoke m n)` before the `repeatedly 5` measured runs), yet every
      call site (`opaque-depth-scaling-test` and `opaque-behaviour-test`)
      precedes the measured binding with a discarded `_ (best-ns-per-call
      d1 n)` warmup line. The extra warmup is redundant ceremony that
      obscures intent: either drop the per-call discarded warmup line and
      rely on the helper's internal warmup, or move the warmup out of the
      helper so the single warmup is explicit and visible at the call site
      (test-shaper: helpers_that_compress(ceremony) ∧
      ¬helpers_that_hide(intent); locally_comprehensible).
      Dropped the redundant discarded warmup lines; documented the
      helper's internal warmup in its docstring.
- [x] `opaque-depth-scaling-test` repeats the depth-1/depth-3 warmup,
      measure and `(< (* 1.5 s1) s3)` ratio structure verbatim across the
      double-cell and reference-cell sub-tests; only the two subject
      `measured/expr` forms differ. Extract a shared depth-oracle helper
      (taking the depth-1 and depth-3 measureds, running the ratio
      assertion) so the incidental structure is written once and the two
      sub-tests differ only in their subjects (test-shaper:
      minimal(incidental_variation); consistent(structure)).
      Extracted `assert-depth-scales`; sub-tests now differ only in
      subject forms.
- [x] The `1.5` depth ratio and the `100`-allocation tolerance are bare
      magic numbers whose failure messages report only the raw ns /
      allocation values, not the gate that was violated. Name them (or
      include the required ratio/threshold in the `is` message) so a
      failure explains the contract it broke, not just the observed numbers
      (test-shaper: meaningful_failures).
      Named `depth-scaling-min-ratio` and `max-constant-allocation-noise`;
      both failure messages now state the violated gate.

## Follow-up — test shaping (test-shaper review, second pass)

- [x] `opaque-behaviour-test` repeats the same arrange/act/assert block
      verbatim across all five sub-tests (reference, long, double, int,
      float): each binds `n 10000000`, a value `x`, `v (volatile! x)`,
      a `subject` and hoist-resistant `control` `measured/expr`, then
      `s`/`c` via `best-ns-per-call` and asserts `(< (* 0.5 (double c)) s)`.
      Only the subject/control forms differ. This is the same incidental
      structure duplication that motivated extracting `assert-depth-scales`
      for `opaque-depth-scaling-test`, left un-applied to the parallel
      behaviour test. Extract a shared helper (taking subject + control
      measureds and `n`, running the floor assertion) so the five sub-tests
      differ only in their subjects/controls (test-shaper:
      minimal(incidental_variation); consistent(structure);
      consistent(test_abstractions) with the sibling depth test).
      Extracted `assert-not-hoisted`; the five sub-tests now bind only
      value + volatile and differ only in subject/control forms.
- [x] The `0.5` hoisting-floor multiplier in `opaque-behaviour-test`
      (`(< (* 0.5 (double c)) s)`, repeated five times) is a bare magic
      number: its failure message reports only `"subject <s> ns, control
      <c> ns"`, not the required subject/control ratio that was violated.
      This is the same `meaningful_failures` gap the earlier pass fixed for
      the `1.5` ratio and `100` tolerance (named `depth-scaling-min-ratio`,
      `max-constant-allocation-noise`) but did not apply to the sibling
      `0.5` floor. Name it (e.g. `hoisting-floor-ratio`) and state the gate
      in the `is` message so a failure explains the contract it broke, not
      just the observed numbers (test-shaper: meaningful_failures;
      consistent assertion-style with the named depth/allocation gates).
      Named `hoisting-floor-ratio`; `assert-not-hoisted`'s failure message
      now states the required subject/control ratio.
- [x] The `n 10000000` eval-count literal is repeated six times across
      `opaque-behaviour-test` (5×) and `opaque-depth-scaling-test` (1×, as a
      `let`). It is the same measurement-budget constant in every behavioural
      timing test; hoist it to a single named `def` (e.g.
      `behaviour-eval-count`) so the shared budget is stated once and the
      sub-tests carry no incidental numeric variation (test-shaper:
      minimal(incidental_variation); consistent(data_shapes)).
      Hoisted to `behaviour-eval-count`; both tests now bind `n` from it.

## Follow-up — test shaping (test-shaper review, third pass)

- [x] `opaque-no-per-iteration-allocation-test` compares the allocation
      count at eval-count `1` against eval-count `10000`, but both are bare
      literals at the `thread-allocation-count` call sites. These two counts
      are the arrange values that give `max-constant-allocation-noise` (100)
      its teeth: one per-iteration allocation would add ≥ the large count
      (10000), far above 100. The constant's own comment and the `is` failure
      message hard-code the prose "n=1 → n=10000", so if the `10000` literal
      changes the stated gate silently drifts out of sync with the actual
      measurement. Name the two eval counts (e.g. `alloc-probe-count` and a
      small baseline, or derive the message from the literals) so the gate
      the threshold defends is stated once and the failure message can not
      disagree with the measurement (test-shaper: meaningful_failures;
      minimal(incidental_variation); consistent with the sibling
      `behaviour-eval-count` that named the behavioural budget).
      Named `alloc-baseline-count` (1) and `alloc-probe-count` (10000); the
      noise-constant comment and the `is` message now derive from them, so
      they can not drift from the measurement.
- [x] The allocation test's warmup `(dotimes [_ 1000] (measured/invoke m
      args 1000))` uses two bare `1000` literals with no stated intent — one
      is the warmup iteration count, the other the per-invoke eval-count, and
      the two meanings are not distinguishable at the call site. The earlier
      passes established an explicit, documented warmup convention for the
      behavioural tests (`best-ns-per-call` warms internally, noted in its
      docstring; redundant call-site warmups dropped). Apply the same
      treatment here: name the warmup counts (or extract a documented warmup
      helper) so the warmup's intent is visible and consistent with the other
      timing tests (test-shaper: locally_comprehensible;
      consistent(test_abstractions); helpers_that_compress(ceremony) ∧
      ¬helpers_that_hide(intent)).
      Named `alloc-warmup-iterations` and `alloc-warmup-eval-count` (both
      documented), distinguishing the two `1000` meanings at the call site.

## Follow-up — test shaping (test-shaper review, fourth pass)

- [x] `opaque-escape-structure-test` is specified (design, plan Slice 2,
      Slice 2 steps) to be "driven from `cell-kind`, not a re-enumerated tag
      list", but it hard-codes the full tag literal `'[long int double float
      java.lang.String nil boolean byte short char]` and only *filters* it
      with `:when (not= :fallback (impl/cell-kind tag))`. The set of kinds
      the escape structure is proven for is therefore a re-enumeration, not
      derived from the kind taxonomy: a new opaque-eligible tag added to
      `cell-kind` would be silently uncovered until this literal is also
      edited, and the test would still pass green — a false-confidence gap
      against the design's own "single source of truth" intent (design
      Concepts / implementation.md: "`cell-kind` is the single source of
      truth for the tag taxonomy; reuse it, do not re-enumerate"). Derive the
      covered opaque-eligible tags from the taxonomy (e.g. one representative
      tag per non-`:fallback` `cell-kind` value, taken from `cell-kind`'s own
      domain) so adding a kind extends coverage automatically, keeping the
      fallback tags as an explicit negative list (test-shaper:
      economical: cover_by(partitions); meaningful_failures; consistent with
      the design's stated taxonomy-as-source-of-truth).
      Factored the primitive tags into a named `PRIMITIVE-CELL-TAGS` set
      (single source for `cast-fn`) and derived
      `impl/opaque-eligible-tags-by-kind` (one representative tag per
      non-`:fallback` kind) from it; the test now loops that map instead of
      the hard-coded literal. Fallback tags stay an explicit negative list.

## Follow-up — test shaping (test-shaper review, fifth pass)

- [x] The fourth-pass taxonomy-drive fix (deriving coverage from
      `impl/opaque-eligible-tags-by-kind` instead of a re-enumerated tag
      literal) was applied to `opaque-escape-structure-test` but not to its
      sibling structural test `opaque-structure-test` (measured_test.clj
      ≈line 1042), which still hard-codes
      `'[long int double float java.lang.String nil]`. Both tests assert the
      per-kind structure of the generated expr-fn (cell ctor once / opaque
      read inside the loop / ctor-before-`start#`) over exactly the
      opaque-eligible kinds; one now derives its kinds from the taxonomy and
      the other re-enumerates them. A new opaque-eligible primitive added to
      `PRIMITIVE-CELL-TAGS` would therefore gain escape-structure coverage
      automatically but keep no ctor/read-structure coverage, with
      `opaque-structure-test` still green — the same false-confidence gap the
      fourth pass closed, left on the parallel test. Drive
      `opaque-structure-test` from `impl/opaque-eligible-tags-by-kind` too
      (`nil`/`java.lang.String` both resolve to the `:reference`
      representative already in the map), so the two sibling structural tests
      share one taxonomy source and adding a kind extends both automatically
      (test-shaper: economical: cover_by(partitions); meaningful_failures;
      consistent(test_abstractions) with `opaque-escape-structure-test`;
      consistent with the design's stated taxonomy-as-single-source-of-truth).
      Drove `opaque-structure-test` from `impl/opaque-eligible-tags-by-kind`
      too; both sibling structural tests now share one taxonomy source and a
      new opaque-eligible kind extends both automatically.

## Follow-up — user-facing docs (review-task-docs review)

- [x] `CHANGELOG.md` `[Unreleased]` → Performance → opaque-input-barrier
      entry (line ≈21) still quotes the pre-escape reference cost
      "about 0.11 ns per reference argument". Slice 5 updated this number to
      "0.2-0.35 ns per reference argument" in README.md (line 50) and the
      `measured/expr` docstring (measured.clj), but not in the CHANGELOG.
      The barrier is unreleased, so the CHANGELOG is the user-visible record
      of its final cost; the stale figure now contradicts README and the
      docstring. Update the CHANGELOG reference cost to match
      (0.2-0.35 ns), keeping the ~25-30% run-to-run variance note.
      Updated the CHANGELOG reference cost to 0.2-0.35 ns, keeping the
      ~25-30% variance note; now consistent with README and the docstring.
- [x] The same `CHANGELOG.md` `[Unreleased]` barrier entry describes only the
      task-001 mechanism (store-once-in-cell + `getOpaque` read) and omits the
      task-002 change: each cell is now published once to the blackhole at
      setup so escape analysis can not prove it thread-local, which is what
      actually keeps cheap pure subjects (downstream of the read) measured
      rather than folded. This is a user-visible behaviour change (sub-ns pure
      expressions now report genuine, non-folded time). README.md (lines
      38-41) and the `measured/expr` docstring both state the publication;
      the CHANGELOG does not. Add the one-time cell publication to the
      CHANGELOG barrier entry so the released description matches the
      implementation and the other docs.
      Added the one-time cell publication (escape) and the sub-ns
      non-folded-time behaviour change to the CHANGELOG barrier entry;
      now matches README and the docstring.
