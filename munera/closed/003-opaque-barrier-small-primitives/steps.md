# Steps — 003 opaque barrier for small primitives

## Slice 1 — Taxonomy

- [x] In `bases/criterium/src/criterium/measured/impl.clj`, rename
      `OTHER-PRIMITIVE-TAGS` to `SMALL-PRIMITIVE-CELL-TAGS`, change
      `^:private` to `^:no-doc`, and update its docstring to name it the
      small-primitive side of the taxonomy (widen into `AtomicInteger`).
- [x] Extend `cell-kind`: a tag in `SMALL-PRIMITIVE-CELL-TAGS` returns its
      like-named kind keyword (`(keyword tag)`), not `:fallback`; update
      the `cell-kind` docstring so `:fallback` means array tags only.
- [x] Update the `opaque-tag?` docstring (small primitives are now
      eligible; false only for array tags).
- [x] Rebuild `opaque-eligible-tags-by-kind` from
      `(conj (into PRIMITIVE-CELL-TAGS SMALL-PRIMITIVE-CELL-TAGS) 'java.lang.String)`
      and update its docstring (derived from the union of both sets).
- [x] In `bases/criterium/test/criterium/measured_test.clj`, update
      `opaque-tag-test`: `boolean`/`byte`/`short`/`char` satisfy
      `opaque-tag?`; `cell-kind` returns `:boolean`/`:byte`/`:short`/`:char`
      for them; the `:fallback` assertions keep only array tags; drive new
      per-kind assertions from the taxonomy sets, not literal lists.
- [x] Add a structural test that `opaque-eligible-tags-by-kind` contains
      an entry for every tag in both primitive sets with
      `(= kind (cell-kind tag))`.
- [x] Run the measured test suite; all tests pass.
- [x] Commit slice 1.

## Slice 2 — Cells

- [x] Add four clauses to the `opaque-cell` `case`, each building an
      `AtomicInteger` cell with the shared escape pair:
      - `:byte`  — init `(new AtomicInteger (int arg))`, read
        `(unchecked-byte (.getOpaque cell))`
      - `:short` — init `(new AtomicInteger (int arg))`, read
        `(unchecked-short (.getOpaque cell))`
      - `:char`  — init `(new AtomicInteger (int arg))`, read
        `(unchecked-char (.getOpaque cell))`
      - `:boolean` — init `(new AtomicInteger (if arg 1 0))`, read
        `(not (zero? (.getOpaque cell)))`
- [x] Add round-trip tests: for each new kind, a `measured/expr` subject
      returns the stored argument exactly, covering boundary values
      (`Byte/MIN_VALUE`/`MAX_VALUE`, `Short/MIN_VALUE`/`MAX_VALUE`,
      `\u0000` and `\uffff`, `true` and `false`).
- [x] Verify no reflection or boxed-math warnings on the generated read
      path for the four new kinds (compile the generated fn with
      `*warn-on-reflection*` true and `*unchecked-math* :warn-on-boxed`,
      mirroring the existing warning checks).
- [x] Run the measured test suite; all tests pass.
- [x] Commit slice 2.

## Slice 3 — Behaviour

- [x] Add anti-hoisting tests for `boolean`, `byte`, `short`, and `char`
      arguments, mirroring the existing int/float cases: a cheap pure
      subject over the argument vs a hoist-resistant control, asserted
      with `assert-not-hoisted` and the existing `hoisting-floor-ratio`.
- [x] Run the full test suite; confirm existing task 001/002 tests and
      `measured` behaviour are preserved.
- [x] Grep for remaining references to `OTHER-PRIMITIVE-TAGS` (code,
      tests, docs); remove or update any stragglers.
- [x] If `design/opaque-input-barrier.md` names the covered types, note
      the four added types there; otherwise no doc change.
- [x] Commit slice 3.

## Follow-up (discovered)

- [ ] Optional: find a subject that is hoisted on the old fallback binding
      for small primitives, so the behavioural tests discriminate barrier vs
      fallback (see implementation.md 2026-10-21). Not required by acceptance.

## Follow-up (implementation review)

- [x] Extend `opaque-no-per-iteration-allocation-test` to the four new
      small-primitive kinds (`:boolean`/`:byte`/`:short`/`:char`). Task
      001's I-2 ("no allocation in the measured loop") is a stated
      constraint of this task, and the plan's Risks flag the `:boolean`
      read `(not (zero? ...))` as returning `Boolean` objects; only the
      compile-time no-boxing check (`opaque-no-boxing-test`) covers the new
      kinds today, so the runtime per-iteration allocation probe does not.
      Add a `:boolean` subject in particular (its read returns a boxed
      `Boolean`, canonical `TRUE`/`FALSE`), to confirm no per-iteration
      allocation, as the plan's Risks require.

## Follow-up (test review, pass 2)

- [x] `opaque-behaviour-test`: the `:boolean` control
      `(if (deref ^clojure.lang.Volatile v) 1 2)` has no `^boolean` hint, so
      its captured arg routes through an `AtomicReference`, not the boolean
      `AtomicInteger` cell under test. The byte/short/char controls carry
      their small-primitive hint, so subject and control traverse the same
      cell; the boolean sub-test alone is asymmetric. The floor comparison is
      still valid (the control only needs to be hoist-resistant), so this is a
      test-symmetry gap, not a correctness defect. A symmetric control
      `(if ^boolean (deref ^clojure.lang.Volatile v) 1 2)` compiles and runs
      (verified); apply it so both sides exercise the boolean cell.

## Follow-up (test review)

- [x] `opaque-behaviour-test`: the `byte`/`short`/`char` sub-tests did NOT
      exercise their small-primitive cells. The subject
      `(measured/expr (unchecked-inc-int (int x)))` factors the subform
      `(int x)` into a single captured `int`-valued argument;
      `capture-arg-types` only widens `Long`/`Double` locals, so the arg
      defaulted to `:reference` and routed through an `AtomicReference`.
      Fixed by putting the bare hinted symbol directly as the op argument:
      `byte`→`(unchecked-inc-int ^byte x)`, `short`→`(unchecked-inc-int
      ^short x)`, `char`→`(int ^char x)` (controls mirror with
      `(deref ^Volatile v)`). Verified the generated expr-fn routes each
      through `AtomicInteger` + its `unchecked-*` narrow (AInt=1, u-byte/
      u-short/u-char=2, no boxing/reflection warnings). The `:boolean`
      sub-test `(if ^boolean x 1 2)` already routed correctly.
- [x] `opaque-no-per-iteration-allocation-test`: the `:byte`/`:short`/`:char`
      subjects had the same `(unchecked-inc-int (int x))` mis-routing and
      probed the `AtomicReference` path, not their `AtomicInteger` cells.
      Applied the same bare-hinted-symbol subject fix
      (`(unchecked-inc-int ^byte b)`, `(unchecked-inc-int ^short s)`,
      `(int ^char c)`) so the allocation probe covers the real
      small-primitive read. Suite green under `:blackhole` + `:with-agent-mac`
      (37 tests / 564 assertions).

## Follow-up (docs review)

- [x] `README.md` (lines 53-55): "Arguments with an array type, with a
      `boolean`, `byte`, `short` or `char` type, and `measured/callable`
      arguments use a plain binding, without the opaque read." This task moved
      the four small-primitive types onto the opaque path, so the statement is
      now false for them. Correct it so only array-typed arguments and
      `measured/callable` arguments are listed as using a plain binding;
      optionally note that primitive arguments (now including
      `boolean`/`byte`/`short`/`char`) read through the opaque cell.
- [x] `CHANGELOG.md` (Unreleased → Performance, the "opaque input barrier"
      entry, lines ~26-29): same stale claim — it still lists
      `boolean`/`byte`/`short`/`char` among the types that "use a plain
      binding, without the opaque read". The small-primitive opaque barrier is
      a user-visible behaviour/performance change, so update this entry (or add
      a dedicated entry) to state that `boolean`/`byte`/`short`/`char`
      arguments now also read through the opaque barrier, leaving only
      array-typed and `measured/callable` arguments on the plain binding.

## Follow-up (test review, pass 3)

- [x] `capture-arg-types-test`: add a deterministic assertion that
      `capture-arg-types` preserves a user small-primitive hint
      (`^byte`/`^short`/`^char`/`^boolean`) on a local symbol, mirroring the
      existing `long`/`String` "preserves user hint on local symbol" cases.
      This task made small-primitive hints load-bearing (each now routes to a
      dedicated `AtomicInteger` cell), and the exact mis-routing bug — a
      small-primitive-hinted arg defaulting to `:reference` — bit the task
      twice (behaviour and allocation subjects). Today the end-to-end routing
      is guarded only by the timing-gated `opaque-behaviour-test` (runs just
      under the compiler blackhole) and the native-agent `opaque-no-per-
      iteration-allocation-test`; a cheap unit assertion at `capture-arg-types`
      pins the hint-preservation contract without depending on those
      environment-gated paths.
- [x] `opaque-mixed-args-test` small-primitive round-trip block: the
      byte/short/char/boolean subjects assert only value preservation, which
      the `AtomicReference` fallback path also satisfies, so this block does
      not discriminate the small-primitive cell from a mis-routing regression.
      Either assert on cell routing (e.g. drive the generated expr-fn, as the
      structural tests do) or add a comment pointing at the structural /
      capture-arg-types tests that carry the routing guard, so the round-trip
      block is not mistaken for a routing guard it is not.

## Follow-up (code-shaper review)

- [x] `impl.clj` `cast-fn` docstring typo: "Return a cast function givent a
      tag." → "given". Fix the misspelling.
- [x] `impl.clj` `cell-kind`: the primitive branch returns `(keyword cast)`
      and the small-primitive branch returns `(keyword tag)` for the same
      keyword-from-tag operation (`cast-fn` returns the tag symbol itself, so
      `cast` ≡ `tag` here). Two idioms for one computation. Make them
      consistent (e.g. both `(keyword tag)`), so the "each tag maps to its
      like-named kind keyword" invariant reads uniformly across both
      primitive source sets.
