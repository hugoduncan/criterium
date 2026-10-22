# Plan — 003 opaque barrier for small primitives

## Approach

Extend the opaque input barrier in
`bases/criterium/src/criterium/measured/impl.clj` so
`boolean`/`byte`/`short`/`char` arguments read through an unboxed
`AtomicInteger` cell, exactly as specified in design.md's cell-selection
table. The change is taxonomy-first:

1. **Taxonomy rename.** Rename `OTHER-PRIMITIVE-TAGS` to
   `SMALL-PRIMITIVE-CELL-TAGS` and change its visibility from `^:private`
   to `^:no-doc` to parallel `PRIMITIVE-CELL-TAGS` (tests and derived
   structures read both sets uniformly). The tags stay out of
   `PRIMITIVE-CELL-TAGS`, so `cast-fn` is unchanged and still returns `nil`
   for them.
2. **`cell-kind` extension.** A tag in `SMALL-PRIMITIVE-CELL-TAGS` maps to
   its like-named kind keyword (`:boolean`/`:byte`/`:short`/`:char`) instead
   of `:fallback`. After the change `:fallback` means arrays only (#156).
   Update the docstrings of `cell-kind` and `opaque-tag?` to match.
3. **`opaque-eligible-tags-by-kind`.** Build from
   `(conj (into PRIMITIVE-CELL-TAGS SMALL-PRIMITIVE-CELL-TAGS)
   'java.lang.String)` so the four new kinds appear with no re-enumeration.
4. **`opaque-cell` clauses.** Add four `case` clauses, all over an
   `AtomicInteger` cell, with the shared once-at-setup escape unchanged:
   - `:byte`  — store `(int b)`, read `(unchecked-byte  (.getOpaque cell))`
   - `:short` — store `(int s)`, read `(unchecked-short (.getOpaque cell))`
   - `:char`  — store `(int c)`, read `(unchecked-char  (.getOpaque cell))`
   - `:boolean` — store `(if b 1 0)` as int, read
     `(not (zero? (.getOpaque cell)))`
5. **Dispatch is automatic.** `arg-bindings` dispatches on `cell-kind`, so
   no change there; the four tags stop reaching
   `binding-with-hint-or-cast`'s type-hint branch with no edit to that
   function.

Tests in `bases/criterium/test/criterium/measured_test.clj`:

- **Structural**: drive the per-kind coverage from
  `opaque-eligible-tags-by-kind` / the taxonomy sets; update
  `opaque-tag-test` expectations (these tags are now eligible, each gets
  its kind; `:fallback` set shrinks to arrays). Do not re-enumerate tags.
- **Round-trip**: exact store/read round-trip for each new kind over
  boundary values (`Byte/MIN_VALUE`, `Byte/MAX_VALUE`, negative shorts,
  `\u0000`/`\uffff` chars, both booleans).
- **Anti-hoisting**: four behavioural tests mirroring the existing
  int/float cases using `assert-not-hoisted` with a hoist-resistant
  control.
- **No boxing/reflection**: run with `*warn-on-reflection*` /
  `*unchecked-math* :warn-on-boxed` on the generated read path (same
  mechanism as existing zero-garbage/warning checks).

Authority: `design/opaque-input-barrier.md` already carries the
`amendedBy: task 003` note (done during design review); no further
authority edits unless the added-types note needs a touch-up.

## Key decisions

- Single source of truth stays structural: only the two tag sets and the
  `opaque-cell` `case` enumerate per-kind information; everything else
  derives.
- `boolean` uses `(if b 1 0)` / `(not (zero? ...))` because no numeric
  widening exists; all other small primitives use widen-on-store /
  `unchecked-*` narrow-on-read.
- No backward compatibility obligation; the `OTHER-PRIMITIVE-TAGS` name is
  removed, not aliased.

## Risks

- **Boxed boolean read**: `(not (zero? ...))` returns `Boolean` objects
  (canonical `Boolean/TRUE`/`FALSE`, no allocation); confirm the measured
  loop does not allocate and no boxed-math warning fires. If the
  `if`-branch form is needed instead, keep the round-trip exact.
- **Anti-hoisting test flakiness**: timing-floor tests are inherently
  noisy; reuse the calibrated `hoisting-floor-ratio`/`assert-not-hoisted`
  machinery rather than inventing new thresholds.
- **Hidden callers of `OTHER-PRIMITIVE-TAGS`**: grep shows only
  `impl.clj` and the test namespace; verify the clj-kondo cache noise is
  not a real consumer.
- **`case` kind dispatch on boolean literals in generated code**: the
  store form `(if b 1 0)` must be wrapped so the `AtomicInteger`
  constructor receives a primitive `int`; verify with reflection warnings.

## Slice order

1. **Taxonomy**: rename/repurpose the tag set; extend `cell-kind`,
   `opaque-eligible-tags-by-kind`; docstring updates. Structural tests
   updated/green.
2. **Cells**: add the four `opaque-cell` clauses; round-trip and
   no-boxing/reflection verification.
3. **Behaviour**: anti-hoisting tests for the four kinds; full test suite
   green; confirm task 001/002 tests preserved.
