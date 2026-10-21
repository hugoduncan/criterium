# Plan — 001-opaque-input-barrier

Design is the authority (`design.md`; `design/opaque-input-barrier.md`
D-1..D-4). Design is review-clean; both design-steps are resolved.

## Approach

All code changes live in `criterium.measured.impl`
(`bases/criterium/src/criterium/measured/impl.clj`). No new namespace,
base, option, or Java source.

Key decisions (from design, held during implementation):

1. **Cell selection is a pure code-gen-time predicate over `:tag`.**
   Opaque iff tag ∈ `#{long int double float}` (mirrors `cast-fn`'s set),
   a reference-class symbol, or absent. Non-opaque fallback for
   `boolean`/`byte`/`short`/`char`, array tags (`long[]` ...), and any
   other unrecognised tag. Array detection: tag symbol name ends in `[]`.
2. **Cells are `java.util.concurrent.atomic` types** (D-3): `AtomicLong`
   for `long`; `AtomicInteger` for `int`; `AtomicLong` +
   `Double/doubleToRawLongBits`/`longBitsToDouble` for `double`;
   `AtomicInteger` + `Float/floatToRawIntBits`/`intBitsToFloat` for
   `float`; `AtomicReference` (read re-hinted with the tag, or `Object`
   when untagged) for references.
3. **Two fragments per opaque argument** emitted by `measured-expr-fn`:
   an init (cell construction from the destructured value, with the same
   cast the non-opaque path applies, or the bits conversion) placed after
   destructuring and before `start#`; and a read binding (`getOpaque` +
   re-cast/re-hint/bits-back) used both in the primed `val#` computation
   and at the top of every loop iteration. The loop body becomes a fresh
   `let` of the read bindings around the existing
   `(blackhole/consume ~expr)`; the output sink is unchanged (I-3, I-4).
4. **Invariants**: store exactly once, outside the loop; loop only reads
   (I-1). No boxing on primitive read paths (I-2). Opaque and non-opaque
   arguments coexist in one generated function.
5. **Callable path** (`measured-callable`, `[:as args]`-style binding)
   takes the non-opaque fallback whole; the predicate already yields
   fallback there because the binding is not per-argument decomposed
   (#157 owns callable loop retention).
6. **Unconditional** (D-1): no user-facing flag. Barrier cost is a
   documented artifact, never subtracted (D-4).

Testing: structural macroexpand assertions are the primary deterministic
signal; behavioural smoke tests use tolerant relative checks; boxing
verified structurally and via `*warn-on-reflection*`/`*unchecked-math*`
focused compile. Tests run via `scry`, follow `testing-without-mocks`.

## Risks

- Behavioural timing assertions are noisy → structural test is primary;
  behavioural checks use relative/floor comparisons, not absolute ns.
- A store or cell construction creeping inside the loop silently cancels
  opacity (I-1 erosion) → structural test asserts cell construction is
  outside and reads are inside.
- Mis-tagged primitive-as-reference boxes → no regression (already
  Object today); accepted as a missed optimisation.
- Primed `val#` must use the read bindings, not the raw destructured
  values, to stay consistent → covered by structural test.
- Loop-body `let` rebinding per iteration must not itself allocate for
  primitives → verified by the boxing check.

## Slice order

1. **Cell selection + codegen** — predicate, cell-spec data (init/read
   fragment generation per tag class), rework `measured-expr-fn`;
   confirm callable path falls back.
2. **Tests** — structural macroexpand test, boxing checks per primitive
   cell type, behavioural smoke test (record field read, primitive op),
   mixed opaque/non-opaque case, array-tag fallback case.
3. **Docs** — `measured`/`measured/expr` docstrings and README section
   (what is stopped, what is done, barrier cost artifact).
4. **Verification** — full existing `measured` suite green; lint/format
   per `clojure-coding-standards`.
