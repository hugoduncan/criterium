# Design follow-up steps

- [x] Architectural fit: amend authority decision D-3 in
      `design/opaque-input-barrier.md` so the architecture record stays
      coherent with this change. D-3 currently states
      `boolean`/`byte`/`short`/`char` "have no unboxed atomic cell and take
      the non-opaque fallback" (commitment "firm, pending task review"); this
      task falsifies that rationale (they widen unboxed into an
      `AtomicInteger`) and moves them onto the opaque path. The ramora model
      makes decisions and their rationale traceable authority, so the design
      must record the D-3 amendment (rationale corrected, these four tags
      reclassified opaque-eligible, `:fallback` now means arrays only) rather
      than leave the authority asserting the opposite of the shipped code.
- [x] Ambiguity: specify the single-source-of-truth structure that yields the
      four new kinds. The design says `cell-kind` "gains the kinds"
      `:boolean`/`:byte`/`:short`/`:char`, that `opaque-eligible-tags-by-kind`
      "extends to the new kinds automatically from the taxonomy", and that
      `cast-fn` no longer needs these tags — but the existing taxonomy derives
      primitive kinds through `cast-fn`/`PRIMITIVE-CELL-TAGS` (direct `(tag x)`
      casts), which these four types do not use (`byte` widens via `(int b)`,
      narrows via `unchecked-byte`). The design does not state which set is the
      source for the new kinds, so "automatically from the taxonomy" is
      unresolved: implementers could add the tags to `PRIMITIVE-CELL-TAGS`
      (re-routing them through `cast-fn`, contrary to the stated intent) or
      hand-wire a separate set (leaving the automatic extension of
      `opaque-eligible-tags-by-kind` unspecified). Name the source set and how
      both `cell-kind` dispatch and `opaque-eligible-tags-by-kind` read from it.
- [x] Inconsistency (design.md ↔ `impl.clj`): the Approach says "`cast-fn`
      no longer needs these tags for the fallback binding, since they now take
      the opaque path", implying `cast-fn` currently matches
      `boolean`/`byte`/`short`/`char`. It does not: `cast-fn` is driven solely
      by `PRIMITIVE-CELL-TAGS` (`long`/`int`/`double`/`float`) and already
      returns `nil` for these four tags. In the current fallback they flow
      through the type-hint branch of `binding-with-hint-or-cast`, not through
      any `cast-fn` cast. Correct the statement so it names the function that
      actually handles these tags today (`binding-with-hint-or-cast`) and does
      not imply removing a non-existent `cast-fn` clause.
