# Design follow-ups — 001-opaque-input-barrier

Unchecked items are actionable design clarifications raised by design review.

- [x] Ambiguity: the cell-selection table lists only `long`/`int`/`double`/`float`
      among primitive tags, but `tag-meta` (via `.getCanonicalName`) can produce
      `:tag` values `boolean`/`byte`/`short`/`char` for primitively-typed locals.
      State explicitly how the per-argument predicate classifies these: presumably
      the non-opaque fallback (they are not "reference class" and would box or
      produce an invalid primitive re-hint off an `AtomicReference`/`Object` read).
      Make the reference-vs-fallback boundary unambiguous for all non-{long,int,
      double,float} primitive tags.
- [x] Inconsistency: design.md's cell table maps `float` to `AtomicInteger`
      (via `Float/floatToRawIntBits`/`intBitsToFloat`), but the stated design
      authority `design/opaque-input-barrier.md` D-3 says floating point uses
      `AtomicLong` plus `Double`/`Float` bit reinterpretation. The authority's
      evidence (section 8) measures only `long`/`double`, never `float`/`int`,
      so the divergence is unresolved. Reconcile the `float` cell type (and its
      bits-method pair) between design.md and the authority.
