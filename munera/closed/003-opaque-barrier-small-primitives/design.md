# 003 — Extend the opaque input barrier to `boolean`/`byte`/`short`/`char`

Design authority: `design/opaque-input-barrier.md` (D-1..D-5). Builds on
task 001 (opaque input barrier) and task 002 (escape opaque cells).
Related: #156 (array tags).

---

## Intent

`measured/expr` must apply the opaque input barrier to every primitive
argument, so a cheap pure expression over a `boolean`, `byte`, `short`, or
`char` argument is not hoisted or constant-folded out of the measured loop.

Today only `long`/`int`/`double`/`float` arguments read through an opaque
atomic cell. The four other primitive tags (`boolean`/`byte`/`short`/`char`)
take the non-opaque fallback: they are bound once before the loop and are
loop-invariant, so the JIT can lift any pure expression whose varying inputs
are all of these types. This is the same defect task 001 fixed for the
opaque-eligible primitives, still present for the small primitives.

Constraints and invariants:

- No allocation or boxing in the measured loop (task 001 I-2). The barrier
  for these types must read unboxed.
- The opaque read must return a value equal to the stored argument for every
  value of the type (exact round-trip).
- The anti-hoisting and escape-analysis guarantees must match the existing
  cells: same opaque read mechanism, same once-at-setup escape (task 002).
- Behaviour for non-hoistable subjects is preserved.

Success criteria:

- A cheap pure subject whose only varying input is a `boolean`, `byte`,
  `short`, or `char` reports a per-call cost consistent with a non-hoistable
  control, not a near-zero hoisted value.
- No boxing or reflection warnings on the read path for these arguments.
- `cell-kind` classifies `boolean`/`byte`/`short`/`char` as opaque-eligible
  kinds, and `opaque-cell` builds an unboxed cell for each.

This intent states the problem, not the solution. The chosen approach is
recorded below.

---

## Problem analysis

The existing barrier widens narrow primitives into a wider atomic cell and
narrows on read, staying entirely in primitives: `float` uses an
`AtomicInteger` through `Float/floatToRawIntBits` / `intBitsToFloat`, and
`double` uses an `AtomicLong` through `Double/doubleToRawLongBits` /
`longBitsToDouble`. The design's fallback justification — an
`AtomicReference` "would box" — holds only for `AtomicReference`, not for the
widen-into-a-primitive-cell pattern.

`boolean`/`byte`/`short`/`char` are all narrower than `int`, so each fits
unboxed in an `AtomicInteger`:

- `byte`/`short`/`char`: widen to `int` on store (a primitive conversion),
  narrow back with `unchecked-byte` / `unchecked-short` / `unchecked-char` on
  read. The round-trip is exact across the whole value range.
- `boolean`: no numeric conversion exists, so store `(if b 1 0)` as an `int`
  and read `(not (zero? x))`. The round-trip is exact.

Every step stays unboxed: `(int ^byte x)` is a widening conversion,
`AtomicInteger(int)` is a primitive constructor, `.getOpaque` returns a
primitive `int`, and the `unchecked-*` / `zero?` reads return primitives.

### Cell selection (added rows)

| captured tag | cell | store | read |
|---|---|---|---|
| `byte` | `AtomicInteger` | `(int b)` | `(unchecked-byte  (.getOpaque cell))` |
| `short` | `AtomicInteger` | `(int s)` | `(unchecked-short (.getOpaque cell))` |
| `char` | `AtomicInteger` | `(int c)` | `(unchecked-char  (.getOpaque cell))` |
| `boolean` | `AtomicInteger` | `(if b 1 0)` | `(not (zero? (.getOpaque cell)))` |

The existing `long`/`int`/`double`/`float`/`reference` rows are unchanged.
Array tags stay on the non-opaque fallback (#156).

---

## Approach

- The taxonomy keeps two primitive source sets, each mapping a tag to its
  like-named kind keyword:
  - `PRIMITIVE-CELL-TAGS` (`long`/`int`/`double`/`float`) — the direct-cast
    primitives, bound through `cast-fn`'s `(tag x)` cast. Unchanged.
  - `SMALL-PRIMITIVE-CELL-TAGS` (`boolean`/`byte`/`short`/`char`) — the new
    source set for the four tags that widen into an `AtomicInteger` (store
    widens, read narrows). This renames and repurposes `OTHER-PRIMITIVE-TAGS`:
    it no longer names a fallback set but the small-primitive side of the
    taxonomy. These tags stay out of `PRIMITIVE-CELL-TAGS`, so `cast-fn` still
    returns `nil` for them and they keep their widen/narrow store and read
    rather than a direct cast.
- `cell-kind` reads both sets as its single source of truth: a tag in
  `PRIMITIVE-CELL-TAGS` or `SMALL-PRIMITIVE-CELL-TAGS` maps to its like-named
  kind keyword (so the small set yields `:boolean`/`:byte`/`:short`/`:char`).
  The codomain stays a homogeneous keyword set and the eligibility ⟺ cell
  invariant stays structural.
- `opaque-eligible-tags-by-kind` is built from the union of both primitive
  source sets plus a representative reference tag, so it extends to the new
  kinds automatically: adding a tag to either set extends the map with no
  re-enumeration.
- `opaque-cell`'s `case` gains the four clauses above, each producing an
  `AtomicInteger` cell with its own store and read, plus the shared
  once-at-setup escape (task 002).
- The `:fallback` set then contains only array tags (pending #156), so
  "fallback" stops meaning "small primitives and arrays" and means "arrays".
  Keep the array fallback path and its precondition.
- These four tags take the opaque path, so they no longer reach the
  non-opaque binding. Today they flow through the type-hint branch of
  `binding-with-hint-or-cast` (not through `cast-fn`, which is driven by
  `PRIMITIVE-CELL-TAGS` and already returns `nil` for them); after this change
  `arg-bindings` dispatches them to `opaque-cell` and that branch no longer
  sees them. No `cast-fn` clause changes, because `cast-fn` never matched
  these tags.

## Out of scope

- Array-typed argument tags (#156): still the non-opaque fallback.
- Any change to the reported-number policy or the barrier-cost artifact
  documentation beyond noting the added types, if needed.

## Acceptance

- Behavioural anti-hoisting tests for `boolean`, `byte`, `short`, and `char`
  arguments, mirroring the existing `int`/`float` cases: a cheap pure
  subject reports above a non-hoisted floor.
- Structural tests covering each new opaque-eligible kind (drive from the
  `cell-kind` taxonomy, do not re-enumerate tags).
- No boxing or reflection warnings on the read path for the new types.
- `opaque-eligible-tags-by-kind` extends to the new kinds automatically from
  the taxonomy (the union of both primitive source sets plus a reference
  tag), with no re-enumeration.
- Existing task 001/002 tests and `measured` behaviour are preserved.
