# 001 — Opaque input barrier for `measured/expr`

Source issue: #155. Design authority: `design/opaque-input-barrier.md`
(ramora decisions D-1..D-4). Related: #17, #19, #156, #157, #158.

---

## Intent

`measured/expr` must report a per-call time that reflects the subject
expression, including for pure expressions whose cost is below about one
nanosecond.

Today the harness binds each argument once, before the measured loop, into a
stable local. The loop then evaluates the same expression on the same inputs
every iteration. For a pure expression this lets the JIT lift the work out of
the loop, so the reported time is the empty-loop cost, not the expression.
The defect is sharper than generic loop-invariant code motion: when the
subject reads a `final` field of a loop-invariant object (every `defrecord`
field is `final`), the JIT folds the read to a constant on the ground that
the field is immutable. No output-side memory barrier can prevent this,
because the compiler relies on immutability, not on "memory may have
changed".

Constraints and invariants:

- The fix must not add allocation or boxing to the measured loop.
- The fix must be at least as correct as today for expressions that are not
  hoistable, and must not change the measured value by more than the stated
  barrier cost for such expressions.
- The reported number is not silently corrected; any barrier cost it
  contains is a documented measurement artifact (a noisy dynamic baseline
  subtracted from a same-order result produces negative times — #19).
- The non-opaque tight-loop number for a cheap pure expression is
  misleading, so it must not remain the default behaviour.

Success criteria:

- A trivial pure subject (for example a record field read) reports a
  per-call cost consistent with an independent non-hoistable control, rather
  than a near-zero hoisted value.
- Primitive-valued arguments incur no boxing in the measured loop.
- Existing `measured` behaviour and tests for non-hoistable subjects are
  preserved.

This intent states the problem, not the solution. The chosen solution is
recorded below and in the design authority.

---

## Problem analysis

The enabling fact for the optimisation is a **stable input reference** that
the JIT can prove constant across iterations. The only effective place to
intervene is the input: make the value that feeds the expression come from a
read the compiler cannot prove is stable. Then it cannot prove the object is
the same object as last iteration, cannot apply `final`-field constant
folding, and cannot hoist dependent work.

An **opaque** read (`VarHandle` access mode `getOpaque`) provides exactly
this: the JIT may not merge two opaque reads of the same location, yet opaque
has no acquire/release fence, so it is far cheaper than a volatile read or
the runtime blackhole.

Measured evidence (JDK 25, arm64, best of 9, 1e9 iterations) in the design
authority:

- compiler blackhole, invariant local: 0.027 ns/call (hoisted away).
- runtime blackhole: 0.955 ns/call, and the field read still does not appear
  (final-field folding; a barrier cannot help).
- opaque read + compiler blackhole: the read reappears.
- per-argument opaque-read cost (controlled differential, linear, ~0
  intercept): ~0.07 ns per primitive argument, ~0.11 ns per reference
  argument; reads do not pipeline, so cost scales with arity. The figure
  varies ~25-30% between JVM runs, which is why it is documented, not
  subtracted (D-4; #19).
- `Atomic*` `getOpaque` equals a hand-written `arrayElementVarHandle` shim
  per read for every type; `AtomicLong.getOpaque` (and `AtomicLong` + bit
  reinterpretation for `double`) stay unboxed.
- reading the same cell k times costs k x one read: direct evidence
  `getOpaque` forbids the merge the barrier depends on.

---

## Scope (in)

- Code generation in `criterium.measured.impl/measured-expr-fn`.
- The expr path (`measured-expr*`): for each argument, read it through an
  opaque cell inside the loop instead of binding it once before the loop.
- Per-argument automatic fallback to the current non-opaque binding when an
  argument cannot be given an opaque cell cleanly.
- Documentation of the behaviour and its measurement artifact in the
  `measured`/`measured/expr` docstrings and the README section that lists
  what the harness stops (constant folding, dead-code elimination) and what
  it does (keep work in the loop) and its cost.
- Tests covering: correctness of the generated structure, absence of boxing
  for primitive arguments, preservation of non-hoistable-subject behaviour.

## Scope (out)

- The input pool / rotation feature (#158). The barrier stays scalar
  (one cell per argument); the pool is a separate, later extension that
  reuses the cell abstraction.
- A dynamic baseline subtraction or auto-correction of the reported number
  (rejected in D-4; #19).
- Changes to the platform baseline command (`criterium.platform`) beyond
  documentation; it already measures platform baselines and is sufficient
  for a user to observe the barrier cost.
- Array-typed argument tags (#156): these are a pre-existing defect that
  also breaks the opaque transform, so they take the non-opaque fallback
  here and are fixed under #156.
- A user-facing tuning flag. The barrier is unconditional (D-1).

## Adjacent task-like work (not this task)

- #158 input pool (`:args-pool`).
- #156 array-typed local tags.
- #157 keeping work in the loop for the callable path.
- #159 surfacing harness baselines in reporting.

---

## Concepts

- **Opaque cell** — a mutable place holding one argument value, supporting an
  opaque read. Created and filled exactly once, before the loop; read
  opaquely on every iteration. Owned by the generated measurement function;
  lifetime is one measurement invocation.
- **Per-argument cell selection** — a code-generation decision, driven by the
  argument's captured `:tag`, that picks the concrete cell type or the
  non-opaque fallback.
- **Barrier cost artifact** — the per-argument opaque-read cost the reported
  number includes (~0.07 ns primitive, ~0.11 ns reference; run-to-run
  variance ~25-30%); documented, not subtracted.

### Cell selection by argument tag

`arg-metas` already carries a `:tag` per argument. The `:tag` is a primitive
symbol (`long`/`int`/`double`/`float`, the four that `cast-fn` casts, or one
of `boolean`/`byte`/`short`/`char`, which `tag-meta` can also produce for a
primitively-typed local via `.getCanonicalName`), or a canonical class-name
symbol, including array names such as `long[]`.

Only the four primitive tags that have an unboxed atomic cell
(`long`/`int`/`double`/`float`) are opaque-eligible primitives. Every other
primitive tag (`boolean`/`byte`/`short`/`char`) takes the non-opaque
fallback: no `Atomic*` type reads them unboxed, so an `AtomicReference` cell
would box on store and read and a primitive re-hint off an `Object` read is
invalid. Opaque-eligible reference arguments are exactly the non-primitive
(reference-class or untagged) arguments; this draws the reference-vs-fallback
boundary with no overlap — a tag is opaque only if it is one of the four
listed primitives, a reference class, or absent.

| captured tag | cell | read yields | notes |
|---|---|---|---|
| `long` | `AtomicLong` | `long` | unboxed |
| `int` | `AtomicInteger` | `int` | unboxed |
| `double` | `AtomicLong` | `double` | store/read via `Double/doubleToRawLongBits` / `longBitsToDouble` |
| `float` | `AtomicInteger` | `float` | via `Float/floatToRawIntBits` / `intBitsToFloat` |
| other primitive tag (`boolean`/`byte`/`short`/`char`) | — | — | **fallback** to non-opaque binding (no unboxed atomic cell; would box) |
| reference class | `AtomicReference` | ref | read re-hinted with the tag |
| no tag | `AtomicReference` | `Object` | same typing as today's untyped binding |
| array tag (`long[]` ...) | — | — | **fallback** to non-opaque binding (#156) |

`float` uses an `AtomicInteger` cell because `Float/floatToRawIntBits` yields
an `int`, so an `int`-sized cell is the natural unboxed fit (parallel to the
`int` row); only `double` needs the wider `AtomicLong`. The authority D-3
agrees: `AtomicLong` + `Double` bits for `double`, `AtomicInteger` + `Float`
bits for `float`. (The authority's section 8 evidence measures only `long`
and `double`; the `float`/`int` cells follow by the same unboxed-fit rule,
not from a separate measurement.)

### Invariants

- I-1: the store into a cell happens exactly once, outside the loop; the loop
  only reads opaquely. A store inside the loop adds cost and can enable
  store-to-load forwarding that cancels opacity.
- I-2: no boxing on the read path for primitive arguments.
- I-3: the barrier is orthogonal to blackhole mode; the compiler and runtime
  blackhole output sinks are unchanged.
- I-4: the output side is unchanged — `blackhole/consume` still sinks the
  result; the opaque read is strictly an input-side addition.

### Abstraction / ownership decisions

- Use `java.util.concurrent.atomic` types as the mutable place (D-3). Their
  `getOpaque`/`setOpaque` are ordinary typed methods, so Clojure calls them
  with no `@PolymorphicSignature` varargs boxing and no custom Java artifact.
- Do not use a shared indexed array (D-2): an indexed atomic read measured
  ~4x a scalar read; the pool concern is independent.
- Do not use `deftype`: it offers only plain (hoistable) or volatile
  (over-strong, costly) mutability, never opaque, and its mutable fields are
  private.

---

## Implementation approach

Change `measured-expr-fn` so that, for each argument, it emits two fragments
instead of the current single pre-loop typed binding:

1. an **init** fragment, evaluated once before timing starts: construct the
   argument's atomic cell from the incoming (destructured) argument value,
   applying the same cast the non-opaque path applies (for example `long`),
   or the bit reinterpretation for `double`/`float`;
2. a **read** fragment, evaluated at the top of each loop iteration (and once
   to compute the primed return value): an opaque read of the cell bound to
   the argument symbol, re-applying the argument's tag/cast so the subject
   expression type-checks exactly as it does today.

The loop body then evaluates the existing `expr` over the per-iteration read
bindings and passes the result to `blackhole/consume`, unchanged. The primed
`val#` computed once before the loop must use the same read bindings so the
returned value is consistent.

Fallback: a small predicate over the argument's `:tag` decides opaque-vs-non
-opaque per argument. It returns opaque only for the four unboxed primitive
tags (`long`/`int`/`double`/`float`), a reference-class tag, or no tag; it
returns non-opaque for an array tag and for every other primitive tag
(`boolean`/`byte`/`short`/`char`). A non-opaque argument keeps the current
`binding-with-hint-or-cast` behaviour. Opaque and non-opaque arguments can
coexist in the same generated function.

Callable path: `measured-callable` passes `[:as args]` as the argument
symbol vector and applies the function over the whole args vector. This does
not decompose into per-argument cells, so the callable path takes the
non-opaque fallback for the whole binding in this task. (`#157` covers the
callable loop-retention question.)

Code-generation structure the builder must preserve:

- cells are constructed after argument destructuring and before `start#`;
- the `getOpaque` reads are inside the loop body and in the primed `val#`
  computation, never a stored write inside the loop (I-1);
- primitive cells never box in the loop (I-2);
- the two measurement intents are not exposed as a flag; opaque is
  unconditional (D-1). If the "as-compiled" intent is ever required it is a
  separate, explicitly named escape hatch, out of scope here.

Where it lives: all changes are in `criterium.measured.impl`; no new
namespace, base, or Java source is required. `measured-expr*` needs no new
option. The public `measured`/`bench`/`measured/expr` surface is unchanged
except for docstring text.

### Alignment with existing architecture

- Reuses the existing `arg-metas`/`:tag` capture pipeline and the existing
  `binding-with-hint-or-cast` as the fallback, so typing behaviour is
  consistent with today.
- Keeps the source/sink separation already present: `blackhole` owns the
  output sink; this task adds only the input source barrier.
- Pure JDK dependency (`java.util.concurrent.atomic`), consistent with the
  project avoiding extra artifacts; no new build step unlike the rejected
  Java-shim option.
- Follows `clojure-coding-standards` and `testing-without-mocks` for the
  implementation and tests.

---

## Acceptance criteria

1. For the expr path, the generated measurement function reads every
   opaque-eligible argument through an atomic cell with `getOpaque` inside
   the loop, and constructs each cell exactly once before `start#`
   (verifiable by inspecting the generated/`macroexpand`ed form). (I-1)
2. A trivial pure subject (record field read, and a primitive arithmetic
   expression) reports a per-call time clearly above the hoisted value — it
   is consistent with an independent non-hoistable control rather than the
   near-zero empty-loop value. The behavioural assertion is tolerant of
   machine/noise variation (relative comparison or a floor relative to timer
   granularity), not an absolute ns threshold.
3. Primitive-valued arguments (`long`, `double`, `int`, `float`) produce no
   boxing in the measured loop (no reflection/boxed-math in the generated
   code; verified structurally and/or via `*warn-on-reflection*` /
   `*unchecked-math*` during a focused compile). (I-2)
4. Arguments with array tags, and the callable path, fall back to the
   current non-opaque binding and still produce correct measurements. Mixed
   opaque/non-opaque arguments in one expression work.
5. The existing `measured` test suite passes unchanged; non-hoistable
   subjects measure as before within noise. (I-3, I-4)
6. `measured`/`measured/expr` docstrings and the README state what the
   harness stops (constant folding, DCE), what it now does (keeps work in the
   loop via opaque input reads), and that the reported number for very cheap
   subjects includes a small constant per-argument barrier cost that is not
   subtracted.
7. Tests use `scry` for in-REPL verification where applicable and follow
   `testing-without-mocks`.

---

## Verification expectations

- Structural/code-generation test: macroexpand the expr form and assert each
  eligible argument is read via the atomic cell's `getOpaque` inside the
  loop, and the cell is built once outside it. This is the deterministic,
  non-flaky signal that the barrier is present (and the guard against the
  I-1 erosion risk of a store creeping into the loop).
- Behavioural smoke test: measure a record field read and a primitive op;
  assert the result is meaningfully above the empty-loop/hoisted value using
  a relative/tolerant check, so it is stable across machines.
- Boxing check for each primitive cell type.
- Full existing `measured` suite green.

---

## Risks

- Behavioural timing assertions are inherently noisy; mitigate with the
  structural test as the primary signal and tolerant relative checks for
  behaviour.
- Type capture that mis-tags a primitive argument as a reference would box;
  mitigated because such an argument is already untyped (Object) in today's
  path, so there is no regression, only a missed optimisation.
- The barrier adds a visible per-argument cost to previously near-zero
  results; this is intended and must be documented, not hidden (criterion 6).
