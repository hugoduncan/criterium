---
title: Input cell routing and exact array metadata
status: active
category: measurement
related:
  - ../jit-anti-hoisting-input-barrier.md
depends-on: []
---

# Problem

A pure subject whose inputs are bound once before the measured loop lets the
JIT lift the work out of the loop, so `measured/expr` reports empty-loop cost,
not the expression. The defect is sharper than generic loop-invariant code
motion: when the subject reads a `final` field of a loop-invariant object
(every `defrecord` field is `final`), the JIT folds the read to a constant
because it relies on immutability, not on "memory may have changed".

# Key fact

No output-side memory barrier (compiler or runtime blackhole) can fix
final-field folding. The only effective intervention is the input: make the
value feeding the expression come from a read the compiler can not prove is
stable.

# Solution

Read each argument through an opaque cell inside the loop (`VarHandle`
`getOpaque` access mode). The JIT may not merge two opaque reads of the same
location, so it can not prove the object is the same as last iteration and can
not hoist. Opaque has no acquire/release fence, so it is far cheaper than a
volatile read or the runtime blackhole.

Implementation choices (criterium `measured.impl`):

- Use `java.util.concurrent.atomic` types as the mutable place, NOT raw
  `VarHandle`. Their `getOpaque`/`setOpaque` are ordinary typed methods, so
  Clojure calls them with no `@PolymorphicSignature` varargs boxing and no
  custom Java shim/build step. `deftype` offers only plain (hoistable) or
  volatile (over-strong) mutability, never opaque.
- Cell selection is a pure code-gen-time predicate over the captured `:tag`.
  Opaque-eligible iff tag ∈ `#{long int double float}` (the direct-cast
  primitives, mirror `cast-fn`'s set) OR tag ∈ `#{boolean byte short char}`
  (the small primitives) OR a symbol OR a string OR absent.
  Valid class-name strings and array hints select `:reference`. Other metadata
  values select `:fallback`; this is not a class-hint validation API.
  The `measured/callable` whole-binding path (#157) is separate.
- Unboxed cells: `AtomicLong` for `long`/`double` (`double` via
  `Double/doubleToRawLongBits`/`longBitsToDouble`); `AtomicInteger` for
  `int`/`float` (`float` via `Float/floatToRawIntBits`/`intBitsToFloat`, the
  int-sized-bits → int-sized-cell rule), and for the small primitives
  `boolean`/`byte`/`short`/`char` (task 003) by **widening into the
  `AtomicInteger` on store and narrowing on read**: `byte`/`short`/`char`
  store `(int x)` and read `(unchecked-byte|short|char (.getOpaque cell))`;
  `boolean` stores `(if b 1 0)` and reads `(not (zero? (.getOpaque cell)))`.
  CORRECTION: the earlier "a cell would box, so these fall back" claim was
  wrong — it holds only for `AtomicReference`, NOT for the widen-into-a-
  primitive-cell pattern. Any primitive narrower than `int` fits unboxed in an
  `AtomicInteger`; `(int ^byte x)` widening, the `AtomicInteger(int)` ctor,
  `getOpaque`, and the `unchecked-*`/`zero?` reads all stay unboxed (verified
  under `*warn-on-reflection*` + `*unchecked-math* :warn-on-boxed`, and no
  per-iteration allocation with the native agent — the boolean read's
  `Boolean/TRUE`/`FALSE` are canonical, not allocated).
- Arrays are references, including primitive arrays. Store the original array
  in an `AtomicReference`; do not copy it or box its elements. Escape the cell
  once before timing. Read it opaquely for the initial and loop evaluations,
  with the exact array hint on each read binding. Transport preserves identity
  and contents; it does not prevent mutations by the subject.
- Store exactly once before timing starts (I-1); the loop only reads opaquely.
  A store inside the loop adds cost and enables store-to-load forwarding that
  cancels opacity.

# Barrier cost is an artifact, never subtracted

Per-argument opaque-read cost: ~0.07 ns primitive, ~0.11 ns reference; reads
do not pipeline, so cost scales with arity; run-to-run variance ~25-30%. It is
documented, not subtracted (subtracting a noisy same-order baseline produces
negative times — #19).

# Array compiler metadata

`criterium.measured.impl/tag-meta` uses `Class/getName` strings for arrays:
`"[J"`, `"[Ljava.lang.String;"`, and `"[[J"` preserve component types and
number of dimensions. `Class/getCanonicalName` produces invalid hints such as
`long[]`. A generic `objects` hint loses the exact type. Explicit aliases such
as `longs` and descriptor-string hints remain valid.

Keep metadata capture in `tag-meta` and barrier routing in `cell-kind`.
`criterium.arg-gen/arg-metas-from-example` also uses `tag-meta`; metadata
changes therefore need measured and generated-argument coverage. A compilation
fix alone does not establish opaque routing. Check metadata, cell structure,
and compiled public API behaviour separately.

Unhinted dynamic `make-array` locals can infer Object. Use explicit exact hints
or evaluated non-local arguments to test the exact array-read contract; do not
assume that array transport extends local type inference.

Compiler and functional checks passed on Clojure 1.11.4 and 1.12.4 in fresh
runtime and compiler-blackhole processes. This does not establish array-specific
JIT timing behaviour or native-agent allocation results. The compiler-mode
`opaque-behaviour-test` and `opaque-depth-scaling-test` timing oracles are
calibrated for Linux x86-64, not Darwin arm64. Runtime-mode timing assertions
skip. Report these limits separately from structural and functional results.

# Scope boundaries

One cell per captured argument does not mean scalar-valued arguments only.
Arrays use the same reference-cell contract. Small primitives use
`AtomicInteger` with widening on store and narrowing on read. The input pool
(`:args-pool`, #158) is a separate extension. The callable path (`[:as args]`)
is not decomposed per argument and retains its whole-binding fallback (#157).
Invalid canonical array hints have no compatibility contract.

# Evidence

Array contract: task 004 `design.md`, `plan.md`, and `implementation.md`;
commits `36d4a04` (metadata) and `d8179be` (reference routing).
Version and platform limits: task 004 implementation notes, slice 4.
