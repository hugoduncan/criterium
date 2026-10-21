---
title: JIT anti-hoisting input barrier for cheap pure expressions
status: active
category: measurement
tags: [jit, microbenchmark, measured, opaque, varhandle, final-field, boxing]
related:
  - bases/criterium/src/criterium/measured/impl.clj
  - design/opaque-input-barrier.md
  - "issues: #155 #156 #157 #158 #159 #19 #17"
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
  Opaque-eligible iff tag ∈ `#{long int double float}` (mirror `cast-fn`'s
  set), a reference-class symbol, or absent. Fallback to the plain binding for
  `boolean`/`byte`/`short`/`char`, array tags (`long[]` ...), and anything
  else — no unboxed atomic cell exists for them, so a cell would box.
- Unboxed cells: `AtomicLong` for `long`/`double` (`double` via
  `Double/doubleToRawLongBits`/`longBitsToDouble`); `AtomicInteger` for
  `int`/`float` (`float` via `Float/floatToRawIntBits`/`intBitsToFloat`, the
  int-sized-bits → int-sized-cell rule).
- Store exactly once before timing starts (I-1); the loop only reads opaquely.
  A store inside the loop adds cost and enables store-to-load forwarding that
  cancels opacity.

# Barrier cost is an artifact, never subtracted

Per-argument opaque-read cost: ~0.07 ns primitive, ~0.11 ns reference; reads
do not pipeline, so cost scales with arity; run-to-run variance ~25-30%. It is
documented, not subtracted (subtracting a noisy same-order baseline produces
negative times — #19).

# Scope boundaries

Scalar, one cell per argument; the input pool (`:args-pool`, #158) is a later
extension that reuses the cell abstraction. The callable path (`[:as args]`)
is not per-argument decomposed, so it takes the whole-binding fallback (#157).
Array-typed tags take the fallback and are a pre-existing defect (#156).
