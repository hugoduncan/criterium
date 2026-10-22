🔁 `(c/bench 1)` → ~0.05 ns median is expected, not a bug.

The `1` IS captured as an opaque-barrier argument (decompiles to an
`AtomicLong` holding `doubleToRawLongBits(1.0)`, re-read each iteration via
`getOpaque`). The subject does no work beyond that read, so the reported time
is essentially the per-argument barrier-read cost — which the design authority
itself measured at ~0.055–0.073 ns per primitive read. So the number is
consistent and "realistic" as a barrier-cost/throughput floor.

🌀 But it is sub-cycle (~0.27 cycle at ~5 GHz). A single opaque L1 load has
~3–4 cycle latency (~0.6–0.8 ns); seeing ~0.05 ns means iterations OVERLAP —
the loop is load-port/throughput-bound, not latency-bound. The barrier stops
hoisting, NOT cross-iteration ILP.

❌ Trap: the authority's claim that the cost "cannot hide behind ILP" is only
true within one iteration (arity additivity). Across iterations ILP dominates.

Rule of thumb: a sub-~0.2 ns median = "iterations overlapped / subject is
near-degenerate," not "the expression is that fast."
