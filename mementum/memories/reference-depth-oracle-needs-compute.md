❌ A pure linked field-read chain does NOT scale with depth even when the
opaque cell escapes — so it can not serve as a depth-scaling (folding) oracle
for the `reference` cell kind.

Task 002 planned a depth-N `reference` oracle built from records whose field
points to the next record (chain reached through the opaque reference cell).
Measured JDK 25 arm64, escaped cell: depth 1/3/8 all ~0.22 ns (compiler) /
~0.59 ns (runtime) — flat. Without escape ~0.072 ns (folded), so escape DOES
work, but chain depth is not observable.

🌀 Cause: successive field loads carry no data dependency the core must
serialize, so independent iterations overlap the loads — the same
cross-iteration ILP that makes `(bench 1)` sub-cycle. Only a compute op
downstream of the read is throughput-bound enough to scale with depth.

🔁 Fix: build the reference oracle as a field read feeding a `Math/sqrt` chain
(a `DoubleNode` record), not a pure pointer-chase. For any depth-scaling
folding oracle the per-step work must be a latency/throughput-bound compute op,
not a load.
