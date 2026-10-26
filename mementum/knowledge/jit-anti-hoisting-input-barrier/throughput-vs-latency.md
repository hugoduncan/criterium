---
title: Input barrier throughput versus latency
status: active
category: measurement
related:
  - ../jit-anti-hoisting-input-barrier.md
depends-on: []
---

# Throughput vs latency: the barrier does not serialize iterations

The barrier defeats *hoisting* (lifting work out of the loop). It does NOT
defeat *cross-iteration instruction-level parallelism*. The opaque cell holds
the same value every iteration and there is no loop-carried dependency, so the
out-of-order core overlaps many iterations: `getOpaque` is a plain `ldr` with
no fence, so independent loads to the same address run concurrently, bounded
only by load-port throughput.

Consequence: for a near-degenerate subject the reported median can go
sub-cycle. Observed `(c/bench 1)` ≈ 0.054 ns/call (JDK 25, arm64) — the `1` is
a captured double argument, so the loop does one opaque read + consume and
nothing else, and the median is just the per-argument barrier-read cost
(~0.055–0.073 ns, matching the authority's own figures). At ~5 GHz that is
~0.27 cycle; a single opaque L1 load has ~3–4 cycle latency (~0.6–0.8 ns), so
a ~0.054 ns median is only possible because iterations overlap.

Caveat on the authority text: `design/opaque-input-barrier.md` says the
per-read cost "cannot hide behind instruction-level parallelism." That holds
only *within* one iteration (reading k cells costs ~k× because the loads
saturate load-port throughput, which is itself the throughput-bound, not
latency-bound, signature). *Across* iterations ILP dominates, which is exactly
why a single-read-per-iteration loop lands far below single-load latency.

Rule of thumb: a sub-~0.2 ns median means "iterations overlapped / subject is
near-degenerate," not "the expression is that fast." Forcing a loop-carried
dependency (feed each iteration's read into the next) would turn the loop
latency-bound, but that is a different measurement contract and is NOT in
scope of the callable-path (#157), argument-pool (#158), or reporting-baseline
(#159) work. The array-hint correction (#156) also did not change this contract.

