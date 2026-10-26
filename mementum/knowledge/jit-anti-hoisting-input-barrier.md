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

# Input barrier knowledge

The input barrier uses opaque reads from cells that escape once before timing.
It prevents input-based hoisting, but does not serialize independent iterations.
Arrays use reference cells with exact JVM-name string hints, not plain bindings.
The callable whole-binding path remains separate.

Read the section needed for the change:

- [Input cell contract](jit-anti-hoisting-input-barrier/input-cell-contract.md):
  cell routing, primitive storage, exact array metadata, transport guarantees,
  compiler coverage, and scope limits.
- [Escape analysis](jit-anti-hoisting-input-barrier/escape-analysis.md):
  historical failure, experiment results, setup escape fix, and process-level
  blackhole mode selection.
- [Throughput versus latency](jit-anti-hoisting-input-barrier/throughput-vs-latency.md):
  iteration overlap, sub-cycle results, and measurement limits.
