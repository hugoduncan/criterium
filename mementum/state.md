# Working state

## Current work

Task [004 — Array argument hints and opaque reference reads](../munera/open/004-array-argument-hints/design.md) is open for [GitHub issue #156](https://github.com/hugoduncan/criterium/issues/156). The user selected exact JVM-name string hints for arrays and the existing opaque reference-cell path. Design, plan, steps, and initial implementation notes are written. Implementation has not started.

See [Munera plan](../munera/plan.md) for the task order.

## Relevant knowledge

- [Input barrier](knowledge/jit-anti-hoisting-input-barrier.md): explains cell setup, escape, and opaque reads. Its array-fallback statements describe the current implementation and must be revisited when task 004 lands. Arrays are reference values and can use `AtomicReference` without boxing their elements.
- [Type capture routing](memories/capture-arg-types-only-widens-long-double.md): verify metadata and cell routing directly; functional or timing checks alone can exercise the wrong path.

## Analysis evidence

The current array-hint defect was reproduced on Clojure 1.12.4 / JDK 25. Focused compiler probes on Clojure 1.11.4 and 1.12.4 accepted exact JVM-name strings after opaque reference reads for primitive, reference, and multidimensional arrays. Full-suite and JIT validation remain implementation work.
