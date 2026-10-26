# Working state

## Current work

Task [004 — Array argument hints and opaque reference reads](../munera/open/004-array-argument-hints/design.md) is implemented and remains open. The latest test-review follow-up is complete: populated arrays and independent nested-content snapshots now detect clearing and inner-element changes, with separate identity checks after one and 17 evaluations. Focused measured, array, and arg-gen suites passed (51 tests, 1878 assertions). No unchecked review follow-ups remain. Test review after the content-check changes found no new actionable issues; the focused suites passed again (51 tests, 1878 assertions). Implementation review found no new actionable issues. The escape-check review follow-up is complete: array tests match constructor-bound symbols, not generated names. The mixed array/scalar review follow-up is also complete: bare inputs have metadata, routing, per-cell structure, identity, content, and result checks. The updated focused suites passed (51 tests, 1878 assertions). A fresh review run passed the focused suites (50 tests, 1832 assertions) and the slow issue #156 smoke check (1 test, 1 assertion). All five slices are complete. Exact JVM-name strings and opaque reference cells preserve array types and identity. The acceptance evidence map and proposed knowledge correction are in the task's implementation notes. Clojure 1.11.4 and 1.12.4 each passed fresh runtime (50 tests, 1832 assertions) and compiler (48 tests, 1830 assertions) runs. Compiler timing oracles are unsupported on Darwin arm64; native allocation checks are not claimed. The final documentation pass also passed the focused suites (50 tests, 1832 assertions). Dynamic make-array locals can infer Object; inference expansion is outside scope.

See [Munera plan](../munera/plan.md) for the task order.

## Relevant knowledge

- [Input barrier](knowledge/jit-anti-hoisting-input-barrier.md): explains cell setup, escape, and opaque reads. Its array-fallback statements are stale after task 004; the proposed correction awaits approval or authorized extraction. Arrays are reference values and can use `AtomicReference` without boxing their elements.
- [Type capture routing](memories/capture-arg-types-only-widens-long-double.md): verify metadata and cell routing directly; functional or timing checks alone can exercise the wrong path.

## Analysis evidence

The current array-hint defect was reproduced on Clojure 1.12.4 / JDK 25. Focused compiler probes on Clojure 1.11.4 and 1.12.4 accepted exact JVM-name strings after opaque reference reads for primitive, reference, and multidimensional arrays. Focused version/mode validation is complete; see the task notes for platform and agent limits.
