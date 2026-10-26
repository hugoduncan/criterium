# Working state

## Current work

PR [161](https://github.com/hugoduncan/criterium/pull/161): the lint fix passed all matrix jobs. Run `40346440735` then failed only the Ubuntu non-agent compiler-warning check. Reproduced locally and fixed boxed arithmetic in the new `mixed-array-result` helper with primitive scalar parameter hints. Full warning check now passes (284 namespaces); CI-pinned clj-kondo passes; focused suites pass (51 tests, 1878 assertions); full default suite passes (1103 tests, 13143 assertions). Remote confirmation is pending for this second fix.

CI preflight must include `clojure -M:dev:test:check-warnings`, not only tests and lint. It runs only in the Ubuntu non-agent job. Local clj-kondo 2026.04.15 also reports two warnings in unchanged timing helpers that CI-pinned 2024.09.27 does not report.

Task [004 — Array argument hints and opaque reference reads](../munera/closed/004-array-argument-hints/design.md) is complete and closed at the user's request. All implementation and review follow-up steps are complete. Final implementation, test, documentation, and code-shape reviews found no remaining actionable issues.

Arrays now use exact JVM-name string hints and opaque reference cells. Tests cover metadata, routing, structure, identity, contents, compiler behaviour, public APIs, and generated arguments. The final focused measured, array, and arg-gen suites passed with both original and reversed array-case order: 51 tests and 1878 assertions in each run. Original order was restored.

Knowledge extraction completed in `2d2e834` and `02e5593`. The input-barrier knowledge now describes array reference transport; the type-capture memory records the mixed-input expression-factoring lesson. No knowledge update remains pending for task 004.

No tests were rerun for closure. See the task's implementation notes for the acceptance evidence and review results. See [Munera plan](../munera/plan.md) for scheduled work.

## Relevant knowledge

- [Input barrier](knowledge/jit-anti-hoisting-input-barrier.md): cell routing, exact array metadata, setup escape, opaque reads, and measurement limits. Arrays use `AtomicReference` without array copies or element boxing.
- [Type capture routing](memories/capture-arg-types-only-widens-long-double.md): verify captured metadata and cell routing directly; expression factoring can change which inputs a test exercises.

## Validation limits

Fresh runtime and compiler-mode checks passed on Clojure 1.11.4 and 1.12.4. Compiler timing oracles are unsupported on Darwin arm64; native-agent allocation checks were not run. Dynamic `make-array` locals can infer `Object`; broader inference changes remain outside task 004's scope.
