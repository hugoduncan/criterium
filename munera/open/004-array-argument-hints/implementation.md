# Implementation notes

## Task creation — before step 1

The user selected the recommended fix from the issue #156 analysis: exact JVM-name string hints for arrays, with reference-cell support. No implementation has started.

Analysis confirmed the compilation failure on Clojure 1.12.4 / JDK 25. Separate compiler probes on Clojure 1.11.4 and 1.12.4 accepted string array descriptors after an opaque reference read without `alength` reflection warnings. These probes did not run the full suites or establish array-specific JIT timing behaviour.

The existing array-fallback tests encode the current defect and must not constrain the new design. Existing callable fallback behaviour is outside this task.

## Architecture review — 2026-10-26

- No architectural review feedback.
- `ramora/META.md` and `doc/architecture.md` are absent from this checkout. Review used `AGENTS.md`, `design/opaque-input-barrier.md` (D-2, D-3, D-5), and the current metadata and cell code. D-2's scalar-cell rule means one cell per argument, not a restriction on array-valued arguments.

## Ambiguity review — 2026-10-26

- No ambiguity review feedback.

## Inconsistency review — 2026-10-26

- No inconsistency review feedback.

## Review handoff — 2026-10-26

- Code reference: `bases/criterium/src/criterium/measured/impl.clj` owns `tag-meta`, `cell-kind`, and `opaque-cell`. Keep eligibility derived from `cell-kind`; the existing reference read preserves argument metadata. `bases/arg-gen/src/criterium/arg_gen.clj` delegates generated metadata to `tag-meta` through `arg-metas-from-example`.

## Plan ambiguity review — 2026-10-26

- No new actionable ambiguity feedback. Reviewed the five-slice plan against read-only `steps.md`, prior notes, metadata/cell code, measured and arg-gen tests, barrier documentation, and test/CI configuration; context loaded for the shared `plan-review` inconsistency turn.

## Plan inconsistency review — 2026-10-26

- No new actionable inconsistency feedback across the design, five-slice plan, read-only `steps.md`, and prior implementation notes.

## Slice 1 — 2026-10-26

- `tag-meta` now returns JVM-name strings for arrays; non-array conversions are unchanged. Added a separate measured array test namespace with 13 array shapes and scalar/nil checks. No dependency changes.
- Build test-time compiler forms with `list`, not syntax-quoted sequence construction: the latter produced an empty captured argument vector and a nil result in this path. Expression-factoring changes are outside scope.
- Red check against the original implementation: namespace loaded; Kaocha reported 13 metadata failures and one compiler error (runner stopped that test at the first invalid hint).
- Verification: `clojure -M:kaocha:dev:test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 40 tests, 598 assertions, no failures. Environment: JDK 25.0.2, arm64, runtime blackhole, dev Clojure 1.12.4. Scry is not configured. Timing assertions and native-agent allocation assertions are not claimed as executed.
- Next: string tags still use plain bindings. Slice 2 must establish opaque reference transport; successful compilation here does not prove barrier routing. The design remains unchanged.

## Slice 2 — 2026-10-26

- String class hints now select `:reference`; removed the canonical-array exclusion and its unused string dependency. Invalid canonical symbols have no compatibility handling. No design change or dependency addition.
- Array descriptor and alias form checks cover both timers, constructor storage of the original argument, setup escape ordering, both opaque reads, read metadata, and absence of loop stores/allocations. Real array tests cover identity and contents for one and 17 evaluations, including mixed scalar/array inputs. Callable expectations remain separate.
- Verification: `clojure -M:kaocha:dev:test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 42 tests, 1357 assertions, no failures. Runtime blackhole; the two compiler-mode timing oracles skipped their timing assertions. Native-agent allocation checks are not claimed as executed. Scry is not configured.
- Next slice: add public API, generated-array, and compiler-warning coverage. Structural helpers are reused from `criterium.measured-test` through private vars to avoid duplicating traversal logic or extending that oversized file. The design remains unchanged. Barrier knowledge still contains stale fallback statements; proposed update remains in slice 5.

## Slice 3, compiler coverage pass — 2026-10-26

- Added repeated inferred-local length checks and warning-free explicit-descriptor checks for all 13 array shapes, all eight primitive `aget` operations, and `Arrays/equals` long, double, and String array overloads. Tests use the real compiler and public measured API, without mocks.
- Discovery: unhinted dynamic `make-array` reference/multidimensional constructors leave outer locals typed as Object and emit reflection warnings. Explicit exact descriptors isolate the opaque-read compiler contract without expanding inference scope. Remaining inferred/non-local coverage must account for this distinction.
- Verification: `clojure -M:kaocha:dev:test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 44 tests, 1442 assertions. Runtime blackhole on the existing dev environment; compiler timing assertions skipped, native allocation checks not claimed. No production or dependency changes. Generated arrays, non-local metadata, aliases, smoke check, version/mode validation, and documentation remain.

## Slice 3, generated-array pass — 2026-10-26

- Added deterministic `gen/return` tests at both generated metadata and public macro boundaries. Primitive, user-defined component, primitive multidimensional, and reference multidimensional arrays compile without reflection warnings and preserve reference identity, length, and contents for one and 17 evaluations. No production, API, dependency, or design change.
- `arg-metas-from-example` takes a generator form that produces the argument tuple, not a generator value. The test uses a real tuple generator and does not override inference.
- Verification: `clojure -M:kaocha:dev:test --focus criterium.arg-gen-test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 47 tests, 1504 assertions, no failures. Runtime blackhole; compiler-mode timing assertions skipped, native-agent allocation checks not claimed. Scry remains unconfigured. An unrelated existing boxed-math warning in `build/check_warnings.clj:348` appeared outside captured array compilation.
- Next: finish non-local metadata, explicit aliases, inferred-path compiler warnings, and the slow bench smoke check before version/mode validation and documentation.
