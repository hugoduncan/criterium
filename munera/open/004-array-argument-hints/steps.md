# Implementation steps

## Slice 1 — Exact metadata

- [x] Add table-driven `tag-meta` tests in measured tests for all eight primitive arrays, `Object[]`, `String[]`, a test-defined component class, `long[][]`, and a multidimensional reference array; assert each exact JVM-name string.
- [x] Add non-array `tag-meta` regression assertions for nil, reference classes, and existing scalar conversions.
- [x] Add test-time compiler regressions for inferred array locals; confirm that the current defect fails assertions without preventing test namespace loading.
- [x] Change `tag-meta` in `bases/criterium/src/criterium/measured/impl.clj` to use `Class/getName` strings for arrays while retaining the non-array branch.
- [x] Run the slice's metadata and local compilation tests; record results in `implementation.md`.

## Slice 2 — Reference-cell routing

- [x] Replace array-fallback taxonomy assertions with `:reference` and `opaque-tag?` assertions for inferred descriptors, explicit array aliases, and valid non-array string class hints.
- [x] Update `cell-kind` to route string class hints to reference cells; remove exclusions and helpers needed only for invalid canonical array symbols without adding compatibility handling.
- [x] Extend generated-form checks for array descriptors and aliases to prove one setup store, one setup escape before timing, opaque reads at both subject evaluation sites, and no per-iteration cell allocation or store, with default and custom timers.
- [x] Add assertions that reference-cell read bindings retain the exact array metadata.
- [x] Replace remaining array-fallback binding and mixed-argument expectations with reference-cell expectations; retain callable-path checks as separate tests.
- [x] Add runtime identity and unchanged-content assertions for array transport across single and repeated evaluations, including mixed scalar/array arguments.
- [x] Run the affected taxonomy, structural, identity, scalar, and callable tests; record results in `implementation.md`.

## Slice 3 — Public API and compiler coverage

- [x] Add `measured/expr` result tests for inferred local arrays across the plan's array matrix, using evaluation counts of one and greater than one.
- [x] Add evaluated non-local array result tests and inspect captured metadata to verify the non-local inference path.
- [x] Add explicit array-alias and descriptor-string hint tests that verify preserved metadata and correct results after opaque reads.
- [x] Add deterministic generated-array tests in `bases/arg-gen/test/criterium/arg_gen_test.clj` for exact `arg-metas-from-example` metadata and correct `arg-gen/measured` results, including primitive, user-defined component, and multidimensional arrays.
- [x] Add explicit-descriptor compiler checks for `alength` across the matrix, primitive `aget`, and representative Java overloads.
- [ ] Add compiler-warning assertions for `alength` across the array matrix, `aget` across all eight primitive array types, and representative statically typed Java array-parameter calls; capture warnings during test-time compilation.
- [ ] Add a slow smoke check using issue #156's exact `bench` expression and options from `plan.md`; require successful completion, not a timing value.
- [ ] Run the new public API, generated-argument, compiler-warning, and smoke checks; record results in `implementation.md`.

## Slice 4 — Validation

- [ ] Prepare required local Java classes with the existing build tasks and record the JDK, architecture, test runner, and native-agent availability.
- [ ] Run focused array metadata, compiler, and functional checks in a fresh Clojure 1.11.4 process; verify the effective version and record the command and result.
- [ ] Run the same focused checks in a fresh Clojure 1.12.4 process; verify the effective version and record the command and result.
- [ ] Run the relevant measured and arg-gen suites with the repository test configuration; record runner results and resolve failures caused by this change.
- [ ] Run existing barrier checks and new array structural/functional checks in a fresh runtime-blackhole process; verify and record the active mode and any platform or agent exclusions.
- [ ] Run existing barrier checks and new array structural/functional checks in a fresh compiler-blackhole process where supported; verify the active mode, follow CI's Linux x86-64 timing-test limits, and record results or the exact unsupported-environment reason.

## Slice 5 — Documentation and acceptance

- [ ] Add a user-facing `CHANGELOG.md` entry for issue #156 that describes exact array hints and opaque reference transport without a new version claim.
- [ ] Update affected docstrings and comments in measured implementation, public `measured/expr` documentation, and touched tests to describe arrays as reference-cell values.
- [ ] Update current-contract statements in `design/opaque-input-barrier.md` that describe arrays as a pending defect or non-opaque fallback; preserve labelled historical experiment results and unrelated scope decisions.
- [ ] Identify stale array-fallback statements in `mementum/knowledge/jit-anti-hoisting-input-barrier.md` and record the proposed knowledge update for human approval or the authorized knowledge-extraction workflow.
- [ ] Review the diff against all eight design acceptance criteria and record an evidence map, validation limits, and any remaining blockers in `implementation.md`.
