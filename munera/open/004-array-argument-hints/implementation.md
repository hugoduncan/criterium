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

## Slice 3, explicit-hint pass — 2026-10-26

- Added expression-level alias and descriptor tests: all nine array aliases and all 13 exact descriptors retain user metadata at capture and compile `alength` without reflection warnings. Real public expressions return the correct result for one and 17 evaluations. Existing structural tests establish reference-cell routing for these same tags.
- Verification: `clojure -M:kaocha:dev:test --focus criterium.arg-gen-test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 48 tests, 1592 assertions, no failures. Same runtime-blackhole environment as the previous pass; compiler timing assertions skip, and native-agent allocation checks are not claimed. No production, dependency, or design change.
- Next: non-local inference and inferred-path warning checks still need separate coverage; explicit expression hints avoid the previously recorded dynamic `make-array` local inference limit. The slow bench smoke check, version/mode validation, and documentation remain.

## Slice 3, non-local inference pass — 2026-10-26

- Unhinted array Vars exercise evaluated non-local capture across all 13 shapes. Exact descriptors, warning-free length/identity operations, all eight primitive element reads, identity, and unchanged contents pass for one and 17 evaluations. No production, dependency, or design change.
- Verification: `clojure -M:kaocha:dev:test --focus criterium.arg-gen-test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 49 tests, 1775 assertions, no failures. Runtime blackhole; compiler timing assertions skip, native-agent allocation checks not claimed. Scry is not configured.
- Delimiter repair also changed unrelated balanced forms; restored those forms from HEAD before the successful run. Next: inferred-local warning checks and slow bench smoke coverage, then version/mode validation and documentation.

## Slice 3, inferred-local warning pass — 2026-10-26

- Added real compiler warning checks for unhinted primitive and Object array locals, all eight primitive element reads, and inferred long/double `Arrays/equals` overloads. Results pass for one and 17 evaluations. Dynamic `make-array` inference remains unchanged; exact reference and multidimensional warning coverage uses the existing explicit and non-local tests. No design, production, or dependency change.
- Verification: `clojure -M:kaocha:dev:test --focus criterium.arg-gen-test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 50 tests, 1832 assertions, no failures. Same dev runtime-blackhole environment; compiler timing assertions skip and native-agent allocation checks are not claimed. Scry is not configured.
- Next: add the slow issue #156 bench smoke check, then complete version/mode validation and documentation.

## Slice 3, bench smoke pass — 2026-10-26

- Added a slow public `bench` regression with the exact issue #156 expression and options. Test-time compilation keeps a compiler regression from preventing namespace loading. The assertion checks the returned length (3), not a timing threshold. No design, production, API, or dependency change.
- Verification: `clojure -M:kaocha:dev:test slow --focus criterium.bench-test/array-local-bench-test` exited 0: 1 test, 1 assertion. `clojure -M:kaocha:dev:test all --focus criterium.arg-gen-test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 50 tests, 1832 assertions. Runtime blackhole; compiler timing assertions skip and native-agent allocation checks are not claimed. Scry is not configured.
- Slice 3 is complete. Next: fresh-process version and blackhole-mode validation, then documentation and acceptance mapping. Existing fast-profile exclusions keep the smoke test out of default fast runs.

## Slice 4 — 2026-10-26

- Rebuilt agent and blackhole Java classes with `clojure -T:build javac :project :agent :class-dir '"bases/agent/target/classes"'` and the equivalent `:blackhole` command. Both exited 0. Environment: OpenJDK 25.0.2+10-69, Darwin arm64 (`os.arch` aarch64), Kaocha. The native dylib exists, but these runs did not enable its startup agent; native allocation assertions are not claimed. Scry is not configured.
- Four fresh processes verified both `(clojure-version)` and `(criterium.blackhole/mode)` with assertions before running the suites. Clojure 1.11.4 and 1.12.4 each passed runtime mode (50 tests, 1832 assertions) and compiler mode (48 tests, 1830 assertions); all exited 0 with no failures.
- Reproduction command template (replace VERSION; add `:blackhole` before `:array-validation` for compiler mode):

```sh
clojure -Sdeps '{:aliases {:array-validation {:override-deps {org.clojure/clojure {:mvn/version "VERSION"}} :main-opts []}}}' \
  -M:kaocha:dev:test:array-validation -e '
(require (quote criterium.blackhole) (quote kaocha.runner))
(prn {:clojure (clojure-version) :mode (criterium.blackhole/mode)
      :jvm (System/getProperty "java.version") :arch (System/getProperty "os.arch")})
(assert (= "VERSION" (clojure-version)))
(assert (= :runtime (criterium.blackhole/mode)))
(kaocha.runner/-main "all"
  "--focus" "criterium.measured-array-test"
  "--focus" "criterium.measured-test"
  "--focus" "criterium.arg-gen-test")'
```

- For compiler mode, change the mode assertion to `:compiler` and add runner arguments `"--skip" "criterium.measured-test/opaque-behaviour-test" "--skip" "criterium.measured-test/opaque-depth-scaling-test"`. These two timing oracles are unsupported on Apple Silicon: the volatile control needs an `ldar` barrier, unlike x86-64. CI retains their Linux x86-64 execution. Runtime runs execute only their mode/skip assertions, not timing assertions. All other focused barrier and array checks ran in both modes. No threshold was changed.
- No code, dependency, or design change was needed. Next slice: release notes, current-contract documentation, stale knowledge proposal, and acceptance evidence map. Do not describe the unsupported timing or agent-gated allocation checks as passed.

## Slice 5 — 2026-10-26

- Added an Unreleased fix entry without a version claim. Updated public expression and metadata docstrings and current barrier design statements. Historical experimental results remain unchanged. Touched tests already describe reference transport, so no test changes were needed.
- Verification after documentation changes: `clojure -M:kaocha:dev:test all --focus criterium.arg-gen-test --focus criterium.measured-array-test --focus criterium.measured-test` exited 0: 50 tests, 1832 assertions, no failures. `git diff --check` passed. No API, dependency, or scope change.
- Proposed knowledge update, pending human approval or authorized extraction: `mementum/knowledge/jit-anti-hoisting-input-barrier.md` is stale at lines 48–49, 65–66, 208, and 216. Replace array-only fallback and pending-#156 statements with exact JVM-name string inference and `AtomicReference` transport for arrays; retain the separate callable limitation and historical experiments. No knowledge file was changed without approval.

### Acceptance evidence map

1. `criterium.bench-test/array-local-bench-test` passes the exact slow issue reproduction; inferred-local tests check one and 17 evaluations.
2. `array-tag-meta-test` checks all 13 shapes; `non-array-tag-meta-test` checks unchanged scalar, reference, and nil metadata.
3. `inferred-array-local-compilation-test`, `inferred-non-local-array-test`, `explicit-array-hint-test`, and arg-gen's `generated-array-test` cover the public capture paths and results.
4. `array-public-compiler-test`, `array-java-parameter-test`, inferred-local/non-local warning tests, and generated tests capture real compiler warnings for array length, primitive element reads, and typed Java calls. Dynamic local constructor inference remains outside scope.
5. `array-reference-structure-test` checks setup storage, escape order, both opaque-read sites, retained metadata, and no loop stores or allocations with both timers. `array-reference-identity-test` and public-path tests check identity and contents with real arrays.
6. Measured taxonomy and mixed-input tests now require reference cells for arrays. Callable tests remain separate and unchanged in meaning.
7. Slice 4 records successful fresh Clojure 1.11.4/1.12.4 runtime and compiler runs. Runtime timing assertions skip; the two compiler timing oracles are unsupported on this Darwin arm64 host and excluded per CI limits. Native-agent allocation checks were not executed. No array-specific JIT timing claim is made.
8. CHANGELOG, public and implementation docstrings, and current-contract barrier design statements now describe arrays as opaque reference values. Stale internal knowledge is identified above for approved extraction.

All checklist items are complete. No implementation blocker remains. Next review should retain the validation limits and handle the proposed knowledge update; the task remains open for review.

## Implementation review — 2026-10-26

- No new actionable issues found.

## Test review — 2026-10-26

- Added 2 steps to be addressed.
