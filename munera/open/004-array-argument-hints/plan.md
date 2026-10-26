# Implementation plan

## Approach

Use exact JVM-name strings for inferred array metadata and the existing `AtomicReference` cell for array transport. Keep compiler metadata in `tag-meta` and barrier selection in `cell-kind`, both in `bases/criterium/src/criterium/measured/impl.clj`. Keep eligibility derived from the cell taxonomy. The reference branch of `opaque-cell` already preserves argument metadata; retain that contract.

For array classes, `tag-meta` must return `{:tag (.getName t)}`. Preserve the existing non-array branch, nil handling, and scalar conversions. Classify valid string class hints as references, including non-array class names. Preserve explicit array aliases and descriptor strings. Do not introduce class loading, runtime array inspection, a new cell kind, or a Java helper. Remove canonical-array exclusions that exist only for the defect; invalid `long[]` symbols are not a compatibility contract. Do not add a new invalid-hint validation API.

Add tests at three boundaries: exact metadata, generated barrier forms, and compiled public API behaviour. Use real compiler evaluation and existing structural helpers, without mocks or source-text assertions. Compile regression forms during test execution so the known defect does not prevent namespace loading. Confirm captured metadata directly: a successful result alone does not prove the intended routing.

Use a shared test matrix where practical: boolean, byte, short, char, int, long, float, and double arrays; `Object[]`; `String[]`; a test-defined record/class array; `long[][]`; and a multidimensional reference array. Assert exact component types and dimensions. Compile `alength` across the matrix, primitive `aget` across all eight primitive types, and representative statically resolved Java array-parameter calls. Capture reflection warnings during compilation, not after it. Avoid claiming that the fix changes unrelated inference of array element result types.

Cover local, evaluated non-local, and generated arguments, plus explicit alias and descriptor-string hints. Use deterministic generators in `bases/arg-gen/test/criterium/arg_gen_test.clj` to exercise `arg-metas-from-example` and `arg-gen/measured`. Preserve identity and contents for single and repeated evaluations; subject mutations remain outside the transport guarantee.

## Slice order

1. **Exact metadata:** Add metadata/compiler regressions in measured tests, then change `tag-meta`. Verify exact array tags and unchanged non-array behaviour.
2. **Reference-cell routing:** Extend `cell-kind`, replace obsolete array-fallback expectations, and verify setup, escape, opaque reads, hints, identity, and mixed scalar/array arguments. Keep callable checks separate and unchanged.
3. **Public API coverage:** Exercise local, non-local, explicit-hint, and generated arrays; check warning-free compilation and correct results. Add the exact issue #156 `bench` smoke check without a timing-value assertion.
4. **Validation:** Run focused compiler/functional checks on Clojure 1.11.4 and 1.12.4, relevant suites, and existing barrier checks in separate runtime and compiler-blackhole processes where supported. Record evidence and limits.
5. **Documentation and acceptance:** Update release notes and barrier documentation, identify stale knowledge, and map evidence to all eight acceptance criteria.

## Validation and completion evidence

Extend existing measured structural helpers to prove, per array argument, one reference store at construction and one cell escape before timing, no loop cell allocation or stores, and opaque reads for both initial and loop subject evaluations. Exercise default and custom timers and mixed arguments. Assert exact metadata on read bindings and identity at runtime. Retain scalar, callable, and shared reference-cell regression checks.

Use the repository's Kaocha configuration (`tests.edn`, `:kaocha:dev:test`) for measured and arg-gen suites. Prepare local Java dependencies with the existing build tasks if needed. Run version checks in fresh processes with an explicit dependency override; verify the effective Clojure version because `:dev` pins 1.12.4. No permanent dependency change is needed. Use scry structured results if available in the selected test environment; otherwise record runner exit status and failures.

Runtime mode omits `:blackhole`; compiler mode adds it on JDK 17+. Verify the active mode. Run structural and functional checks in both supported modes. Existing `opaque-behaviour-test` and `opaque-depth-scaling-test` follow `.github/workflows/tests.yml`: their compiler-mode timing oracle is calibrated for Linux x86-64, not Apple Silicon. Do not change thresholds or use timing to prove type correctness. The two timing tests skip their timing assertions in runtime mode; report those skips separately from executed checks. Record unsupported checks explicitly; do not report them as passed. Preserve existing agent-gated allocation checks and state whether they ran.

The issue smoke check is `(let [xs (long-array [1 2 3])] (b/bench (alength xs) :viewer :none :limit-time-s 1))`, with `b` bound to `criterium.bench`. Keep this slower check outside the default fast test path.

## Risks

- Broad type inference or expression factoring changes would exceed scope. Confirm the captured tag rather than compensating with new inference rules.
- A generic `objects` hint loses component types and dimensions and can change method selection.
- Copying arrays or converting elements changes identity and measured work. Use the existing reference cell unchanged.
- Generated metadata comes from a separate base. Direct helper tests alone do not cover that integration.
- Blackhole mode is selected at load time. Separate processes and verified JVM flags are required.
- JDK, architecture, native-agent availability, or dependency downloads can limit validation. Record the exact blocker and required follow-up.

## Planning review

The design is complete. It agrees with issue #156, the metadata/cell implementation, and arg-gen's use of `tag-meta`. Current array-fallback tests and documentation describe the defect; they are replacement targets, not design contradictions. The plan ambiguity review treated `steps.md` as read-only context. It found no unresolved action or ordering ambiguity; runtime timing-test skips are now explicit in this plan. The design consistency review found no internal inconsistency or conflict with referenced artifacts. No blocking design ambiguity was found. The phrase “where supported” is made concrete above using the existing CI platform limits. Slice 1 is complete. Array regressions live in the dedicated `criterium.measured-array-test` namespace because the existing measured test file already exceeds the coding standard file-size limit. Slice 2 is complete. Slice 3 is in progress. Repeated inferred-local results and explicit-descriptor compiler checks pass. Dynamic `make-array` reference and multidimensional constructors do not give the outer local an exact compiler type; warning checks for inferred paths must distinguish that pre-existing inference limit from reference-cell metadata. Structural array checks reuse the existing measured test helpers through private vars; runtime checks use real arrays and opaque reads.

Generated-array coverage now checks exact example metadata and warning-free public macro compilation for primitive, user-defined component, and multidimensional arrays. Real constant generators preserve identity and contents for one and 17 evaluations. Explicit alias and descriptor expression hints now preserve captured metadata and compile warning-free for single and repeated length evaluations. Remaining slice 3 work is non-local inference, inferred-path warning checks, and the slow bench smoke check.
