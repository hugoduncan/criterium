# Implementation steps

- [ ] Add metadata and compiler regression coverage for the array type matrix.
- [ ] Emit exact JVM-name string hints for inferred array classes; preserve scalar behaviour.
- [ ] Route supported string class hints through the existing reference cell.
- [ ] Replace array-fallback expectations with identity, hint-preservation, setup, escape, and opaque-read checks; retain callable checks.
- [ ] Cover local, non-local, explicit-hint, and generated array arguments through public APIs.
- [ ] Verify the issue's `bench` reproduction without asserting a timing value.
- [ ] Run focused checks on Clojure 1.11.4 and 1.12.4 and relevant measured and arg-gen suites.
- [ ] Run existing barrier checks in runtime and compiler-blackhole configurations where supported.
- [ ] Update release notes and affected documentation; identify stale knowledge for the knowledge workflow.
- [ ] Review acceptance criteria and record validation results and any remaining limits.
