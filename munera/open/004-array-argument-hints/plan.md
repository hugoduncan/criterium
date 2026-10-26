# Implementation plan

## Approach

Use JVM-name string metadata for inferred arrays and the existing opaque reference cell for array arguments. Keep type capture and cell classification as separate responsibilities. Do not expand this fix into general inference or callable changes.

## Work sequence

1. Establish regression coverage at the metadata and compilation boundaries. Include all primitive arrays, typed reference arrays, a user-defined component class, and multidimensional arrays. Use evaluated forms where necessary so an expected compiler error does not prevent a test namespace from loading.
2. Update `tag-meta` in `bases/criterium/src/criterium/measured/impl.clj` to use `Class/getName` as a string for array classes. Preserve the existing non-array branch and scalar conversions.
3. Update the cell taxonomy so supported string class hints select `:reference`. Preserve metadata on the opaque read. Review the obsolete canonical-array exclusion and retain only logic needed for valid supported inputs. Do not add compatibility handling for invalid `long[]` symbols.
4. Replace array-fallback assertions with reference-cell contract checks. Keep callable fallback coverage separate. Check identity, cell setup and escape outside timing, and opaque reads inside both subject evaluation sites.
5. Exercise the public `measured/expr` and arg-gen paths. Include explicit alias and descriptor-string hints and non-local arrays. Add a small `bench` smoke check for the issue reproduction, without a timing-value assertion.
6. Run focused checks on both requested Clojure versions and the relevant suites. Run existing barrier checks under runtime and compiler-blackhole process configurations where supported.
7. Update the changelog and affected source, public API, and design documentation. Review `mementum/knowledge/jit-anti-hoisting-input-barrier.md` for statements made stale by the fix and handle updates through the knowledge workflow.

## Validation

Compiler correctness and barrier structure are separate contracts. Test both rather than inferring routing from a successful benchmark or a timing result. Capture reflection warnings for operations with statically resolvable array types. Do not require the fix to eliminate unrelated warnings from operations whose result type Clojure cannot infer.

Use existing test helpers and real compiler evaluation. No mocks or production test switches are needed. The existing shared reference-cell tests supply the JIT behaviour contract; array checks must prove that arrays reach this cell with their metadata intact.

## Risks and decisions

- `cell-kind` currently treats all strings as fallback. Review valid string class hints explicitly, including hints supplied by users, rather than fixing only automatically inferred metadata.
- Generated arguments use the shared helper in another base. Test that integration, not only `tag-meta` directly.
- Exact reference-array types matter for Java method selection. A generic `objects` replacement is not acceptable.
- Existing tests encode the defect's fallback behaviour. Replace those expectations; do not preserve invalid tags to keep the tests green.
- Array transport must preserve identity. Copying or converting elements would change benchmark semantics and cost.
- Blackhole mode is selected at load time. Use separate process configurations for mode coverage.
