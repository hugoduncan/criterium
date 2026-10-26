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
