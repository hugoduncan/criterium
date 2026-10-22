# Plan — 002 escape opaque cells

## Approach

All codegen changes live in `bases/criterium/src/criterium/measured/impl.clj`.

1. **Escape form per cell.** Extend `opaque-cell` to also return an
   `:escape` entry that is a **`let` binding pair**, not a bare form:
   `[_ (blackhole/consume cell-sym)]` — one `consume` of the cell object
   through the `Object` overload of the existing blackhole, bound to an
   ignored name so it can splice into the measured fn's `let` binding
   vector without making the count odd. No new Java artifact, no new
   namespace (design ownership decision). The cell object
   (`AtomicLong`/`AtomicInteger`/`AtomicReference`) is already an Object,
   so `consume` of it adds no boxing (I-2).
2. **Thread escapes through `arg-bindings`.** `arg-bindings` returns
   `{:init [...] :read [...] :escape [...]}` where `:escape` is a flat
   binding-pair vector (same shape as `:init`/`:read`). Opaque-eligible
   args contribute one `_ (blackhole/consume cell-sym)` pair each;
   `:fallback` args contribute none (no cell exists). Empty-`arg-metas`
   callable path contributes none.
3. **Emit escapes in `measured-expr-fn`.** Splice the `:escape`
   binding pairs into the generated `measured` fn's `let` binding vector,
   after the `init` bindings and before `start#` is taken, outside the
   loop. Loop body and opaque reads are unchanged. This places the escape:
   - after each cell's init store (I-1 preserved),
   - before timing starts, so zero per-iteration cost (I-4),
   - before the one-time warm `val#` evaluation and the loop.
4. **Tests** in `bases/criterium/test/criterium/measured_test.clj`:
   - *Structural (primary signal):* macroexpand the expr form for each
     opaque-eligible kind (`long`/`int`/`double`/`float`/`reference`) and
     assert exactly one `blackhole/consume` of each cell symbol, positioned
     in setup (before `start#`) and never inside the `loop` form. Use
     `cell-kind` as the kind taxonomy source — do not re-enumerate tags.
   - *Behavioural folding oracle:* depth-ratio check only for the kinds
     whose dependency op clears the barrier-noise floor:
     - `double`: `Math/sqrt` nested at depth 1 vs depth 3; assert depth-3
       meaningfully greater (relative ratio, no absolute ns threshold).
     - `reference`: record field read through the reference cell feeding a
       `Math/sqrt` chain at depth 1 vs depth 3; same ratio assertion.
       (A pure linked field-read chain was tried and does not scale: see
       implementation.md.)
     - No depth assertion for `long`/`int`/`float` (sub-cycle ops, below
       the barrier read's ~25-30% run-to-run variance); the structural
       check is their guard.
   - *Both blackhole modes:* blackhole mode is a load-time, process-level
     property — `blackhole/consume` is a macro that bakes the choice in at
     the `measured` fn's expansion from the private `detected-mode` delay
     (keyed on JVM version + the `:blackhole`
     `-XX:CompileCommand=blackhole,...` flag). There is no in-process
     switch, so a single test run can not exercise both modes. Write the
     depth-scaling tests **mode-agnostic** (no mode-specific assertions, no
     `with-redefs` of `detected-mode`); both-mode coverage is delivered by
     running the same suite under two process configs: the default-JVM run
     (runtime mode) plus the `:blackhole`-alias run on JVM 17+ (compiler
     mode, which needs the flag, not just JVM 17+). (D-5 / I-4)
     - CI wiring: `.github/workflows/tests.yml` runs the default suite in
       runtime mode, but the compiler-blackhole step is narrowly
       `--focus criterium.measured-test/opaque-behaviour-test` on JVM 21.
       The new depth-scaling tests get compiler-mode coverage only if they
       are reached by that run. Place them in their own standalone,
       always-running deftest — **not** inside `opaque-behaviour-test`.
       `opaque-behaviour-test` (measured_test.clj ≈line 1184) wraps its
       assertions in `(if (blackhole/compiler-blackhole-enabled?)
       <assertions> <skip>)` and executes only the skip branch under the
       default-JVM runtime run; nesting the mode-agnostic depth oracle there
       would suppress it under runtime mode and silently drop the runtime
       half of the both-mode coverage. Obtain compiler-mode coverage by
       widening the compiler-blackhole step's CI `--focus` to name the new
       standalone deftest (in addition to `opaque-behaviour-test`). No new
       alias is needed (`:blackhole` already exists); the change is
       extending the focus.
   - *Boxing/allocation:* assert the escape and the read path introduce no
     boxing or per-iteration allocation for primitive kinds (I-2).
   - *Teeth proof:* temporarily remove the escape emission and confirm the
     depth-scaling test fails (mementum `jit-behavioural-test-teeth`);
     record the result in implementation.md, then restore.
5. **Docs:** update `measured`/`measured/expr` docstrings (mention the
   one-time setup escape) and README numbers only if observed per-call
   values for cheap subjects change materially (D-4 framing unchanged).

Key decisions:
- Escape via the existing blackhole `consume(Object)` — works under both
  modes: runtime mode is a genuine volatile publication; compiler mode uses
  the HotSpot intrinsic escape marking (dependency documented in design
  Risks).
- Exactly one escape per cell, at setup; escape is a per-object property.
- No public API change, no option/knob (D-1).
- Invariant citations in code/tests: I-3 = read-barrier orthogonality only;
  escape-under-both-modes cites D-5 / I-4 (traceability resolution in
  implementation.md).

## Risks

- Behavioural timing assertions are noisy → depth-ratio oracle with a
  tolerant relative bound; structural test is the primary, deterministic
  signal.
- Over-escaping (per-iteration publication) would add cost → structural
  test asserts exactly one escape, at setup, none in the loop.
- Compiler-blackhole escape depends on the HotSpot intrinsic → test both
  modes; runtime volatile publication is the robust fallback.
- Reference-kind oracle may still fold if the chain is not reached through
  the cell → construct the chain so every read is downstream of the opaque
  read.

## Slice order

1. Codegen: emit one escape point per opaque cell in
   `opaque-cell`/`arg-bindings`/`measured-expr-fn`.
2. Structural escape-point test for all five opaque-eligible kinds.
3. Behavioural depth-scaling oracle (`double`, `reference`) under both
   blackhole modes, plus teeth proof.
4. Boxing/allocation checks and full existing `measured` suite green;
   confirm fallback cases unaffected.
5. Docstring/README updates only if cheap-subject numbers changed
   materially.
