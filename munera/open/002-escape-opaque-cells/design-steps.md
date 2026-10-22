# Design follow-up steps

## Ambiguity

- [x] Clarify the per-kind depth-scaling folding oracle (acceptance 2 +
      Verification expectations). A concrete dependency-chain subject is given
      only for the `double` kind (nested `Math/sqrt`), yet the guard must hold
      "per cell kind" for `long`/`int`/`float`/`reference`. Specify, per kind,
      what depth-varying chain to measure — the `reference` kind needs a linked
      object graph of chained field reads, not the single field read mentioned
      elsewhere — and resolve whether an op cheap enough to fall below the
      barrier-noise floor (e.g. integer add/multiply) can register the
      "meaningfully greater" ratio, or else restrict the depth-scaling oracle
      to kinds with a costly-enough op and cover the rest by the structural
      escape-point check. Goal: make acceptance 2 verifiable per kind.
      (In-scope clarification; does not change the scope boundary.)

## Inconsistency

- [x] Reconcile the I-3 restatement with the design authority. design.md marks
      I-3 "(unchanged)" yet appends "escape must work under both the compiler
      and the runtime blackhole" — which is a change, and diverges from
      `design/opaque-input-barrier.md`, where I-3 covers only the opaque
      read-barrier's mode-orthogonality and escape-mode-independence is a
      D-5/I-4 property. Acceptance criterion 3 then cites "(I-3)" for the
      escape. Keep I-3 = read-barrier orthogonality (per authority) and
      attribute the escape-in-both-modes requirement + acceptance 3 to
      I-4/D-5, or state explicitly that design.md deliberately widens I-3 and
      retag accordingly. (Traceability only; no scope change.)

## Plan ambiguity

- [x] Specify the mechanism for "run the depth-scaling oracle under both the
      compiler blackhole and the runtime blackhole" (plan Slice 3, steps Slice
      3, acceptance 3 / D-5 / I-4). `criterium.blackhole/consume` is a macro
      that expands to the compiler-blackhole static call or the runtime-instance
      call based on `@detected-mode`, a private load-time `delay` keyed on the
      JVM version and the `-XX:CompileCommand=blackhole,...` flag. There is no
      in-process mode switch (no dynamic var, no supported redef), so a single
      test run can not exercise both modes, and a naive `with-redefs
      detected-mode` will not re-expand the already-compiled `measured` fn.
      Mode is a process-level property: compiler mode requires JVM 17+ *and*
      the `:blackhole` alias flag (plain JVM 17+ without the flag falls back to
      runtime). State that the depth-scaling tests must be written mode-agnostic
      (no mode-specific assertions) and that both-mode coverage is delivered by
      running the suite under both process configs — the existing default-JVM
      (runtime) run plus the `:blackhole`-alias run on JVM 17+ — and confirm
      whether the new tests are picked up by both existing CI configs or need a
      new alias/CI entry. (In-scope clarification; no scope change.)

- [x] Specify the representation and splice shape of the per-cell escape
      (plan step "Escape form per cell" / step 3; steps Slice 1). `arg-bindings`
      returns `:init`/`:read` as flat binding-pair vectors spliced into the
      measured fn's `let` binding vector, but the plan describes `:escape` as a
      vector of side-effecting *forms* (`(blackhole/consume cell-sym)`). A bare
      form can not be spliced into a `let` binding-vector position (odd count /
      malformed). Decide and state how each escape is emitted so it lands after
      the cell's `:init` and before `start#`: e.g. as a `_ (blackhole/consume
      cell-sym)` binding pair in the same `let`, or in a `do`/separate binding
      value. This shape determines both the generated form and what the Slice 2
      structural test must match as "exactly one `consume` of each cell symbol
      in the setup section (before `start#`)". (In-scope clarification; no scope
      change.)

- [x] Resolve the non-equivalent CI-placement options for the mode-agnostic
      depth-scaling tests (plan Slice 3 "CI wiring"; steps Slice 3). The plan
      offers "either add them under the `opaque-behaviour-test` deftest or widen
      the CI `--focus` to include the new deftest(s)" as equivalent, but they
      are not. The existing `measured_test.clj` `opaque-behaviour-test`
      (≈line 1184) wraps its assertions in
      `(if (blackhole/compiler-blackhole-enabled?) <assertions> <skip>)` and,
      under the default-JVM runtime run, executes only the skip branch. Nesting
      the mode-agnostic depth oracle inside that deftest would suppress it under
      runtime mode and silently remove the runtime half of the both-mode
      coverage the plan requires (default-JVM runtime run + `:blackhole`
      compiler run). State that the depth-scaling tests must live in their own
      always-running deftest (not inside the compiler-guarded
      `opaque-behaviour-test`), and that compiler-mode coverage is obtained by
      widening the CI `--focus` to name that deftest. (In-scope clarification;
      no scope change.)
