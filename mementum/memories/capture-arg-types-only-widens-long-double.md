❌ Expression factoring can make a barrier test exercise the wrong cell.

`capture-arg-types` only widens `Long`/`Double` locals. In
`(measured/expr (unchecked-inc-int (int x)))`, factoring captures `(int x)`
as one argument. It routes through `AtomicReference`, not the small-primitive
`AtomicInteger` cell. This passed both behavioural and allocation tests while
exercising the wrong route.

Put the hint on the bare operation argument: `(unchecked-inc-int ^byte x)`,
`(int ^char x)`, or `(if ^boolean b 1 2)`. Hint controls too, for cell symmetry.

Array/scalar tests have the same trap: `(do (aget xs index) xs)` and nested
`aget` subjects can capture a subexpression rather than separate inputs. Use a
call with bare array and scalar arguments (task 004, `e413453`).

Assert captured value-to-tag and value-to-cell-kind mappings, then check setup
stores, escapes, and reads per cell. Aggregate cell counts, correct results,
and timing or allocation checks alone do not prove which inputs crossed the
barrier.
