❌ `capture-arg-types` only widens `Long`/`Double` locals — a small-primitive
hint must sit on the bare op argument, or the arg mis-routes to `:reference`.

Testing the task-003 opaque barrier, subjects like
`(measured/expr (unchecked-inc-int (int x)))` factor the `(int x)` subform into
a single captured `int`-valued argument. `capture-arg-types` only widens
`Long`/`Double` locals, so that arg defaults to tag `:reference` and routes
through an `AtomicReference`, NOT the `AtomicInteger` small-primitive cell the
test names. This bit the task TWICE (behaviour + allocation subjects), silently
passing while probing the wrong cell.

🔁 Fix: put the bare hinted symbol directly as the op argument so the user hint
is preserved — `(unchecked-inc-int ^byte x)`, `(int ^char x)`,
`(if ^boolean b 1 2)`. Hint controls too, for cell symmetry.

🎯 Routing is a load-bearing contract now (each small-primitive tag → its own
cell). Guard it deterministically at `capture-arg-types`/structural
(macroexpand) level — NOT only via timing-/agent-gated behaviour+allocation
tests, which can pass while routing through the wrong cell.
