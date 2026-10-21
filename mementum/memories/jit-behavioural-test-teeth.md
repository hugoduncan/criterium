🔁 JIT-dependent behavioural tests must be proven to FAIL when the mechanism
is broken, or they are vacuous.

Two recurring traps found while testing the opaque input barrier (#155):

1. Loose timing floor has no teeth. A floor of `(< (* 0.1 c) s)` (subject vs
   control) passed even with the barrier broken: a hoisted subject (~0.4×
   control) cleared `0.1×c` just as a working barrier (~1.0×) did. Tightened to
   `0.5×c` and verified empirically that a faithfully-hoisted subject now
   fails. Always demonstrate the assertion fails on the broken form.

2. Vacuous pass under the wrong blackhole. Anti-hoisting tests only
   discriminate under the compiler blackhole; under the default runtime
   blackhole both subject and control are ≥1 ns, so the test passes regardless.
   Guard on `criterium.blackhole/compiler-blackhole-enabled?`: run the
   discriminating assertion under `:compiler`, skip explicitly (assert
   `mode` = `:runtime`) otherwise, and run the focused test under the
   `:blackhole` alias in CI so a hoisting regression fails there.

General lesson: for JIT/timing behaviour, the structural (macroexpand) test is
the deterministic primary signal; behavioural checks are secondary and must
have verified teeth and a regime guard.
