❌ Escape analysis defeats the task-001 opaque input barrier on the default JVM.

The opaque `AtomicLong`/`AtomicReference` cell is thread-local, never escapes,
and is never written inside the loop. C2 escape analysis proves no concurrent
writer can exist, so it treats the `getOpaque` load's value as the
loop-invariant constant it provably is and FOLDS downstream pure computation
out of the loop. Opaque only means "assume memory may change"; EA proves it
cannot.

✅ Proof (JDK 25 arm64, compiler blackhole):
  EA on:  (Math/sqrt x) 0.054 ns; 3-nested-sqrt 0.054 ns  (depth-independent → folded)
  EA off: (Math/sqrt x) 0.442 ns; 3-nested-sqrt 1.33 ns   (≈3× → sqrts execute, throughput-bound)
Toggle: -XX:-DoEscapeAnalysis.

🎯 Consequence: the barrier keeps the READ in the loop but NOT pure computation
downstream of it. It only reliably measures subjects where the read IS the work
(record field read). Arithmetic/sqrt/div subjects fold to a sub-cycle floor.

🔁 Honest sub-ns numbers need: EA off, OR a cell that escapes, OR a loop-carried
dependency (none provided by the current harness). Candidate issue vs task-001.
