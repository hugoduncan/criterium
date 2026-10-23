(ns alloc-inwindow-experiment
  "Experiment: how much does the measurement harness allocate *inside* the
  `with-allocation-tracing` window?

  This is the allocation that the native agent attributes to the wrapped code,
  i.e. measurement-specific memory pressure that contaminates a precise
  garbage-generation measurement. We trace a series of bodies and count the
  records/bytes the agent reports for the calling thread.

  REQUIRES the native agent attached at JVM startup, e.g. start the REPL with:
    clojure -J-agentpath:<repo>/agent-cpp/build/libcriterium.dylib -M:dev

  Results (OpenJDK 25), bytes are :object_size totals on the calling thread:

    nil                                  -> 0 records,   0 bytes   (harness floor)
    (jvm/timestamp) -> long              -> 0 records,   0 bytes
    (do ts ts nil)                       -> 0 records,   0 bytes
    (f 3 4) direct call                  -> 0 records,   0 bytes
    (apply f args)                       -> 3 records, 120 bytes   (PersistentVector$ChunkedSeq x3)
    (invoke-unrolled f args)             -> 0 records,   0 bytes   (apply unrolled -> direct .invoke)
    [elapsed result] tuple               -> 2 records,  64 bytes   (Object[] + PersistentVector)
    measured-f shape (timed apply+tuple) -> 6 records, 208 bytes   (apply 120 + Long + tuple 64)

  Conclusions:
   - with-allocation-tracing's harness floor is zero; primitive jvm/timestamp
     reads are free.
   - The `records` container is filled AFTER stop! (report-allocation-tracing),
     so vector-vs-list there does not affect measurement precision.
   - The real contamination is the measured-f path: `apply` + Long/tuple boxing,
     ~208 bytes/invocation. A precise path must use direct-arity invocation and
     build the tuple/summary outside the trace window."
  (:require
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]))

(defn invoke-unrolled
  "Unrolled replacement for (apply f args): dispatch on arg count to an
  arity-explicit .invoke. Allocation-free (no ChunkedSeq), unlike apply."
  [^clojure.lang.IFn f ^clojure.lang.IPersistentVector args]
  (case (.count args)
    0 (.invoke f)
    1 (.invoke f (.nth args 0))
    2 (.invoke f (.nth args 0) (.nth args 1))
    3 (.invoke f (.nth args 0) (.nth args 1) (.nth args 2))
    ;; ... extend through arity 20 in real code, mirroring the deftype ...
    (.applyTo f (seq args))))

(defn summarize [[records _]]
  (when records
    (let [mine (filter (agent/allocation-on-thread? (jvm/current-thread-id))
                       records)]
      {:total-records (count records)
       :my-records    (count mine)
       :my-bytes      (reduce + (keep :object_size mine))
       :my-types      (frequencies (map :object-type mine))})))

(defn run []
  (assert (agent/attached?) "native agent must be attached (-agentpath)")
  (let [f    (fn [a b] (unchecked-add (long a) (long b)))
        args [3 4]]
    ;; warmup
    (dotimes [_ 10]
      (agent/with-allocation-tracing nil)
      (agent/with-allocation-tracing (apply f args))
      (agent/with-allocation-tracing (invoke-unrolled f args))
      (agent/with-allocation-tracing
        (let [s (jvm/timestamp) r (apply f args) e (jvm/timestamp)]
          [(unchecked-subtract e s) r])))
    {:nil-body          (summarize (agent/with-allocation-tracing nil))
     :return-long       (summarize (agent/with-allocation-tracing (jvm/timestamp)))
     :two-ts-nil        (summarize (agent/with-allocation-tracing
                                     (do (jvm/timestamp) (jvm/timestamp) nil)))
     :direct-call       (summarize (agent/with-allocation-tracing (f 3 4)))
     :apply-only        (summarize (agent/with-allocation-tracing (apply f args)))
     :unrolled-only     (summarize (agent/with-allocation-tracing (invoke-unrolled f args)))
     :tuple-only        (summarize (agent/with-allocation-tracing [(long 7) :v]))
     :timed-apply-nil   (summarize (agent/with-allocation-tracing
                                     (let [s (jvm/timestamp) r (apply f args) e (jvm/timestamp)]
                                       nil)))
     :timed-apply-tuple (summarize (agent/with-allocation-tracing
                                     (let [s (jvm/timestamp) r (apply f args) e (jvm/timestamp)]
                                       [(unchecked-subtract e s) r])))}))

(defn -main [& _]
  (run!
   (fn [[k v]] (println (format "%-20s %s" (name k) (pr-str v))))
   (run)))
