(ns alloc-container-experiment
  "Experiment: measure the garbage produced when accumulating N pre-built
  record maps into a Clojure container, comparing the PersistentVector used
  today by `with-allocation-tracing`'s `records` atom against a PersistentList.

  We isolate the *container* cost: the element maps are pre-allocated, so the
  only allocation measured is the accumulation itself (the new collection nodes
  produced by `conj`, plus the atom/swap! machinery in the atom variants).

  Allocation is measured with ThreadMXBean.getThreadAllocatedBytes, which counts
  bytes allocated by the current thread. This needs no native agent."
  (:import [java.lang.management ManagementFactory]
           [com.sun.management ThreadMXBean]))

(def ^ThreadMXBean tmx
  (ManagementFactory/getThreadMXBean))

(defn thread-alloc-bytes ^long []
  (.getThreadAllocatedBytes tmx (.getId (Thread/currentThread))))

(defn measure-bytes
  "Run thunk once and return bytes allocated on this thread during the call.
  The subtraction of two longs is itself allocation-free (primitive)."
  ^long [thunk]
  (let [before (thread-alloc-bytes)]
    (thunk)
    (let [after (thread-alloc-bytes)]
      (- after before))))

(defn measure-min
  "Best-of-k measurement to remove one-off JIT / safepoint noise.
  Returns the minimum bytes observed across k runs."
  [k thunk]
  ;; warmup
  (dotimes [_ 3] (thunk))
  (reduce min (repeatedly k (fn [] (measure-bytes thunk)))))

;; Pre-build the element maps once, outside measurement, so we only pay for the
;; container nodes during the measured accumulation. These maps mirror the shape
;; built by data-fn in criterium.agent.core (same keys/types is not essential to
;; the container comparison, but keep them representative small maps).
(defn make-elements [n]
  (object-array
    (mapv (fn [i]
            {:object-type "java.lang.Object"
             :object_size (long i)
             :thread 1
             :freed false})
          (range n))))

(defn into-vector ^clojure.lang.PersistentVector [^objects els]
  (let [n (alength els)]
    (loop [i 0 acc []]
      (if (< i n)
        (recur (unchecked-inc i) (conj acc (aget els i)))
        acc))))

(defn into-list [^objects els]
  (let [n (alength els)]
    (loop [i 0 acc '()]
      (if (< i n)
        (recur (unchecked-inc i) (conj acc (aget els i)))
        acc))))

;; Atom variants mirror the real code path: (swap! records conj x) per element.
(defn into-vector-atom [^objects els]
  (let [n (alength els)
        a (atom [])]
    (loop [i 0]
      (if (< i n)
        (do (swap! a conj (aget els i)) (recur (unchecked-inc i)))
        @a))))

(defn into-list-atom [^objects els]
  (let [n (alength els)
        a (atom '())]
    (loop [i 0]
      (if (< i n)
        (do (swap! a conj (aget els i)) (recur (unchecked-inc i)))
        @a))))

(defn into-transient-vector [^objects els]
  (let [n (alength els)]
    (loop [i 0 acc (transient [])]
      (if (< i n)
        (recur (unchecked-inc i) (conj! acc (aget els i)))
        (persistent! acc)))))

(defn run [n k]
  (let [els (make-elements n)
        baseline (measure-min k (fn [] nil))]
    (println (format "n=%d  best-of-%d   (baseline empty-thunk = %d bytes)" n k baseline))
    (doseq [[label f]
            [["loop conj  []     (vector)"      into-vector]
             ["loop conj  ()     (list)"        into-list]
             ["transient  []     (vector!)"     into-transient-vector]
             ["atom swap! conj  [] (vector)"    into-vector-atom]
             ["atom swap! conj  () (list)"      into-list-atom]]]
      (let [bytes (measure-min k (fn [] (f els)))
            per   (double (/ (- bytes baseline) n))]
        (println (format "  %-30s total=%9d bytes   per-element=%7.2f bytes"
                         label bytes per))))))

(defn -main [& _]
  (println "JVM:" (System/getProperty "java.vm.name") (System/getProperty "java.version"))
  (println)
  (doseq [n [1000 10000 100000]]
    (run n 50)
    (println)))
