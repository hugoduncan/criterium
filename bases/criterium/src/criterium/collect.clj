(ns criterium.collect
  "Collect samples using a metrics collector."
  (:require
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.metric :as metric]
   [criterium.typed-samples :as typed-samples]))

;;; Transform of samples

(defn- sample-arrays->sample-maps
  [sample-arrays collector]
  (mapv (partial collector/transform collector) sample-arrays))

(defn- extract-double-samples
  "Extract samples at path into a DoubleSamples wrapper."
  [samples path]
  (let [n   (count samples)
        arr (double-array n)]
    (dotimes [i n]
      (aset arr i (double (get-in (samples i) path))))
    (typed-samples/double-samples arr)))

(defn- extract-long-samples
  "Extract samples at path into a LongSamples wrapper."
  [samples path]
  (let [n   (count samples)
        arr (long-array n)]
    (dotimes [i n]
      (aset arr i (long (get-in (samples i) path))))
    (typed-samples/long-samples arr)))

(defn- extract-object-samples
  "Extract samples at path into an ObjectSamples wrapper."
  [samples path]
  (let [n   (count samples)
        arr (object-array n)]
    (dotimes [i n]
      (aset arr i (get-in (samples i) path)))
    (typed-samples/object-samples arr)))

(defn sample-maps->map-of-samples
  "Transform a sequence of sample maps into a map of typed sample arrays.

  Takes a vector of sample maps (each containing metrics at various paths)
  and a metrics-defs configuration. Returns a map from metric path to a
  typed samples wrapper (DoubleSamples, LongSamples, or ObjectSamples)
  based on the metric type."
  [samples metrics-defs]
  (reduce
   (fn [res {:keys [path type]}]
     (assoc res path
            (case type
              :quantitative (extract-double-samples samples path)
              :event        (extract-long-samples samples path)
              :nominal      (extract-object-samples samples path))))
   {}
   (metric/all-metric-configs metrics-defs)))

(defn transform
  [collection-map]
  (let [collector    (:collector collection-map)
        metrics-defs (:metrics-defs collector)]
    (-> (:collections collection-map)
        (sample-arrays->sample-maps collector)
        (sample-maps->map-of-samples metrics-defs))))

;;; Memory management
(def ^:private force-gc-measured
  (measured/expr (jvm/run-finalization-and-force-gc!)))

(defn force-gc-no-capture!
  "Force garbage collection and finalisers so that execution time
  associated with this is not incurred at another time. Up to
  max-attempts are run to clear all pending finalizers and free as much
  memory as possible."
  [^long num-gcs]
  (dotimes [_ num-gcs]
    (jvm/run-finalization-and-force-gc!)))

(def ^:private force-gc-collector
  (collector/collector
   {:stages     (mapv
                 collector/maybe-var-get-stage
                 [:garbage-collector :finalization])
    :terminator (collector/maybe-var-get-stage :elapsed-time)}))

(defn force-gc!
  "Force garbage collection and finalisers so that execution time
  associated with this is not incurred at another time. Up to
  max-attempts are run to clear all pending finalizers and free as much
  memory as possible.

  Returns samples with GC execution time, total changes in memory, and
  in object finalizers pending.

  Must be zero garbage sampling. Execution time is not critical."
  [^long num-gcs]
  (let [args         (measured/args force-gc-measured)
        collector    force-gc-collector
        ti           (unchecked-dec ^long (:length collector))
        collections  (make-array Object num-gcs)
        max-attempts (unchecked-dec num-gcs)
        [num-attempts elapsed-time]
        (loop [attempt      0
               elapsed-time 0]
          (let [sample  (collector/collect-array
                         collector
                         force-gc-measured
                         args
                         1)
                ^long t (.nth
                         ^clojure.lang.PersistentVector
                         (aget ^objects sample ti)
                         0)]
            (aset ^objects collections attempt sample)
            (if (< attempt max-attempts)
              (recur (inc attempt) (unchecked-add elapsed-time t))
              [attempt elapsed-time])))]
    {:eval-count   num-attempts
     :elapsed-time elapsed-time
     :collections  collections
     :num-samples  num-attempts
     :batch-size   1
     :collector    collector}))

;;; Batch Size

(defn batch-size
  "Return batch-size for the given time estimate and batch execution-time."
  ^long [^long t0 ^long batch-time-ns]
  (max 1 (long (/ batch-time-ns t0))))

;;; Timing

(def ^:private throw-away-collector
  (collector/collector
   {:stages     []
    :terminator :elapsed-time}))

(defn throw-away-collection
  "The initial measured evaluation is always un-representative.
  This function throws it away, returning nil."
  [measured]
  (collector/collect-array
   throw-away-collector
   measured
   (measured/args measured)
   1)
  nil)

(defn collect-arrays
  "Take num-samples samples of measured using batch-size.

  The collector is used to collect each sample.

  This is memory allocation garbage free collection.

  Return a data map with the collected metric arrays on the :samples key.
  This will need to be transformed to get the metrics data."
  [collector
   measured
   batch-size-obj
   num-samples]
  (let [num-samples     (max 2 ^long num-samples)
        num-samples-m-1 (unchecked-dec num-samples)
        collections     (make-array Object num-samples)
        ti              (unchecked-dec ^long (:length collector))
        batch-size      (long batch-size-obj)]
    (loop [eval-count   0
           elapsed-time 0
           i            0]
      ;; Try and get the scheduler to take the thread when we are not
      ;; in the middle of a sample
      (Thread/yield)
      (let [args         (measured/args measured)
            sample       (collector/collect-array
                          collector measured args batch-size-obj)
            t            (long
                          (.nth
                           ^clojure.lang.PersistentVector
                           (aget ^objects sample ti)
                           0))
            elapsed-time (unchecked-add elapsed-time t)
            eval-count   (unchecked-add eval-count batch-size)]
        (aset ^objects collections i sample)
        (if (< i num-samples-m-1)
          (recur eval-count
                 elapsed-time
                 (unchecked-inc i))
          {:eval-count   eval-count
           :elapsed-time elapsed-time
           :collections  collections
           :num-samples  (count collections)
           :batch-size   batch-size
           :collector    collector})))))

(def ^:private elapsed-time-collector
  (collector/collector
   {:stages     []
    :terminator :elapsed-time}))

(defn elapsed-time-point-estimate
  "Run measured for an initial estimate of the elapsed-time.

  Returns an estimated execution elapsed-time in ns."
  ^long [measured]
  (let [args (measured/args measured)
        s0   (collector/collect elapsed-time-collector measured args 1)]
    (metric/elapsed-time s0)))

(defn elapsed-time-min-estimate
  "Return an estimate for the execution elapsed-time of a measured.

  Repeatedly times the invocation of the function and returns the
  minimum invocation time.

  For quick functions limit execution count, while for slower functions
  limit total execution time. Limit evaluations to eval-budget, or
  elapsed time to time-budget-ns."
  [measured num-samples ^long batch-size]
  (let [collected   (collect-arrays
                     elapsed-time-collector
                     measured
                     batch-size
                     num-samples)
        collections (:collections collected)
        num-samples (long num-samples)
        min-t       (loop [i 0 min-t Long/MAX_VALUE]
                      (if (>= i num-samples)
                        min-t
                        (let [sample (aget ^objects collections i)
                              vs     (aget ^objects sample 0)
                              t      (long  (.nth
                                             ^clojure.lang.PersistentVector vs
                                             0))]
                          (recur
                           (unchecked-inc i)
                           (if (< min-t t) min-t t)))))
        min-t       (max 1 (long (/ (long min-t) batch-size)))
        sum-t       (:elapsed-time collected)]
    (assoc collected
           :t min-t
           :total-time sum-t)))

(defn warmup
  "Run measured for the given number of collections to enable JIT compilation.
  Return a sampled map."
  [collector measured ^long num-samples ^long batch-size]
  (loop [i            num-samples
         elapsed-time 0
         min-time     Long/MAX_VALUE
         collections  []]
    (let [args         (measured/args measured)
          collected    (collector/collect collector measured args batch-size)
          t            (metric/elapsed-time collected)
          elapsed-time (unchecked-add elapsed-time t)]
      (if (pos? i)
        (recur
         (unchecked-dec i)
         elapsed-time
         (min min-time t)
         (conj collections collected))
        {:eval-count   (* num-samples batch-size)
         :elapsed-time elapsed-time
         :min-time     min-time
         :collections  (conj collections collected)
         :num-samples  (count collections)
         :batch-size   batch-size
         :collector    collector}))))
