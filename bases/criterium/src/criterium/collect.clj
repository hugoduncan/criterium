(ns criterium.collect
  "Collect samples using a metrics collector."
  (:require
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.metric :as metric]))

;;; Transform of samples

(defn- sample-arrays->sample-maps
  [pipeline]
  (fn [sample-arrays]
    (mapv (partial collector/transform pipeline) sample-arrays)))

(defn sample-maps->map-of-samples [metrics-configs]
  (fn [samples]
    (reduce
     (fn [res {:keys [path]}]
       (assoc res path (mapv #(get-in % path) samples)))
     {}
     (metric/all-metric-configs metrics-configs))))

(defn transform
  [metrics-configs pipeline sampled]
  (-> sampled
      (update :samples (sample-arrays->sample-maps pipeline))
      (update :samples (sample-maps->map-of-samples metrics-configs))))

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

(def ^:private force-gc-pipeline
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
  (let [args            (measured/args force-gc-measured)
        pipeline        force-gc-pipeline
        metrics-configs (:metrics-configs pipeline)
        samples         (make-array Object num-gcs)
        max-attempts    (unchecked-dec num-gcs)]
    (loop [attempt 0]
      (let [sample (collector/collect-array pipeline force-gc-measured args 1)]
        (aset ^objects samples attempt sample)
        (when (< attempt max-attempts)
          (recur (inc attempt)))))
    ;; this will create garbage
    (transform metrics-configs pipeline {:samples samples})))

;;; Batch Size

(defn batch-size
  "Return batch-size for the given time estimate and batch execution-time."
  ^long [^long t0 ^long batch-time-ns]
  (max 1 (long (/ batch-time-ns t0))))

;;; Timing

(def ^:private throw-away-pipeline
  (collector/collector
   {:stages     []
    :terminator :elapsed-time}))

(defn throw-away-collection
  "The initial measured evaluation is always un-representative.
  This function throws it away, returning nil."
  [measured]
  (collector/collect-array
   throw-away-pipeline
   measured
   (measured/args measured)
   1)
  nil)

(defn collect-arrays
  "Take num-samples samples of measured using batch-size.

  The pipeline is used to collect each sample.

  This is memory allocation garbage free collection.

  Return a data map with the collected metric arrays on the :samples key.
  This will need to be transformed to get the metrics data."
  [pipeline
   measured
   batch-size-obj
   num-samples]
  (let [num-samples (max 2 ^long num-samples)
        samples     (make-array Object num-samples)
        ti          (unchecked-dec ^long (:length pipeline))
        batch-size  (long batch-size-obj)]
    (loop [eval-count   0
           elapsed-time 0
           i            0]
      ;; Try and get the scheduler to take the thread when we are not
      ;; in the middle of a sample
      (Thread/yield)
      (let [args         (measured/args measured)
            sample       (collector/collect-array
                          pipeline measured args batch-size-obj)
            ^long t      (.nth
                          ^clojure.lang.PersistentVector
                          (aget ^objects sample ti)
                          0)
            elapsed-time (unchecked-add elapsed-time t)
            eval-count   (unchecked-add eval-count batch-size)]
        (aset ^objects samples i sample)
        (if (< i (dec num-samples))
          (recur eval-count
                 elapsed-time
                 (unchecked-inc i))
          {:eval-count   eval-count
           :elapsed-time elapsed-time
           :samples      samples
           :num-samples  (count samples)
           :batch-size   batch-size})))))

(def ^:private elapsed-time-pipeline
  (collector/collector
   {:stages     []
    :terminator :elapsed-time}))

(defn elapsed-time-point-estimate
  "Run measured for an initial estimate of the elapsed-time.

  Returns an estimated execution elapsed-time in ns."
  ^long [measured]
  (let [args (measured/args measured)
        s0   (collector/collect elapsed-time-pipeline measured args 1)]
    (metric/elapsed-time s0)))

(defn elapsed-time-min-estimate
  "Return an estimate for the execution elapsed-time of a measured.

  Repeatedly times the invocation of the function and returns the
  minimum invocation time.

  For quick functions limit execution count, while for slower functions
  limit total execution time. Limit evaluations to eval-budget, or
  elapsed time to time-budget-ns."
  [measured num-samples ^long batch-size]
  (let [sampled      (->>
                      (collect-arrays
                       elapsed-time-pipeline
                       measured
                       batch-size
                       num-samples)
                      (transform
                       (:metrics-configs elapsed-time-pipeline)
                       elapsed-time-pipeline))
        elapsed-time ((:samples sampled) [:elapsed-time])
        min-t        (max 1 (long (/ (long (reduce min elapsed-time))
                                     batch-size)))
        sum-t        (long (/ (long (reduce + elapsed-time))
                              batch-size))]
    (assoc sampled
           :t min-t
           :total-time sum-t)))

(defn warmup
  "Run measured for the given number of collections to enable JIT compilation.
  Return a sampled map."
  [pipeline measured ^long num-samples ^long batch-size]
  (loop [i            num-samples
         elapsed-time 0
         samples      []]
    (let [args         (measured/args measured)
          collected    (collector/collect pipeline measured args batch-size)
          t            (metric/elapsed-time collected)
          elapsed-time (unchecked-add elapsed-time t)]
      (if (pos? i)
        (recur
         (unchecked-dec i)
         elapsed-time
         (conj samples collected))
        {:eval-count   (* num-samples batch-size)
         :elapsed-time elapsed-time
         :samples      (conj samples collected)
         :num-samples  (count samples)
         :batch-size   batch-size}))))
