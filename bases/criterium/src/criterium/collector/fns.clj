(ns criterium.collector.fns
  "A pipeline function takes a sample, a measured state, and a measured,
  calls the next pipeline function and returns an updated sample state.
  It is usually called via the execute function.

  A pipeline function can be composed with other pipeline functions and
  a pipeline terminal function, which is responsible for actually
  calling the measured.

  Each pipeline function collects one or metrics around the measured's
  invocation."
  (:require
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

;;; Helpers

(defn sample-gensym []
  (with-meta (gensym "sample") {:tag 'objects}))

(defn- capture-syms [sample measured state eval-count result-index]
  (let [sample-sym       (sample-gensym)
        measured-sym     (gensym "measured")
        state-sym        (gensym "state")
        eval-count-sym   (gensym "eval-count")
        result-index-sym (gensym "result-index")]
    {:let-block        (mapcat
                        #(vector %1 %2)
                        [sample-sym measured-sym state-sym
                         eval-count-sym result-index-sym]
                        [sample measured state eval-count result-index])
     :invoke-next      (fn [next-fns]
                         ((first next-fns)
                          (rest next-fns)
                          sample-sym
                          measured-sym
                          state-sym
                          eval-count-sym
                          `(unchecked-inc ~result-index-sym)))
     :sample-sym       sample-sym
     :measured-sym     measured-sym
     :state-sym        state-sym
     :eval-count-sym   eval-count-sym
     :result-index-sym result-index-sym}))

(defrecord ^:private SampleStage
  [m x id])

;;; Terminal function

(defn- elapsed-time-sample-m
  "A terminal function to execute measured, adding results to the sample.

  Puts:
    - elapsed time in nanoseconds onto the :elapsed-time key in data.
    - (an example of) the expression value on the :expr-value key.
    - the number of evals on the :eval-count key."
  [next-fns sample measured state eval-count result-index]
  {:pre [(empty? next-fns)]}
  (let [sample-sym (sample-gensym)]
    `(let [~sample-sym   ~sample
           measured#     ~measured
           state#        ~state
           eval-count#   ~eval-count
           result-index# ~result-index]
       (aset ~sample-sym result-index#
             (measured/invoke measured# state# eval-count#)))))

(defn- elapsed-time-xform
  [sample ^long result-index]
  (let [v (aget ^objects sample result-index)]
    (aset ^objects sample result-index
          {:elapsed-time (v 0)
           :expr-value   (v 1)})
    nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def elapsed-time
  (with-meta
    (->SampleStage elapsed-time-sample-m elapsed-time-xform :elapsed-time)
    {:criterium.collector/stage-type :terminal}))

;;; Sample Pipeline Stages

;; Stages can be composed.

;;;; Measured state

(defn- measured-args-sample-m
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym state-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym ~state-sym)
       nil)))

(defn- measured-args-xform
  [next-fn]
  (fn [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:args (aget ^objects sample result-index)})
    nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def measured-args
  (->SampleStage measured-args-sample-m measured-args-xform :measured-args))

;;;; Class Loader

(defn- class-loader-sample-m
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block
           start# (jvm/class-loader-counts)]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym [start# (jvm/class-loader-counts)])
       nil)))

(defn- class-loader-xform
  [next-fn]
  (fn [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:class-loader
           (apply jvm/class-loader-counts-change
                  (aget ^objects sample result-index))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def class-loader
  (->SampleStage class-loader-sample-m class-loader-xform :class-loader))

;;;; Compilation

(defn- compilation-sample-m
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block
           start# (jvm/compilation-sample)]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym [start# (jvm/compilation-sample)])
       nil)))

(defn- compilation-xform
  [next-fn]
  (fn [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:compilation
           (apply jvm/compilation-change
                  (aget ^objects sample result-index))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def compilation
  (->SampleStage compilation-sample-m compilation-xform :compilation))

;;;; Memory

(defn- memory-sample-m
  "Execute measured, add compilation time to the data map.

  Adds a map to the :memory key in data.  The map contains sub-maps for
  each type of memory, and the total memory (on the :total key).  Each
  sub-map contains the :init, :committed, :max and :used keys.

  Uses the MemoryMXBean."
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block
           start# (jvm/memory-sample)]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym [start# (jvm/memory-sample)])
       nil)))

(defn- memory-xform
  [next-fn]
  (fn [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:memory (apply jvm/memory-change
                          (aget ^objects sample result-index))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def memory
  (->SampleStage memory-sample-m memory-xform :memory))

;;;; Finalization

(defn- finalization-sample-m
  "Execute measured, add pending finalization count to the data map.

  Adds maps to the :finalization key in data, with the :start, :finish,
  and :delta sub-keys.

  Uses the MemoryMXBean."
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block
           start# (jvm/finalization-sample)]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym
             [start# (jvm/finalization-sample)])
       nil)))

(defn- finalization-xform
  [next-fn]
  (fn finalization-xform [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:finalization
           (apply jvm/finalization-change
                  (aget ^objects sample result-index))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def finalization
  (->SampleStage finalization-sample-m finalization-xform :finalization))

;;;; Garbage-collector

(defn- garbage-collector-sample-m
  "Execute measured, add garbage collection counts and times to the data map.

  Uses the GarbageCollectorMXBean beans."
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block
           start# (jvm/garbage-collector-sample)]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym
             [start# (jvm/garbage-collector-sample)])
       nil)))

(defn- garbage-collector-xform
  [next-fn]
  (fn garbage-collector-xform
    [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:garbage-collector
           (apply jvm/garbage-collector-change
                  (aget ^objects sample result-index))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def garbage-collector
  (->SampleStage
   garbage-collector-sample-m garbage-collector-xform :garbage-collector))

;;;; Thread Memory Allocation

(defn- thread-allocation-sample-m
  "Collect sample with the thread memory allocation."
  [next-fns sample measured state eval-count result-index]
  (let [{:keys [let-block invoke-next sample-sym result-index-sym]}
        (capture-syms sample measured state eval-count result-index)]
    `(let [~@let-block
           start# (jvm/thread-allocated-bytes)]
       ~(invoke-next next-fns)
       (aset ~sample-sym ~result-index-sym
             [start# (jvm/thread-allocated-bytes)])
       nil)))

(defn- thread-allocation-xform
  [next-fn]
  (fn thread-allocation-xform
    [sample ^long result-index]
    (next-fn sample (unchecked-inc result-index))
    (aset ^objects sample result-index
          {:thread-allocation
           (apply jvm/thread-allocated-change
                  (aget ^objects sample result-index))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def thread-allocation
  (->SampleStage
   thread-allocation-sample-m thread-allocation-xform :thread-allocation))
