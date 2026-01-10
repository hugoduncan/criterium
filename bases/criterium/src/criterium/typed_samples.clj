(ns criterium.typed-samples
  "Typed sample collections using primitive arrays to avoid boxing.

  Provides wrappers around primitive arrays (double-array, long-array)
  that can be used for efficient sample storage during benchmarking.
  Three record types are provided for the three metric types:
    - DoubleSamples for :quantitative metrics
    - LongSamples for :event metrics
    - ObjectSamples for :nominal metrics"
  (:require
   [criterium.typed-samples.protocol :as p]))

(defrecord DoubleSamples [^doubles array]
  p/TypedSamples
  (elem-type [_] :double)
  (sample-count [_] (alength array))
  (fold [_ f init]
        (areduce array i acc init
                 (f acc (aget array i)))))

(defrecord LongSamples [^longs array]
  p/TypedSamples
  (elem-type [_] :long)
  (sample-count [_] (alength array))
  (fold [_ f init]
        (areduce array i acc init
                 (f acc (aget array i)))))

(defrecord ObjectSamples [^objects array]
  p/TypedSamples
  (elem-type [_] :object)
  (sample-count [_] (alength array))
  (fold [_ f init]
        (areduce array i acc init
                 (f acc (aget array i)))))

(defn elem-type
  "Returns the element type keyword: :double, :long, or :object."
  [samples]
  (p/elem-type samples))

(defn sample-count
  "Returns the number of samples in the collection."
  [samples]
  (p/sample-count samples))

(defn fold
  "Reduces over the samples with function f and initial value init.
  f is called as (f acc sample) for each sample."
  [samples f init]
  (p/fold samples f init))

(defn double-samples
  "Creates a DoubleSamples from a double-array."
  [^doubles arr]
  (->DoubleSamples arr))

(defn long-samples
  "Creates a LongSamples from a long-array."
  [^longs arr]
  (->LongSamples arr))

(defn object-samples
  "Creates an ObjectSamples from an object-array."
  [^objects arr]
  (->ObjectSamples arr))

(defn metric-type->elem-type
  "Maps metric type keywords to element type keywords.
  :quantitative -> :double
  :event -> :long
  :nominal -> :object"
  [metric-type]
  (case metric-type
    :quantitative :double
    :event        :long
    :nominal      :object))

(defn samples-for-metric-type
  "Creates an appropriately typed samples wrapper from a collection of values.

  metric-type is :quantitative, :event, or :nominal.
  values is a sequence of values to convert to a typed array.

  Returns a DoubleSamples, LongSamples, or ObjectSamples."
  [metric-type values]
  (case metric-type
    :quantitative (->DoubleSamples (double-array values))
    :event        (->LongSamples (long-array values))
    :nominal      (->ObjectSamples (object-array values))))
