(ns criterium.typed-samples
  "Typed sample collections using primitive arrays to avoid boxing.

  Provides wrappers around primitive arrays (double-array, long-array)
  that can be used for efficient sample storage during benchmarking.
  Three types are provided for the three metric types:
    - DoubleSamples for :quantitative metrics
    - LongSamples for :event metrics
    - ObjectSamples for :nominal metrics"
  (:require
   [criterium.typed-samples.interface])
  (:import
   [criterium.typed_samples.interface
    ITypedSamples IFold IDoubleFold ILongFold]))

(deftype DoubleSamples [^doubles array]
  ITypedSamples
  (elemType [_] :double)
  (sampleCount [_] (alength array))

  IDoubleFold
  (^double fold [_ ^clojure.lang.IFn$DDD f ^double init]
    (let [len (alength array)]
      (loop [i   0
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget array i)))
          acc))))

  IFold
  (^Object fold [_ f init]
    (areduce array i acc init
             (f acc (aget array i)))))

(deftype LongSamples [^longs array]
  ITypedSamples
  (elemType [_] :long)
  (sampleCount [_] (alength array))

  ILongFold
  (^long fold [_ ^clojure.lang.IFn$LLL f ^long init]
    (let [len (alength array)]
      (loop [i   0
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget array i)))
          acc))))

  IFold
  (^Object fold [_ f init]
    (areduce array i acc init
             (f acc (aget array i)))))

(deftype ObjectSamples [^objects array]
  ITypedSamples
  (elemType [_] :object)
  (sampleCount [_] (alength array))

  IFold
  (fold [_ f init]
    (areduce array i acc init
             (f acc (aget array i)))))

(defn elem-type
  "Returns the element type keyword: :double, :long, or :object."
  [^ITypedSamples samples]
  (.elemType samples))

(defn sample-count
  "Returns the number of samples in the collection."
  ^long [^ITypedSamples samples]
  (.sampleCount samples))

(defn fold
  "Reduces over the samples with function f and initial value init.
  f is called as (f acc sample) for each sample."
  [^IFold samples f init]
  (.fold samples f init))

(defn fold-double
  "Reduces over double samples with a primitive double function.
  f must be a function of (^double [^double acc ^double val]).
  Returns a primitive double."
  ^double [^IDoubleFold samples f ^double init]
  (.fold samples f init))

(defn fold-long
  "Reduces over long samples with a primitive long function.
  f must be a function of (^long [^long acc ^long val]).
  Returns a primitive long."
  ^long [^ILongFold samples f ^long init]
  (.fold samples f init))

(defn double-samples
  "Creates a DoubleSamples from a double-array."
  ^DoubleSamples [^doubles arr]
  (DoubleSamples. arr))

(defn long-samples
  "Creates a LongSamples from a long-array."
  ^LongSamples [^longs arr]
  (LongSamples. arr))

(defn object-samples
  "Creates an ObjectSamples from an object-array."
  ^ObjectSamples [^objects arr]
  (ObjectSamples. arr))

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
    :quantitative (DoubleSamples. (double-array values))
    :event        (LongSamples. (long-array values))
    :nominal      (ObjectSamples. (object-array values))))
