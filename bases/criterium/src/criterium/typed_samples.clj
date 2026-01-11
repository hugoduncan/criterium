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
    ITypedSamples IFold IDoubleFold ILongFold IDoubleObjectFold ILongObjectFold
    ISampleOps]))

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

  IDoubleObjectFold
  (foldObject [_ ^clojure.lang.IFn$ODO f init]
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

  IDoubleFold
  (^double fold [_ ^clojure.lang.IFn$DDD f ^double init]
    (let [len (alength array)]
      (loop [i   0
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (double (aget array i))))
          acc))))

  ILongObjectFold
  (foldObject [_ ^clojure.lang.IFn$OLO f init]
    (let [len (alength array)]
      (loop [i   0
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget array i)))
          acc))))

  IDoubleObjectFold
  (foldObject [_ ^clojure.lang.IFn$ODO f init]
    (let [len (alength array)]
      (loop [i   0
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (double (aget array i))))
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

(deftype SampleOps []
  ISampleOps
  (^double sum [_ ^DoubleSamples samples]
    (let [arr (.array samples)
          len (alength arr)]
      (loop [i   0
             acc 0.0]
        (if (< i len)
          (recur (unchecked-inc i)
                 (+ acc (aget arr i)))
          acc))))
  (^long sum [_ ^LongSamples samples]
    (let [arr (.array samples)
          len (alength arr)]
      (loop [i   0
             acc (long 0)]
        (if (< i len)
          (recur (unchecked-inc i)
                 (+ acc (aget arr i)))
          acc))))
  (^long getAt [_ ^LongSamples samples ^long index]
    (aget ^longs (.array samples) index))
  (^double getAt [_ ^DoubleSamples samples ^long index]
    (aget ^doubles (.array samples) index)))

(def ^:private ^ISampleOps sample-ops (SampleOps.))

(defn sum-double
  "Returns the sum of samples as a primitive double.
  Works with DoubleSamples."
  ^double [^DoubleSamples samples]
  (.sum sample-ops samples))

(defn sum-long
  "Returns the sum of samples as a primitive long.
  Only works with LongSamples."
  ^long [^LongSamples samples]
  (.sum sample-ops samples))

(defn get-long
  "Returns the value at index as a primitive long."
  ^long [^LongSamples samples ^long index]
  (.getAt sample-ops samples index))

(defn get-double
  "Returns the value at index as a primitive double."
  ^double [^DoubleSamples samples ^long index]
  (.getAt sample-ops samples index))

(defn lpos?
  "Primitive long positive check."
  [^long v]
  (pos? v))

(defn dpos?
  "Primitive double positive check."
  [^double v]
  (pos? v))

(defn fold
  "Reduces over the samples with function f and initial value init.
  f is called as (f acc sample) for each sample.
  This is the generic version that boxes primitive values."
  [^IFold samples f init]
  (.fold samples f init))

(defn fold-double
  "Reduces over samples with a primitive double function.
  Works with both DoubleSamples and LongSamples (longs are converted to doubles).
  f must be (fn ^double [^double acc ^double val] ...).
  Returns a primitive double."
  ^double [^IDoubleFold samples f ^double init]
  (.fold samples f init))

(defn dplus
  "Primitive double addition for use with fold-double."
  ^double [^double a ^double b]
  (+ a b))

(defn lplus
  "Primitive long addition for use with fold-long."
  ^long [^long a ^long b]
  (+ a b))

(defn fold-long
  "Reduces over long samples with a primitive long function.
  f must be (fn ^long [^long acc ^long val] ...).
  Returns a primitive long."
  ^long [^ILongFold samples f ^long init]
  (.fold samples f init))

(defn dfold
  "Reduces over samples with an object-returning function, receiving primitive doubles.
  Works with both DoubleSamples and LongSamples (longs are converted to doubles).
  f must be (fn [acc ^double val] ...) - receives primitive double, returns Object.
  Use this when accumulating into a collection."
  [^IDoubleObjectFold samples f init]
  (.foldObject samples f init))

(defn lfold
  "Reduces over long samples with an object-returning function.
  f must be (fn [acc ^long val] ...) - receives primitive long, returns Object.
  Use this when accumulating into a collection from LongSamples."
  [^ILongObjectFold samples f init]
  (.foldObject samples f init))

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
