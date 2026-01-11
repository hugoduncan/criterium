(ns criterium.array
  "Typed array collections using primitive arrays to avoid boxing.

  Provides wrappers around primitive arrays (double-array, long-array)
  that can be used for efficient sample storage during benchmarking.
  Three types are provided for the three metric types:
    - DoubleArray for :quantitative metrics
    - LongArray for :event metrics
    - ObjectArray for :nominal metrics"
  (:require
   [criterium.array.interface])
  (:import
   [criterium.array.interface
    ITypedArray IFold IDoubleFold ILongFold IDoubleObjectFold ILongObjectFold
    IIndexed IArrayOps]
   [java.util Arrays]))

(deftype DoubleArray [^doubles array]
  ITypedArray
  (elemType [_] :double)
  (length [_] (alength array))

  IIndexed
  (^double getDouble [_ ^long index] (aget array index))
  (^long getLong [_ ^long index] (long (aget array index)))
  (getObject [_ ^long index] (aget array index))

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

(deftype LongArray [^longs array]
  ITypedArray
  (elemType [_] :long)
  (length [_] (alength array))

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

(deftype ObjectArray [^objects array]
  ITypedArray
  (elemType [_] :object)
  (length [_] (alength array))

  IFold
  (fold [_ f init]
    (areduce array i acc init
             (f acc (aget array i)))))

(defn elem-type
  "Returns the element type keyword: :double, :long, or :object."
  [^ITypedArray arr]
  (.elemType arr))

(defn length
  "Returns the number of elements in the array."
  ^long [^ITypedArray arr]
  (.length arr))

(deftype ArrayOps []
  IArrayOps
  (^double sum [_ ^DoubleArray arr]
    (let [^doubles a (.array arr)
          len        (alength a)]
      (loop [i   0
             acc 0.0]
        (if (< i len)
          (recur (unchecked-inc i)
                 (+ acc (aget a i)))
          acc))))
  (^long sum [_ ^LongArray arr]
    (let [^longs a (.array arr)
          len      (alength a)]
      (loop [i   0
             acc (long 0)]
        (if (< i len)
          (recur (unchecked-inc i)
                 (+ acc (aget a i)))
          acc))))
  (^long getAt [_ ^LongArray arr ^long index]
    (aget ^longs (.array arr) index))
  (^double getAt [_ ^DoubleArray arr ^long index]
    (aget ^doubles (.array arr) index)))

(def ^:private ^IArrayOps array-ops (ArrayOps.))

(defn sum-double
  "Returns the sum of elements as a primitive double.
  Works with DoubleArray."
  ^double [^DoubleArray arr]
  (.sum array-ops arr))

(defn sum-long
  "Returns the sum of elements as a primitive long.
  Only works with LongArray."
  ^long [^LongArray arr]
  (.sum array-ops arr))

(defn get-long
  "Returns the value at index as a primitive long."
  ^long [^LongArray arr ^long index]
  (.getAt array-ops arr index))

(defn get-double
  "Returns the value at index as a primitive double."
  ^double [^DoubleArray arr ^long index]
  (.getAt array-ops arr index))

(defn lpos?
  "Primitive long positive check."
  [^long v]
  (pos? v))

(defn dpos?
  "Primitive double positive check."
  [^double v]
  (pos? v))

(defn fold
  "Reduces over the elements with function f and initial value init.
  f is called as (f acc elem) for each element.
  This is the generic version that boxes primitive values."
  [^IFold arr f init]
  (.fold arr f init))

(defn fold-double
  "Reduces over elements with a primitive double function.
  Works with both DoubleArray and LongArray (longs are converted to doubles).
  f must be (fn ^double [^double acc ^double val] ...).
  Returns a primitive double."
  ^double [^IDoubleFold arr f ^double init]
  (.fold arr f init))

(defn dplus
  "Primitive double addition for use with fold-double."
  ^double [^double a ^double b]
  (+ a b))

(defn lplus
  "Primitive long addition for use with fold-long."
  ^long [^long a ^long b]
  (+ a b))

(defn fold-long
  "Reduces over long elements with a primitive long function.
  f must be (fn ^long [^long acc ^long val] ...).
  Returns a primitive long."
  ^long [^ILongFold arr f ^long init]
  (.fold arr f init))

(defn dfold
  "Reduces over elements with an object-returning function, receiving primitive doubles.
  Works with both DoubleArray and LongArray (longs are converted to doubles).
  f must be (fn [acc ^double val] ...) - receives primitive double, returns Object.
  Use this when accumulating into a collection."
  [^IDoubleObjectFold arr f init]
  (.foldObject arr f init))

(defn lfold
  "Reduces over long elements with an object-returning function.
  f must be (fn [acc ^long val] ...) - receives primitive long, returns Object.
  Use this when accumulating into a collection from LongArray."
  [^ILongObjectFold arr f init]
  (.foldObject arr f init))

(defn ->double-array
  "Creates a DoubleArray from a double-array."
  ^DoubleArray [^doubles arr]
  (DoubleArray. arr))

(defn ->long-array
  "Creates a LongArray from a long-array."
  ^LongArray [^longs arr]
  (LongArray. arr))

(defn ->object-array
  "Creates an ObjectArray from an object-array."
  ^ObjectArray [^objects arr]
  (ObjectArray. arr))

(defn first-element
  "Returns the first element from a typed array.
  Returns nil for empty arrays."
  [arr]
  (when (pos? (length arr))
    (cond
      (instance? DoubleArray arr)
      (get-double arr 0)

      (instance? LongArray arr)
      (get-long arr 0)

      (instance? ObjectArray arr)
      (aget ^objects (.array ^ObjectArray arr) 0))))

(defn last-element
  "Returns the last element from a typed array.
  Returns nil for empty arrays."
  [arr]
  (let [len (length arr)]
    (when (pos? len)
      (let [idx (dec len)]
        (cond
          (instance? DoubleArray arr)
          (get-double arr idx)

          (instance? LongArray arr)
          (get-long arr idx)

          (instance? ObjectArray arr)
          (aget ^objects (.array ^ObjectArray arr) idx))))))

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

(defn array-for-metric-type
  "Creates an appropriately typed array wrapper from a collection of values.

  metric-type is :quantitative, :event, or :nominal.
  values is a sequence of values to convert to a typed array.

  Returns a DoubleArray, LongArray, or ObjectArray."
  [metric-type values]
  (case metric-type
    :quantitative (DoubleArray. (double-array values))
    :event        (LongArray. (long-array values))
    :nominal      (ObjectArray. (object-array values))))

(defn sorted
  "Returns a new sorted DoubleArray.
  Uses Java's Arrays.sort for efficient primitive sorting.
  LongArray is converted to DoubleArray during sorting."
  ^DoubleArray [arr]
  (cond
    (instance? DoubleArray arr)
    (let [^doubles a   (.array ^DoubleArray arr)
          ^doubles cpy (Arrays/copyOf a (alength a))]
      (Arrays/sort cpy)
      (DoubleArray. cpy))

    (instance? LongArray arr)
    (let [^longs a   (.array ^LongArray arr)
          len        (alength a)
          ^doubles cpy (double-array len)]
      (dotimes [i len]
        (aset cpy i (double (aget a i))))
      (Arrays/sort cpy)
      (DoubleArray. cpy))))

(defn fold-double-skip
  "Fold over elements skipping the element at skip-idx.
  Used for jackknife resampling.
  f must be (fn ^double [^double acc ^double val] ...).
  Returns a primitive double."
  ^double [^DoubleArray arr ^long skip-idx ^clojure.lang.IFn$DDD f ^double init]
  (let [^doubles a (.array arr)
        len        (alength a)]
    (loop [i   (long 0)
           acc init]
      (if (< i len)
        (if (== i skip-idx)
          (recur (unchecked-inc i) acc)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget a i))))
        acc))))

(defn dfold-skip
  "Fold over elements skipping the element at skip-idx, returning Object.
  Used for jackknife resampling when accumulating into a collection.
  f must be (fn [acc ^double val] ...) - receives primitive double, returns Object."
  [^DoubleArray arr ^long skip-idx ^clojure.lang.IFn$ODO f init]
  (let [^doubles a (.array arr)
        len        (alength a)]
    (loop [i   (long 0)
           acc init]
      (if (< i len)
        (if (== i skip-idx)
          (recur (unchecked-inc i) acc)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget a i))))
        acc))))

(defn filter-indices
  "Creates a new DoubleArray containing only elements at indices NOT in exclude-set.
  exclude-set is a set of long indices to exclude."
  ^DoubleArray [^DoubleArray arr exclude-set]
  (let [^doubles a   (.array arr)
        len          (alength a)
        exclude-size (count exclude-set)
        new-len      (- len exclude-size)
        ^doubles out (double-array new-len)]
    (loop [i   (long 0)
           j   (long 0)]
      (if (< i len)
        (if (contains? exclude-set i)
          (recur (unchecked-inc i) j)
          (do
            (aset out j (aget a i))
            (recur (unchecked-inc i) (unchecked-inc j))))
        (DoubleArray. out)))))

(defn to-double-vec
  "Converts a typed array to a vector of doubles.
  Works with DoubleArray and LongArray (longs are converted to doubles)."
  [arr]
  (fold arr conj []))

(defn indexed-dfold
  "Fold over elements with index, receiving primitive doubles.
  f is called as (f acc idx val) for each element.
  Works with both DoubleArray and LongArray."
  [arr f init]
  (cond
    (instance? DoubleArray arr)
    (let [^doubles a (.array ^DoubleArray arr)
          len        (alength a)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (f acc i (aget a i)))
          acc)))

    (instance? LongArray arr)
    (let [^longs a (.array ^LongArray arr)
          len      (alength a)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (f acc i (double (aget a i))))
          acc)))))

(defn sum
  "Returns the sum of elements.
  For DoubleArray returns double, for LongArray returns long."
  [arr]
  (cond
    (instance? DoubleArray arr) (sum-double arr)
    (instance? LongArray arr)   (sum-long arr)))

(defn get-at
  "Returns the element at index.
  Works with DoubleArray, LongArray, and ObjectArray."
  [arr ^long index]
  (cond
    (instance? DoubleArray arr)
    (get-double arr index)

    (instance? LongArray arr)
    (get-long arr index)

    (instance? ObjectArray arr)
    (aget ^objects (.array ^ObjectArray arr) index)))

(defn dmap
  "Maps f over elements, returning a new DoubleArray.
  f receives a primitive double and should return a double.
  Works with DoubleArray and LongArray."
  ^DoubleArray [arr f]
  (cond
    (instance? DoubleArray arr)
    (let [^doubles a   (.array ^DoubleArray arr)
          len          (alength a)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (double (f (aget a i)))))
      (DoubleArray. out))

    (instance? LongArray arr)
    (let [^longs a     (.array ^LongArray arr)
          len          (alength a)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (double (f (double (aget a i))))))
      (DoubleArray. out))))

(defn dmap-indexed
  "Maps f over elements with index, returning a new DoubleArray.
  f receives (index, value) and should return a double.
  Works with DoubleArray and LongArray."
  ^DoubleArray [arr f]
  (cond
    (instance? DoubleArray arr)
    (let [^doubles a   (.array ^DoubleArray arr)
          len          (alength a)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (double (f i (aget a i)))))
      (DoubleArray. out))

    (instance? LongArray arr)
    (let [^longs a     (.array ^LongArray arr)
          len          (alength a)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (double (f i (double (aget a i))))))
      (DoubleArray. out))))

(defn any-positive?
  "Returns true if any element is positive (> 0).
  Works with DoubleArray and LongArray."
  [arr]
  (cond
    (instance? DoubleArray arr)
    (let [^doubles a (.array ^DoubleArray arr)
          len        (alength a)]
      (loop [i 0]
        (if (< i len)
          (if (pos? (aget a i))
            true
            (recur (unchecked-inc i)))
          false)))

    (instance? LongArray arr)
    (let [^longs a (.array ^LongArray arr)
          len      (alength a)]
      (loop [i 0]
        (if (< i len)
          (if (pos? (aget a i))
            true
            (recur (unchecked-inc i)))
          false)))))
