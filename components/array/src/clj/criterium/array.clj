(ns criterium.array
  "Typed array collections using primitive arrays to avoid boxing.

  Provides wrappers around primitive arrays (double-array, long-array)
  that can be used for efficient sample storage during benchmarking.
  Three types are provided for the three metric types:
    - DoubleArray for :quantitative metrics
    - LongArray for :event metrics
    - ObjectArray for :nominal metrics"
  (:require
   [criterium.array.interface]
   [criterium.transducer.interface]
   [criterium.util.invariant :refer [have?]])
  (:import
   [criterium.array.types ITypedArray IDoubleArray ILongArray]
   [criterium.array.interface
    IFold IDoubleFold ILongFold IDoubleObjectFold ILongObjectFold
    IDoubleMap IDoubleMapIndexed ILongMap ILongMapIndexed
    IDoubleAny ILongAny IArrayEquals ISortable
    IDoubleFoldSkip IDoubleObjectFoldSkip
    IFilterIndices IIndexedDoubleFold IIndexedDoubleObjectFold
    IIndexed IIndexedSet IArrayOps]
   [criterium.transducer.interface
    IDDDReducible
    ILLLReducible
    IODOReducible
    IOLOReducible]
   [java.util Arrays]))

(deftype DoubleArray [^doubles array]
  IDoubleArray
  ITypedArray
  (elemType [_] :double)
  (length [_] (alength array))

  IIndexed
  (^double getDouble [_ ^long index] (aget array index))
  (^long getLong [_ ^long index] (long (aget array index)))
  (getObject [_ ^long index] (aget array index))

  IIndexedSet
  (^double setDouble [_ ^long index ^double v] (aset array index v))
  (^long setLong [_ ^long index ^long v]
    (do
      (aset array index (double v))
      v))
  (setObject [_ ^long index v]
    (do
      (aset array index (double v))
      v))

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
             (f acc (aget array i))))

  IDoubleMap
  (dmap [_ ^clojure.lang.IFn$DD f]
    (let [len          (alength array)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (.invokePrim f (aget array i))))
      (DoubleArray. out)))

  IDoubleMapIndexed
  (dmapIndexed [_ ^clojure.lang.IFn$LDD f]
    (let [len          (alength array)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (.invokePrim f i (aget array i))))
      (DoubleArray. out)))

  IDoubleAny
  (^boolean dany [_ ^clojure.lang.IFn$DO f]
    (let [len (alength array)]
      (loop [i 0]
        (if (< i len)
          (if (.invokePrim f (aget array i))
            true
            (recur (unchecked-inc i)))
          false))))

  IArrayEquals
  (^boolean arrayEquals [_ expected]
    (let [expected-vec (vec expected)
          n            (count expected-vec)]
      (and (== n (alength array))
           (loop [i 0]
             (if (< i n)
               (if (== (aget array i) (double (nth expected-vec i)))
                 (recur (unchecked-inc i))
                 false)
               true)))))

  ISortable
  (sorted [_]
    (let [^doubles cpy (Arrays/copyOf array (alength array))]
      (Arrays/sort cpy)
      (DoubleArray. cpy)))

  IDoubleFoldSkip
  (^double foldSkip [_ ^long skip-idx ^clojure.lang.IFn$DDD f ^double init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (if (== i skip-idx)
            (recur (unchecked-inc i) acc)
            (recur (unchecked-inc i)
                   (.invokePrim f acc (aget array i))))
          acc))))

  IDoubleObjectFoldSkip
  (foldObjectSkip [_ ^long skip-idx ^clojure.lang.IFn$ODO f init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (if (== i skip-idx)
            (recur (unchecked-inc i) acc)
            (recur (unchecked-inc i)
                   (.invokePrim f acc (aget array i))))
          acc))))

  IFilterIndices
  (filterIndices [_ exclude-set]
    (let [len          (alength array)
          exclude-size (count exclude-set)
          new-len      (- len exclude-size)
          ^doubles out (double-array new-len)]
      (loop [i (long 0)
             j (long 0)]
        (if (< i len)
          (if (contains? exclude-set i)
            (recur (unchecked-inc i) j)
            (do
              (aset out j (aget array i))
              (recur (unchecked-inc i) (unchecked-inc j))))
          (DoubleArray. out)))))

  IIndexedDoubleFold
  (^double indexedFold [_ ^clojure.lang.IFn$DLDD f ^double init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc i (aget array i)))
          acc))))

  IIndexedDoubleObjectFold
  (indexedFoldObject [_ ^clojure.lang.IFn$OLDO f init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc i (aget array i)))
          acc))))

  IDDDReducible
  (reduce [_ f init]
    (let [n (alength array)]
      (loop [i 0 acc init]
        (if (< i n)
          (recur (inc i)
                 (.invokePrim ^clojure.lang.IFn$DDD f acc (aget array i)))
          acc))))
  IODOReducible
  (reduceDouble
    [_ f init]
    (let [n (alength array)]
      (loop [i 0 acc init]
        (if (< i n)
          (recur (inc i)
                 (.invokePrim ^clojure.lang.IFn$ODO f acc (aget array i)))
          acc)))))

(deftype LongArray [^longs array]
  ILongArray
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
             (f acc (aget array i))))

  IDoubleMap
  (dmap [_ ^clojure.lang.IFn$DD f]
    (let [len          (alength array)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (.invokePrim f (double (aget array i)))))
      (DoubleArray. out)))

  IDoubleMapIndexed
  (dmapIndexed [_ ^clojure.lang.IFn$LDD f]
    (let [len          (alength array)
          ^doubles out (double-array len)]
      (dotimes [i len]
        (aset out i (.invokePrim f i (double (aget array i)))))
      (DoubleArray. out)))

  ILongMap
  (lmap [_ ^clojure.lang.IFn$LL f]
    (let [len        (alength array)
          ^longs out (long-array len)]
      (dotimes [i len]
        (aset out i (.invokePrim f (aget array i))))
      (LongArray. out)))

  ILongMapIndexed
  (lmapIndexed [_ ^clojure.lang.IFn$LLL f]
    (let [len        (alength array)
          ^longs out (long-array len)]
      (dotimes [i len]
        (aset out i (.invokePrim f i (aget array i))))
      (LongArray. out)))

  IDoubleAny
  (^boolean dany [_ ^clojure.lang.IFn$DO f]
    (let [len (alength array)]
      (loop [i 0]
        (if (< i len)
          (if (.invokePrim f (double (aget array i)))
            true
            (recur (unchecked-inc i)))
          false))))

  ILongAny
  (^boolean lany [_ ^clojure.lang.IFn$LO f]
    (let [len (alength array)]
      (loop [i 0]
        (if (< i len)
          (if (.invokePrim f (aget array i))
            true
            (recur (unchecked-inc i)))
          false))))

  IArrayEquals
  (^boolean arrayEquals [_ expected]
    (let [expected-vec (vec expected)
          n            (count expected-vec)]
      (and (== n (alength array))
           (loop [i 0]
             (if (< i n)
               (if (== (aget array i) (long (nth expected-vec i)))
                 (recur (unchecked-inc i))
                 false)
               true)))))

  ISortable
  (sorted [_]
    (let [len          (alength array)
          ^doubles cpy (double-array len)]
      (dotimes [i len]
        (aset cpy i (double (aget array i))))
      (Arrays/sort cpy)
      (DoubleArray. cpy)))

  IDoubleFoldSkip
  (^double foldSkip [_ ^long skip-idx ^clojure.lang.IFn$DDD f ^double init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (if (== i skip-idx)
            (recur (unchecked-inc i) acc)
            (recur (unchecked-inc i)
                   (.invokePrim f acc (double (aget array i)))))
          acc))))

  IDoubleObjectFoldSkip
  (foldObjectSkip [_ ^long skip-idx ^clojure.lang.IFn$ODO f init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (if (== i skip-idx)
            (recur (unchecked-inc i) acc)
            (recur (unchecked-inc i)
                   (.invokePrim f acc (double (aget array i)))))
          acc))))

  IFilterIndices
  (filterIndices [_ exclude-set]
    (let [len          (alength array)
          exclude-size (count exclude-set)
          new-len      (- len exclude-size)
          ^longs out   (long-array new-len)]
      (loop [i (long 0)
             j (long 0)]
        (if (< i len)
          (if (contains? exclude-set i)
            (recur (unchecked-inc i) j)
            (do
              (aset out j (aget array i))
              (recur (unchecked-inc i) (unchecked-inc j))))
          (LongArray. out)))))

  IIndexedDoubleFold
  (^double indexedFold [_ ^clojure.lang.IFn$DLDD f ^double init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc i (double (aget array i))))
          acc))))

  IIndexedDoubleObjectFold
  (indexedFoldObject [_ ^clojure.lang.IFn$OLDO f init]
    (let [len (alength array)]
      (loop [i   (long 0)
             acc init]
        (if (< i len)
          (recur (unchecked-inc i)
                 (.invokePrim f acc i (double (aget array i))))
          acc))))

  ILLLReducible
  (reduce [_ f init]
    (let [n (dec (alength array))]
      (loop [i 0 acc init]
        (if (< i n)
          (recur (inc i)
                 (.invokePrim ^clojure.lang.IFn$LLL f acc (aget array i)))
          acc))))
  IOLOReducible
  (reduceLong
    [_ f init]
    (let [n (dec (alength array))]
      (loop [i 0 acc init]
        (if (< i n)
          (recur (inc i)
                 (.invokePrim ^clojure.lang.IFn$OLO f acc (aget array i)))
          acc)))))

(deftype ObjectArray [^objects array]
  ITypedArray
  (elemType [_] :object)
  (length [_] (alength array))

  IFold
  (fold [_ f init]
    (areduce array i acc init
             (f acc (aget array i))))

  IArrayEquals
  (^boolean arrayEquals [_ expected]
    (let [expected-vec (vec expected)
          n            (count expected-vec)]
      (and (== n (alength array))
           (loop [i 0]
             (if (< i n)
               (if (= (aget array i) (nth expected-vec i))
                 (recur (unchecked-inc i))
                 false)
               true))))))

(defn elem-type
  "Returns the element type keyword: :double, :long, or :object."
  [^ITypedArray arr]
  (.elemType arr))

(defn length
  "Returns the number of elements in the array."
  ^long [^ITypedArray arr]
  (.length arr))

(defn typed-array?
  "Returns true if x is a typed array (DoubleArray, LongArray, or ObjectArray)."
  [x]
  (instance? ITypedArray x))

(defn double-array?
  "Returns true if x is a DoubleArray."
  [x]
  (instance? DoubleArray x))

(defn long-array?
  "Returns true if x is a LongArray."
  [x]
  (instance? LongArray x))

(defn object-array?
  "Returns true if x is an ObjectArray."
  [x]
  (instance? ObjectArray x))

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
    (aget ^doubles (.array arr) index))
  (^long setAt [_ ^LongArray arr ^long index ^long value]
    (aset ^longs (.array arr) index value))
  (^double setAt [_ ^DoubleArray arr ^long index ^double value]
    (aset ^doubles (.array arr) index value)))

(def ^IArrayOps array-ops (ArrayOps.))

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
  "Creates a DoubleArray from a primitive double array.
  Use (double-array coll) to convert a collection to a primitive array first."
  ^DoubleArray [^doubles arr]
  {:pre [(have? #(instance? (Class/forName "[D") %) arr)]}
  (DoubleArray. arr))

(defn ->long-array
  "Creates a LongArray from a primitive long array.
  Use (long-array coll) to convert a collection to a primitive array first."
  ^LongArray [^longs arr]
  {:pre [(have? #(instance? (Class/forName "[J") %) arr)]}
  (LongArray. arr))

(defn ->object-array
  "Creates an ObjectArray from an object array.
  Use (object-array coll) to convert a collection to an object array first."
  ^ObjectArray [^objects arr]
  {:pre [(have? #(instance? (Class/forName "[Ljava.lang.Object;") %) arr)]}
  (ObjectArray. arr))

(defn first-double
  "Returns the first element from a DoubleArray as a primitive double."
  ^double [^DoubleArray arr]
  (aget ^doubles (.array arr) 0))

(defn last-double
  "Returns the last element from a DoubleArray as a primitive double."
  ^double [^DoubleArray arr]
  (let [^doubles a (.array arr)]
    (aget a (dec (alength a)))))

(defn first-long
  "Returns the first element from a LongArray as a primitive long."
  ^long [^LongArray arr]
  (aget ^longs (.array arr) 0))

(defn last-long
  "Returns the last element from a LongArray as a primitive long."
  ^long [^LongArray arr]
  (let [^longs a (.array arr)]
    (aget a (dec (alength a)))))

(defn first-object
  "Returns the first element from an ObjectArray."
  [^ObjectArray arr]
  (aget ^objects (.array arr) 0))

(defn last-object
  "Returns the last element from an ObjectArray."
  [^ObjectArray arr]
  (let [^objects a (.array arr)]
    (aget a (dec (alength a)))))

(defn first-element
  "Returns the first element from a typed array.
  Dispatches based on runtime array type for cases where the type
  cannot be statically determined."
  [arr]
  (cond
    (instance? DoubleArray arr) (first-double arr)
    (instance? LongArray arr)   (first-long arr)
    (instance? ObjectArray arr) (first-object arr)))

(defn sorted
  "Returns a new sorted DoubleArray.
  Uses Java's Arrays.sort for efficient primitive sorting.
  LongArray is converted to DoubleArray during sorting."
  ^DoubleArray [^ISortable arr]
  (.sorted arr))

(defn fold-double-skip
  "Fold over elements skipping the element at skip-idx.
  Used for jackknife resampling.
  f must be (fn ^double [^double acc ^double val] ...).
  Works with DoubleArray and LongArray.
  Returns a primitive double."
  ^double [^IDoubleFoldSkip arr ^long skip-idx ^clojure.lang.IFn$DDD f ^double init]
  (.foldSkip arr skip-idx f init))

(defn dfold-skip
  "Fold over elements skipping the element at skip-idx, returning Object.
  Used for jackknife resampling when accumulating into a collection.
  f must be (fn [acc ^double val] ...) - receives primitive double, returns Object.
  Works with DoubleArray and LongArray."
  [^IDoubleObjectFoldSkip arr ^long skip-idx ^clojure.lang.IFn$ODO f init]
  (.foldObjectSkip arr skip-idx f init))

(defn filter-indices
  "Returns a new typed array containing only elements at indices NOT in exclude-set.
  Returns the same type as input: DoubleArray returns DoubleArray, LongArray returns LongArray.
  exclude-set is a set of long indices to exclude."
  [^IFilterIndices arr exclude-set]
  (.filterIndices arr exclude-set))

(defn indexed-fold-double
  "Fold over elements with index, accumulating and returning primitive double.
  f must be (fn ^double [^double acc ^long idx ^double val] ...).
  Uses .invokePrim to avoid boxing.
  Works with both DoubleArray and LongArray."
  ^double [^IIndexedDoubleFold arr ^clojure.lang.IFn$DLDD f ^double init]
  (.indexedFold arr f init))

(defn indexed-dfold
  "Fold over elements with index, accumulating and returning Object.
  f must be (fn [acc ^long idx ^double val] ...).
  Uses .invokePrim to avoid boxing on the index and value arguments.
  Works with both DoubleArray and LongArray."
  [^IIndexedDoubleObjectFold arr ^clojure.lang.IFn$OLDO f init]
  (.indexedFoldObject arr f init))

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

(defmacro set-at!
  "Returns the element at index.
  Works with DoubleArray, LongArray, and ObjectArray."
  [arr index value]
  `(.setAt array-ops ~arr ~index ~value))

(defn dmap
  "Maps f over elements, returning a new DoubleArray.
  f must be (fn ^double [^double x] ...).
  Works with DoubleArray and LongArray."
  ^DoubleArray [^IDoubleMap arr f]
  (.dmap arr f))

(defn dmap-indexed
  "Maps f over elements with index, returning a new DoubleArray.
  f must be (fn ^double [^long idx ^double x] ...).
  Works with DoubleArray and LongArray."
  ^DoubleArray [^IDoubleMapIndexed arr f]
  (.dmapIndexed arr f))

(defn lmap
  "Maps f over elements, returning a new LongArray.
  f must be (fn ^long [^long x] ...).
  Works only with LongArray."
  ^LongArray [^ILongMap arr f]
  (.lmap arr f))

(defn lmap-indexed
  "Maps f over elements with index, returning a new LongArray.
  f must be (fn ^long [^long idx ^long x] ...).
  Works only with LongArray."
  ^LongArray [^ILongMapIndexed arr f]
  (.lmapIndexed arr f))

(defn dany?
  "Returns true if any element satisfies the predicate.
  pred must be (fn [^double x] ...) returning truthy/falsy.
  Works with DoubleArray and LongArray."
  [^IDoubleAny arr pred]
  (.dany arr pred))

(defn lany?
  "Returns true if any element satisfies the predicate.
  pred must be (fn [^long x] ...) returning truthy/falsy.
  Works only with LongArray."
  [^ILongAny arr pred]
  (.lany arr pred))

(defn array=
  "Returns true if typed array elements equal the expected sequence.
  Compares element-by-element using == for doubles/longs, = for objects."
  [^IArrayEquals arr expected]
  (.arrayEquals arr expected))
