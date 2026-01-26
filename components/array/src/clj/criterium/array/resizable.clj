(ns criterium.array.resizable
  "Resizable array types for algorithms where size isn't known upfront.

  Provides mutable-size wrappers around primitive arrays that can be
  resized down (but not up) from their initial capacity. Use cases include
  algorithms like Knuth histogram binning where the final array size
  depends on the data.

  Three types are provided:
    - ResizableDoubleArray for double values
    - ResizableLongArray for long values
    - ResizableObjectArray for arbitrary objects

  All operations respect the current size (not capacity). Map/filter
  operations return fixed arrays (DoubleArray, LongArray, ObjectArray)."
  (:require
   [criterium.array.interfaces])
  (:import
   [criterium.array.interfaces ITypedArray IDoubleArray ILongArray IResizable]
   [criterium.array.interfaces
    IFold IDoubleFold ILongFold IDoubleObjectFold ILongObjectFold
    IDoubleMap IDoubleMapIndexed ILongMap ILongMapIndexed
    IDoubleAny ILongAny IArrayEquals ISortable
    IDoubleFoldSkip IDoubleObjectFoldSkip
    IFilterIndices IIndexedDoubleFold IIndexedDoubleObjectFold
    IIndexed IIndexedSet]
   [criterium.transducer.interfaces
    IDDDReducible
    ILLLReducible
    IODOReducible
    IOLOReducible]
   [java.util Arrays]))

;;; Forward declarations for fixed array types
;; These are created by criterium.array and passed back via map/filter operations

(declare ->DoubleArray ->LongArray ->ObjectArray)

;;; Validation helpers

(defn- check-index-bounds
  "Throws IndexOutOfBoundsException if index is out of bounds for size."
  [^long index ^long size]
  (when (or (neg? index) (>= index size))
    (throw (IndexOutOfBoundsException.
            (str "index " index " out of bounds for size " size)))))

(defn- check-resize-bounds
  "Throws IllegalArgumentException if new-size is out of bounds for capacity."
  [^long new-size ^long capacity]
  (when (or (neg? new-size) (> new-size capacity))
    (throw (IllegalArgumentException.
            (str "new-size must be between 0 and capacity ("
                 capacity "), got: " new-size)))))

(defn- check-initial-size
  "Throws IllegalArgumentException if initial-size is out of bounds for capacity."
  [^long initial-size ^long capacity]
  (when (or (neg? initial-size) (> initial-size capacity))
    (throw (IllegalArgumentException.
            (str "initial-size must be between 0 and capacity ("
                 capacity "), got: " initial-size)))))

;;; ResizableDoubleArray

(deftype ResizableDoubleArray [^doubles array
                               ^:unsynchronized-mutable ^long size]
  IResizable
  (^long resize [_ ^long new-size]
    (check-resize-bounds new-size (alength array))
    (set! size new-size)
    new-size)
  (^long capacity [_] (alength array))

  IDoubleArray
  ITypedArray
  (elemType [_] :double)
  (length [_] size)

  IIndexed
  (^double getDouble [_ ^long index]
    (check-index-bounds index size)
    (aget array index))
  (^long getLong [_ ^long index]
    (check-index-bounds index size)
    (long (aget array index)))
  (getObject [_ ^long index]
    (check-index-bounds index size)
    (aget array index))

  IIndexedSet
  (^double setDouble [_ ^long index ^double v]
    (check-index-bounds index size)
    (aset array index v))
  (^long setLong [_ ^long index ^long v]
    (check-index-bounds index size)
    (aset array index (double v))
    v)
  (setObject [_ ^long index v]
    (check-index-bounds index size)
    (aset array index (double v))
    v)

  IDoubleFold
  (^double fold [_ ^clojure.lang.IFn$DDD f ^double init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc (aget array i)))
        acc)))

  IDoubleObjectFold
  (foldObject [_ ^clojure.lang.IFn$ODO f init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc (aget array i)))
        acc)))

  IFold
  (^Object fold [_ f init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (f acc (aget array i)))
        acc)))

  IDoubleMap
  (dmap [_ ^clojure.lang.IFn$DD f]
    (let [^doubles out (double-array size)]
      (dotimes [i size]
        (aset out i (.invokePrim f (aget array i))))
      (->DoubleArray out)))

  IDoubleMapIndexed
  (dmapIndexed [_ ^clojure.lang.IFn$LDD f]
    (let [^doubles out (double-array size)]
      (dotimes [i size]
        (aset out i (.invokePrim f i (aget array i))))
      (->DoubleArray out)))

  IDoubleAny
  (^boolean dany [_ ^clojure.lang.IFn$DO f]
    (loop [i 0]
      (if (< i size)
        (if (.invokePrim f (aget array i))
          true
          (recur (unchecked-inc i)))
        false)))

  IArrayEquals
  (^boolean arrayEquals [_ expected]
    (let [expected-vec (vec expected)
          n            (count expected-vec)]
      (and (== n size)
           (loop [i 0]
             (if (< i n)
               (if (== (aget array i) (double (nth expected-vec i)))
                 (recur (unchecked-inc i))
                 false)
               true)))))

  ISortable
  (sorted [_]
    (let [^doubles cpy (Arrays/copyOf array (int size))]
      (Arrays/sort cpy)
      (->DoubleArray cpy)))

  IDoubleFoldSkip
  (^double foldSkip [_ ^long skip-idx ^clojure.lang.IFn$DDD f ^double init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (if (== i skip-idx)
          (recur (unchecked-inc i) acc)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget array i))))
        acc)))

  IDoubleObjectFoldSkip
  (foldObjectSkip [_ ^long skip-idx ^clojure.lang.IFn$ODO f init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (if (== i skip-idx)
          (recur (unchecked-inc i) acc)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (aget array i))))
        acc)))

  IFilterIndices
  (filterIndices [_ exclude-set]
    (let [exclude-size (count exclude-set)
          new-len      (- size exclude-size)
          ^doubles out (double-array new-len)]
      (loop [i (long 0)
             j (long 0)]
        (if (< i size)
          (if (contains? exclude-set i)
            (recur (unchecked-inc i) j)
            (do
              (aset out j (aget array i))
              (recur (unchecked-inc i) (unchecked-inc j))))
          (->DoubleArray out)))))

  IIndexedDoubleFold
  (^double indexedFold [_ ^clojure.lang.IFn$DLDD f ^double init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc i (aget array i)))
        acc)))

  IIndexedDoubleObjectFold
  (indexedFoldObject [_ ^clojure.lang.IFn$OLDO f init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc i (aget array i)))
        acc)))

  IDDDReducible
  (reduce [_ f init]
    (loop [i 0 acc init]
      (if (< i size)
        (recur (inc i)
               (.invokePrim ^clojure.lang.IFn$DDD f acc (aget array i)))
        acc)))
  IODOReducible
  (reduceDouble
    [_ f init]
    (loop [i 0 acc init]
      (if (< i size)
        (recur (inc i)
               (.invokePrim ^clojure.lang.IFn$ODO f acc (aget array i)))
        acc))))

;;; ResizableLongArray

(deftype ResizableLongArray [^longs array
                             ^:unsynchronized-mutable ^long size]
  IResizable
  (^long resize [_ ^long new-size]
    (check-resize-bounds new-size (alength array))
    (set! size new-size)
    new-size)
  (^long capacity [_] (alength array))

  ILongArray
  ITypedArray
  (elemType [_] :long)
  (length [_] size)

  IIndexed
  (^double getDouble [_ ^long index]
    (check-index-bounds index size)
    (double (aget array index)))
  (^long getLong [_ ^long index]
    (check-index-bounds index size)
    (aget array index))
  (getObject [_ ^long index]
    (check-index-bounds index size)
    (aget array index))

  IIndexedSet
  (^double setDouble [_ ^long index ^double v]
    (check-index-bounds index size)
    (aset array index (long v))
    v)
  (^long setLong [_ ^long index ^long v]
    (check-index-bounds index size)
    (aset array index v))
  (setObject [_ ^long index v]
    (check-index-bounds index size)
    (aset array index (long v))
    v)

  ILongFold
  (^long fold [_ ^clojure.lang.IFn$LLL f ^long init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc (aget array i)))
        acc)))

  IDoubleFold
  (^double fold [_ ^clojure.lang.IFn$DDD f ^double init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc (double (aget array i))))
        acc)))

  ILongObjectFold
  (foldObject [_ ^clojure.lang.IFn$OLO f init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc (aget array i)))
        acc)))

  IDoubleObjectFold
  (foldObject [_ ^clojure.lang.IFn$ODO f init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc (double (aget array i))))
        acc)))

  IFold
  (^Object fold [_ f init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (f acc (aget array i)))
        acc)))

  IDoubleMap
  (dmap [_ ^clojure.lang.IFn$DD f]
    (let [^doubles out (double-array size)]
      (dotimes [i size]
        (aset out i (.invokePrim f (double (aget array i)))))
      (->DoubleArray out)))

  IDoubleMapIndexed
  (dmapIndexed [_ ^clojure.lang.IFn$LDD f]
    (let [^doubles out (double-array size)]
      (dotimes [i size]
        (aset out i (.invokePrim f i (double (aget array i)))))
      (->DoubleArray out)))

  ILongMap
  (lmap [_ ^clojure.lang.IFn$LL f]
    (let [^longs out (long-array size)]
      (dotimes [i size]
        (aset out i (.invokePrim f (aget array i))))
      (->LongArray out)))

  ILongMapIndexed
  (lmapIndexed [_ ^clojure.lang.IFn$LLL f]
    (let [^longs out (long-array size)]
      (dotimes [i size]
        (aset out i (.invokePrim f i (aget array i))))
      (->LongArray out)))

  IDoubleAny
  (^boolean dany [_ ^clojure.lang.IFn$DO f]
    (loop [i 0]
      (if (< i size)
        (if (.invokePrim f (double (aget array i)))
          true
          (recur (unchecked-inc i)))
        false)))

  ILongAny
  (^boolean lany [_ ^clojure.lang.IFn$LO f]
    (loop [i 0]
      (if (< i size)
        (if (.invokePrim f (aget array i))
          true
          (recur (unchecked-inc i)))
        false)))

  IArrayEquals
  (^boolean arrayEquals [_ expected]
    (let [expected-vec (vec expected)
          n            (count expected-vec)]
      (and (== n size)
           (loop [i 0]
             (if (< i n)
               (if (== (aget array i) (long (nth expected-vec i)))
                 (recur (unchecked-inc i))
                 false)
               true)))))

  ISortable
  (sorted [_]
    (let [^doubles cpy (double-array size)]
      (dotimes [i size]
        (aset cpy i (double (aget array i))))
      (Arrays/sort cpy)
      (->DoubleArray cpy)))

  IDoubleFoldSkip
  (^double foldSkip [_ ^long skip-idx ^clojure.lang.IFn$DDD f ^double init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (if (== i skip-idx)
          (recur (unchecked-inc i) acc)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (double (aget array i)))))
        acc)))

  IDoubleObjectFoldSkip
  (foldObjectSkip [_ ^long skip-idx ^clojure.lang.IFn$ODO f init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (if (== i skip-idx)
          (recur (unchecked-inc i) acc)
          (recur (unchecked-inc i)
                 (.invokePrim f acc (double (aget array i)))))
        acc)))

  IFilterIndices
  (filterIndices [_ exclude-set]
    (let [exclude-size (count exclude-set)
          new-len      (- size exclude-size)
          ^longs out   (long-array new-len)]
      (loop [i (long 0)
             j (long 0)]
        (if (< i size)
          (if (contains? exclude-set i)
            (recur (unchecked-inc i) j)
            (do
              (aset out j (aget array i))
              (recur (unchecked-inc i) (unchecked-inc j))))
          (->LongArray out)))))

  IIndexedDoubleFold
  (^double indexedFold [_ ^clojure.lang.IFn$DLDD f ^double init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc i (double (aget array i))))
        acc)))

  IIndexedDoubleObjectFold
  (indexedFoldObject [_ ^clojure.lang.IFn$OLDO f init]
    (loop [i   (long 0)
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (.invokePrim f acc i (double (aget array i))))
        acc)))

  ILLLReducible
  (reduce [_ f init]
    (loop [i 0 acc init]
      (if (< i size)
        (recur (inc i)
               (.invokePrim ^clojure.lang.IFn$LLL f acc (aget array i)))
        acc)))
  IOLOReducible
  (reduceLong
    [_ f init]
    (loop [i 0 acc init]
      (if (< i size)
        (recur (inc i)
               (.invokePrim ^clojure.lang.IFn$OLO f acc (aget array i)))
        acc))))

;;; ResizableObjectArray

(deftype ResizableObjectArray [^objects array
                               ^:unsynchronized-mutable ^long size]
  IResizable
  (^long resize [_ ^long new-size]
    (check-resize-bounds new-size (alength array))
    (set! size new-size)
    new-size)
  (^long capacity [_] (alength array))

  ITypedArray
  (elemType [_] :object)
  (length [_] size)

  IFold
  (^Object fold [_ f init]
    (loop [i   0
           acc init]
      (if (< i size)
        (recur (unchecked-inc i)
               (f acc (aget array i)))
        acc)))

  IArrayEquals
  (^boolean arrayEquals [_ expected]
    (let [expected-vec (vec expected)
          n            (count expected-vec)]
      (and (== n size)
           (loop [i 0]
             (if (< i n)
               (if (= (aget array i) (nth expected-vec i))
                 (recur (unchecked-inc i))
                 false)
               true))))))

;;; Constructors for fixed array types
;; These are set by criterium.array during initialization to avoid circular deps

(def ^:private fixed-array-constructors (atom nil))

(defn set-fixed-array-constructors!
  "Called by criterium.array to provide constructors for fixed array types."
  [constructors]
  (reset! fixed-array-constructors constructors))

(defn- ->DoubleArray [^doubles arr]
  ((:double @fixed-array-constructors) arr))

(defn- ->LongArray [^longs arr]
  ((:long @fixed-array-constructors) arr))

(defn- ->ObjectArray [^objects arr]
  ((:object @fixed-array-constructors) arr))

;;; Type predicates

(defn resizable-double-array?
  "Returns true if x is a ResizableDoubleArray."
  [x]
  (instance? ResizableDoubleArray x))

(defn resizable-long-array?
  "Returns true if x is a ResizableLongArray."
  [x]
  (instance? ResizableLongArray x))

(defn resizable-object-array?
  "Returns true if x is a ResizableObjectArray."
  [x]
  (instance? ResizableObjectArray x))

;;; Factory functions

(defn resizable-double-array
  "Creates a ResizableDoubleArray with given capacity.
  With one argument, creates array with size equal to capacity.
  With two arguments, creates array with given capacity and initial size.
  The initial size must be between 0 and capacity (inclusive)."
  (^ResizableDoubleArray [^long capacity]
   (ResizableDoubleArray. (double-array capacity) capacity))
  (^ResizableDoubleArray [^long capacity ^long initial-size]
   (check-initial-size initial-size capacity)
   (ResizableDoubleArray. (double-array capacity) initial-size)))

(defn resizable-long-array
  "Creates a ResizableLongArray with given capacity.
  With one argument, creates array with size equal to capacity.
  With two arguments, creates array with given capacity and initial size.
  The initial size must be between 0 and capacity (inclusive)."
  (^ResizableLongArray [^long capacity]
   (ResizableLongArray. (long-array capacity) capacity))
  (^ResizableLongArray [^long capacity ^long initial-size]
   (check-initial-size initial-size capacity)
   (ResizableLongArray. (long-array capacity) initial-size)))

(defn resizable-object-array
  "Creates a ResizableObjectArray with given capacity.
  With one argument, creates array with size equal to capacity.
  With two arguments, creates array with given capacity and initial size.
  The initial size must be between 0 and capacity (inclusive)."
  (^ResizableObjectArray [^long capacity]
   (ResizableObjectArray. (object-array capacity) capacity))
  (^ResizableObjectArray [^long capacity ^long initial-size]
   (check-initial-size initial-size capacity)
   (ResizableObjectArray. (object-array capacity) initial-size)))

;;; Operations

(defn resize!
  "Resizes a resizable array to a new size.
  The new size must be between 0 and capacity (inclusive).
  Returns the new size."
  ^long [^IResizable arr ^long new-size]
  (.resize arr new-size))

(defn capacity
  "Returns the maximum capacity of a resizable array."
  ^long [^IResizable arr]
  (.capacity arr))

(defn to-fixed
  "Converts a resizable array to a fixed array of the same element type.
  Creates a new array containing only the active elements (0 to size-1).
  ResizableDoubleArray -> DoubleArray, ResizableLongArray -> LongArray,
  ResizableObjectArray -> ObjectArray."
  [arr]
  (cond
    (instance? ResizableDoubleArray arr)
    (let [^ResizableDoubleArray a arr
          size     (.length a)
          ^doubles src (.array a)
          ^doubles dst (Arrays/copyOf src (int size))]
      (->DoubleArray dst))

    (instance? ResizableLongArray arr)
    (let [^ResizableLongArray a arr
          size   (.length a)
          ^longs src (.array a)
          ^longs dst (Arrays/copyOf src (int size))]
      (->LongArray dst))

    (instance? ResizableObjectArray arr)
    (let [^ResizableObjectArray a arr
          size    (.length a)
          ^objects src (.array a)
          ^objects dst (Arrays/copyOf src (int size))]
      (->ObjectArray dst))

    :else
    (throw (IllegalArgumentException.
            (str "to-fixed expects a resizable array, got: " (type arr))))))
