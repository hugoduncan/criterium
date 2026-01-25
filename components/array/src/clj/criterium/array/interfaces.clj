(ns criterium.array.interfaces
  "Interfaces for typed array collections with primitive support.

  These interfaces define the contract for typed array wrappers that
  avoid boxing overhead when working with primitive arrays.

  Interface hierarchy:
  - ITypedArray: basic array metadata (element type, length)
  - IFold: generic object-returning fold
  - IDoubleFold/ILongFold: primitive-in, primitive-out folds
  - IDoubleObjectFold/ILongObjectFold: primitive-in, object-out folds
  - IIndexed: indexed access to elements
  - IArrayOps: type-specific operations (sum, getAt)

  Base interfaces ITypedArray, ILongArray, IDoubleArray are defined in Java
  at criterium.array.interfaces to support proper inheritance hierarchies.")

(definterface IFold
  (fold [f init]))

(definterface IDoubleFold
  (^double fold [^clojure.lang.IFn$DDD f ^double init]))

(definterface ILongFold
  (^long fold [^clojure.lang.IFn$LLL f ^long init]))

(definterface IDoubleObjectFold
  (foldObject [^clojure.lang.IFn$ODO f init]))

(definterface ILongObjectFold
  (foldObject [^clojure.lang.IFn$OLO f init]))

(definterface IDoubleMap
  (dmap [^clojure.lang.IFn$DD f]))

(definterface IDoubleMapIndexed
  (dmapIndexed [^clojure.lang.IFn$LDD f]))

(definterface ILongMap
  (lmap [^clojure.lang.IFn$LL f]))

(definterface ILongMapIndexed
  (lmapIndexed [^clojure.lang.IFn$LLL f]))

(definterface IDoubleAny
  (^boolean dany [^clojure.lang.IFn$DO f]))

(definterface ILongAny
  (^boolean lany [^clojure.lang.IFn$LO f]))

(definterface IArrayEquals
  (^boolean arrayEquals [expected]))

(definterface IIndexed
  (^double getDouble [^long index])
  (^long getLong [^long index])
  (getObject [^long index]))

(definterface IIndexedSet
  (^double setDouble [^long index ^double v])
  (^long setLong [^long index ^long v])
  (setObject [^long index value]))

(definterface ISortable
  (sorted []))

(definterface IDoubleFoldSkip
  (^double foldSkip [^long skip-idx ^clojure.lang.IFn$DDD f ^double init]))

(definterface IDoubleObjectFoldSkip
  (foldObjectSkip [^long skip-idx ^clojure.lang.IFn$ODO f init]))

(definterface IArrayOps
  (^double sum [^criterium.array.DoubleArray arr])
  (^long sum [^criterium.array.LongArray arr])
  (^long getAt [^criterium.array.LongArray arr ^long index])
  (^double getAt [^criterium.array.DoubleArray arr ^long index])
  (^long setAt [^criterium.array.LongArray arr ^long index ^long value])
  (^double setAt [^criterium.array.DoubleArray arr ^long index ^double value]))

(definterface IFilterIndices
  (filterIndices [exclude-set]))

(definterface IIndexedDoubleFold
  (^double indexedFold [^clojure.lang.IFn$DLDD f ^double init]))

(definterface IIndexedDoubleObjectFold
  (indexedFoldObject [^clojure.lang.IFn$OLDO f init]))
