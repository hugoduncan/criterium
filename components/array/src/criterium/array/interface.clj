(ns criterium.array.interface
  "Interfaces for typed array collections with primitive support.

  These interfaces define the contract for typed array wrappers that
  avoid boxing overhead when working with primitive arrays.

  Interface hierarchy:
  - ITypedArray: basic array metadata (element type, length)
  - IFold: generic object-returning fold
  - IDoubleFold/ILongFold: primitive-in, primitive-out folds
  - IDoubleObjectFold/ILongObjectFold: primitive-in, object-out folds
  - IIndexed: indexed access to elements
  - IArrayOps: type-specific operations (sum, getAt)")

(definterface ITypedArray
  (^clojure.lang.Keyword elemType [])
  (^long length []))

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

(definterface IIndexed
  (^double getDouble [^long index])
  (^long getLong [^long index])
  (getObject [^long index]))

(definterface IArrayOps
  (^double sum [^criterium.array.DoubleArray arr])
  (^long sum [^criterium.array.LongArray arr])
  (^long getAt [^criterium.array.LongArray arr ^long index])
  (^double getAt [^criterium.array.DoubleArray arr ^long index]))
