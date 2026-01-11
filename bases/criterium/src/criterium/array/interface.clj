(ns criterium.array.interface
  "Interfaces for typed array collections with primitive support.")

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

(definterface IArrayOps
  (^double sum [^criterium.array.DoubleArray arr])
  (^long sum [^criterium.array.LongArray arr])
  (^long getAt [^criterium.array.LongArray arr ^long index])
  (^double getAt [^criterium.array.DoubleArray arr ^long index]))
