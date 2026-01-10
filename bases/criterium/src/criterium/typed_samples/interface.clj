(ns criterium.typed-samples.interface
  "Interfaces for typed sample collections with primitive support.")

(definterface ITypedSamples
  (^clojure.lang.Keyword elemType [])
  (^long sampleCount []))

(definterface IFold
  (fold [f init]))

(definterface IDoubleFold
  (^double fold [^clojure.lang.IFn$DDD f ^double init]))

(definterface ILongFold
  (^long fold [^clojure.lang.IFn$LLL f ^long init]))
