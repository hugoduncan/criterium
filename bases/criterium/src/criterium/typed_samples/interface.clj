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

(definterface IDoubleObjectFold
  (foldObject [^clojure.lang.IFn$ODO f init]))

(definterface ILongObjectFold
  (foldObject [^clojure.lang.IFn$OLO f init]))

(definterface ISampleOps
  (^double sum [^criterium.typed_samples.DoubleSamples samples])
  (^long sum [^criterium.typed_samples.LongSamples samples])
  (^long getAt [^criterium.typed_samples.LongSamples samples ^long index])
  (^double getAt [^criterium.typed_samples.DoubleSamples samples ^long index]))
