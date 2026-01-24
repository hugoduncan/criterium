(ns criterium.transducer.interface
  (:require
   [criterium.array])
  (:import
   [criterium.array
    DoubleArray
    LongArray]))

(definterface ILLLReducible
  (^long reduce [^clojure.lang.IFn$LLL f ^long init]))

(definterface IDDDReducible
  (^double reduce [^clojure.lang.IFn$DDD f ^double init]))

(definterface IOLOReducible
  (reduceLong [^clojure.lang.IFn$OLO f init]))

(definterface IODOReducible
  (^criterium.array.DoubleArray reduceDouble
   [^clojure.lang.IFn$ODO f ^criterium.array.DoubleArray init]))

(definterface ILongBuilder
  (^ILongBuilder acceptLong [^long x])
  (^criterium.array.LongArray build []))

(definterface IDoubleBuilder
  (^IDoubleBuilder acceptDouble [^double x])
  (^criterium.array.DoubleArray build []))

(definterface IPrimOps
  (^long transduce
   [xform
    rf
    ^long init
    ^criterium.transducer.interface.ILLLReducible source])
  (^double transduce
   [xform
    rf
    ^double init
    ^criterium.transducer.interface.IDDDReducible source])
  (^long reduce
   [rf
    ^long init
    ^criterium.transducer.interface.ILLLReducible source])
  (^double reduce
   [rf
    ^double init
    ^criterium.transducer.interface.IDDDReducible source])

  (^criterium.array.LongArray into
   [^criterium.array.LongArray target
    xform
    ^criterium.transducer.interface.IOLOReducible source])
  (^criterium.array.DoubleArray into
   [^criterium.array.DoubleArray target
    xform
    ^criterium.transducer.interface.IODOReducible source]))
