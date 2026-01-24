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
  (^criterium.array.LongArray reduceLong
   [^clojure.lang.IFn$OLO f ^criterium.array.LongArray init]))

(definterface IODOReducible
  (^criterium.array.DoubleArray reduceDouble
   [^clojure.lang.IFn$ODO f ^criterium.array.DoubleArray init]))

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
  (^criterium.array.LongArray transduce
   [xform
    rf
    ^criterium.array.LongArray init
    ^criterium.transducer.interface.IOLOReducible source])
  (^criterium.array.DoubleArray transduce
   [xform
    rf
    ^criterium.array.DoubleArray init
    ^criterium.transducer.interface.IODOReducible source])
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
