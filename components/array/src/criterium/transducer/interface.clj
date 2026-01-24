(ns criterium.transducer.interface
  (:require
   [criterium.array.interface]))

(definterface ILLLReducible
  (^long reduce [^clojure.lang.IFn$LLL f ^long init]))

(definterface IDDDReducible
  (^double reduce [^clojure.lang.IFn$DDD f ^double init]))

(definterface IOLOReducible
  (^criterium.array.interface.ILongArray reduceLong
   [^clojure.lang.IFn$OLO f ^criterium.array.interface.ILongArray init]))

(definterface IODOReducible
  (^criterium.array.interface.IDoubleArray reduceDouble
   [^clojure.lang.IFn$ODO f ^criterium.array.interface.IDoubleArray init]))

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
  (^criterium.array.interface.ILongArray transduce
   [xform
    rf
    ^criterium.array.interface.ILongArray init
    ^criterium.transducer.interface.IOLOReducible source])
  (^criterium.array.interface.IDoubleArray transduce
   [xform
    rf
    ^criterium.array.interface.IDoubleArray init
    ^criterium.transducer.interface.IODOReducible source])
  (^long reduce
   [rf
    ^long init
    ^criterium.transducer.interface.ILLLReducible source])
  (^double reduce
   [rf
    ^double init
    ^criterium.transducer.interface.IDDDReducible source])

  (^criterium.array.interface.ILongArray into
   [^criterium.array.interface.ILongArray target
    xform
    ^criterium.transducer.interface.IOLOReducible source])

  (^criterium.array.interface.IDoubleArray into
   [^criterium.array.interface.IDoubleArray target
    xform
    ^criterium.transducer.interface.IODOReducible source]))
