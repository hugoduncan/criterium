(ns criterium.transducer.interface
  (:require
   [criterium.array.interface]))

(definterface ILLLReducible
  (^long reduce [^clojure.lang.IFn$LLL f ^long init]))

(definterface IDDDReducible
  (^double reduce [^clojure.lang.IFn$DDD f ^double init]))

(definterface IOLOReducible
  (^criterium.array.types.ILongArray reduceLong
   [^clojure.lang.IFn$OLO f ^criterium.array.types.ILongArray init]))

(definterface IODOReducible
  (^criterium.array.types.IDoubleArray reduceDouble
   [^clojure.lang.IFn$ODO f ^criterium.array.types.IDoubleArray init]))

(definterface ILongReducible)
(definterface IDoubleReducible)

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
  (^criterium.array.types.ILongArray transduce
   [xform
    rf
    ^criterium.array.types.ILongArray init
    ^criterium.transducer.interface.IOLOReducible source])
  (^criterium.array.types.IDoubleArray transduce
   [xform
    rf
    ^criterium.array.types.IDoubleArray init
    ^criterium.transducer.interface.IODOReducible source])
  (^long reduce
   [rf
    ^long init
    ^criterium.transducer.interface.ILLLReducible source])
  (^double reduce
   [rf
    ^double init
    ^criterium.transducer.interface.IDDDReducible source])

  (^criterium.array.types.ILongArray into
   [^criterium.array.types.ILongArray target
    xform
    ^criterium.transducer.interface.IOLOReducible source])

  (^criterium.array.types.IDoubleArray into
   [^criterium.array.types.IDoubleArray target
    xform
    ^criterium.transducer.interface.IODOReducible source])

  (^criterium.transducer.interface.ILongReducible range
   [^long start ^long end])
  (^criterium.transducer.interface.IDoubleReducible range
   [^double start ^double end ^double step]))
