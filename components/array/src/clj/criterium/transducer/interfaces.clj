(ns criterium.transducer.interfaces
  "Interfaces for primitive transducer operations.

  Interface hierarchy:
  - ILLLReducible: reduce with primitive long accumulator
  - IDDDReducible: reduce with primitive double accumulator
  - IOLOReducible: reduce long elements into array
  - IODOReducible: reduce double elements into array
  - ILongReducible: extends ILLLReducible, IOLOReducible
  - IDoubleReducible: extends IDDDReducible, IODOReducible
  - IPrimOps: transduce/reduce/into/range operations"
  (:require
   [criterium.array.interfaces]
   [criterium.array.util :refer [definterface+]]))

;;; Base reducible interfaces (no inheritance)

(definterface+ IDDDReducible
  (^double reduce [^clojure.lang.IFn$DDD f ^double init]))

(definterface+ ILLLReducible
  (^long reduce [^clojure.lang.IFn$LLL f ^long init]))

(definterface+ IDLDReducible
  (^double reduce [^clojure.lang.IFn$DLD f ^double init]))

(definterface+ ILDLReducible
  (^long reduce [^clojure.lang.IFn$LDL f ^long init]))

;;; Array-returning reducible interfaces

(definterface+ IODOReducible
  (^criterium.array.interfaces.IDoubleArray reduceDouble
   [^clojure.lang.IFn$ODO f ^criterium.array.interfaces.IDoubleArray init]))

(definterface+ IOLOReducible
  (^criterium.array.interfaces.ILongArray reduceLong
   [^clojure.lang.IFn$OLO f ^criterium.array.interfaces.ILongArray init]))

;;; Cross-type reducible interfaces

(definterface+ IODLOReducible
  (reduceDouble [^clojure.lang.IFn$ODO f init]))

(definterface+ IOLDOReducible
  (reduceLong [^clojure.lang.IFn$OLO f init]))

;;; Composite reducible interfaces

(definterface+ IDoubleReducible [IDDDReducible IODOReducible])

(definterface+ ILongReducible [ILLLReducible IOLOReducible])

;;; Primitive operations interface

(definterface+ IPrimOps
  ;; transduce with long accumulator
  (^long transduce
   [xform rf ^long init ^criterium.transducer.interfaces.ILLLReducible source])
  ;; transduce with double accumulator
  (^double transduce
   [xform
    rf
    ^double init
    ^criterium.transducer.interfaces.IDDDReducible source])
  ;; transduce with long accumulator from double source
  (^long transduce
   [xform rf ^long init ^criterium.transducer.interfaces.ILDLReducible source])
  ;; transduce with double accumulator from long source
  (^double transduce
   [xform
    rf
    ^double init
    ^criterium.transducer.interfaces.IDLDReducible source])
  ;; transduce into long array
  (^criterium.array.interfaces.ILongArray transduce
   [xform
    rf
    ^criterium.array.interfaces.ILongArray init
    ^criterium.transducer.interfaces.IOLOReducible source])
  ;; transduce into double array
  (^criterium.array.interfaces.IDoubleArray transduce
   [xform
    rf
    ^criterium.array.interfaces.IDoubleArray init
    ^criterium.transducer.interfaces.IODOReducible source])
  ;; transduce cross-type: long→double
  (transduce
   [xform rf init ^criterium.transducer.interfaces.IOLDOReducible source])
  ;; transduce cross-type: double→long
  (transduce
   [xform rf init ^criterium.transducer.interfaces.IODLOReducible source])

  ;; reduce operations
  (^long reduce
   [rf ^long init ^criterium.transducer.interfaces.ILLLReducible source])
  (^double reduce
   [rf ^double init ^criterium.transducer.interfaces.IDDDReducible source])
  ;; reduce cross-type: double→object
  (reduce [rf init ^criterium.transducer.interfaces.IODLOReducible source])
  ;; reduce cross-type: long→object
  (reduce [rf init ^criterium.transducer.interfaces.IOLDOReducible source])

  ;; into operations
  (^criterium.array.interfaces.ILongArray into
   [^criterium.array.interfaces.ILongArray target
    xform
    ^criterium.transducer.interfaces.IOLOReducible source])
  (^criterium.array.interfaces.IDoubleArray into
   [^criterium.array.interfaces.IDoubleArray target
    xform
    ^criterium.transducer.interfaces.IODOReducible source])

  ;; range operations
  (^criterium.transducer.interfaces.ILongReducible range
   [^long start ^long end])
  (^criterium.transducer.interfaces.IDoubleReducible range
   [^double start ^double end ^double step])

  ;; fill operations
  (^criterium.array.interfaces.IDoubleFill fill
   [^criterium.array.interfaces.IDoubleFill arr ^double value])
  (^criterium.array.interfaces.ILongFill fill
   [^criterium.array.interfaces.ILongFill arr ^long value])
  (^criterium.array.interfaces.IObjectFill fill
   [^criterium.array.interfaces.IObjectFill arr value]))
