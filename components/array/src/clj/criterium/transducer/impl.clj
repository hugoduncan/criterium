(ns criterium.transducer.impl
  "Implementation types for primitive transducers."
  (:require
   [criterium.array :as arr]
   [criterium.transducer.interfaces])
  (:import
   [criterium.array
    DoubleArray
    LongArray]
   [criterium.array.interfaces
    IDoubleFill
    ILongFill
    IObjectFill]
   [criterium.transducer.interfaces
    IDDDReducible
    IDLDReducible
    IDoubleReducible
    ILDLReducible
    ILLLReducible
    ILongReducible
    IODLOReducible
    IODOReducible
    IOLDOReducible
    IOLOReducible
    IPrimOps]))

(deftype ArraySetter
         [^:unsynchronized-mutable ^long idx]
  clojure.lang.IFn$OLO
  (invokePrim [_ acc ^long x]
    (arr/set-at! ^LongArray acc idx x)
    (set! idx (unchecked-inc idx))
    acc)
  clojure.lang.IFn$ODO
  (invokePrim [_ acc ^double x]
    (arr/set-at! ^DoubleArray acc idx x)
    (set! idx (unchecked-inc idx))
    acc))

(defn prim-set-at
  ^ArraySetter []
  (ArraySetter. 0))

;;; Sources

(deftype LongRange [^long start ^long end]
  ILongReducible
  ILLLReducible
  (reduce [_ f init]
    (loop [i start, acc init]
      (if (< i end)
        (recur (unchecked-inc i)
               (.invokePrim ^clojure.lang.IFn$LLL f acc i))
        acc)))
  IOLOReducible
  (reduceLong [_ f init]
    (loop [i start, acc init]
      (if (< i end)
        (recur (unchecked-inc i)
               (.invokePrim ^clojure.lang.IFn$OLO f acc i))
        acc))))

(deftype DoubleRange
         [^double start ^double end ^double step]
  IDoubleReducible
  IDDDReducible
  (reduce [_ f init]
    (loop [x start, acc init]
      (if (< x end)
        (recur (+ x step)
               (.invokePrim ^clojure.lang.IFn$DDD f acc x))
        acc)))
  IODOReducible
  (reduceDouble
    [_ f init]
    (loop [x start, acc init]
      (if (< x end)
        (recur (+ x step)
               (.invokePrim ^clojure.lang.IFn$ODO f acc x))
        acc))))

(def ^IPrimOps ops
  (reify IPrimOps
    (^long transduce [_ xform rf ^long init ^ILLLReducible source]
      (.reduce source ^clojure.lang.IFn$LLL (xform rf) init))
    (^double transduce [_ xform rf ^double init ^IDDDReducible source]
      (.reduce source ^clojure.lang.IFn$DDD (xform rf) init))
    (^double transduce [_ xform rf ^double init ^IDLDReducible source]
      (.reduce source ^clojure.lang.IFn$DLD (xform rf) init))
    (^long transduce [_ xform rf ^long init ^ILDLReducible source]
      (.reduce source ^clojure.lang.IFn$LDL (xform rf) init))
    (^criterium.array.interfaces.ILongArray transduce
      [_
       xform
       rf
       ^criterium.array.interfaces.ILongArray init
       ^IOLOReducible source]
      (.reduceLong source ^clojure.lang.IFn$OLO (xform rf) init))
    (^criterium.array.interfaces.IDoubleArray transduce
      [_
       xform
       rf
       ^criterium.array.interfaces.IDoubleArray init
       ^IODOReducible source]
      (.reduceDouble source ^clojure.lang.IFn$OLO (xform rf) init))
    (^Object transduce
      [_ xform rf init ^IODLOReducible source]
      (.reduceDouble source ^clojure.lang.IFn$ODO (xform rf) init))
    (^Object transduce
      [_ xform rf init ^IOLDOReducible source]
      (.reduceLong source ^clojure.lang.IFn$OLO (xform rf) init))
    (^long reduce [_ rf ^long init ^ILLLReducible source]
      (.reduce source ^clojure.lang.IFn$LLL rf init))
    (^double reduce [_ rf ^double init ^IDDDReducible source]
      (.reduce source ^clojure.lang.IFn$DDD rf init))
    (^criterium.array.interfaces.ILongArray into
      [this
       ^criterium.array.interfaces.ILongArray target
       xform
       ^IOLOReducible source]
      (.transduce this xform (prim-set-at) target source))
    (^criterium.array.interfaces.IDoubleArray into
      [this
       ^criterium.array.interfaces.IDoubleArray target
       xform
       ^IODOReducible source]
      (.transduce this xform (prim-set-at) target source))

    (^criterium.transducer.interfaces.ILongReducible range
      [_ ^long start ^long end]
      (LongRange. start end))

    (^criterium.transducer.interfaces.IDoubleReducible range
      [_ ^double start ^double end ^double step]
      (DoubleRange. start end step))

    (^IDoubleFill fill [_ ^IDoubleFill arr ^double value]
      (.dfill arr value))

    (^ILongFill fill [_ ^ILongFill arr ^long value]
      (.lfill arr value))

    (^IObjectFill fill [_ ^IObjectFill arr value]
      (.ofill arr value))))
