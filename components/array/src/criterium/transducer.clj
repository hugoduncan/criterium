(ns criterium.transducer
  (:refer-clojure
   :exclude [into reduce transduce])
  (:require
   [criterium.array :as arr]
   [criterium.transducer.interface])
  (:import
   [criterium.array
    DoubleArray
    LongArray]
   [criterium.transducer.interface
    IDDDReducible
    ILLLReducible
    IODOReducible
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

(def ^:private ^IPrimOps ops
  (reify IPrimOps
    (^long transduce [_ xform rf ^long init ^ILLLReducible source]
      (.reduce source ^clojure.lang.IFn$LLL (xform rf) init))
    (^double transduce [_ xform rf ^double init ^IDDDReducible source]
      (.reduce source ^clojure.lang.IFn$DDD (xform rf) init))
    (^criterium.array.LongArray transduce
      [_
       xform
       rf
       ^criterium.array.LongArray init
       ^IOLOReducible source]
      (.reduceLong source ^clojure.lang.IFn$OLO (xform rf) init))
    (^criterium.array.DoubleArray transduce
      [_
       xform
       rf
       ^criterium.array.DoubleArray init
       ^IODOReducible source]
      (.reduceDouble source ^clojure.lang.IFn$OLO (xform rf) init))
    (^long reduce [_ rf ^long init ^ILLLReducible source]
      (.reduce source ^clojure.lang.IFn$LLL rf init))
    (^double reduce [_ rf ^double init ^IDDDReducible source]
      (.reduce source ^clojure.lang.IFn$DDD rf init))
    (^criterium.array.LongArray into
      [this ^criterium.array.LongArray target xform ^IOLOReducible source]
      (.transduce this xform (prim-set-at) target source))
    (^criterium.array.DoubleArray into
      [this ^criterium.array.DoubleArray target xform ^IODOReducible source]
      (.transduce this xform (prim-set-at) target source))))

;; ============================================================
;; Sources
;; ============================================================

(deftype LongRange [^long start ^long end]
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

(defn prim-map
  ^clojure.lang.IFn [f]
  (fn prim-map-impl [rf]
    (reify
      clojure.lang.IFn$LLL
      (^long invokePrim [_ ^long acc ^long x]
        (.invokePrim ^clojure.lang.IFn$LLL rf acc
                     (.invokePrim ^clojure.lang.IFn$LL f x)))
      clojure.lang.IFn$DDD
      (^double invokePrim [_ ^double acc ^double x]
        (.invokePrim ^clojure.lang.IFn$DDD rf acc
                     (.invokePrim ^clojure.lang.IFn$DD f x)))
      clojure.lang.IFn$OLO
      (invokePrim [_ acc ^long x]
        (.invokePrim ^clojure.lang.IFn$OLO rf acc
                     (.invokePrim ^clojure.lang.IFn$LL f x)))
      clojure.lang.IFn$ODO
      (invokePrim [_ acc ^double x]
        (.invokePrim ^clojure.lang.IFn$ODO rf acc
                     (.invokePrim ^clojure.lang.IFn$DD f x))))))

(def prim-sum
  (reify
    clojure.lang.IFn$LLL
    (^long invokePrim [_ ^long acc ^long x] (unchecked-add acc x))
    clojure.lang.IFn$DDD
    (^double invokePrim [_ ^double acc ^double x] (+ acc x))))

(.transduce ops
            (prim-map (fn ^long [^long x] (* x x)))
            prim-sum
            0
            (LongRange. 0 1000))

;; (.transduce ops
;;             (comp (prim-filter (fn ^long [^long x] (- 1 (rem x 2))))
;;                   (prim-map (fn ^long [^long x] (* x x))))
;;             prim-sum
;;             0
;;             (LongRange. 0 1000))
;; ;; => 166166500

;; (.transduce prim
;;             (prim-map (fn ^double [^double x] (Math/sqrt x)))
;;             prim-sum
;;             0.0
;;             (DoubleRange. 1.0 100.0 1.0))
;; ;; => 661.46...

;; Simple reduce
(.reduce ops prim-sum 0 (LongRange. 0 1000000))
(.reduce ops prim-sum 0.0 (DoubleRange. 0.0 1000000.0 1.0))

(arr/get-at
 (.transduce ops
             (prim-map (fn ^long [^long x] (* x x)))
             (prim-set-at)
             (LongArray. (long-array 10))
             (LongRange. 0 10))
 3)

(arr/get-at
 (.transduce ops
             (prim-map (fn ^double [^double x] (* x x)))
             (prim-set-at)
             (DoubleArray. (double-array 10))
             (DoubleRange. 0.0 10.0 1.0))
 3)

(arr/get-at
 (.transduce ops
             (comp
              (prim-map (fn ^double [^double x] (* x x)))
              (prim-map (fn ^double [^double x] (Math/sqrt x))))
             (prim-set-at)
             (DoubleArray. (double-array 10))
             (DoubleRange. 0.0 10.0 1.0))
 3)

(.into ops
       (DoubleArray. (double-array 5))
       (prim-map (fn ^double [^double x] (Math/sqrt x)))
       (DoubleRange. 1.0 5.0 1.0))

(defmacro reduce
  [rf init source]
  `(.reduce ~'criterium.transducer/ops ~rf ~init ~source))

(reduce prim-sum 0 (LongRange. 0 1000000))
(reduce prim-sum 0.0 (DoubleRange. 0.0 1000000.0 1.0))

(defmacro into
  [target rf source]
  `(.into ~'criterium.transducer/ops ~target ~rf ~source))

(into
 (DoubleArray. (double-array 5))
 (prim-map (fn ^double [^double x] (Math/sqrt x)))
 (DoubleRange. 1.0 5.0 1.0))
