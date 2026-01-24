(ns criterium.transducer
  (:refer-clojure
   :exclude [reduce])
  (:require
   [criterium.array :as arr]
   [criterium.transducer.interface])
  (:import
   [criterium.array
    DoubleArray
    LongArray]
   [criterium.transducer.interface
    IDDDReducible
    IDoubleBuilder
    ILLLReducible
    ILongBuilder
    IODOReducible
    IOLOReducible
    IPrimOps]))

;; (deftype LongArrayBuilder
;;   [^:unsynchronized-mutable ^longs arr
;;    ^:unsynchronized-mutable ^long idx]
;;   ILongBuilder
;;   (acceptLong [this x]
;;     (when (>= idx (alength arr))
;;       (let [new-arr (long-array (* 2 (alength arr)))]
;;         (System/arraycopy arr 0 new-arr 0 (alength arr))
;;         (set! arr new-arr)))
;;     (aset arr idx x)
;;     (set! idx (unchecked-inc idx))
;;     this)
;;   (build [_]
;;     (let [result (long-array idx)]
;;       (System/arraycopy arr 0 result 0 idx)
;;       (arr/->LongArray result))))

;; (deftype DoubleArrayBuilder
;;   [^:unsynchronized-mutable ^doubles arr
;;    ^:unsynchronized-mutable ^long idx]
;;   IDoubleBuilder
;;   (acceptDouble [this x]
;;     (when (>= idx (alength arr))
;;       (let [new-arr (double-array (* 2 (alength arr)))]
;;         (System/arraycopy arr 0 new-arr 0 (alength arr))
;;         (set! arr new-arr)))
;;     (aset arr idx x)
;;     (set! idx (unchecked-inc idx))
;;     this)
;;   (build [_]
;;     (let [result (double-array idx)]
;;       (System/arraycopy arr 0 result 0 idx)
;;       (DoubleArray. result))))

;; (defn long-builder [] (LongArrayBuilder. (long-array 16) 0))
;; (defn double-builder [] (DoubleArrayBuilder. (double-array 16) 0))

(def ^:private ^criterium.transducer.interface.IPrimOps ops
  (reify IPrimOps
    (^long transduce [_ xform rf ^long init ^ILLLReducible source]
      (.reduce source ^clojure.lang.IFn$LLL (xform rf) init))
    (^double transduce [_ xform rf ^double init ^IDDDReducible source]
      (.reduce source ^clojure.lang.IFn$DDD (xform rf) init))
    (^long reduce [_ rf ^long init ^ILLLReducible source]
      (.reduce source ^clojure.lang.IFn$LLL rf init))
    (^double reduce [_ rf ^double init ^IDDDReducible source]
      (.reduce source ^clojure.lang.IFn$DDD rf init))
    (^LongArray into
      [_ ^LongArray target xform ^IOLOReducible source]
      (.reduceLong source xform target))
    (^DoubleArray into
      [_ ^DoubleArray target xform ^IODOReducible source]
      (.reduceDouble source xform target))))

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
  (^criterium.array.DoubleArray reduceDouble
    [_ f ^criterium.array.DoubleArray acc]
    (loop [i 0
           x start]
      (if (< x end)
        (do
          (arr/set! acc i (.invokePrim ^clojure.lang.IFn$DDD f acc x))
          (recur
           (unchecked-inc i)
           (+ x step)))
        acc))))

(defn prim-map
  [f]
  (fn [rf]
    (reify
      clojure.lang.IFn$LLL
      (^long invokePrim [_ ^long acc ^long x]
        (.invokePrim ^clojure.lang.IFn$LLL rf acc
                     (.invokePrim ^clojure.lang.IFn$LL f x)))
      clojure.lang.IFn$DDD
      (^double invokePrim [_ ^double acc ^double x]
        (.invokePrim ^clojure.lang.IFn$DDD rf acc
                     (.invokePrim ^clojure.lang.IFn$DD f x)))
      ;; clojure.lang.IFn$OLO
      ;; (invokePrim [_ acc ^long x]
      ;;   (.invokePrim ^clojure.lang.IFn$OLO rf acc
      ;;                (.invokePrim ^clojure.lang.IFn$LL f x)))
      ;; clojure.lang.IFn$ODO
      ;; (invokePrim [_ acc ^double x]
      ;;   (.invokePrim ^clojure.lang.IFn$ODO rf acc
      ;;                (.invokePrim ^clojure.lang.IFn$DD f x)))
      )))

(def prim-sum
  (reify
    clojure.lang.IFn$LLL
    (^long invokePrim [_ ^long acc ^long x] (unchecked-add acc x))
    clojure.lang.IFn$DDD
    (^double invokePrim [_ ^double acc ^double x] (+ acc x))))

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

(.into ops
       (arr/->DoubleArray (double-array 3))
       (prim-map (fn ^double [^double x] (Math/sqrt x)))
       (DoubleRange. 1.0 5.0 1.0))

(defmacro reduce
  [rf init source]
  `(.reduce ~'criterium.transducer/ops ~rf ~init ~source))

(reduce prim-sum 0 (LongRange. 0 1000000))
(reduce prim-sum 0.0 (DoubleRange. 0.0 1000000.0 1.0))
