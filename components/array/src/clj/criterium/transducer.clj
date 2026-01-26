(ns criterium.transducer
  (:refer-clojure
   :exclude [filter into map reduce range transduce])
  (:require
   [criterium.transducer.impl :as impl]
   [criterium.transducer.interfaces]))

(defn prim-set-at
  "Create a reducing function that sets array elements sequentially."
  ^criterium.transducer.impl.ArraySetter []
  (impl/prim-set-at))

(def prim-sum
  "Reducing function that sums primitive values."
  (reify
    clojure.lang.IFn$LLL
    (^long invokePrim [_ ^long acc ^long x] (unchecked-add acc x))
    clojure.lang.IFn$DDD
    (^double invokePrim [_ ^double acc ^double x] (+ acc x))))

(defn map
  "Return a transducer that applies f to each primitive element.

  Supports same-type reductions (long→long, double→double) and cross-type
  reductions (long→double, double→long) when the mapping function
  produces the appropriate output type.

  For cross-type reductions:
  - long source → double accumulator: f must be IFn$LD (long→double)
  - double source → long accumulator: f must be IFn$DL (double→long)"
  ^clojure.lang.IFn [f]
  (fn prim-map-impl [rf]
    (reify
      ;; Same-type: long → long
      clojure.lang.IFn$LLL
      (^long invokePrim [_ ^long acc ^long x]
        (.invokePrim ^clojure.lang.IFn$LLL rf acc
                     (.invokePrim ^clojure.lang.IFn$LL f x)))
      ;; Same-type: double → double
      clojure.lang.IFn$DDD
      (^double invokePrim [_ ^double acc ^double x]
        (.invokePrim ^clojure.lang.IFn$DDD rf acc
                     (.invokePrim ^clojure.lang.IFn$DD f x)))
      ;; Object accumulator with long element
      clojure.lang.IFn$OLO
      (invokePrim [_ acc ^long x]
        (.invokePrim ^clojure.lang.IFn$OLO rf acc
                     (.invokePrim ^clojure.lang.IFn$LL f x)))
      ;; Object accumulator with double element
      clojure.lang.IFn$ODO
      (invokePrim [_ acc ^double x]
        (.invokePrim ^clojure.lang.IFn$ODO rf acc
                     (.invokePrim ^clojure.lang.IFn$DD f x)))
      ;; Cross-type: long source → double accumulator
      ;; map fn: IFn$LD (long → double), rf: IFn$DDD
      clojure.lang.IFn$DLD
      (^double invokePrim [_ ^double acc ^long x]
        (.invokePrim ^clojure.lang.IFn$DDD rf acc
                     (.invokePrim ^clojure.lang.IFn$LD f x)))
      ;; Cross-type: double source → long accumulator
      ;; map fn: IFn$DL (double → long), rf: IFn$LLL
      clojure.lang.IFn$LDL
      (^long invokePrim [_ ^long acc ^double x]
        (.invokePrim ^clojure.lang.IFn$LLL rf acc
                     (.invokePrim ^clojure.lang.IFn$DL f x))))))

(defn filter
  "Return a transducer that filters primitive elements with pred."
  ^clojure.lang.IFn [pred]
  (fn prim-filter-impl [rf]
    (reify
      clojure.lang.IFn$LLL
      (^long invokePrim [_ ^long acc ^long x]
        (if (.invokePrim ^clojure.lang.IFn$LO pred x)
          (.invokePrim ^clojure.lang.IFn$LLL rf acc x)
          acc))
      clojure.lang.IFn$DDD
      (^double invokePrim [_ ^double acc ^double x]
        (if (.invokePrim ^clojure.lang.IFn$DO pred x)
          (.invokePrim ^clojure.lang.IFn$DDD rf acc x)
          acc))
      clojure.lang.IFn$OLO
      (invokePrim [_ acc ^long x]
        (if (.invokePrim ^clojure.lang.IFn$LO pred x)
          (.invokePrim ^clojure.lang.IFn$OLO rf acc x)
          acc))
      clojure.lang.IFn$ODO
      (invokePrim [_ acc ^double x]
        (if (.invokePrim ^clojure.lang.IFn$DO pred x)
          (.invokePrim ^clojure.lang.IFn$ODO rf acc x)
          acc)))))

(defn cross-map
  "Return a transducer for cross-type reductions with object accumulators.

  Use when the map function converts between primitive types and the
  accumulator is an object (array, collection, etc.).

  For double source → long element → object accumulator:
    f must be IFn$DL (double → long)
    rf must be IFn$OLO (Object, long → Object)
    Returns ODO transducer wrapper

  For long source → double element → object accumulator:
    f must be IFn$LD (long → double)
    rf must be IFn$ODO (Object, double → Object)
    Returns OLO transducer wrapper"
  ^clojure.lang.IFn [f]
  (fn cross-map-impl [rf]
    (reify
      ;; double source → long element → object accumulator
      ;; Receives double from source, converts via DL, calls OLO rf
      clojure.lang.IFn$ODO
      (invokePrim [_ acc ^double x]
        (.invokePrim ^clojure.lang.IFn$OLO rf acc
                     (.invokePrim ^clojure.lang.IFn$DL f x)))
      ;; long source → double element → object accumulator
      ;; Receives long from source, converts via LD, calls ODO rf
      clojure.lang.IFn$OLO
      (invokePrim [_ acc ^long x]
        (.invokePrim ^clojure.lang.IFn$ODO rf acc
                     (.invokePrim ^clojure.lang.IFn$LD f x))))))

(defmacro transduce
  [xform rf init source]
  `(.transduce
    ~(vary-meta
      'criterium.transducer.impl/ops
      assoc :tag 'criterium.transducer.interfaces.IPrimOps)
    ~xform ~rf ~init ~source))

(defmacro reduce
  [rf init source]
  `(.reduce
    ~(vary-meta
      'criterium.transducer.impl/ops
      assoc :tag 'criterium.transducer.interfaces.IPrimOps)
    ~rf ~init ~source))

(defmacro into
  [target rf source]
  `(.into
    ~(vary-meta
      'criterium.transducer.impl/ops
      assoc :tag 'criterium.transducer.interfaces.IPrimOps)
    ~target ~rf ~source))

(defmacro range
  ([start end]
   `(.range
     ~(vary-meta
       'criterium.transducer.impl/ops
       assoc :tag 'criterium.transducer.interfaces.IPrimOps)
     ~start ~end))
  ([start end step]
   `(.range
     ~(vary-meta
       'criterium.transducer.impl/ops
       assoc :tag 'criterium.transducer.interfaces.IPrimOps)
     ~start ~end ~step)))
