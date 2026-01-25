(ns criterium.transducer
  (:refer-clojure
   :exclude [filter into map reduce range transduce])
  (:require
   [criterium.transducer.impl :as impl]
   [criterium.transducer.interface]))

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
  "Return a transducer that applies f to each primitive element."
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

(defmacro transduce
  [xform rf init source]
  `(.transduce
    ~(vary-meta
      'criterium.transducer.impl/ops
      assoc :tag 'criterium.transducer.interface.IPrimOps)
    ~xform ~rf ~init ~source))

(defmacro reduce
  [rf init source]
  `(.reduce
    ~(vary-meta
      'criterium.transducer.impl/ops
      assoc :tag 'criterium.transducer.interface.IPrimOps)
    ~rf ~init ~source))

(defmacro into
  [target rf source]
  `(.into
    ~(vary-meta
      'criterium.transducer.impl/ops
      assoc :tag 'criterium.transducer.interface.IPrimOps)
    ~target ~rf ~source))

(defmacro range
  ([start end]
   `(.range
     ~(vary-meta
       'criterium.transducer.impl/ops
       assoc :tag 'criterium.transducer.interface.IPrimOps)
     ~start ~end))
  ([start end step]
   `(.range
     ~(vary-meta
       'criterium.transducer.impl/ops
       assoc :tag 'criterium.transducer.interface.IPrimOps)
     ~start ~end ~step)))
