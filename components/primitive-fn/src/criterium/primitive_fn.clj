(ns criterium.primitive-fn
  "Primitive-typed versions of core Clojure functions.

  These functions are properly type-hinted to work with .invokePrim,
  avoiding boxing overhead when passed to higher-order primitive
  functions like fold-double, fold-long, dmap, and lmap.

  Use these instead of inline lambdas when the operation matches
  a standard function (add, min, max, etc.).

  Naming convention: d-prefix for double, l-prefix for long."
  (:require
   [criterium.primitive-fn.interface])
  (:import
   [criterium.primitive_fn.interface
    DB DDB
    LB LLB]))

;;; Double arithmetic

(defn dadd
  "Primitive double addition."
  ^double [^double a ^double b]
  (+ a b))

(defn dadd-unchecked
  "Unchecked primitive double addition."
  ^double [^double a ^double b]
  (unchecked-add a b))

(defn dsubtract
  "Primitive double subtraction."
  ^double [^double a ^double b]
  (- a b))

(defn dmultiply
  "Primitive double multiplication."
  ^double [^double a ^double b]
  (* a b))

(defn ddivide
  "Primitive double division."
  ^double [^double a ^double b]
  (/ a b))

(defn dmin
  "Primitive double minimum."
  ^double [^double a ^double b]
  (Math/min a b))

(defn dmax
  "Primitive double maximum."
  ^double [^double a ^double b]
  (Math/max a b))

;;; Long arithmetic

(defn ladd
  "Primitive long addition."
  ^long [^long a ^long b]
  (+ a b))

(defn lsubtract
  "Primitive long subtraction."
  ^long [^long a ^long b]
  (- a b))

(defn lmultiply
  "Primitive long multiplication."
  ^long [^long a ^long b]
  (* a b))

(defn ldivide
  "Primitive long division (integer division)."
  ^long [^long a ^long b]
  (quot a b))

(defn lmin
  "Primitive long minimum."
  ^long [^long a ^long b]
  (Math/min a b))

(defn lmax
  "Primitive long maximum."
  ^long [^long a ^long b]
  (Math/max a b))

(defmacro invoke-dd
  "Invoke a primitive double -> double function."
  [f v]
  `(.invokePrim ~(vary-meta f assoc :tag 'clojure.lang.IFn$DD) ~v))

(defmacro bfn
  "Create a primitive boolean function.

   Like fn, but implements both IFn and the appropriate primitive interface
   to support .invokePrim calls without boxing.

   Supports one or two arguments:
   - 1 arg: DB for double, LB for long
   - 2 args: DDB for (double, double), LLB for (long, long)

   All arguments must be hinted with ^double or ^long (and must match
   for 2-arg forms).

   Example:
     (bfn [^double x] (> x 0.0))
     (bfn [^long n] (pos? n))
     (bfn [^double a ^double b] (< a b))
     (bfn [^long a ^long b] (< a b))"
  [args & body]
  (case (count args)
    1 (let [[arg] args
            tag   (-> arg meta :tag)]
        (case tag
          double `(reify
                    clojure.lang.IFn
                    (invoke [_# v#] (let [~arg v#] ~@body))
                    DB
                    (~'invokePrim [_# ~(vary-meta arg dissoc :tag)] ~@body))
          long   `(reify
                    clojure.lang.IFn
                    (invoke [_# v#] (let [~arg v#] ~@body))
                    LB
                    (~'invokePrim [_# ~(vary-meta arg dissoc :tag)] ~@body))
          (throw (ex-info "bfn requires ^double or ^long hint on argument"
                          {:arg arg :tag tag}))))
    2 (let [[arg1 arg2] args
            tag1        (-> arg1 meta :tag)
            tag2        (-> arg2 meta :tag)]
        (when (not= tag1 tag2)
          (throw (ex-info "bfn requires matching type hints on both arguments"
                          {:arg1 arg1 :tag1 tag1 :arg2 arg2 :tag2 tag2})))
        (case tag1
          double `(reify
                    clojure.lang.IFn
                    (invoke [_# v1# v2#]
                      (let [~arg1 v1# ~arg2 v2#] ~@body))
                    DDB
                    (~'invokePrim [_#
                                   ~(vary-meta arg1 dissoc :tag)
                                   ~(vary-meta arg2 dissoc :tag)]
                      ~@body))
          long   `(reify
                    clojure.lang.IFn
                    (invoke [_# v1# v2#]
                      (let [~arg1 v1# ~arg2 v2#] ~@body))
                    LLB
                    (~'invokePrim [_#
                                   ~(vary-meta arg1 dissoc :tag)
                                   ~(vary-meta arg2 dissoc :tag)]
                      ~@body))
          (throw (ex-info "bfn requires ^double or ^long hint on arguments"
                          {:arg1 arg1 :tag1 tag1}))))
    (throw (ex-info "bfn requires 1 or 2 arguments"
                    {:args args :count (count args)}))))

(defmacro defbfn
  "Define a named primitive boolean function.

   Like defn, but the defined function implements both IFn and the
   appropriate primitive interface to support .invokePrim calls
   without boxing.

   Supports one or two arguments:
   - 1 arg: DB for double, LB for long
   - 2 args: DDB for (double, double), LLB for (long, long)

   All arguments must be hinted with ^double or ^long (and must match
   for 2-arg forms).

   Example:
     (defbfn positive-double?
       \"Check if double is positive.\"
       [^double x]
       (> x 0.0))

     (defbfn positive-long?
       [^long n]
       (pos? n))

     (defbfn d<?
       \"Primitive double less-than comparison.\"
       [^double a ^double b]
       (< a b))"
  {:arglists '([name docstring? [arg] & body]
               [name docstring? [arg1 arg2] & body])}
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (rest args)]
                           [nil args])
        [params & body]  args]
    `(def ~(if docstring
             (vary-meta name assoc :doc docstring)
             name)
       (bfn ~params ~@body))))

;;; Double predicates

(defbfn dpos?
  "Primitive double positive check."
  [^double v]
  (pos? v))

(defbfn dneg?
  "Primitive double negative check."
  [^double v]
  (neg? v))

(defbfn dzero?
  "Primitive double zero check."
  [^double v]
  (zero? v))

;;; Long predicates

(defbfn lpos?
  "Primitive long positive check."
  [^long v]
  (pos? v))

(defbfn lneg?
  "Primitive long negative check."
  [^long v]
  (neg? v))

(defbfn lzero?
  "Primitive long zero check."
  [^long v]
  (zero? v))

(defbfn lodd?
  "Primitive double zero check."
  [^long v]
  (odd? v))

(defbfn leven?
  "Primitive double zero check."
  [^long v]
  (even? v))
