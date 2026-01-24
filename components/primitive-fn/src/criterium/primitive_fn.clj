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

;;; Macro definitions

(defmacro ^:private defprim-wrappers
  "Define primitive wrapper functions for a given type and arity.

   Takes the primitive type (double or long), the argument count (1 or 2),
   and a list of wrapper specifications. Each spec is a vector of
   [suffix docstring wrapped-fn].

   The wrapper name is constructed by prefixing the suffix with 'd' for
   double or 'l' for long.

   Example:
     (defprim-wrappers double 2
       [add \"Primitive double addition.\" +]
       [min \"Primitive double minimum.\" Math/min])

   Expands to:
     (defn dadd \"Primitive double addition.\" ^double [^double a ^double b] (+ a b))
     (defn dmin \"Primitive double minimum.\" ^double [^double a ^double b] (Math/min a b))"
  [prim-type arg-count & specs]
  (let [prefix   (case prim-type double "d" long "l")
        type-sym (case prim-type double 'double long 'long)
        arg-a    (with-meta 'a {:tag type-sym})
        arg-b    (with-meta 'b {:tag type-sym})
        arg-v    (with-meta 'v {:tag type-sym})]
    (cons 'do
          (for [[suffix docstring wrapped-fn] specs]
            (let [fn-name (symbol (str prefix suffix))
                  args-1  (with-meta [arg-v] {:tag type-sym})
                  args-2  (with-meta [arg-a arg-b] {:tag type-sym})]
              (case (long arg-count)
                1 (list 'defn fn-name docstring args-1
                        (list wrapped-fn arg-v))
                2 (list 'defn fn-name docstring args-2
                        (list wrapped-fn arg-a arg-b))))))))

;;; Double arithmetic

(defprim-wrappers double 2
  [add "Primitive double addition." +]
  [add-unchecked "Unchecked primitive double addition." unchecked-add]
  [subtract "Primitive double subtraction." -]
  [multiply "Primitive double multiplication." *]
  [divide "Primitive double division." /]
  [min "Primitive double minimum." Math/min]
  [max "Primitive double maximum." Math/max])

;;; Long arithmetic

(defprim-wrappers long 2
  [add "Primitive long addition." +]
  [subtract "Primitive long subtraction." -]
  [multiply "Primitive long multiplication." *]
  [divide "Primitive long division (integer division)." quot]
  [min "Primitive long minimum." Math/min]
  [max "Primitive long maximum." Math/max])

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

(defmacro ^:private defbfn-wrappers
  "Define primitive boolean wrapper functions for a given type.

   Takes the primitive type (double or long) and a list of wrapper
   specifications. Each spec is a vector of [suffix docstring wrapped-fn].

   The wrapper name is constructed by prefixing the suffix with 'd' for
   double or 'l' for long.

   Example:
     (defbfn-wrappers double
       [pos? \"Primitive double positive check.\" pos?]
       [neg? \"Primitive double negative check.\" neg?])

   Expands to:
     (defbfn dpos? \"Primitive double positive check.\" [^double v] (pos? v))
     (defbfn dneg? \"Primitive double negative check.\" [^double v] (neg? v))"
  [prim-type & specs]
  (let [prefix   (case prim-type double "d" long "l")
        type-sym (case prim-type double 'double long 'long)]
    `(do
       ~@(for [[suffix docstring wrapped-fn] specs]
           (let [fn-name (symbol (str prefix suffix))]
             `(defbfn ~fn-name
                ~docstring
                [~(with-meta 'v {:tag type-sym})]
                (~wrapped-fn ~'v)))))))

;;; Double predicates

(defbfn-wrappers double
  [pos? "Primitive double positive check." pos?]
  [neg? "Primitive double negative check." neg?]
  [zero? "Primitive double zero check." zero?])

;;; Long predicates

(defbfn-wrappers long
  [pos? "Primitive long positive check." pos?]
  [neg? "Primitive long negative check." neg?]
  [zero? "Primitive long zero check." zero?]
  [odd? "Primitive long odd check." odd?]
  [even? "Primitive long even check." even?])
