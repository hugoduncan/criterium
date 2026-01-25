(ns criterium.primitive-fn
  "Primitive-typed versions of core Clojure functions.

  These functions are properly type-hinted to work with .invokePrim,
  avoiding boxing overhead when passed to higher-order primitive
  functions like fold-double, fold-long, dmap, and lmap.

  Use these instead of inline lambdas when the operation matches
  a standard function (add, min, max, etc.).

  Naming convention: d-prefix for double, l-prefix for long.")

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

(defmacro ^:private defpred-wrappers
  "Define primitive predicate wrapper functions for a given type.

   Like defprim-wrappers but for predicates that return boolean.
   No return type hint is added since predicates return Object (boxed boolean).

   Example:
     (defpred-wrappers double
       [pos? \"Primitive double positive check.\" pos?])

   Expands to:
     (defn dpos? \"Primitive double positive check.\" [^double v] (pos? v))"
  [prim-type & specs]
  (let [prefix   (case prim-type double "d" long "l")
        type-sym (case prim-type double 'double long 'long)
        arg-v    (with-meta 'v {:tag type-sym})]
    (cons 'do
          (for [[suffix docstring wrapped-fn] specs]
            (let [fn-name (symbol (str prefix suffix))]
              (list 'defn fn-name docstring [arg-v]
                    (list wrapped-fn arg-v)))))))

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

;;; Predicates

(defpred-wrappers double
  [pos? "Primitive double positive check." pos?]
  [neg? "Primitive double negative check." neg?]
  [zero? "Primitive double zero check." zero?])

(defpred-wrappers long
  [pos? "Primitive long positive check." pos?]
  [neg? "Primitive long negative check." neg?]
  [zero? "Primitive long zero check." zero?]
  [odd? "Primitive long odd check." odd?]
  [even? "Primitive long even check." even?])
