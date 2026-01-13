(ns criterium.primitive-fn
  "Primitive-typed versions of core Clojure functions.

  These functions are properly type-hinted to work with .invokePrim,
  avoiding boxing overhead when passed to higher-order primitive
  functions like fold-double, fold-long, dmap, and lmap.

  Use these instead of inline lambdas when the operation matches
  a standard function (add, min, max, etc.).

  Naming convention: d-prefix for double, l-prefix for long.")

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

;;; Double predicates

(defn dpos?
  "Primitive double positive check."
  [^double v]
  (pos? v))

(defn dneg?
  "Primitive double negative check."
  [^double v]
  (neg? v))

(defn dzero?
  "Primitive double zero check."
  [^double v]
  (zero? v))

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

;;; Long predicates

(defn lpos?
  "Primitive long positive check."
  [^long v]
  (pos? v))

(defn lneg?
  "Primitive long negative check."
  [^long v]
  (neg? v))

(defn lzero?
  "Primitive long zero check."
  [^long v]
  (zero? v))

(defmacro invoke-dd
  "Invoke a primitive double -> double function."
  [f v]
  `(.invokePrim ~(vary-meta f assoc :tag 'clojure.lang.IFn$DD) ~v))
