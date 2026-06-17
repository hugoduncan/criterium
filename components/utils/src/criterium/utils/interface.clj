(ns criterium.utils.interface
  "Public API for the utils component.

  Provides generic utilities including:
  - Assertion macros (have, have?)
  - Control flow macros (cond*)
  - Math utilities (sqr, sqrd, cubed, trunc)
  - Collection utilities (update-vals, filter-map, deep-merge)
  - Tree walking (walk, postwalk)
  - Debugging (spy, report)"
  (:refer-clojure :exclude [update-vals])
  (:require
   [criterium.utils.forms :as forms]
   [criterium.utils.helpers :as helpers]
   [criterium.utils.invariant :as invariant]))

;;; Invariant macros

(defmacro have
  "Assertion macro inspired by truss.
  Returns argument if predicate succeeds, throws AssertionError otherwise."
  {:arglists '[[x] [f x] [f x data]]}
  [x & args]
  `(invariant/have ~x ~@args))

(defmacro have?
  "Assertion macro inspired by truss.
  Returns true if predicate succeeds, throws AssertionError otherwise."
  [x & args]
  `(invariant/have? ~x ~@args))

(defn truthy?
  "Return true if x is neither nil nor false."
  [x]
  (invariant/truthy? x))

(defn assertion-error
  "Create an AssertionError with message and data, with stack trace adjusted
  to appear at assertion site."
  [msg data]
  (invariant/assertion-error msg data))

;;; Forms macros

(defmacro cond*
  "A cond variant that allows :let bindings visible to subsequent clauses."
  {:style/indent 1}
  [& clauses]
  `(forms/cond* ~@clauses))

(def ^:no-doc max-unrolled-arity
  "Highest arity for which function invocation is unrolled to a direct call."
  forms/max-unrolled-arity)

(defn ^:no-doc unrolled-apply-form
  "Return a form invoking f-sym with the elements of args-sym, dispatching on
  arg count to avoid apply's per-call seq allocation.  f-sym must be a symbol."
  [f-sym args-sym]
  (forms/unrolled-apply-form f-sym args-sym))

(defmacro unrolled-invoke
  "Invoke f with the elements of the args collection, unrolled by argument
  count to avoid apply's per-call seq allocation."
  [f args]
  `(forms/unrolled-invoke ~f ~args))

;;; Helpers - math

(defmacro sqr
  "Square of argument (macro for inlining)"
  [x]
  `(helpers/sqr ~x))

(defn sqrd
  "Square of argument (function)."
  ^double [^double x]
  (helpers/sqrd x))

(defn cubed
  "Cube of argument."
  ^double [^double x]
  (helpers/cubed x))

(defn trunc
  "Round towards zero to an integral value."
  ^double [^double x]
  (helpers/trunc x))

;;; Helpers - collections

(def update-vals-impl helpers/update-vals-impl)

(defmacro provide-update-vals
  "Polyfill macro for update-vals"
  []
  `(helpers/provide-update-vals))

(defn update-vals
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where
  the keys of m are mapped to result of applying f to the corresponding
  values of m."
  [m f]
  (helpers/update-vals m f))

(defn filter-map
  "Filter map entries based on a predicate applied to values.
  Return a new map containing only entries where (pred value) returns true."
  [pred m]
  (helpers/filter-map pred m))

(defn reduce-double-vector
  "Reduce a double primitive value over a vector."
  ^double [^clojure.lang.IFn$DOD f
           ^double init
           ^clojure.lang.APersistentVector v]
  (helpers/reduce-double-vector f init v))

(defn deep-merge
  "Merge maps recursively."
  [& ms]
  (apply helpers/deep-merge ms))

;;; Helpers - tree walking

(defn walk
  "Traverses form, an arbitrary data structure (preserves metadata).
  Applies inner to each element, building up a data structure of the same type,
  then applies outer to the result."
  [inner outer form]
  (helpers/walk inner outer form))

(defn postwalk
  "Performs a depth-first, post-order traversal of form.
  Calls f on each sub-form, uses f's return value in place of the original."
  [f form]
  (helpers/postwalk f form))

;;; Helpers - misc

(defn assoc-tag
  "Associate a type tag to a symbol's metadata."
  [sym t]
  (helpers/assoc-tag sym t))

(defn spy
  "Debug helper: print message and value, return value."
  [msg x]
  (helpers/spy msg x))

(defn report
  "Print format output."
  [format-string & values]
  (apply helpers/report format-string values))
