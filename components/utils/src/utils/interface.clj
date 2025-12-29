(ns utils.interface
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
   [utils.forms :as forms]
   [utils.helpers :as helpers]
   [utils.invariant :as invariant]))

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

(def truthy? invariant/truthy?)
(def assertion-error invariant/assertion-error)

;;; Forms macros

(defmacro cond*
  "A cond variant that allows :let bindings visible to subsequent clauses."
  {:style/indent 1}
  [& clauses]
  `(forms/cond* ~@clauses))

;;; Helpers - math

(defmacro sqr
  "Square of argument (macro for inlining)"
  [x]
  `(helpers/sqr ~x))

(def sqrd
  "Square of argument (function)"
  helpers/sqrd)

(def cubed
  "Cube of argument"
  helpers/cubed)

(def trunc
  "Round towards zero to an integral value"
  helpers/trunc)

;;; Helpers - collections

(def update-vals-impl helpers/update-vals-impl)

(defmacro provide-update-vals
  "Polyfill macro for update-vals"
  []
  `(helpers/provide-update-vals))

(def update-vals
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where
  the keys of m are mapped to result of applying f to the corresponding
  values of m."
  helpers/update-vals)

(def filter-map
  "Filter map entries based on a predicate applied to values."
  helpers/filter-map)

(def reduce-double-vector
  "Reduce a double primitive value over a vector."
  helpers/reduce-double-vector)

(def deep-merge
  "Merge maps recursively."
  helpers/deep-merge)

;;; Helpers - tree walking

(def walk
  "Traverses form, an arbitrary data structure (preserves metadata)."
  helpers/walk)

(def postwalk
  "Performs a depth-first, post-order traversal of form."
  helpers/postwalk)

;;; Helpers - misc

(def assoc-tag
  "Associate a type tag to a symbol's metadata."
  helpers/assoc-tag)

(def spy
  "Debug helper: print message and value, return value."
  helpers/spy)

(def report
  "Print format output"
  helpers/report)
