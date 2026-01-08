(ns criterium.util.invariant
  "Assertion macros inspired by truss.

  This namespace delegates to criterium.utils.interface for the core implementation
  and is retained for backward compatibility."
  (:require
   [criterium.utils.interface :as utils]))

(def truthy? utils/truthy?)
(def assertion-error utils/assertion-error)

(defmacro have
  "Assertion macro inspired by truss."
  {:arglists '[[x] [f x] [f x data]]}
  ([x & args]
   `(utils/have ~x ~@args)))

(defmacro have?
  "Assertion macro inspired by truss."
  ([x & args]
   `(utils/have? ~x ~@args)))
