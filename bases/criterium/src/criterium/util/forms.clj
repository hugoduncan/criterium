(ns criterium.util.forms
  "Control flow macros.

  This namespace delegates to criterium.utils.interface for the core
  implementation and is retained for backward compatibility."
  (:require
   [criterium.utils.interface :as utils]))

(defmacro cond*
  "A cond variant that allows :let bindings visible to subsequent clauses.

  Example:
     (cond*
       (foo?) (handle-foo)
       :let [a 5]
       (> a 3) (handle-big a)
       :let [b (+ a 1)]
       (bar? b) (handle-bar b)
       :else (default-handler a b))"
  {:style/indent 1}
  [& clauses]
  `(utils/cond* ~@clauses))

(def ^:no-doc max-unrolled-arity
  "Highest arity for which function invocation is unrolled to a direct call."
  utils/max-unrolled-arity)

(defn ^:no-doc unrolled-apply-form
  "Return a form invoking f-sym with the elements of args-sym, dispatching on
  arg count to avoid apply's per-call seq allocation.  f-sym must be a symbol."
  [f-sym args-sym]
  (utils/unrolled-apply-form f-sym args-sym))

(defmacro unrolled-invoke
  "Invoke f with the elements of the args collection, unrolled by argument
  count to avoid apply's per-call seq allocation."
  [f args]
  `(utils/unrolled-invoke ~f ~args))
