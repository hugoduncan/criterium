(ns criterium.util.forms
  "Control flow macros.

  This namespace delegates to utils.interface for the core implementation
  and is retained for backward compatibility."
  (:require
   [utils.interface :as utils]))

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
