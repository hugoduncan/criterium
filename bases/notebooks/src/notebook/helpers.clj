(ns notebook.helpers
  "Helper utilities for criterium notebooks."
  (:require
   [scicloj.kindly.v4.kind :as kind]))

(defmacro bench-display
  "Display a bench expression and its output for notebooks.
  Shows the code followed by the captured stdout."
  [expr]
  `(kind/fragment
    [(kind/code ~(pr-str expr))
     (kind/code (with-out-str ~expr))]))
