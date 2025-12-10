(ns criterium.notebook.helpers
  "Helper utilities for criterium notebooks."
  (:require
   [criterium.bench :as bench]
   [criterium.viewer.kindly :as kindly]
   [scicloj.kindly.v4.kind :as kind]))

(defmacro bench-display
  "Display a bench expression and its output for notebooks.
  Shows the code followed by the captured stdout."
  [expr]
  `(kind/fragment
    [(kind/code ~(pr-str expr))
     (kind/code (with-out-str ~expr))]))

(defmacro bench-kindly
  "Run a benchmark with Kindly viewer and return the Kindly fragment.

  This macro is designed for Clay notebooks where the cell's return value
  is rendered. The benchmark results are formatted as Kindly-annotated
  tables and Vega-Lite charts.

  Example:
    (bench-kindly (reduce + (range 1000)))
    (bench-kindly (reduce + (range 1000)) :limit-time-s 1)"
  [expr & options]
  `(do
     (bench/bench ~expr :viewer :kindly ~@options)
     @kindly/last-fragment))
