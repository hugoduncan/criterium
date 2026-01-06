(ns criterium.call-graph.plans
  "Pre-configured call graph analysis plans.

  Call graph plans specify how to analyze and view call tracing results.
  Each plan is a map with:

    :analyse - Vector of analysis specs to invoke
    :view    - Vector of view specs to invoke

  Analysis specs are keywords that resolve to functions in criterium.analyse:
    :most-called - Aggregate methods by total call count

  View specs are keywords that resolve to multimethods in criterium.view:
    :call-tree   - ASCII tree or Vega hierarchical tree diagram
    :call-flame  - Flame chart where width represents call count
    :most-called - Table or bar chart of most frequently called methods")

(def default
  "Default call graph plan with tree, flame chart, and most-called views.

  Displays an ASCII tree (or Vega tree diagram with :portal/:kindly viewers),
  a flame chart showing call distribution, and a most-called methods table."
  {:analyse [:most-called]
   :view [:call-tree :call-flame :most-called]})

(def tree-only
  "Call graph plan showing only the tree view.

  Useful when flame chart is not needed or for simpler output."
  {:analyse []
   :view [:call-tree]})

(def flame-only
  "Call graph plan showing only the flame chart.

  Useful for visualizing call distribution without the tree structure."
  {:analyse []
   :view [:call-flame]})

(def most-called-only
  "Call graph plan showing only the most-called methods.

  Useful for quickly identifying hot methods without the full tree structure."
  {:analyse [:most-called]
   :view [:most-called]})
