(ns criterium.call-graph.plans
  "Pre-configured call graph analysis plans.

  Call graph plans specify how to analyze and view call tracing results.
  Each plan is a map with:

    :analyse - Vector of analysis specs (currently empty, reserved for future use)
    :view    - Vector of view specs to invoke

  View specs are keywords that resolve to multimethods in criterium.view:
    :call-tree  - ASCII tree or Vega hierarchical tree diagram
    :call-flame - Flame chart where width represents call count")

(def default
  "Default call graph plan with both tree and flame chart views.

  Displays an ASCII tree (or Vega tree diagram with :portal/:kindly viewers)
  followed by a flame chart showing call distribution."
  {:analyse []
   :view [:call-tree :call-flame]})

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
