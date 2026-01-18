(ns criterium.viewer.common-charts.util
  "Shared helpers for common-charts sub-namespaces.

  Provides utility functions used across multiple chart generation namespaces
  to avoid circular dependencies.")

(defn metric-type-prefix
  "Extract metric type (mean/median) from metric path for y-axis titles.
  Returns \"mean\" or \"median\" if found in path, nil otherwise."
  [metric-path]
  (when (and (vector? metric-path) (>= (count metric-path) 3))
    (let [value-key (nth metric-path 2)]
      (when (#{:mean :median} value-key)
        (name value-key)))))

(defn chart-layer
  "Build a chart layer by merging chart-options with mark and encoding.
  Common structure for bar and line chart layers."
  [data chart-options mark encoding]
  (merge
   chart-options
   {:data {:values data}
    :mark mark
    :encoding encoding}))
