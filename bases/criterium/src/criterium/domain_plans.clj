(ns criterium.domain-plans
  "Pre-configured domain analysis plans.

  Domain plans specify how to analyze and view benchmark results across
  a domain of runs. Each plan is a map with:

    :analyse - Vector of analysis specs resolved from criterium.domain
    :view    - Vector of view specs resolved from criterium.view
    :viewer  - Keyword specifying output format (:print, :portal, :none)

  Analysis specs are either keywords or vectors of [keyword options-map].
  View specs follow the same pattern.")

(def complexity-analysis
  "Analyze scaling complexity by extracting elapsed time and fitting models.

  Fits O(log n), O(n), O(n log n), O(n²) models to determine algorithmic
  complexity. Requires map coordinates with an :n key for input size.

  Example:
    (analyse-domain complexity-analysis my-domain)"
  {:analyse [[:domain-extract-fn {:metric-path [:stats :elapsed-time :mean]}]
             [:domain-regression-fn {:axis :n}]]
   :view    [[:domain-extract {}]
             [:domain-regression {}]]
   :viewer  :print})

(def implementation-comparison
  "Compare metric values across different implementations.

  Groups runs by :impl axis and compares elapsed time. Requires map
  coordinates with an :impl key distinguishing implementations.

  Example:
    (analyse-domain implementation-comparison my-domain)"
  {:analyse [[:domain-compare-fn {:axis-key :impl
                                  :metric-path [:stats :elapsed-time :mean]}]]
   :view    [[:domain-comparison {}]]
   :viewer  :print})

(def extract-elapsed-time
  "Extract elapsed time mean values from all runs.

  Simple plan for viewing metric values across a domain without
  additional analysis.

  Example:
    (analyse-domain extract-elapsed-time my-domain)"
  {:analyse [[:domain-extract-fn {:metric-path [:stats :elapsed-time :mean]}]]
   :view    [[:domain-extract {}]]
   :viewer  :print})
