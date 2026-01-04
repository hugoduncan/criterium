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
  "Analyze scaling complexity by extracting all quantitative metrics and fitting models.

  Fits O(log n), O(n), O(n log n), O(n²) models to determine algorithmic
  complexity. Requires map coordinates with an :n key for input size.

  Includes error bounds (±3σ) for each data point when viewed with kindly.

  Example:
    (analyse-domain complexity-analysis my-domain)"
  {:analyse [[:domain-extract-fn {:with-error-bounds true}]
             [:domain-regression-fn {:axis :n}]]
   :view [[:domain-extract {}]
          [:domain-regression {}]]})

(def implementation-comparison
  "Compare metric values across different implementations.

  Groups runs by :impl axis and compares all quantitative metrics.
  Requires map coordinates with an :impl key distinguishing implementations.

  Includes error bounds (±3σ) for each data point when viewed with portal or kindly.

  In the output, the baseline implementation (first in :implementations) shows
  absolute values with SI units, while other implementations show factors
  relative to the baseline.

  Example:
    (analyse-domain implementation-comparison my-domain)"
  {:analyse [[:domain-compare-fn {:axis-key :impl :with-error-bounds true}]]
   :view [[:domain-comparison {}]]})

(def extract-metrics
  "Extract all quantitative metrics from all runs.

  Discovers available metrics automatically (elapsed-time, thread-allocation, etc.)
  and extracts mean values for each.

  Includes error bounds (±3σ) for each data point when viewed with portal or kindly.

  Example:
    (analyse-domain extract-metrics my-domain)"
  {:analyse [[:domain-extract-fn {:with-error-bounds true}]]
   :view [[:domain-extract {}]]})

(def extract-elapsed-time
  "Extract elapsed time mean values from all runs.

  Simple plan for viewing elapsed time across a domain. For all available
  metrics, use complexity-analysis or omit :metric-path from domain-extract-fn.

  Example:
    (analyse-domain extract-elapsed-time my-domain)"
  {:analyse [[:domain-extract-fn {:metric-path [:stats :elapsed-time :mean]}]]
   :view [[:domain-extract {}]]})
