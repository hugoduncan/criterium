(ns criterium.analyse.methods
  "Analysis dispatch based on ample-map type")

(defmulti transform
  "Transform sample values."
  (fn [sample-map _metric-configs _f _inv-f _options]
    (:type sample-map)))

(defmulti quantiles
  "Calculate quantiles."
  (fn [sample-map _metric-configs _options]
    (:type sample-map)))

(defmulti outliers
  "Calculate outliers."
  (fn [sample-map _quantiles _metric-configs _options]
    (:type sample-map)))

(defmulti stats
  "Calculate sample statistics."
  (fn [sample-map _outliers _metric-configs _options]
    (:type sample-map)))

(defmulti event-stats
  "Calculate sample statistics for events."
  (fn [sample-map _metric-configs _options]
    (:type sample-map)))

(defmulti histogram
  "Calculate histogram."
  (fn [sample-map _quantiles _outliers _metric-configs _options]
    (:type sample-map)))

(defmulti kde
  "Calculate kernel density estimation.
  Returns nil if sample data is not available (e.g., digest-based samples)."
  (fn [sample-map _outliers _metric-configs _options]
    (:type sample-map)))

(defmulti modes
  "Calculate mode analysis with statistical validation.
  Takes KDE output and computes modes with Silverman's test for significance.
  Returns nil if raw sample data is not available."
  (fn [kde-map _samples _outliers _metric-configs _options]
    (:type kde-map)))
