(ns criterium.analyse.methods)

(defmulti transform
  "Transform sample values."
  (fn [sample-map metric-configs f inv-f options]
    (:type sample-map)))

(defmulti quantiles
  "Calculate quantiles."
  (fn [sample-map metric-configs options]
    (:type sample-map)))

(defmulti outliers
  "Calculate outliers."
  (fn [sample-map quantiles metric-configs options]
    (:type sample-map)))

(defmulti stats
  "Calculate sample statistics."
  (fn [sample-map outliers metric-configs options]
    (:type sample-map)))

(defmulti event-stats
  "Calculate sample statistics for events."
  (fn [sample-map metric-configs options]
    (:type sample-map)))

(defmulti histogram
  "Calculate histogram."
  (fn [sample-map quantiles outliers metric-configs options]
    (:type sample-map)))
