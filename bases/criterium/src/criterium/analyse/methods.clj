(ns criterium.analyse.methods)

(defmulti transform
  "Transform sample values."
  (fn [sample-map metric-configs f inv-f options]
    (:type sample-map)))
