(ns criterium.stats.sampling
  "Sampling utilities: uniform distribution, sample functions, confidence intervals."
  (:require
   [criterium.array :as arr])
  (:import
   [criterium.array DoubleArray]))

(defn uniform-distribution
  "Return uniformly distributed deviates on 0..max-val using the specified rng."
  [^double max-val rng]
  (map (fn [^double x] (* x max-val)) rng))

(defn sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val."
  [n max-val rng]
  (take n (uniform-distribution max-val rng)))

(defn sample
  "Sample with replacement."
  [x rng]
  (let [n (count x)]
    (map #(nth x %1) (sample-uniform n n rng))))

(defn confidence-interval
  "Find the significance of outliers given bootstrapped mean and variance
   estimates. This uses the bootstrapped statistic's variance, but we should use
   BCa or ABC."
  [^double mean ^double variance]
  (let [n-sigma 1.96 ; use 95% confidence interval
        delta   (* n-sigma (Math/sqrt variance))]
    [(- mean delta) (+ mean delta)]))

(defn sample-doubles
  "Sample with replacement from a DoubleArray, returning a new DoubleArray.
  rng is a lazy sequence of random doubles in [0,1)."
  ^DoubleArray [^DoubleArray arr rng]
  (let [^doubles a (.array arr)
        n (alength a)
        ^doubles result (double-array n)
        nd (double n)]
    (loop [i 0
           rs (seq rng)]
      (when (< i n)
        (let [idx (long (* (double (first rs)) nd))]
          (aset result i (aget a idx))
          (recur (unchecked-inc i) (rest rs)))))
    (DoubleArray. result)))
