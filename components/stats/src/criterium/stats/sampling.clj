(ns criterium.stats.sampling
  "Sampling utilities: sample functions, confidence intervals.

  All sampling functions take mutable uniform RNGs and call next-double!
  to generate random values."
  (:require
   [criterium.array :as arr]
   [criterium.random.interface :as random])
  (:import
   [criterium.array DoubleArray]
   [criterium.random.well WellRng1024a]))

(defn sample-uniform
  "Provide n samples from a uniform distribution on [0, max-val).
  rng is a mutable uniform RNG.
  Returns a vector of n doubles."
  [^long n ^double max-val ^WellRng1024a rng]
  (let [result (double-array n)]
    (dotimes [i n]
      (aset result i (* (random/next-double! rng) max-val)))
    (vec result)))

(defn sample
  "Sample n items with replacement from collection x.
  rng is a mutable uniform RNG.
  Returns a vector of sampled items."
  [x ^WellRng1024a rng]
  (let [n (count x)
        indices (sample-uniform n (double n) rng)]
    (mapv #(nth x (long %)) indices)))

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
  rng is a mutable uniform RNG."
  ^DoubleArray [^DoubleArray arr ^WellRng1024a rng]
  (let [^doubles a (.array arr)
        n (alength a)
        ^doubles result (double-array n)
        nd (double n)]
    (dotimes [i n]
      (let [idx (long (* (random/next-double! rng) nd))]
        (aset result i (aget a idx))))
    (DoubleArray. result)))
