(ns criterium.stats.sampling
  "Sampling utilities: uniform distribution, sample functions, confidence intervals.")

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
