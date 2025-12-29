(ns stats.interface
  "Public API for the stats component.

  Provides statistical functions including:
  - Core stats: min, max, mean, sum, variance, median, quartiles, quantile
  - Outlier detection: boxplot-outlier-thresholds
  - Sampling: uniform-distribution, sample-uniform, sample, confidence-interval
  - Probability: erf, normal-cdf, normal-pdf, normal-quantile
  - Histogram: histogram (Freedman-Diaconis binning)
  - T-digest: streaming quantile estimation"
  (:refer-clojure :exclude [min max])
  (:require
   [stats.core :as core]
   [stats.histogram :as histogram]
   [stats.outliers :as outliers]
   [stats.probability :as probability]
   [stats.sampling :as sampling]
   [stats.t-digest :as t-digest]))

;;; Core statistics

(def transpose
  "Transpose a vector of vectors."
  core/transpose)

(defn min
  "Minimum value in data."
  ([data] (core/min data))
  ([data count] (core/min data count)))

(defn max
  "Maximum value in data."
  ([data] (core/max data))
  ([data count] (core/max data count)))

(def unchecked-add-d
  "Unchecked double addition."
  core/unchecked-add-d)

(defn mean
  "Arithmetic mean of data."
  ([data] (core/mean data))
  ([data count] (core/mean data count)))

(def sum
  "Sum of each data point."
  core/sum)

(def sum-of-squares
  "Sum of the squares of each data point."
  core/sum-of-squares)

(def variance*
  "Variance based on subtracting mean."
  core/variance*)

(defn variance
  "Return the variance of data.

  By default returns the sample variance with (- (count data) 1) degrees
  of freedom.

  The population variance can be returned using (variance data 0), which uses
  (count data) degrees of freedom."
  ([data] (core/variance data))
  ([data df] (core/variance data df)))

(def median
  "Calculate the median of a sorted data set.
  Return [median, [vals less than median] [vals greater than median]]"
  core/median)

(def quartiles
  "Calculate the quartiles of a sorted data set."
  core/quartiles)

(def quantile
  "Calculate the quantile of a sorted data set."
  core/quantile)

;;; Outliers

(def boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles.
  Returns [severe-low mild-low mild-high severe-high]."
  outliers/boxplot-outlier-thresholds)

;;; Sampling

(def uniform-distribution
  "Return uniformly distributed deviates on 0..max-val using the specified rng."
  sampling/uniform-distribution)

(def sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val."
  sampling/sample-uniform)

(def sample
  "Sample with replacement."
  sampling/sample)

(def confidence-interval
  "Find the significance of outliers given bootstrapped mean and variance
   estimates."
  sampling/confidence-interval)

;;; Probability

(def polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
  descending order (so the last element of coefficients is the constant term)."
  probability/polynomial-value)

(def erf
  "erf polynomial approximation.  Maximum error is 1.5e-7.
  Handbook of Mathematical Functions: with Formulas, Graphs, and Mathematical
  Tables. Milton Abramowitz (Editor), Irene A. Stegun (Editor), 7.1.26"
  probability/erf)

(def normal-cdf
  "Probability p(X<x), for a normal distrubtion.  Uses the polynomial erf
  approximation above, and so is not super accurate."
  probability/normal-cdf)

(def normal-pdf
  "Probability density function for the normal distribution."
  probability/normal-pdf)

(def normal-quantile
  "Normal quantile function. Given a quantile in (0,1), return the normal value
  for that quantile.

  Wichura, MJ. 'Algorithm AS241' The Percentage Points of the Normal
  Distribution. Applied Statistics, 37, 477-484 "
  probability/normal-quantile)

;;; Histogram

(defn histogram
  "Compute histogram from vector of numeric values using Freedman-Diaconis rule.
   Optional pre-computed IQR can be provided.
   Returns map containing:
   - :counts - vector of bin counts
   - :centers - vector of bin centers
   - :width - bin width
   - :density - vector of probability density values
   - :n - total number of samples
   - :min - minimum value
   - :max - maximum value

   Throws:
   - ex-info {:error :histogram/no-values} for empty input
   - ex-info {:error :histogram/same-values} when all values are the same"
  ([values]
   (histogram/histogram values))
  ([values precomputed-iqr]
   (histogram/histogram values precomputed-iqr)))

;;; T-digest streaming quantile estimation

(defn digest-new
  "Creates a new t-digest with optional compression factor."
  ([] (t-digest/new-digest))
  ([compression] (t-digest/new-digest compression))
  ([compression buffer-size] (t-digest/new-digest compression buffer-size)))

(defn digest-add-point
  "Add a single value into the digest."
  ([digest value]
   (t-digest/add-point digest value))
  ([digest value weight]
   (t-digest/add-point digest value weight)))

(def digest-compress
  "Merge any buffered points into the digest."
  t-digest/compress)

(def digest-quantile
  "Return estimated value at given quantile [0,1].
   Return NaN if digest is empty."
  t-digest/quantile)

(def digest-cdf
  "Return the cumulative probability at x.
   Return NaN if digest is empty."
  t-digest/cdf)

(def digest-sample-count
  "Return the sample count in the digest."
  t-digest/sample-count)

(def digest-minimum
  "Return the minimum value in the digest."
  t-digest/minimum)

(def digest-maximum
  "Return the maximum value in the digest."
  t-digest/maximum)

(defn digest-mean
  "Return the mean estimate.
   Return NaN if digest is empty."
  [digest]
  (t-digest/mean digest))

(defn digest-variance
  "Return the variance estimate.
   Return NaN if digest is empty."
  ([digest]
   (t-digest/variance digest))
  ([digest mean]
   (t-digest/variance digest mean)))

(def digest-transform
  "Transform digest values using the given function."
  t-digest/transform)

(def digest-centroid-means
  "Return a vector of centroid means."
  t-digest/centroid-means)

(def digest-histogram
  "Returns a histogram of the digest using centroid centers as bin locations."
  t-digest/histogram)

(def digest-filter-outliers
  "Filter outliers from the digest."
  t-digest/filter-outliers)
