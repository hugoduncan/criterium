(ns stats.interface
  "Public API for the stats component.

  Provides statistical functions including:
  - Core stats: min, max, mean, sum, variance, median, quartiles, quantile
  - Outlier detection: boxplot-outlier-thresholds
  - Sampling: uniform-distribution, sample-uniform, sample, confidence-interval"
  (:refer-clojure :exclude [min max])
  (:require
   [stats.core :as core]
   [stats.outliers :as outliers]
   [stats.sampling :as sampling]))

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
