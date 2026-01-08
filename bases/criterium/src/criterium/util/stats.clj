(ns criterium.util.stats
  "A collection of statistical methods used by criterium.

  All functions are delegated to stats and optimisation components."
  (:refer-clojure :exclude [min max])
  (:require
   [criterium.stats.interface :as stats]
   [optimisation.interface :as optimisation]))

;;; Core statistics (delegated to stats component)

(def transpose
  "Transpose a vector of vectors."
  stats/transpose)

(defn min
  "Minimum value in data."
  ([data] (stats/min data))
  ([data count] (stats/min data count)))

(defn max
  "Maximum value in data."
  ([data] (stats/max data))
  ([data count] (stats/max data count)))

(def unchecked-add-d
  "Unchecked double addition."
  stats/unchecked-add-d)

(defn mean
  "Arithmetic mean of data."
  ([data] (stats/mean data))
  ([data count] (stats/mean data count)))

(def sum
  "Sum of each data point."
  stats/sum)

(def sum-of-squares
  "Sum of the squares of each data point."
  stats/sum-of-squares)

(def variance*
  "Variance based on subtracting mean."
  stats/variance*)

(defn variance
  "Return the variance of data.

  By default returns the sample variance with (- (count data) 1) degrees
  of freedom.

  The population variance can be returned using (variance data 0), which uses
  (count data) degrees of freedom."
  ([data] (stats/variance data))
  ([data df] (stats/variance data df)))

(def median
  "Calculate the median of a sorted data set.
  Return [median, [vals less than median] [vals greater than median]]"
  stats/median)

(def quartiles
  "Calculate the quartiles of a sorted data set."
  stats/quartiles)

(def quantile
  "Calculate the quantile of a sorted data set."
  stats/quantile)

;;; Outliers (delegated to stats component)

(def boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles."
  stats/boxplot-outlier-thresholds)

(def adjusted-boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles adjusted for skewness.
  Uses the adjusted boxplot method from Hubert & Vandervieren (2008)."
  stats/adjusted-boxplot-outlier-thresholds)

(def medcouple-kernel
  "Compute the medcouple kernel h(x_i, x_j)."
  stats/medcouple-kernel)

(def medcouple
  "Compute the medcouple, a robust measure of skewness.
  Returns a value in [-1, 1] where positive indicates right-skew
  and negative indicates left-skew."
  stats/medcouple)

;;; Sampling (delegated to stats component)

(def uniform-distribution
  "Return uniformly distributed deviates on 0..max-val using the specified rng."
  stats/uniform-distribution)

(def sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val."
  stats/sample-uniform)

(def sample
  "Sample with replacement."
  stats/sample)

(def confidence-interval
  "Find the significance of outliers given bootstrapped mean and variance
   estimates."
  stats/confidence-interval)

;;; Kernel functions (delegated to stats component)

(def modal-estimation-constant
  "Kernel function for estimation of multi-modality.
  h-k is the critical bandwidth, sample-variance is the observed sample variance."
  stats/modal-estimation-constant)

(def smoothed-sample
  "Smoothed estimation function."
  stats/smoothed-sample)

(def gaussian-weight
  "Weight function for gaussian kernel."
  stats/gaussian-weight)

(def kernel-density-estimator
  "Kernel density estimator for x, given n samples X, weights K and width h."
  stats/kernel-density-estimator)

;;; Linear regression (delegated to optimisation component)

(def sum-square-delta
  "Sum of squared differences from a mean value."
  optimisation/sum-square-delta)

(def linear-regression
  "Perform simple linear regression: y = a0 + a1*x.

  Returns a map with:
  - :coeffs [a0 a1] - intercept and slope
  - :variance - residual variance (MSE with n-2 degrees of freedom)
  - :r-sqr - coefficient of determination (R-squared)"
  optimisation/linear-regression)
