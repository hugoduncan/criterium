(ns stats.interface
  "Public API for the stats component.

  Provides statistical functions including:
  - Core stats: min, max, mean, sum, variance, median, quartiles, quantile
  - Outlier detection: boxplot-outlier-thresholds
  - Sampling: uniform-distribution, sample-uniform, sample, confidence-interval
  - Probability: erf, normal-cdf, normal-pdf, normal-quantile
  - Histogram: histogram (Freedman-Diaconis binning)
  - T-digest: streaming quantile estimation
  - Kernel: modal estimation, kernel density estimators
  - KDE: bandwidth selection, Gaussian KDE, mode detection, multimodality tests"
  (:refer-clojure :exclude [min max])
  (:require
   [stats.core :as core]
   [stats.histogram :as histogram]
   [stats.kde :as kde]
   [stats.kernel :as kernel]
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

;;; Kernel functions

(def modal-estimation-constant
  "Kernel function for estimation of multi-modality.
  h-k is the critical bandwidth, sample-variance is the observed sample variance."
  kernel/modal-estimation-constant)

(def smoothed-sample
  "Smoothed estimation function.
  Generates a lazy sequence of smoothed values from data using kernel smoothing."
  kernel/smoothed-sample)

(def gaussian-weight
  "Weight function for gaussian kernel.
  K(t) = (1/sqrt(2*pi)) * exp(-t^2/2)"
  kernel/gaussian-weight)

(def kernel-density-estimator
  "Kernel density estimator for x, given n samples X, weights K and width h.
  Computes f(x) = (1/nh) * sum_i K((x - X_i)/h)"
  kernel/kernel-density-estimator)

;;; KDE - Kernel Density Estimation

(def dct-ii
  "Discrete Cosine Transform Type II.
  Direct O(n²) implementation without FFT dependency."
  kde/dct-ii)

(def linear-bin
  "Bin data onto a regular grid using linear interpolation.
  Returns vector of bin weights that sum to 1.0."
  kde/linear-bin)

(def silverman-bandwidth
  "Silverman's rule of thumb bandwidth selector.
  h = 0.9 * min(σ, IQR/1.34) * n^(-1/5)"
  kde/silverman-bandwidth)

(def isj-bandwidth
  "Improved Sheather-Jones bandwidth selector.
  Uses DCT-based algorithm from Botev et al. for optimal bandwidth
  selection that works well for multimodal distributions."
  kde/isj-bandwidth)

(def gaussian-kde
  "Compute Gaussian kernel density estimate at grid points.
  Returns vector of density values at each grid point."
  kde/gaussian-kde)

(def find-modes
  "Find modes (local maxima) in a density estimate.
  Returns vector of maps with :location and :density for each mode,
  sorted by density (highest first)."
  kde/find-modes)

(def kde-bootstrap-sample
  "Generate a bootstrap sample of KDE density at fixed grid points."
  kde/kde-bootstrap-sample)

(defn kde-confidence-bands
  "Compute bootstrap confidence bands for KDE.
  Returns map with :lower and :upper vectors."
  ([data bandwidth grid]
   (kde/kde-confidence-bands data bandwidth grid))
  ([data bandwidth grid opts]
   (kde/kde-confidence-bands data bandwidth grid opts)))

(defn mode-confidence-intervals
  "Compute bootstrap confidence intervals for mode locations.
  Returns vector of mode CIs, each with :location, :ci-lower, :ci-upper."
  ([data bandwidth grid n-modes]
   (kde/mode-confidence-intervals data bandwidth grid n-modes))
  ([data bandwidth grid n-modes opts]
   (kde/mode-confidence-intervals data bandwidth grid n-modes opts)))

(def count-modes
  "Count number of modes in KDE with given bandwidth."
  kde/count-modes)

(def critical-bandwidth
  "Find smallest bandwidth giving at most k modes via binary search.
  Returns the critical bandwidth h_k."
  kde/critical-bandwidth)

(def locate-modes
  "Find mode and antimode locations using critical bandwidth.
  Returns {:modes [...] :antimodes [...] :critical-bandwidth h_k}"
  kde/locate-modes)

(def silverman-bootstrap-sample
  "Generate a smoothed bootstrap sample for Silverman's test."
  kde/silverman-bootstrap-sample)

(def silverman-test
  "Silverman's bootstrap test for H0: at most k modes.
  Returns map with :k, :critical-bandwidth, :p-value, :corrected?"
  kde/silverman-test)

(def acr-test
  "ACR test for H0: at most k modes.
  Combines critical bandwidth and excess mass approaches.
  Returns map with :k, :excess-mass, :critical-bandwidth, :p-value"
  kde/acr-test)

(def excess-mass
  "Compute excess mass statistic for testing k modes.
  Returns map with :statistic, :k, :n"
  kde/excess-mass)

(defn kde
  "Compute KDE analysis on sample data.
  Returns map with :type, :bandwidth, :grid, :density, :lower-band, :upper-band, :n"
  ([data] (kde/kde data))
  ([data opts] (kde/kde data opts)))
