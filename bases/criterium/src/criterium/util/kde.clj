(ns criterium.util.kde
  "Kernel Density Estimation utilities.

  All functions are delegated to the stats component.
  This namespace provides backward compatibility."
  (:require
   [criterium.stats.interface :as stats]))

;;; Excess Mass computation

(def excess-mass
  "Compute excess mass statistic for testing k modes.
  Returns map with :statistic, :k, :n"
  stats/excess-mass)

;;; DCT-II implementation

(def dct-ii
  "Discrete Cosine Transform Type II.
  Direct O(n²) implementation without FFT dependency."
  stats/dct-ii)

;;; Linear binning

(def linear-bin
  "Bin data onto a regular grid using linear interpolation.
  Returns vector of bin weights that sum to 1.0."
  stats/linear-bin)

;;; ISJ bandwidth selection

(def silverman-bandwidth
  "Silverman's rule of thumb bandwidth selector.
  h = 0.9 * min(σ, IQR/1.34) * n^(-1/5)"
  stats/silverman-bandwidth)

(def isj-bandwidth
  "Improved Sheather-Jones bandwidth selector.
  Uses DCT-based algorithm from Botev et al. for optimal bandwidth
  selection that works well for multimodal distributions."
  stats/isj-bandwidth)

;;; Gaussian kernel density estimation

(def gaussian-kde
  "Compute Gaussian kernel density estimate at grid points.
  Returns vector of density values at each grid point."
  stats/gaussian-kde)

;;; Mode finding

(def find-modes
  "Find modes (local maxima) in a density estimate.
  Returns vector of maps with :location and :density for each mode,
  sorted by density (highest first)."
  stats/find-modes)

;;; Bootstrap confidence bands

(def kde-bootstrap-sample
  "Generate a bootstrap sample of KDE density at fixed grid points."
  stats/kde-bootstrap-sample)

(defn kde-confidence-bands
  "Compute bootstrap confidence bands for KDE.
  Returns map with :lower and :upper vectors."
  ([data bandwidth grid]
   (stats/kde-confidence-bands data bandwidth grid))
  ([data bandwidth grid opts]
   (stats/kde-confidence-bands data bandwidth grid opts)))

;;; Mode confidence intervals

(defn mode-confidence-intervals
  "Compute bootstrap confidence intervals for mode locations.
  Returns vector of mode CIs, each with :location, :ci-lower, :ci-upper."
  ([data bandwidth grid n-modes]
   (stats/mode-confidence-intervals data bandwidth grid n-modes))
  ([data bandwidth grid n-modes opts]
   (stats/mode-confidence-intervals data bandwidth grid n-modes opts)))

;;; Silverman's test for multimodality

(def count-modes
  "Count number of modes in KDE with given bandwidth."
  stats/count-modes)

(def critical-bandwidth
  "Find smallest bandwidth giving at most k modes via binary search.
  Returns the critical bandwidth h_k."
  stats/critical-bandwidth)

(def locate-modes
  "Find mode and antimode locations using critical bandwidth.
  Returns {:modes [...] :antimodes [...] :critical-bandwidth h_k}"
  stats/locate-modes)

(def silverman-bootstrap-sample
  "Generate a smoothed bootstrap sample for Silverman's test."
  stats/silverman-bootstrap-sample)

(def silverman-test
  "Silverman's bootstrap test for H0: at most k modes.
  Returns map with :k, :critical-bandwidth, :p-value, :corrected?"
  stats/silverman-test)

(def acr-test
  "ACR test for H0: at most k modes.
  Combines critical bandwidth and excess mass approaches.
  Returns map with :k, :excess-mass, :critical-bandwidth, :p-value"
  stats/acr-test)

;;; Main KDE function

(defn kde
  "Compute KDE analysis on sample data.
  Returns map with :type, :bandwidth, :grid, :density, :lower-band, :upper-band, :n"
  ([data] (stats/kde data))
  ([data opts] (stats/kde data opts)))
