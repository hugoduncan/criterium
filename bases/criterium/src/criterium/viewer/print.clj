(ns criterium.viewer.print
  "A print viewer

  Core functionality (metrics, stats, extremes, bootstrap, samples, outliers,
  events, GC, OS, runtime) is in criterium.viewer.print.core.

  Domain analysis (grouped, extract, comparison, regression, apply) is in
  criterium.viewer.print.domain.

  Allocation profiling (summary, hotspots, by-type, treemap) is in
  criterium.viewer.print.allocation.

  Distribution fit (models, parameter CIs) is in
  criterium.viewer.print.distribution.

  Tail analysis (summary, ratios, high quantiles) is in
  criterium.viewer.print.tail.

  Shape statistics (skewness, kurtosis, CV) is in
  criterium.viewer.print.shape.

  Modal analysis (multimodal warnings) is in criterium.viewer.print.modal.

  Autocorrelation analysis (lag analysis, effective sample size, classification)
  is in criterium.viewer.print.autocorrelation."
  (:require
   [criterium.viewer.print.allocation]
   [criterium.viewer.print.autocorrelation :as print.autocorrelation]
   [criterium.viewer.print.core :as print.core]
   [criterium.viewer.print.distribution]
   [criterium.viewer.print.domain]
   [criterium.viewer.print.modal]
   [criterium.viewer.print.shape]
   [criterium.viewer.print.tail]))

;;; Re-exported functions from print.core for backwards compatibility
(def print-stat
  "Print a single metric's stats. Delegates to criterium.viewer.print.core."
  print.core/print-stat)

(def print-extreme
  "Print a single metric's min/max values. Delegates to criterium.viewer.print.core."
  print.core/print-extreme)

(def print-bootstrap-stat
  "Print bootstrap statistics for a metric. Delegates to criterium.viewer.print.core."
  print.core/print-bootstrap-stat)

(def print-outlier-count
  "Print outlier counts for a metric. Delegates to criterium.viewer.print.core."
  print.core/print-outlier-count)

;;; Re-exported functions from print.autocorrelation for backwards compatibility
(def print-autocorrelation
  "Print autocorrelation analysis summary. Delegates to criterium.viewer.print.autocorrelation."
  print.autocorrelation/print-autocorrelation)

