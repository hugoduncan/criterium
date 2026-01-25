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
   [criterium.viewer.print.autocorrelation]
   [criterium.viewer.print.core]
   [criterium.viewer.print.distribution]
   [criterium.viewer.print.domain]
   [criterium.viewer.print.modal]
   [criterium.viewer.print.shape]
   [criterium.viewer.print.tail]))

