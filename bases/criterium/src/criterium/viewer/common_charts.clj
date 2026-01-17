(ns criterium.viewer.common-charts
  "Common Vega-Lite chart generation functions for viewers.

  Provides reusable chart-building functions extracted from the Portal viewer
  for generating histograms, scatter plots, percentile charts, and treemaps."
  (:require
   [criterium.viewer.common-charts.comparison :as comparison]
   [criterium.viewer.common-charts.distribution :as distribution]
   [criterium.viewer.common-charts.profile :as profile]
   [criterium.viewer.common-charts.quantile :as quantile]
   [criterium.viewer.common-charts.regression :as regression]
   [criterium.viewer.common-charts.samples :as samples]))

;;; Sample visualization re-exports (moved to common-charts.samples)

(def metric-layer
  "Build a scatter plot layer showing samples with outlier coloring.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-layer)

(def metric-computed-histo-layer
  "Build a histogram bar chart layer from pre-computed histogram data.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-computed-histo-layer)

(def normal-pdf-points
  "Generate points for a normal probability density function curve.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/normal-pdf-points)

(def metric-sample-stats-layer
  "Build a normal distribution overlay layer for histogram charts.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-sample-stats-layer)

(def metric-bootstrap-boxplot-layer
  "Build a boxplot-style layer showing median CI and spread percentiles.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-bootstrap-boxplot-layer)

(def event-occurrence
  "Extract event occurrence data for a single sample index.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/event-occurrence)

(def event-layer
  "Build an event marker layer showing when events occurred during sampling.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/event-layer)

(def samples-vega-spec
  "Build a complete Vega-Lite spec for sample visualization.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/samples-vega-spec)

(def histogram-vega-spec
  "Build a complete Vega-Lite spec for histogram visualization.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/histogram-vega-spec)

(def metric-percentile-layer
  "Build a percentile distribution chart layer.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-percentile-layer)

(def metric-diff-layer
  "Build a sample differences chart layer.
  Re-exported from criterium.viewer.common-charts.samples."
  samples/metric-diff-layer)

;;; Regression chart re-exports (moved to common-charts.regression)

(def regression-scatter-layer
  "Build scatter plot layer for regression points.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-scatter-layer)

(def regression-error-layer
  "Build error bar layer for regression points with error bounds.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-error-layer)

(def regression-line-layer
  "Build fit line layer for regression models.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-line-layer)

(def regression-chart-spec
  "Build complete regression scatter plot with fit lines.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-chart-spec)

(def regression-residual-layer
  "Build scatter layer for residual plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-residual-layer)

(def regression-loess-layer
  "Build loess smoothing layer for residual plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-loess-layer)

(def regression-zero-line-layer
  "Build zero reference line layer for residual plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-zero-line-layer)

(def regression-residual-spec
  "Build complete residual plot spec.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/regression-residual-spec)

(def log-log-scatter-layer
  "Build scatter layer for log-log plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/log-log-scatter-layer)

(def log-log-line-layer
  "Build fit line layer for log-log plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/log-log-line-layer)

(def log-log-error-layer
  "Build error bar layer for log-log plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/log-log-error-layer)

(def log-log-chart-spec
  "Build complete log-log scatter plot with fit line.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/log-log-chart-spec)

(def log-log-residual-layer
  "Build scatter layer for log-log residual plot.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/log-log-residual-layer)

(def log-log-residual-spec
  "Build complete log-log residual plot spec.
  Re-exported from criterium.viewer.common-charts.regression."
  regression/log-log-residual-spec)

;;; Comparison chart re-exports (moved to common-charts.comparison)

(def prepare-single-point-box-data
  "Prepare data for single-point box plot from domain extract.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/prepare-single-point-box-data)

(def prepare-single-point-bar-data
  "Prepare data for single-point bar chart from domain extract.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/prepare-single-point-bar-data)

(def single-point-bar-chart-spec
  "Build a Vega-Lite bar chart spec for single-point multi-impl comparison.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/single-point-bar-chart-spec)

(def single-point-box-chart-spec
  "Build a Vega-Lite box plot spec for single-point multi-impl comparison.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/single-point-box-chart-spec)

(def comparison-bar-chart-spec
  "Build a Vega-Lite bar chart spec for single-point comparison data.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/comparison-bar-chart-spec)

(def comparison-box-chart-spec
  "Build a Vega-Lite box plot spec for single-point comparison data.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/comparison-box-chart-spec)

(def domain-line-chart-spec
  "Build a Vega-Lite line chart spec for single-axis multi-point domain extract.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/domain-line-chart-spec)

(def comparison-line-chart-spec
  "Build a Vega-Lite line chart spec for single-axis multi-point comparison data.
  Re-exported from criterium.viewer.common-charts.comparison."
  comparison/comparison-line-chart-spec)

;;; KDE and Distribution re-exports (moved to common-charts.distribution)

(def kde-density-layer
  "Build a density curve layer for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-density-layer)

(def kde-confidence-band-layer
  "Build a confidence band area layer for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-confidence-band-layer)

(def kde-modes-layer
  "Build mode marker layers for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-modes-layer)

(def kde-vega-spec
  "Build a complete Vega-Lite spec for KDE visualization.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/kde-vega-spec)

;;; Distribution PDF overlay re-exports (moved to common-charts.distribution)

(def distribution-pdf-layer
  "Build a PDF curve layer for a single fitted distribution.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-pdf-layer)

(def distribution-pdf-overlay-layers
  "Build PDF overlay layers for all fitted distributions.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-pdf-overlay-layers)

(def distribution-pdf-vega-spec
  "Build a complete Vega-Lite spec for KDE with distribution PDF overlays.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-pdf-vega-spec)

;;; Distribution CDF overlay re-exports (moved to common-charts.distribution)

(def ecdf-layer
  "Build an ECDF (empirical cumulative distribution function) step layer.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/ecdf-layer)

(def distribution-cdf-layer
  "Build a CDF curve layer for a single fitted distribution.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-cdf-layer)

(def distribution-cdf-overlay-layers
  "Build CDF overlay layers for all fitted distributions.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-cdf-overlay-layers)

(def distribution-cdf-vega-spec
  "Build a complete Vega-Lite spec for ECDF with distribution CDF overlays.
  Re-exported from criterium.viewer.common-charts.distribution."
  distribution/distribution-cdf-vega-spec)

;;; Q-Q plot re-exports (moved to common-charts.quantile)

(def qq-points
  "Generate Q-Q plot points comparing sample quantiles to theoretical quantiles.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/qq-points)

(def distribution-qq-layer
  "Build a Q-Q scatter layer for a single fitted distribution.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/distribution-qq-layer)

(def qq-reference-line-layer
  "Build a y=x reference line layer for Q-Q plots.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/qq-reference-line-layer)

(def distribution-qq-overlay-layers
  "Build Q-Q overlay layers for all fitted distributions.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/distribution-qq-overlay-layers)

(def distribution-qq-vega-spec
  "Build a complete Vega-Lite spec for Q-Q plots with separate subplots per distribution.
  Re-exported from criterium.viewer.common-charts.quantile."
  quantile/distribution-qq-vega-spec)

;;; Profile visualization re-exports (moved to common-charts.profile)

(def treemap-vega-spec
  "Build a complete Vega spec for treemap visualization.
  Re-exported from criterium.viewer.common-charts.profile."
  profile/treemap-vega-spec)

(def call-tree-tree-vega-spec
  "Build a Vega spec for hierarchical tree visualization of call graph.
  Re-exported from criterium.viewer.common-charts.profile."
  profile/call-tree-tree-vega-spec)

(def call-tree-flame-vega-spec
  "Build a Vega spec for flame chart visualization of call graph.
  Re-exported from criterium.viewer.common-charts.profile."
  profile/call-tree-flame-vega-spec)

(def most-called-vega-lite-spec
  "Build a Vega-Lite horizontal bar chart spec for most-called methods.
  Re-exported from criterium.viewer.common-charts.profile."
  profile/most-called-vega-lite-spec)
