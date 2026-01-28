(ns criterium.bench-plans
  "Provide pre-configured benchmark definitions.")

(def default-collector-config
  {:stages []
   :terminator :elapsed-time})

(def one-shot
  "Single execution profiling without warmup or sampling.

  Runs a single batch without JVM warmup or statistical sampling. Primarily
  useful for allocation analysis and quick exploratory checks.

  Includes:
  - Basic execution metrics and timing
  - Detailed allocation tracking: summary, hotspots, by-type breakdown
  - JVM event statistics (compilation, class loading)

  Use when you want to see raw allocation patterns without the overhead of
  statistical analysis, or for a quick sanity check before running a full
  benchmark."
  {:collector-config default-collector-config
   :analyse [:event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type]
   :view [:metrics
          :event-stats
          :collect-plan
          :allocation-summary
          :allocation-by-type
          :allocation-hotspots]
   :viewer :print})

(def default
  "Standard benchmarking with full statistical analysis.

  The recommended plan for typical performance measurement. Includes warmup,
  multiple sampling iterations, and comprehensive statistical analysis.

  Includes:
  - Bootstrap confidence intervals for mean, variance, and other statistics
  - Outlier detection and classification
  - Autocorrelation analysis with effective sample size computation
  - KDE and mode detection for multimodal warnings (no histogram display)
  - Memory and allocation tracking (summary, hotspots, treemap)
  - JVM event statistics (compilation, class loading)

  The analysis detects multimodal distributions and autocorrelation patterns,
  displaying warnings when results may be unreliable. Use this plan for
  everyday benchmarking where you need statistically sound results."
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:autocorrelation {:id :autocorrelation-raw}]
             [:autocorrelation-classification {:id :autocorrelation-classification-raw
                                               :autocorrelation-id :autocorrelation-raw}]
             [:quantiles {:quantiles [0.9 0.99]}]
             :outliers
             [:autocorrelation {:id :autocorrelation-filtered
                                :outlier-id :outliers}]
             [:effective-sample-size-analysis {:id :effective-sample-size-filtered
                                               :autocorrelation-id :autocorrelation-filtered}]
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]
                                :ess-id :effective-sample-size-filtered}]
             :kde
             :modes
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [:collect-plan
          [:stats {:metric-ids [:memory]}]
          :bootstrap-stats
          :extremes
          :shape-stats
          :outlier-counts
          [:multimodal-warning {:modes-id :modes}]
          [:autocorrelation-classification
           {:classification-id :autocorrelation-classification-raw}]
          [:effective-sample-size {:ess-id :effective-sample-size-filtered}]
          [:acf-plot {:autocorrelation-id :autocorrelation-raw
                      :min-severity :moderate}]
          :event-stats
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap
          #_[:final-gc-warnings
             {:warn-threshold 0.01}]]
   :viewer :print})

(def histogram
  "Non-parametric distribution analysis with histogram visualization.

  Consolidates histogram, KDE density estimation, and mode detection into a
  single comprehensive plan. Uses Knuth's Bayesian optimal binning.

  Includes:
  - Histogram with automatic bin optimization via Knuth's method
  - KDE (Kernel Density Estimation) overlay for smooth density visualization
  - Mode detection with statistical validation (ACR test)
  - Full statistical summary with bootstrap confidence intervals

  Use when investigating sample distribution shape, detecting multimodality,
  or visualizing benchmark latency distributions."
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:autocorrelation {:id :autocorrelation-raw}]
             [:autocorrelation-classification {:id :autocorrelation-classification-raw
                                               :autocorrelation-id :autocorrelation-raw}]
             [:quantiles {:quantiles [0.9 0.99]}]
             :outliers
             [:autocorrelation {:id :autocorrelation-filtered
                                :outlier-id :outliers}]
             [:effective-sample-size-analysis {:id :effective-sample-size-filtered
                                               :autocorrelation-id :autocorrelation-filtered}]
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]
                                :ess-id :effective-sample-size-filtered}]
             [:histogram {:method :knuth}]
             :kde
             :kde-stats
             :modes
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [:collect-plan
          [:stats {:metric-ids [:memory]}]
          :bootstrap-stats
          :extremes
          :shape-stats
          :outlier-counts
          [:autocorrelation-classification {:classification-id :autocorrelation-classification-raw}]
          [:effective-sample-size {:ess-id :effective-sample-size-filtered}]
          [:acf-plot {:autocorrelation-id :autocorrelation-raw
                      :min-severity :moderate}]
          :event-stats
          [:stats {:stats-id :kde-stats}]
          :quantiles
          [:histogram {:stats-id :log-stats}]
          [:kde {:histogram-id :histograms :modes-id :modes}]
          :sample-percentiles
          :samples
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap]
   :viewer :print})

(def distribution-analysis
  "Benchmark plan with distribution fitting and shape analysis.

  Fits parametric distributions (gamma, log-normal, inverse-gaussian, Weibull)
  to sample data using maximum likelihood estimation. Includes:
  - Shape statistics: skewness, kurtosis, coefficient of variation
  - Model selection via AIC/BIC with small-sample correction (AICc)
  - Goodness-of-fit testing: Kolmogorov-Smirnov, Cramér-von Mises
  - Bootstrap confidence intervals for best model parameters
  - PDF/CDF overlays and Q-Q plots for visual assessment

  Uses outlier-filtered samples by default. Distribution fit requires KDE
  analysis to run first (for visualization overlays).

  The analysis pipeline order is:
  1. transform-log - for log-scale analysis
  2. autocorrelation-raw - for sample independence analysis (pattern detection)
  3. autocorrelation-classification - for pattern classification
  4. quantiles - for percentile calculations
  5. outliers - for outlier detection
  6. autocorrelation-filtered - for effective sample size (on filtered samples)
  7. effective-sample-size-analysis - for ESS computation
  8. kde - required for distribution-fit visualizations
  9. bootstrap-stats - for shape statistics (skewness, kurtosis, CV)
  10. distribution-fit - MLE fitting with model selection"
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:autocorrelation {:id :autocorrelation-raw}]
             [:autocorrelation-classification {:id :autocorrelation-classification-raw
                                               :autocorrelation-id :autocorrelation-raw}]
             [:quantiles {:quantiles [0.9 0.99]}]
             :outliers
             [:autocorrelation {:id :autocorrelation-filtered
                                :outlier-id :outliers}]
             [:effective-sample-size-analysis {:id :effective-sample-size-filtered
                                               :autocorrelation-id :autocorrelation-filtered}]
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             :histogram
             :kde
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]
                                :ess-id :effective-sample-size-filtered}]
             :distribution-fit
             :outlier-significance
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type]
   :view [[:stats {:metric-ids [:memory]}]
          :bootstrap-stats
          :extremes
          :shape-stats
          [:autocorrelation-classification {:classification-id :autocorrelation-classification-raw}]
          [:effective-sample-size {:ess-id :effective-sample-size-filtered}]
          [:acf-plot {:autocorrelation-id :autocorrelation-raw
                      :min-severity :moderate}]
          :distribution-models
          :distribution-parameter-cis
          :distribution-pdf
          :distribution-cdf
          :distribution-qq
          :quantiles
          :event-stats
          :outlier-counts
          :outlier-significance
          :collect-plan
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type]
   :viewer :print})

(def tail-analysis
  "Benchmark plan for extreme value tail analysis.

  Analyzes the tail behavior of latency distributions to understand worst-case
  performance (p99, p999). Useful for SLA validation and understanding rare
  but impactful latency spikes.

  Includes:
  - Tail ratios (p99/p95, p999/p99) indicating tail heaviness
  - Hill estimator for tail index estimation
  - Generalized Pareto Distribution (GPD) fitting for exceedances
  - Mean residual life plot for threshold selection guidance
  - High quantile estimation via GPD extrapolation
  - Zipf plot (log-log complementary CDF)
  - Q-Q plots against exponential and GPD distributions

  Unlike other analyses, tail analysis uses raw samples WITHOUT outlier
  filtering because extreme values ARE the tail being analyzed. Only one
  autocorrelation analysis is computed (on raw samples) since there is no
  outlier filtering.

  Recommended: Use sufficient iterations (1000+ samples) for reliable
  tail estimation. The default collect plan targets adequate sample sizes."
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:autocorrelation {:id :autocorrelation-raw}]
             [:autocorrelation-classification {:id :autocorrelation-classification-raw
                                               :autocorrelation-id :autocorrelation-raw}]
             [:effective-sample-size-analysis {:id :effective-sample-size-raw
                                               :autocorrelation-id :autocorrelation-raw}]
             [:quantiles {:quantiles [0.9 0.95 0.99 0.999]}]
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99 0.999]
                                :estimate-quantiles [0.025 0.975]
                                :ess-id :effective-sample-size-raw}]
             :tail-analysis
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type]
   :view [[:stats {:metric-ids [:memory]}]
          :bootstrap-stats
          [:autocorrelation-classification {:classification-id :autocorrelation-classification-raw}]
          [:effective-sample-size {:ess-id :effective-sample-size-raw}]
          [:acf-plot {:autocorrelation-id :autocorrelation-raw
                      :min-severity :moderate}]
          :extremes
          :quantiles
          :tail-summary
          :tail-ratios
          :tail-high-quantiles
          :tail-ratios-chart
          :hill-plot
          :mrl-plot
          :zipf-plot
          :exponential-qq-plot
          :gpd-qq-plot
          :event-stats
          :collect-plan
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type]
   :viewer :print})
