(ns criterium.bench-plans
  "Provide pre-configured benchmark definitions.")

(def default-collector-config
  {:stages []
   :terminator :elapsed-time})

(def default-one-shot
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

(def default-with-warmup
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]}]
             :kde
             :modes
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [[:stats {:metric-ids [:memory]}]
          :bootstrap-stats
          [:stats {:stats-id :log-stats}]
          [:multimodal-warning {:modes-id :modes}]
          :event-stats
          :outlier-counts
          :collect-plan
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap
          #_[:final-gc-warnings
             {:warn-threshold 0.01}]]
   :viewer :print})

(def log-histogram
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]}]
             :histogram
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [[:stats {:metric-ids [:memory]}]
          :bootstrap-stats
          [:stats {:stats-id :log-stats}]
          :quantiles
          :event-stats
          :outlier-counts
          :collect-plan
          [:histogram {:stats-id :log-stats}]
          :sample-percentiles
          :samples
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap]
   :viewer :print})

(def knuth-histogram
  "Benchmark plan using Knuth's Bayesian optimal histogram binning.

  Uses Knuth's method to automatically determine the optimal number of
  histogram bins by maximizing a log-posterior. Better than Freedman-Diaconis
  for distributions with complex structure.

  The histogram includes :optimal-bins and :log-posterior keys."
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]}]
             [:histogram {:method :knuth}]
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [[:stats {:metric-ids [:memory]}]
          [:stats {:stats-id :log-stats}]
          :quantiles
          :event-stats
          :outlier-counts
          :collect-plan
          [:histogram {:stats-id :log-stats}]
          :sample-percentiles
          :samples
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap]
   :viewer :print})

(def kde-histogram
  "Benchmark plan with KDE analysis for density estimation and mode detection.

  Includes histogram and KDE analysis for visualizing sample distributions.
  Not part of default-with-warmup; use explicitly when density analysis is needed."
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]}]
             :histogram
             :kde
             :kde-stats
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [[:stats {:metric-ids [:memory]}]
          [:stats {:stats-id :log-stats}]
          [:stats {:stats-id :kde-stats}]
          :quantiles
          :event-stats
          :outlier-counts
          :collect-plan
          [:histogram {:stats-id :log-stats}]
          [:kde {:histogram-id :histograms}]
          :sample-percentiles
          :samples
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap]
   :viewer :print})

(def kde-modes
  "Benchmark plan with KDE and mode detection using ACR test.

  Includes histogram, KDE, and statistically validated mode analysis.
  Use when you need to detect and validate multimodality in sample distributions.
  Mode detection tests from k=1 up to max-modes. Supports ACR (default) and
  Silverman test methods via :modes analysis options."
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]}]
             :histogram
             :kde
             :kde-stats
             :modes
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type
             :allocation-treemap]
   :view [[:stats {:metric-ids [:memory]}]
          [:stats {:stats-id :log-stats}]
          [:stats {:stats-id :kde-stats}]
          :quantiles
          :event-stats
          :outlier-counts
          :collect-plan
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
  2. quantiles - for percentile calculations
  3. outliers - for outlier detection
  4. kde - required for distribution-fit visualizations
  5. bootstrap-stats - for shape statistics (skewness, kurtosis, CV)
  6. distribution-fit - MLE fitting with model selection"
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             :histogram
             :kde
             [:bootstrap-stats {:quantiles [0.99]
                                :estimate-quantiles [0.025 0.975]}]
             :distribution-fit
             :outlier-significance
             :event-stats
             :allocation-summary
             [:allocation-hotspots {:limit 10}]
             :allocation-by-type]
   :view [[:stats {:metric-ids [:memory]}]
          [:stats {:stats-id :log-stats}]
          :bootstrap-stats
          :shape-stats
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
