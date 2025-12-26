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
   :analyse          [:transform-log
                      [:quantiles {:quantiles [0.9 0.99 0.99]}]
                      :outliers
                      [:stats {}]
                      [:stats {:samples-id :log-samples :id :log-stats}]
                      :event-stats
                      :allocation-summary
                      [:allocation-hotspots {:limit 10}]
                      :allocation-by-type
                      :allocation-treemap]
   :view             [[:stats {:metric-ids [:memory]}]
                      [:stats {:stats-id :log-stats}]
                      :event-stats
                      :outlier-counts
                      :collect-plan
                      :allocation-summary
                      :allocation-hotspots
                      :allocation-by-type
                      :allocation-treemap
                      #_[:final-gc-warnings
                         {:warn-threshold 0.01}]]
   :viewer           :print})

(def log-histogram
  {:collector-config default-collector-config
   :analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             :histogram
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
             :histogram
             :kde
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
          [:kde {:histogram-id :histograms}]
          :sample-percentiles
          :samples
          :allocation-summary
          :allocation-hotspots
          :allocation-by-type
          :allocation-treemap]
   :viewer :print})
