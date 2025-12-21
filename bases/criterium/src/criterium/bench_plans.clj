(ns criterium.bench-plans
  "Provide pre-configured benchmark definitions.")

(def default-collector-config
  {:stages     []
   :terminator :elapsed-time})

(def default-one-shot
  {:collector-config default-collector-config
   :analyse          [:event-stats]
   :view             [:metrics
                      :event-stats
                      :collect-plan]
   :viewer           :print})

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
                      :allocation-by-type]
   :view             [[:stats {:metric-ids [:memory]}]
                      [:stats {:stats-id :log-stats}]
                      :event-stats
                      :collect-plan
                      :allocation-summary
                      :allocation-hotspots
                      :allocation-by-type
                      #_[:final-gc-warnings
                         {:warn-threshold 0.01}]]
   :viewer           :print})

(def log-histogram
  {:collector-config default-collector-config
   :analyse          [:transform-log
                      [:quantiles {:quantiles [0.9 0.99 0.99]}]
                      :outliers
                      [:stats {}]
                      [:stats {:samples-id :log-samples :id :log-stats}]
                      :histogram
                      :event-stats
                      :allocation-summary
                      [:allocation-hotspots {:limit 10}]
                      :allocation-by-type]
   :view             [[:stats {:metric-ids [:memory]}]
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
                      :allocation-by-type]
   :viewer           :print})
