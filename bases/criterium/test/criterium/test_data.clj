(ns criterium.test-data
    (:require
     [criterium.analyse.metrics-samples :as metrics-samples]
     [criterium.collect-plan :as collect-plan]
     [criterium.collector.metrics :as metrics]
     [criterium.metric :as metric]))

(defn bench-stats-map []
      {:data
       {:samples
        {:type           :criterium/metrics-samples
         :metrics-defs   (select-keys
                          (metrics/metrics)
                          [:elapsed-time])
         :metric->values {[:elapsed-time] []}
         :transform      collect-plan/identity-transforms
         :batch-size     1
         :eval-count     1
         :num-samples    0}
        :stats
        {:type         :criterium/stats
         :stats        {:elapsed-time
                        {:mean              100.0
                         :variance          16.0
                         :mean-plus-3sigma  112.0
                         :mean-minus-3sigma 88.0
                         :min-val           89.0
                         :max-val           114.0}}
         :transform    collect-plan/identity-transforms
         :batch-size   1
         :source-id    :samples
         :outliers-id  nil
         :metrics-defs (select-keys
                        (metrics/metrics)
                        [:elapsed-time])}}})

(defn samples-with-2-values-map []
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
           {:metrics-defs metrics-defs
            :data
            {:samples
             {:type           :criterium/metrics-samples
              :metrics-defs   metrics-defs
              :metric->values {[:elapsed-time] [1 1]}
              :transform      collect-plan/identity-transforms
              :batch-size     1
              :eval-count     2
              :num-samples    2
              :elapsed-time   1}}}))

(defn samples-with-transformed-values-map []
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
           {:metrics-defs metrics-defs
            :data
            {:samples
             {:type           :criterium/metrics-samples
              :metrics-defs   metrics-defs
              :metric->values {[:elapsed-time] [2 4 8]}
              :transform      (#'collect-plan/batch-transforms 2)
              :batch-size     3
              :eval-count     6
              :num-samples    3
              :elapsed-time   14}}}))

(defn samples-with-variance-12-map []
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
           {:metrics-defs metrics-defs
            :data
            {:samples
             {:type           :criterium/metrics-samples
              :metrics-defs   metrics-defs
              :metric->values {[:elapsed-time] [1 1 1 5 5 5 9 9 9]}
              :transform      collect-plan/identity-transforms
              :batch-size     1
              :eval-count     9
              :num-samples    9
              :elapsed-time   42}}}))

(defn samples-with-outliers-values-map []
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
           {:metrics-defs metrics-defs
            :data
            {:samples
             {:type           :criterium/metrics-samples
              :metrics-defs   metrics-defs
              :metric->values {[:elapsed-time] [9 10 9 10 9 10 10000]}
              :transform      collect-plan/identity-transforms
              :batch-size     1
              :num-samples    7
              :eval-count     1
              :elapsed-time   1}}}))

(defn outlier-count-map []
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
           {:data
            {:outliers
             {:type         :criterium/outliers
              :metrics-defs metrics-defs
              :outliers     {:elapsed-time
                             {:outlier-counts
                              (metrics-samples/outlier-count 0 2 3 0)}}
              :num-samples  1
              :source-id    :samples
              :quantiles-id :quantiles
              :transform    collect-plan/identity-transforms}}}))

(defn outlier-significance-map []
      (let [metrics-defs (-> (metrics/metrics)
                             (metric/select-metrics [:elapsed-time])
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))]
           {:data
            {:outlier-significance
             {:type                 :criterium/outlier-significance
              :outlier-significance {:elapsed-time
                                     {:effect       :moderate
                                      :significance 0.25}}
              :metrics-defs         metrics-defs
              :source-id            :samples
              :outliers-id          :outliers
              :transform            collect-plan/identity-transforms}}}))

(defn samples-for-event-stats-map
      []
      (let [metrics-defs
            (->
             (select-keys
              (metrics/metrics)
              [:class-loader :compilation])
             (assoc-in
              [:garbage-collector]
              {:type :event
               :groups
               {:total
                {:summary
                 (str "%s: ran %s times"
                      " for a total of %s in %s samples")
                 :values
                 [{:path      [:garbage-collector :total :count]
                   :scale     1
                   :type      :event
                   :label     "GC total count"
                   :dimension :count}
                  {:path      [:garbage-collector :total :time-ms]
                   :scale     1e-3
                   :type      :event
                   :label     "GC total time"
                   :dimension :time}]
                 :label "Garbage Collector"}}}))]
           {:data
            {:samples
             {:type           :criterium/metrics-samples
              :metrics-defs   metrics-defs
              :metric->values {[:elapsed-time]                      [1]
                               [:compilation :time-ms]              [3]
                               [:garbage-collector :total :time-ms] [1]
                               [:garbage-collector :total :count]   [2]
                               [:class-loader :loaded-count]        [1]
                               [:class-loader :unloaded-count]      [1]}
              :transform      collect-plan/identity-transforms
              :elapsed-time   0
              :num-samples    1
              :batch-size     1
              :eval-count     1
              :expr-value     1}}}))
