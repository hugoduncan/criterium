(ns criterium.test-data
  (:require
   [criterium.analyse :as analyse]
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
                     :median            100.0
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
       :metric->values {[:elapsed-time] [1 1]
                        [:expr-value]   [42 42]}
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

(defn samples-with-non-numeric-value-map
  "Creates a samples map with a non-numeric metric value for testing viewer
  error handling."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:metrics-defs metrics-defs
     :data
     {:samples
      {:type           :criterium/metrics-samples
       :metrics-defs   metrics-defs
       :metric->values {[:elapsed-time] ["unavailable"]}
       :transform      collect-plan/identity-transforms
       :batch-size     1
       :eval-count     1
       :num-samples    1
       :elapsed-time   1}}}))

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

(defn quantiles-map []
  (let [metrics-defs       (select-keys (metrics/metrics) [:elapsed-time])
        ;; Filter to only quantitative metrics (matches analyse.clj quantiles behavior)
        quant-metrics-defs (metric/filter-metrics
                            metrics-defs
                            (metric/type-pred :quantitative))]
    {:data
     {:samples
      {:type           :criterium/metrics-samples
       :metrics-defs   metrics-defs
       :metric->values {[:elapsed-time] [25 50 75]}
       :transform      collect-plan/identity-transforms
       :batch-size     1
       :eval-count     3
       :num-samples    3}
      :quantiles
      {:type         :criterium/quantiles
       :metrics-defs quant-metrics-defs
       :quantiles    {:elapsed-time {0.25 25.0 0.5 50.0 0.75 75.0}}
       :source-id    :samples
       :transform    collect-plan/identity-transforms}}}))

(defn collect-plan-map []
  {:data
   {:samples    {:batch-size   10
                 :num-samples  100}
    :warmup     {:batch-size   5
                 :num-samples  50}
    :estimation {:batch-size   1
                 :num-samples  10}}})

;;; Chart test data factories

(defn samples-data-map
  "Create a data-map suitable for samples-vega-spec testing."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:samples
     {:type           :criterium/metrics-samples
      :metrics-defs   metrics-defs
      :metric->values {[:elapsed-time] [100 105 98 102 101]}
      :transform      collect-plan/identity-transforms
      :batch-size     1
      :eval-count     5
      :num-samples    5}}))

(defn histogram-data-map
  "Create a data-map suitable for histogram-vega-spec testing."
  []
  (let [base-map (:data (samples-with-outliers-values-map))
        quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
        outliers (analyse/outliers)
        stats (analyse/stats)
        histogram (analyse/histogram)]
    (->> base-map
         quantiles
         outliers
         stats
         histogram)))

(defn histogram-with-bootstrap-data-map
  "Create a data-map suitable for histogram-vega-spec with boxplot overlay.

  Includes bootstrap-stats with complete bootstrap structure matching real
  bootstrap output: mean, variance, min-val, mean-plus-3sigma, mean-minus-3sigma,
  and quantiles (0.1, 0.25, 0.5, 0.75, 0.9). Exercises the boxplot overlay code
  path with median CI and spread percentiles."
  []
  (let [base-map (histogram-data-map)
        metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    (assoc base-map
           :bootstrap-stats
           {:type :criterium/bootstrap
            :bootstrap
            {:elapsed-time
             {:mean {:point-estimate 9.8
                     :estimate-quantiles [{:value 9.5 :alpha 0.025}
                                          {:value 10.1 :alpha 0.975}]}
              :variance {:point-estimate 0.5
                         :estimate-quantiles [{:value 0.3 :alpha 0.025}
                                              {:value 0.8 :alpha 0.975}]}
              :min-val {:point-estimate 9.0
                        :estimate-quantiles [{:value 8.8 :alpha 0.025}
                                             {:value 9.2 :alpha 0.975}]}
              :mean-plus-3sigma {:point-estimate 11.9
                                 :estimate-quantiles [{:value 11.1 :alpha 0.025}
                                                      {:value 12.7 :alpha 0.975}]}
              :mean-minus-3sigma {:point-estimate 7.7
                                  :estimate-quantiles [{:value 6.9 :alpha 0.025}
                                                       {:value 8.5 :alpha 0.975}]}
              :quantiles
              {0.1 {:point-estimate 9.2
                    :estimate-quantiles []}
               0.25 {:point-estimate 9.5
                     :estimate-quantiles []}
               0.5 {:point-estimate 9.75
                    :estimate-quantiles [{:value 9.3 :alpha 0.025}
                                         {:value 10.2 :alpha 0.975}]}
               0.75 {:point-estimate 10.0
                     :estimate-quantiles []}
               0.9 {:point-estimate 10.5
                    :estimate-quantiles []}}}}
            :metrics-defs metrics-defs
            :transform collect-plan/identity-transforms
            :batch-size 1
            :source-id :samples})))

(defn kde-data-map
  "Create a data-map suitable for kde-vega-spec testing."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:kde {:type :criterium/kde
           :metrics-defs metrics-defs
           :transform {:sample-> identity :->sample identity}
           :kdes {[:elapsed-time]
                  {:type :criterium/kde
                   :bandwidth 0.5
                   :grid [1.0 2.0 3.0 4.0 5.0]
                   :density [0.1 0.25 0.3 0.25 0.1]
                   :lower-band [0.08 0.20 0.25 0.20 0.08]
                   :upper-band [0.12 0.30 0.35 0.30 0.12]
                   :n 100}}}}))


