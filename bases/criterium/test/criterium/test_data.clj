(ns criterium.test-data
  (:require
   [criterium.analyse :as analyse]
   [criterium.analyse.metrics-samples :as metrics-samples]
   [criterium.array :as arr]
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
     :metric->values {[:elapsed-time] (arr/->double-array (double-array 0))}
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

(defn samples-with-1-value-map
  "Create a samples map with a single sample for :one-shot testing."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:metrics-defs metrics-defs
     :data
     {:samples
      {:type           :criterium/metrics-samples
       :metrics-defs   metrics-defs
       :metric->values {[:elapsed-time] (arr/->double-array (double-array [42.5]))}
       :transform      collect-plan/identity-transforms
       :batch-size     1
       :eval-count     1
       :num-samples    1
       :elapsed-time   1}}}))

(defn samples-with-2-values-map []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:metrics-defs metrics-defs
     :data
     {:samples
      {:type           :criterium/metrics-samples
       :metrics-defs   metrics-defs
       :metric->values {[:elapsed-time] (arr/->double-array (double-array [1.0 1.0]))
                        [:expr-value]   (arr/->object-array (object-array [42 42]))}
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
       :metric->values {[:elapsed-time] (arr/->double-array (double-array [2.0 4.0 8.0]))}
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
       :metric->values {[:elapsed-time] (arr/->double-array (double-array [1.0 1.0 1.0 5.0 5.0 5.0 9.0 9.0 9.0]))}
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
       :metric->values {[:elapsed-time] (arr/->double-array (double-array [9.0 10.0 9.0 10.0 9.0 10.0 10000.0]))}
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

(defn outlier-significance-nil-map
  "Creates an outlier-significance map with nil significance.
  This occurs when there are no positive outlier counts.
  Optionally specify which metrics to include (defaults to [:elapsed-time])."
  ([]
   (outlier-significance-nil-map [:elapsed-time]))
  ([metrics]
   (let [metrics-defs (-> (metrics/metrics)
                          (metric/select-metrics metrics)
                          (metric/filter-metrics
                           (metric/type-pred :quantitative)))]
     {:data
      {:outlier-significance
       {:type                 :criterium/outlier-significance
        :outlier-significance (zipmap metrics
                                      (repeat {:effect       nil
                                               :significance nil}))
        :metrics-defs         metrics-defs
        :source-id            :samples
        :outliers-id          :outliers
        :transform            collect-plan/identity-transforms}}})))

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
       :metric->values {[:elapsed-time] (arr/->object-array (object-array ["unavailable"]))}
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
       :metric->values {[:elapsed-time]                      (arr/->double-array (double-array [1.0]))
                        [:compilation :time-ms]              (arr/->long-array (long-array [3]))
                        [:garbage-collector :total :time-ms] (arr/->long-array (long-array [1]))
                        [:garbage-collector :total :count]   (arr/->long-array (long-array [2]))
                        [:class-loader :loaded-count]        (arr/->long-array (long-array [1]))
                        [:class-loader :unloaded-count]      (arr/->long-array (long-array [1]))}
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
       :metric->values {[:elapsed-time] (arr/->double-array (double-array [25.0 50.0 75.0]))}
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
      :metric->values {[:elapsed-time] (arr/->double-array (double-array [100.0 105.0 98.0 102.0 101.0]))}
      :transform      collect-plan/identity-transforms
      :batch-size     1
      :eval-count     5
      :num-samples    5}}))

(defn histogram-data-map
  "Create a data-map suitable for histogram-vega-spec testing."
  []
  (let [base-map (:data (samples-with-outliers-values-map))
        quantiles (analyse/quantiles {:quantiles [0.9 0.99]})
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

  Includes bootstrap-stats with bootstrap structure: mean, variance, and
  quantiles (0.1, 0.25, 0.5, 0.75, 0.9). Exercises the boxplot overlay code
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
            :source-id :samples
            :outliers-id nil})))

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

(defn bootstrap-stats-with-shape-map
  "Create a data-map with bootstrap-stats including skewness, kurtosis, and CV.
  Used for testing shape-stats views."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:bootstrap-stats
     {:type :criterium/bootstrap
      :bootstrap
      {:elapsed-time
       {:mean {:point-estimate 100.0
               :estimate-quantiles [{:value 95.0 :alpha 0.025}
                                    {:value 105.0 :alpha 0.975}]}
        :variance {:point-estimate 16.0
                   :estimate-quantiles [{:value 12.0 :alpha 0.025}
                                        {:value 20.0 :alpha 0.975}]}
        :min-val {:point-estimate 80.0
                  :estimate-quantiles [{:value 75.0 :alpha 0.025}
                                       {:value 85.0 :alpha 0.975}]}
        :max-val {:point-estimate 120.0
                  :estimate-quantiles [{:value 115.0 :alpha 0.025}
                                       {:value 125.0 :alpha 0.975}]}
        :skewness {:point-estimate 0.35
                   :estimate-quantiles [{:value 0.20 :alpha 0.025}
                                        {:value 0.50 :alpha 0.975}]}
        :kurtosis {:point-estimate 2.8
                   :estimate-quantiles [{:value 2.5 :alpha 0.025}
                                        {:value 3.1 :alpha 0.975}]}
        :cv {:point-estimate 0.04
             :estimate-quantiles [{:value 0.03 :alpha 0.025}
                                  {:value 0.05 :alpha 0.975}]}
        :mean-plus-3sigma {:point-estimate 112.0}
        :mean-minus-3sigma {:point-estimate 88.0}
        :quantiles {}}}
      :metrics-defs metrics-defs
      :transform collect-plan/identity-transforms
      :batch-size 1
      :source-id :samples
      :outliers-id nil}}))

(defn distribution-fit-data-map
  "Create a data-map with KDE, samples and distribution-fit data for testing
  PDF overlays. Includes successfully fitted distributions and one that was
  skipped."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:samples
     {:type :criterium/metrics-samples
      :metrics-defs metrics-defs
      :metric->values {[:elapsed-time] [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0]}
      :transform {:sample-> identity :->sample identity}
      :batch-size 1
      :eval-count 9
      :num-samples 9}
     :kde {:type :criterium/kde
           :metrics-defs metrics-defs
           :transform {:sample-> identity :->sample identity}
           :kdes {[:elapsed-time]
                  {:type :criterium/kde
                   :bandwidth 0.5
                   :grid [1.0 2.0 3.0 4.0 5.0]
                   :density [0.1 0.25 0.3 0.25 0.1]
                   :lower-band [0.08 0.20 0.25 0.20 0.08]
                   :upper-band [0.12 0.30 0.35 0.30 0.12]
                   :n 100}}}
     :distribution-fit
     {:type :criterium/distribution-fit
      :transform collect-plan/identity-transforms
      :fits {[:elapsed-time]
             {:n 100
              :best-model :gamma
              :sample-range [1.0 5.0]
              :distributions
              {:gamma {:params {:shape 2.0 :scale 1.5}
                       :log-likelihood -150.0
                       :aic 304.0
                       :bic 309.2
                       :aicc 304.1
                       :delta-aic 0.0
                       :ks-test {:statistic 0.05 :p-value 0.85}
                       :cvm-test {:statistic 0.02 :p-value 0.90}}
               :lognormal {:params {:mu 0.5 :sigma 0.8}
                           :log-likelihood -155.0
                           :aic 314.0
                           :bic 319.2
                           :aicc 314.1
                           :delta-aic 10.0
                           :ks-test {:statistic 0.08 :p-value 0.45}
                           :cvm-test {:statistic 0.05 :p-value 0.50}}
               :weibull {:params {:shape 1.8 :scale 3.2}
                         :log-likelihood -152.0
                         :aic 308.0
                         :bic 313.2
                         :aicc 308.1
                         :delta-aic 4.0
                         :ks-test {:statistic 0.06 :p-value 0.70}
                         :cvm-test {:statistic 0.03 :p-value 0.75}}
               :inverse-gaussian {:skipped :moment-match-failed
                                  :prefilter-result {:valid? false
                                                     :reason :negative-lambda}}}
              :parameter-cis {:gamma {:shape {:point-estimate 2.0
                                              :ci-lower 1.7
                                              :ci-upper 2.3}
                                      :scale {:point-estimate 1.5
                                              :ci-lower 1.2
                                              :ci-upper 1.8}}}}}}}))

(defn distribution-cdf-data-map
  "Create a data-map with samples and distribution-fit data for testing CDF overlays.
  Includes sample data for ECDF and fitted distributions for CDF curves."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:samples
     {:type :criterium/metrics-samples
      :metrics-defs metrics-defs
      :metric->values {[:elapsed-time] (arr/->double-array
                                        (double-array [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0]))}
      :transform {:sample-> identity :->sample identity}
      :batch-size 1
      :eval-count 9
      :num-samples 9}
     :distribution-fit
     {:type :criterium/distribution-fit
      :transform {:sample-> identity :->sample identity}
      :fits {[:elapsed-time]
             {:n 9
              :best-model :gamma
              :distributions
              {:gamma {:params {:shape 2.0 :scale 1.5}
                       :log-likelihood -15.0
                       :aic 34.0
                       :bic 34.6
                       :aicc 36.0
                       :delta-aic 0.0
                       :ks-test {:statistic 0.1 :p-value 0.9}
                       :cvm-test {:statistic 0.05 :p-value 0.85}}
               :lognormal {:params {:mu 0.5 :sigma 0.6}
                           :log-likelihood -16.0
                           :aic 36.0
                           :bic 36.6
                           :aicc 38.0
                           :delta-aic 2.0
                           :ks-test {:statistic 0.12 :p-value 0.8}
                           :cvm-test {:statistic 0.06 :p-value 0.75}}
               :weibull {:params {:shape 2.0 :scale 3.0}
                         :log-likelihood -15.5
                         :aic 35.0
                         :bic 35.6
                         :aicc 37.0
                         :delta-aic 1.0
                         :ks-test {:statistic 0.11 :p-value 0.85}
                         :cvm-test {:statistic 0.055 :p-value 0.8}}}}}}}))

(defn distribution-qq-data-map
  "Create a data-map with samples and distribution-fit data for testing Q-Q plots.
  Includes sample data and fitted distributions for Q-Q scatter overlays."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])]
    {:samples
     {:type :criterium/metrics-samples
      :metrics-defs metrics-defs
      :metric->values {[:elapsed-time] (arr/->double-array
                                        (double-array [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0]))}
      :transform {:sample-> identity :->sample identity}
      :batch-size 1
      :eval-count 9
      :num-samples 9}
     :distribution-fit
     {:type :criterium/distribution-fit
      :transform {:sample-> identity :->sample identity}
      :fits {[:elapsed-time]
             {:n 9
              :best-model :gamma
              :distributions
              {:gamma {:params {:shape 2.0 :scale 1.5}
                       :log-likelihood -15.0
                       :aic 34.0
                       :bic 34.6
                       :aicc 36.0
                       :delta-aic 0.0
                       :ks-test {:statistic 0.1 :p-value 0.9}
                       :cvm-test {:statistic 0.05 :p-value 0.85}}
               :lognormal {:params {:mu 0.5 :sigma 0.6}
                           :log-likelihood -16.0
                           :aic 36.0
                           :bic 36.6
                           :aicc 38.0
                           :delta-aic 2.0
                           :ks-test {:statistic 0.12 :p-value 0.8}
                           :cvm-test {:statistic 0.06 :p-value 0.75}}
               :weibull {:params {:shape 2.0 :scale 3.0}
                         :log-likelihood -15.5
                         :aic 35.0
                         :bic 35.6
                         :aicc 37.0
                         :delta-aic 1.0
                         :ks-test {:statistic 0.11 :p-value 0.85}
                         :cvm-test {:statistic 0.055 :p-value 0.8}}}}}}}))

(defn tail-analysis-data-map
  "Create a data-map with samples and tail-analysis data for testing tail charts.
  Includes sample data and tail analysis results (Hill, GPD, MRL, tail ratios)."
  []
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
        ;; Generate samples with a heavy tail - log-normal style
        samples (double-array [1.0 1.2 1.5 1.8 2.0 2.2 2.5 2.8 3.0 3.5
                               4.0 4.5 5.0 6.0 7.0 8.0 10.0 12.0 15.0 20.0])]
    {:samples
     {:type :criterium/metrics-samples
      :metrics-defs metrics-defs
      :metric->values {[:elapsed-time] (arr/->double-array samples)}
      :transform {:sample-> identity :->sample identity}
      :batch-size 1
      :eval-count 20
      :num-samples 20}
     :tail-analysis
     {:type :criterium/tail-analysis
      :transform {:sample-> identity :->sample identity}
      :metrics-defs metrics-defs
      :tail-analysis
      {[:elapsed-time]
       {:n 20
        :threshold 5.0
        :threshold-quantile 0.9
        :tail-ratios {:p99-p95 1.5 :p999-p99 1.8 :p999-p95 2.7}
        :hill {:k-range [3 4 5 6 7 8]
               :estimates [0.8 0.85 0.82 0.81 0.83 0.84]
               :tail-indices [1.25 1.18 1.22 1.23 1.20 1.19]
               :stable-estimate 0.82}
        :gpd {:threshold 5.0
              :xi 0.3
              :sigma 2.5
              :log-likelihood -25.0
              :exceedances-count 6}
        :mrl {:thresholds [2.0 3.0 4.0 5.0 6.0 7.0]
              :values [4.5 5.2 6.0 7.0 8.5 10.0]
              :n-exceed [15 12 9 6 4 2]}
        :high-quantiles {0.99 18.0 0.999 25.0 0.9999 35.0}
        :empirical-quantiles {:p95 12.0 :p99 18.0 :p999 20.0}}}}}))
