(ns criterium.analyse-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.benchmark :as benchmark]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.helpers :as util]
   [criterium.test-data :as test-data]))

(deftest outlier-significance-impl--test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier significance"
    (let [batch-size 67108864]
      (is (= 0.9960022873987793
             (analyse/outlier-significance*
              (/ 1.395522860870968 batch-size)
              (/ (* 0.0013859776344426547 0.0013859776344426547)
                 batch-size)
              batch-size))))))

(deftest transform-log-test
  (testing "transform-log"
    (let [bench-map {:data
                     {:samples
                      {:type         :criterium/collected-metrics-samples
                       :metric->values
                       {[:elapsed-time]         [(Math/exp 1)
                                                 (Math/exp 2)
                                                 (Math/exp 3)]
                        [:compilation :time-ms] [0 0 0]}
                       :transform    collect-plan/identity-transforms
                       :batch-size   1
                       :eval-count   4
                       :metrics-defs (select-keys
                                      (metrics/metrics)
                                      [:compilation :elapsed-time])}}}
          result    ((analyse/transform-log) bench-map)]
      (testing "puts the log transformed metrics into the result-path"
        (is (= [1.0 2.0 3.0]
               (-> result
                   :data
                   :log-samples
                   :metric->values
                   (get [:elapsed-time])))))
      (testing "adds transfprms for the values"
        (let [transforms (util/get-transforms (:data result) :log-samples)
              vs         (mapv
                          #(util/transform-sample-> % transforms)
                          (get
                           (-> result :data :log-samples :metric->values)
                           [:elapsed-time]))]
          (is (seq transforms))
          (is (= [(Math/exp 1) (Math/exp 2) (Math/exp 3)]
                 vs))))
      (testing "doesn't transform event-metrics "
        (is (not (contains? (:log-samples result) [:compilation])))))))

(deftest quantiles-test
  (testing "stats"
    (let [bench-map
          {:data
           {:samples
            {:type           :criterium/collected-metrics-samples
             :metric->values {[:elapsed-time]         [1 2 3]
                              [:compilation :time-ms] [0 0 0]}
             :transform      collect-plan/identity-transforms
             :batch-size     1
             :eval-count     3
             :metrics-defs   (select-keys
                              (metrics/metrics)
                              [:compilation :elapsed-time])}}}
          result ((analyse/quantiles {:quantiles [0.025 0.975]})
                  bench-map)]
      (testing "puts the quantiles into the result-path"
        (is (= {0.25  1.5,
                0.5   2.0,
                0.75  2.5,
                0.025 1.05,
                0.975 2.9499999999999997}
               (->> result :data :quantiles util/quantiles :elapsed-time))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :quantiles)))
        (is (= [:elapsed-time]
               (->> result :data :quantiles util/quantiles keys)))))))

(deftest outliers-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outliers"
    (let [bench-map (test-data/samples-with-outliers-values-map)
          quantiles (analyse/quantiles {:quantiles []})
          outliers  (analyse/outliers)]
      (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
             (-> bench-map
                 quantiles
                 outliers
                 :data
                 :outliers
                 util/outliers
                 :elapsed-time
                 :outlier-counts))))))

(deftest outlier-counts-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier counts"
    (let [bench-map
          {:data
           {:samples
            {:type           :criterium/collected-metrics-samples
             :metric->values {[:elapsed-time] [1 1 1 1000]}
             :transform      collect-plan/identity-transforms
             :batch-size     1
             :eval-count     4
             :metrics-defs   (select-keys
                              (metrics/metrics)
                              [:elapsed-time])}}}
          benchmark (benchmark/->benchmark
                     {:analyse
                      [[:quantiles {:quantiles [0.025 0.975]}]
                       :outliers]})]
      (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
             (-> (benchmark bench-map)
                 :data
                 :outliers
                 util/outliers
                 :elapsed-time
                 :outlier-counts))))))

(deftest stats-test
  (testing "stats"
    (let [bench-map
          {:data
           {:samples
            {:type           :criterium/collected-metrics-samples
             :metric->values {[:elapsed-time]         [1 2 3]
                              [:compilation :time-ms] [0 0 0]}
             :transform      collect-plan/identity-transforms
             :batch-size     1
             :eval-count     3
             :metrics-defs   (select-keys
                              (metrics/metrics)
                              [:compilation :elapsed-time])}}}
          result ((analyse/stats) bench-map)]
      (testing "puts the stats into the result-path"
        (is (= {:min-val           1.0,
                :max-val           3.0,
                :mean              2.0,
                :mean-plus-3sigma  5.0,
                :variance          1.0,
                :mean-minus-3sigma -1.0}
               (->> result :data :stats util/stats :elapsed-time))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :stats)))
        (is (= [:elapsed-time]
               (->> result :data :stats util/stats keys))))))
  (testing "stats variance"
    (let [bench-map (test-data/samples-with-variance-12-map)
          result    ((analyse/stats) bench-map)]
      (testing "calculates sample variance"
        (is (= 12.0 (:variance
                     (->> result :data :stats util/stats :elapsed-time))))))
    (let [bench-map (update-in
                     (test-data/samples-with-variance-12-map)
                     [:data :samples]
                     merge
                     {:batch-size 2
                      :transform  (#'collect-plan/batch-transforms 2)})
          result    ((analyse/stats) bench-map)]
      (testing "scales with batch size"
        (is (= 6.0 (:variance
                    (->> result :data :stats util/stats :elapsed-time)))))))
  (testing "excludes outliers"
    (let [bench-map (test-data/samples-with-outliers-values-map)
          quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
          outliers  (analyse/outliers)
          stats     (analyse/stats)
          result    (-> bench-map
                        quantiles
                        outliers
                        stats)]
      (testing "calculates sample variance"
        (test-max-error
         0.3
         (:variance (->> result :data :stats util/stats :elapsed-time))
         1e-5)))
    (let [bench-map (update-in
                     (test-data/samples-with-variance-12-map)
                     [:data :samples]
                     merge
                     {:batch-size 2
                      :transform  (#'collect-plan/batch-transforms 2)})
          result    ((analyse/stats) bench-map)]
      (testing "scales with batch size"
        (is (= 6.0 (:variance
                    (->> result :data :stats util/stats :elapsed-time))))))))

(deftest event-stats-test
  (testing "event-stats"
    (let [bench-map
          {:data
           {:samples
            {:type           :criterium/collected-metrics-samples
             :metrics-defs
             (-> (select-keys
                  (metrics/metrics)
                  [:class-loader :compilation
                   :elapsed-time :garbage-collector])
                 (assoc-in
                  [:garbage-collector]
                  {:type :event
                   :groups
                   {:total
                    {:summary
                     (str "%32s: ran %s times"
                          " for a total of %s in %s samples")
                     :values
                     [{:path      [:garbage-collector :total :count]
                       :scale     1
                       :dimension :count
                       :label     "GC total count"
                       :type      :event}
                      {:path      [:garbage-collector :total :time-ms]
                       :scale     1e-3
                       :dimension :time
                       :label     "GC total time"
                       :type      :event}]
                     :label "Garbage Collector"}}}))
             :metric->values {[:elapsed-time]                      [1 2 3]
                              [:compilation :time-ms]              [3 5 0]
                              [:garbage-collector :total :time-ms] [1 1 1]
                              [:garbage-collector :total :count]   [2 1 1]
                              [:class-loader :loaded-count]        [2 2 0]
                              [:class-loader :unloaded-count]      [0 0 0]}
             :batch-size     1
             :eval-count     3}}}
          result ((analyse/event-stats) bench-map)]
      (testing "puts the event-stats into the output-path"
        (is (= {[:class-loader :loaded-count]             4,
                [:class-loader :unloaded-count]           0,
                [:class-loader :sample-count]             2,
                [:compilation :time-ms]                   8,
                [:compilation :sample-count]              2,
                [:garbage-collector :total :count]        4,
                [:garbage-collector :total :time-ms]      3,
                [:garbage-collector :total :sample-count] 3}
               #_{:compilation       {:time-ms 8 :sample-count 2}
                  :garbage-collector {:total
                                      {:time-ms 3 :count 4 :sample-count 3}}
                  :class-loader      {:sample-count 2
                                      :loaded-count 4 :unloaded-count 0}}
               (->> result :data :event-stats util/event-stats)))))))

(deftest outlier-effect-test
  (is (= :unaffected (analyse/outlier-effect 0.009)))
  (is (= :slight (analyse/outlier-effect 0.09)))
  (is (= :moderate (analyse/outlier-effect 0.49)))
  (is (= :severe (analyse/outlier-effect 0.51))))

(deftest outlier-significance-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier counts"
    (let [bench-map
          {:data
           {:samples
            {:type           :criterium/collected-metrics-samples
             :metric->values {[:elapsed-time] [1 1 1 1000]}
             :transform      collect-plan/identity-transforms
             :batch-size     1
             :eval-count     4
             :metrics-defs   (select-keys
                              (metrics/metrics)
                              [:elapsed-time])}}}
          benchmark (benchmark/->benchmark
                     {:analyse
                      [[:quantiles {:quantiles [0.025 0.975]}]
                       :outliers
                       :stats
                       :outlier-significance]})]

      (is (= {:significance 0
              :effect       :unaffected}
             (-> (benchmark bench-map)
                 :data
                 :outlier-significance
                 util/outlier-significance
                 :elapsed-time))))))
