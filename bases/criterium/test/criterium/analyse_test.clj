(ns criterium.analyse-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.benchmark :as benchmark]
   [criterium.collector.metrics :as metrics]
   [criterium.util.helpers :as util]))

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

(defn identity-transform [samples]
  (with-meta samples {:transform {:sample-> identity :->sample identity}}))

(deftest transform-log-test
  (testing "transform-log"
    (let [sampled {:samples         (identity-transform
                                     {[:elapsed-time]
                                      [(Math/exp 1) (Math/exp 2) (Math/exp 3)]
                                      [:compilation :time-ms]
                                      [0 0 0]})
                   :batch-size      1
                   :eval-count      4
                   :metrics-configs (select-keys
                                     (metrics/metrics)
                                     [:compilation :elapsed-time])}
          result  ((analyse/transform-log) sampled)]
      (testing "puts the log transformed metrics into the result-path"
        (is (= [1.0 2.0 3.0]
               (get (:log-samples result) [:elapsed-time]))))
      (testing "adds transfprms for the values"
        (let [transforms (util/get-transforms result :log-samples)
              vs         (mapv
                          #(util/transform-sample-> % transforms)
                          (get (:log-samples result) [:elapsed-time]))]
          (is (seq transforms))
          (is (= [(Math/exp 1) (Math/exp 2) (Math/exp 3)]
                 vs))))
      (testing "doesn't transform event-metrics "
        (is (not (contains? (:log-samples result) [:compilation])))))))

(deftest quantiles-test
  (testing "stats"
    (let [sampled {:samples         (identity-transform
                                     {[:elapsed-time]         [1 2 3]
                                      [:compilation :time-ms] [0 0 0]})
                   :batch-size      1
                   :eval-count      3
                   :metrics-configs (select-keys
                                     (metrics/metrics)
                                     [:compilation :elapsed-time])}
          result  ((analyse/quantiles
                    {:quantiles [0.025 0.975]})
                   sampled)]
      (testing "puts the quantiles into the result-path"
        (is (= {0.25  1.5,
                0.5   2.0,
                0.75  2.5,
                0.025 1.05,
                0.975 2.9499999999999997}
               (->> result :quantiles :elapsed-time))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :quantiles)))
        (is (= [:elapsed-time]
               (->> result :quantiles keys)))))))

(deftest outlier-counts-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier counts"
    (let [sampled   {:samples         (identity-transform
                                       {[:elapsed-time] [1 1 1 1000]})
                     :batch-size      1
                     :eval-count      4
                     :metrics-configs (select-keys
                                       (metrics/metrics)
                                       [:elapsed-time])}
          benchmark (benchmark/->benchmark
                     {:analyse
                      [[:quantiles {:quantiles [0.025 0.975]}]
                       :outliers]})]
      (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
             (-> (benchmark sampled)
                 :outliers
                 :elapsed-time
                 :outlier-counts))))))

(deftest stats-test
  (testing "stats"
    (let [sampled {:samples         (identity-transform
                                     {[:elapsed-time]         [1 2 3]
                                      [:compilation :time-ms] [0 0 0]})
                   :batch-size      1
                   :eval-count      3
                   :metrics-configs (select-keys
                                     (metrics/metrics)
                                     [:compilation :elapsed-time])}
          result  ((analyse/stats) sampled)]
      (testing "puts the stats into the result-path"
        (is (= {:min-val           1.0,
                :max-val           3.0,
                :mean              2.0,
                :mean-plus-3sigma  5.0,
                :variance          1.0,
                :mean-minus-3sigma -1.0}
               (->> result :stats :elapsed-time))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :stats)))
        (is (= [:elapsed-time]
               (->> result :stats keys)))))))

(deftest event-stats-test
  (testing "event-stats"
    (let [sampled {:metrics-configs
                   (-> (select-keys
                        (metrics/metrics)
                        [:class-loader :compilation
                         :elapsed-time])
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
                             :dimension :count}
                            {:path      [:garbage-collector :total :time-ms]
                             :scale     1e-3
                             :dimension :time}]
                           :label "Garbage Collector"}}}))
                   :samples    (identity-transform
                                {[:elapsed-time]                      [1 2 3]
                                 [:compilation :time-ms]              [3 5 0]
                                 [:garbage-collector :total :time-ms] [1 1 1]
                                 [:garbage-collector :total :count]   [2 1 1]
                                 [:class-loader :loaded-count]        [2 2 0]
                                 [:class-loader :unloaded-count]      [0 0 0]})
                   :batch-size 1
                   :eval-count 3}
          result  ((analyse/event-stats) sampled)]
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
               (->> result :event-stats)))))))

(deftest outlier-effect-test
  (is (= :unaffected (analyse/outlier-effect 0.009)))
  (is (= :slight (analyse/outlier-effect 0.09)))
  (is (= :moderate (analyse/outlier-effect 0.49)))
  (is (= :severe (analyse/outlier-effect 0.51))))

(deftest outlier-significance-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier counts"
    (let [sampled   {:samples         (identity-transform
                                       {[:elapsed-time] [1 1 1 1000]})
                     :batch-size      1
                     :eval-count      4
                     :metrics-configs (select-keys
                                       (metrics/metrics)
                                       [:elapsed-time])}
          benchmark (benchmark/->benchmark
                     {:analyse
                      [[:quantiles {:quantiles [0.025 0.975]}]
                       :outliers
                       :stats
                       :outlier-significance]})]

      (is (= {:significance 0
              :effect       :unaffected}
             (-> (benchmark sampled)
                 :outlier-significance
                 :elapsed-time))))))
