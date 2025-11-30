(ns criterium.analyse-test
    (:require
     [clojure.test :refer [deftest is testing]]
     [criterium.analyse :as analyse]
     [criterium.benchmark :as benchmark]
     [criterium.collect-plan :as collect-plan]
     [criterium.collector.metrics :as metrics]
     [criterium.test-utils :refer [approx=]]
     [criterium.types :as types]
     [criterium.util.helpers :as util]
     [criterium.util.invariant :refer [have?]]))

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

(defn metrics-samples
      [data ^long batch-size]
      {:post [(have? types/metrics-samples-map? %)]}
      (let [n (count (first (vals data)))]
           {:type           :criterium/metrics-samples
            :metric->values data
            :transform      (if (= batch-size 1)
                                collect-plan/identity-transforms
                                (#'collect-plan/batch-transforms batch-size))
            :num-samples    n
            :batch-size     batch-size
            :eval-count     (* n batch-size)
            :metrics-defs   (select-keys
                             (metrics/metrics)
                             (mapv first (keys data)))
            :source-id      nil
            :expr-value     (ffirst (vals data))}))

(defn transformed-metric-values
      [data-map id p]
      (let [m          (-> data-map id)
            transforms (util/get-transforms data-map id)]
           (mapv
            #(util/transform-sample-> % transforms)
            (get (:metric->values m) p))))

(defn transformed-values
      [data-map id vs]
      (let [transforms (util/get-transforms data-map id)]
           (prn :transforms transforms)
           (mapv
            #(util/transform-sample-> % transforms)
            vs)))

(deftest transform-log-test
         (testing "transform-log"
                  (let [raw-data [(Math/exp 1) (Math/exp 2) (Math/exp 3)]
                        samples  (metrics-samples
                                  {[:elapsed-time]         raw-data
                                   [:compilation :time-ms] [0 0 0]}
                                  10)
                        data-map {:samples samples}
                        result   ((analyse/transform-log) data-map)]
                       (testing "puts the log transformed metrics into the result-path"
                                (is (= [1.0 2.0 3.0]
                                       (-> result
                                           :log-samples
                                           :metric->values
                                           (get [:elapsed-time])))))
                       (testing "doesnot change original samples"
                                (is (= samples (:samples result))))
                       (testing "adds transfprms for the values"
                                (is (approx=
                                     (mapv (fn [^double v] (/ v 10.0)) raw-data)
                                     (transformed-metric-values result :log-samples [:elapsed-time]))))
                       (testing "doesn't transform event-metrics "
                                (is (not (contains? (:log-samples result) [:compilation])))))))

(deftest quantiles-test
         (testing "quantiles"
                  (let [raw-data [10 20 30]
                        samples  (metrics-samples
                                  {[:elapsed-time]         raw-data
                                   [:compilation :time-ms] [0 0 0]}
                                  10)
                        data-map {:samples samples}
                        result   ((analyse/quantiles {:quantiles [0.025 0.975]})
                                  data-map)]
                       (testing "puts the quantiles into the result-path"
                                (let [qs [0.25 0.5 0.75 0.025 0.975]
                                      vs (-> result :quantiles util/quantiles :elapsed-time)]
                                     (is (approx=
                                          [15 20 25 10.5 29.5]
                                          (mapv vs qs)))
                                     (is (approx=
                                          [1.5 2.0 2.5 1.05 2.95]
                                          (transformed-values result :quantiles (mapv vs qs))))))
                       (testing "doesn't transform event-metrics "
                                (is (every?
                                     #(not (contains? % :compilation))
                                     (->> result :quantiles)))
                                (is (= [:elapsed-time]
                                       (->> result :quantiles util/quantiles keys)))))))

(deftest outliers-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
         (testing "Outliers"
                  (let [raw-data  [9 10 9 10 9 10 10000]
                        samples   (metrics-samples
                                   {[:elapsed-time]         raw-data
                                    [:compilation :time-ms] [0 0 0]}
                                   10)
                        data-map  {:samples samples}
                        quantiles (analyse/quantiles {:quantiles []})
                        outliers  (analyse/outliers)]
                       (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
                              (-> data-map
                                  quantiles
                                  outliers
                                  :outliers
                                  util/outliers
                                  :elapsed-time
                                  :outlier-counts))))))

(deftest outlier-counts-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
         (testing "Outlier counts"
                  (let [data-map
                        {:samples
                         {:type           :criterium/collected-metrics-samples
                          :metric->values {[:elapsed-time] [1 1 1 1000]}
                          :transform      collect-plan/identity-transforms
                          :batch-size     1
                          :eval-count     4
                          :metrics-defs   (select-keys
                                           (metrics/metrics)
                                           [:elapsed-time])}}
                        analyse (benchmark/->analyse
                                 [[:quantiles {:quantiles [0.025 0.975]}]
                                  :outliers])]
                       (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
                              (-> (analyse data-map)
                                  :outliers
                                  util/outliers
                                  :elapsed-time
                                  :outlier-counts))))))

(deftest stats-test
         (testing "stats"
                  (let [raw-data [1 2 3]
                        samples  (metrics-samples
                                  {[:elapsed-time]         raw-data
                                   [:compilation :time-ms] [0 0 0]}
                                  10)
                        data-map {:samples samples}
                        result   ((analyse/stats) data-map)]
                       (testing "puts the stats into the result-path"
                                (is (= {:min-val           1.0,
                                        :max-val           3.0,
                                        :mean              2.0,
                                        :mean-plus-3sigma  5.0,
                                        :variance          1.0,
                                        :mean-minus-3sigma -1.0
                                        :n                 3}
                                       (->> result :stats util/stats :elapsed-time))))
                       (testing "doesn't transform event-metrics "
                                (is (every?
                                     #(not (contains? % :compilation))
                                     (->> result :stats)))
                                (is (= [:elapsed-time]
                                       (->> result :stats util/stats keys))))))
         (testing "stats variance"
                  (let [raw-data [1 1 1 5 5 5 9 9 9]
                        samples  (metrics-samples
                                  {[:elapsed-time]         raw-data
                                   [:compilation :time-ms] [0 0 0]}
                                  1)
                        data-map {:samples samples}
                        result   ((analyse/stats) data-map)]
                       (testing "calculates sample variance"
                                (is (= 12.0 (:variance (->> result :stats util/stats :elapsed-time))))))
                  (let [raw-data [1 1 1 5 5 5 9 9 9]
                        samples  (metrics-samples
                                  {[:elapsed-time]         raw-data
                                   [:compilation :time-ms] [0 0 0]}
                                  10)
                        data-map {:samples samples}
                        result   ((analyse/stats) data-map)]
                       (testing "scales with batch size"
                                (let [v (:variance (->> result :stats util/stats :elapsed-time))]
                                     (is (= 12.0 v))
                                     (is (= 1.20 (util/transform-sample->
                                                  v
                                                  (util/get-transforms result :stats))))))))
         (testing "excludes outliers"
                  (let [raw-data  [9 10 9 10 9 10 10000]
                        samples   (metrics-samples
                                   {[:elapsed-time]         raw-data
                                    [:compilation :time-ms] [0 0 0]}
                                   1)
                        data-map  {:samples samples}
                        quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
                        outliers  (analyse/outliers)
                        stats     (analyse/stats)
                        result    (-> data-map
                                      quantiles
                                      outliers
                                      stats)
                        smap      (->> result :stats util/stats :elapsed-time)
                        smap'     (util/transform-vals->
                                   (->> result :stats util/stats :elapsed-time)
                                   (util/get-transforms result :stats))]
                       (testing "calculates sample variance"
                                (is (approx= 9.5 (:mean smap)))
                                (is (approx= 0.3 (:variance smap)))
                                (is (approx= 9 (:min-val smap)))
                                (is (approx= 10 (:max-val smap)))
                                (is (approx= 11.14316767 (:mean-plus-3sigma smap)))
                                (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap)))
                                (is (= 6 (:n smap)))

                                (is (approx= 9.5 (:mean smap')))
                                (is (approx= 0.3 (:variance smap')))
                                (is (approx= 9 (:min-val smap')))
                                (is (approx= 10 (:max-val smap')))
                                (is (approx= 11.14316767 (:mean-plus-3sigma smap')))
                                (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap')))))

                  (testing "scales with batch size"
                           (let [raw-data  [9 10 9 10 9 10 10000]
                                 samples   (metrics-samples
                                            {[:elapsed-time]         raw-data
                                             [:compilation :time-ms] [0 0 0]}
                                            2)
                                 quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
                                 data-map  {:samples samples}
                                 outliers  (analyse/outliers)
                                 stats     (analyse/stats)
                                 result    (-> data-map quantiles outliers stats)
                                 smap      (-> result :stats util/stats :elapsed-time)
                                 smap'     (util/transform-vals->
                                            (-> result :stats util/stats :elapsed-time)
                                            (util/get-transforms result :stats))]
                                (is (approx= 9.5 (:mean smap)))
                                (is (approx= 0.3 (:variance smap)))
                                (is (approx= 9 (:min-val smap)))
                                (is (approx= 10 (:max-val smap)))
                                (is (approx= 11.14316767 (:mean-plus-3sigma smap)))
                                (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap)))

                                (is (approx= (/ 9.5 2.0) (:mean smap')))
                                (is (approx= (/ 0.3 2.0) (:variance smap')))
                                (is (approx= 4.5 (:min-val smap')))
                                (is (approx= 5 (:max-val smap')))
                                (is (approx= (/ 11.14316767 2) (:mean-plus-3sigma smap')))
                                (is (approx= (/ 7.8568323274845016 2) (:mean-minus-3sigma smap')))))))

(deftest event-stats-test
         (testing "event-stats"
                  (let [data-map
                        {:samples
                         {:type           :criterium/metrics-samples
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
                          :eval-count     3}}
                        result ((analyse/event-stats) data-map)]
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
                                       (->> result :event-stats util/event-stats)))))))

(deftest outlier-effect-test
         (is (= :unaffected (analyse/outlier-effect 0.009)))
         (is (= :slight (analyse/outlier-effect 0.09)))
         (is (= :moderate (analyse/outlier-effect 0.49)))
         (is (= :severe (analyse/outlier-effect 0.51))))

(deftest outlier-significance-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
         (testing "Outlier counts"
                  (let [data-map
                        {:samples
                         {:type           :criterium/collected-metrics-samples
                          :metric->values {[:elapsed-time] [1 1 1 1000]}
                          :transform      collect-plan/identity-transforms
                          :batch-size     1
                          :eval-count     4
                          :metrics-defs   (select-keys
                                           (metrics/metrics)
                                           [:elapsed-time])}}
                        analyse (benchmark/->analyse
                                 [[:quantiles {:quantiles [0.025 0.975]}]
                                  :outliers
                                  :stats
                                  :outlier-significance])]

                       (is (= {:significance 0
                               :effect       :unaffected}
                              (-> (analyse data-map)
                                  :outlier-significance
                                  util/outlier-significance
                                  :elapsed-time))))))
