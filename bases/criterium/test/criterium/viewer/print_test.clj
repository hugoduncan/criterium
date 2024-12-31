(ns criterium.viewer.print-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.view :as view]
   [criterium.viewer.print :as print]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collect-plan :as collect-plan]
   [criterium.viewer.test-data :as test-data]))

(deftest print-stat-test
  (testing "print-stat"
    (is (= "Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"
           (str/trim
            (with-out-str
              (print/print-stat
               {:label     "Elapsed Time"
                :scale     1e-9
                :dimension :time}
               {:mean              100.0
                :variance          16.0
                :mean-plus-3sigma  112.0
                :mean-minus-3sigma 88.0
                :min-val           89.0})))))))

(defn identity-transform [samples]
  (with-meta samples {:transform {:sample-> identity :->sample identity}}))

(deftest print-stats-test
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= ["Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"]
             (trimmed-lines
              (with-out-str
                (view/stats*
                 {}
                 (->(test-data/bench-stats-map)
                    (assoc :viewer :print)))))))

      (is (= ["Elapsed Time: 1.00 ns  3σ [1.00 1.00]  min 1.00"]
             (let [bench-map  (-> (test-data/samples-with-2-values-map)
                                  (assoc :viewer :print))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      stats
                      view-stats))))))))
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= ["Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"]
             (trimmed-lines
              (with-out-str
                (view/stats*
                 {}
                 (->(test-data/bench-stats-map)
                    (assoc :viewer :print)))))))

      (is (= ["Elapsed Time: 1.00 ns  3σ [1.00 1.00]  min 1.00"]
             (let [bench-map  (-> (test-data/samples-with-2-values-map)
                                  (assoc :viewer :print))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      stats
                      view-stats)))))))
    (testing "prints via output-view"
      (is (= ["Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"]
             (trimmed-lines
              (with-out-str
                (view/stats*
                 {}
                 (->(test-data/bench-stats-map)
                    (assoc :viewer :print)))))))

      (is (= ["Elapsed Time: 5.00 ns  3σ [-5.39 15.4]  min 1.00"]
             (let [bench-map  (-> (test-data/samples-with-variance-12-map)
                                  (assoc :viewer :print))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      stats
                      view-stats))))))
      (is (= ["Elapsed Time: 2.50 ns  3σ [-2.70 7.70]  min 0.500"]
             (let [bench-map
                   (-> (update-in
                        (test-data/samples-with-variance-12-map)
                        [:data :samples]
                        merge
                        {:batch-size 2
                         :transform  (#'collect-plan/batch-transforms 2)})
                       (assoc :viewer :print))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      stats
                      view-stats)))))))))


(deftest print-booststrap-stat-test
  (testing "print-bootstrap-stat"
    (is (= ["Elapsed Time min: 16.0 ns CI [9.00 25.0] (0.050 0.950)"
            "Elapsed Time mean: 100 ns CI [95.0 105] (0.050 0.950)"
            "Elapsed Time 3σ: [76.0 124] ns"]
           (trimmed-lines
            (with-out-str
              (print/print-bootstrap-stat
               {:scale 1e-9 :dimension :time :path [:elapsed-time]
                :label "Elapsed Time"}
               {:mean              {:point-estimate 100.0
                                    :estimate-quantiles
                                    [{:value 95.0 :alpha 0.05}
                                     {:value 105.0 :alpha 0.95}]}
                :variance          {:point-estimate 16.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}
                :min-val           {:point-estimate 16.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}
                :mean-plus-3sigma  {:point-estimate 124.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}
                :mean-minus-3sigma {:point-estimate 76.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}})))))
    (is (= ["Elapsed Time min: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
            "Elapsed Time mean: 1.00 ns CI [1.00 1.00] (0.025 0.975)"
            "Elapsed Time 3σ: [1.00 1.00] ns"]
           (let [bench-map
                 {:metrics-defs (select-keys
                                 (metrics/metrics)
                                 [:elapsed-time])
                  :data
                  {:samples
                   {:type           :criterium/collected-metrics-samples
                    :metric->values {[:elapsed-time] [1 1 1]}
                    :metrics-defs   (select-keys
                                     (metrics/metrics)
                                     [:elapsed-time])
                    :transform      collect-plan/identity-transforms
                    :batch-size     1
                    :eval-count     1
                    :elapsed-time   1}}
                  :viewer       :print}
                 bootstrap (bootstrap/bootstrap-stats
                            {:quantiles          [0.025 0.975]
                             :estimate-quantiles [0.025 0.975]})
                 view      (view/bootstrap-stats)]
             (trimmed-lines
              (with-out-str
                (-> bench-map
                    bootstrap
                    view))))))))

(deftest print-samples-test
  (testing "print-samples"
    (testing "prints via view"
      (is (= ["Samples: 7 samples with batch-size 1"
              "[6] 10.0 µs high-severe"]
             (let [bench-map
                   (-> (test-data/samples-with-outliers-values-map)
                       (assoc :viewer :print))
                   quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
                   outliers  (analyse/outliers)
                   stats     (analyse/stats)
                   view      (view/samples)]
               (trimmed-lines
                (with-out-str
                  (-> bench-map
                      quantiles
                      outliers
                      stats
                      view)))))))))

(deftest print-outlier-count-test
  (testing "print-outlier-count"
    (testing "prints all outliers when all present"
      (is (= ["M: Found 10 outliers in 100 samples (10.0 %)"
              "low-severe\t 1 (1.0000 %)"
              "low-mild\t 2 (2.0000 %)"
              "high-mild\t 3 (3.0000 %)"
              "high-severe\t 4 (4.0000 %)"]
             (trimmed-lines
              (with-out-str (print/print-outlier-count
                             {:label "M"}
                             100
                             {:outlier-counts
                              (analyse/outlier-count 1 2 3 4)}))))))
    (testing "prints only present outliers"
      (is (= ["M: Found 5 outliers in 100 samples (5.00 %)"
              "low-mild\t 2 (2.0000 %)"
              "high-mild\t 3 (3.0000 %)"]
             (trimmed-lines
              (with-out-str
                (print/print-outlier-count
                 {:label "M"}
                 100
                 {:outlier-counts
                  (analyse/outlier-count
                   0 2 3 0)}))))))
    (testing "prints via view"
      (is (= ["Elapsed Time: Found 5 outliers in 1 samples (500 %)"
              "low-mild\t 2 (200.0000 %)"
              "high-mild\t 3 (300.0000 %)"]
             (trimmed-lines
              (with-out-str
                (let [bench-map
                      (-> (test-data/outlier-count-map)
                          (assoc :viewer :print))
                      view (view/outlier-counts)]
                  (view bench-map)))))))))

(deftest print-outlier-significance-test
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [(str "Elapsed Time Variance contribution from outliers : 25.0 %"
                   "Elapsed Time Variance is moderately inflated by outliers")]
             (trimmed-lines
              (with-out-str
                ((view/outlier-significance)
                 (-> (test-data/outlier-significance-map)
                     (assoc :viewer :print))))))))))

(deftest print-event-stats-test
  (testing "print-event-stats"
    (testing "prints via report"
      (is (=
           ["ClassLoader: loaded 1 and unloaded 1 classes in 1 samples"
            "JIT compilation: ran for 3.00 ms in 1 samples"
            (str "Garbage Collector: ran 2 times for a total of 1.00 ms "
                 "in 1 samples")]
           (let [bench-map   (-> (test-data/samples-for-event-stats-map)
                                 (assoc :viewer :print))
                 event-stats (analyse/event-stats)
                 view        (view/event-stats)]
             (trimmed-lines
              (with-out-str
                (-> bench-map
                    event-stats
                    view)))))))))

(deftest print-final-gc-warnings-test
  (testing "print-final-gc-warnings-test"
    (testing "prints via view"
      (is (= ["Final GC ran for 1.00 ms, 1.0% of total sampling time (100 ms)"]
             (let [metrics-defs (->
                                 (select-keys
                                  (metrics/metrics)
                                  [:elapsed-time :class-loader :compilation])
                                 (assoc-in
                                  [:garbage-collector :values]
                                  [{:path
                                    [:garbage-collector :total :count]
                                    :label     "GC total count"
                                    :scale     1
                                    :type      :event
                                    :dimension :count}
                                   {:path
                                    [:garbage-collector :total :time-ms]
                                    :label     "GC total time"
                                    :scale     1e-3
                                    :type      :event
                                    :dimension :time}]))
                   view1        (view/final-gc-warnings
                                 {:warn-threshold 0.01
                                  :sampled-path   [:sampled]})
                   view2        (view/final-gc-warnings
                                 {:view-type      :final-gc-warnings
                                  :warn-threshold 0.02
                                  :sampled-path   [:sampled]})
                   bench-map
                   {:data
                    {:samples
                     {:type         :criterium/collected-metrics-samples
                      :metric->values
                      {[:elapsed-time] [99999999]}
                      :metrics-deps metrics-defs
                      :batch-size   1
                      :eval-count   1
                      :elapsed-time 1}
                     :final-gc
                     {:type         :criterium/collected-metrics-samples
                      :metric->values
                      {[:compilation :time-ms]              [3]
                       [:garbage-collector :total :time-ms] [1]
                       [:elapsed-time]                      [1]}
                      :metrics-deps metrics-defs
                      :batch-size   1
                      :eval-count   1
                      :elapsed-time 1} }
                    :viewer :print}]
               (trimmed-lines
                (with-out-str
                  (view1 bench-map)
                  (view2 bench-map)))))))))

(deftest print-os-test
  (let [s (with-out-str ((view/os) {:viewer :print}))]
    (is (str/ends-with? s "cpu(s)\n"))))

(deftest print-runtime-test
  (let [s (with-out-str ((view/runtime) {:viewer :print}))]
    (is (not (str/blank? s)))))
