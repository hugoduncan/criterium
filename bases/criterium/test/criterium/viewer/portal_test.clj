(ns criterium.viewer.portal-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.metric :as metric]
   [criterium.view :as view]
   [criterium.viewer.portal]
   [criterium.viewer.test-data :as test-data]))

(set! *unchecked-math*  false)

(defmacro with-tap-out [& body]
  `(let [v# (volatile! [])
         f# (fn [x#] (vswap! v# conj x#))]
     (try
       (add-tap f#)
       ~@body
       (loop []
         (when-not (.isEmpty @#'clojure.core/tapq)
           (recur)))
       (loop []
         (when (empty? @v#)
           (recur)))
       @v#
       (finally
         (remove-tap f#)))))

(deftest portal-samples-test
  (testing "portal-samples"
    (testing "charts the sample data"
      (let [[chart] (with-tap-out
                      (view/samples*
                       {}
                       (-> (test-data/samples-with-2-values-map)
                           (assoc :viewer :portal))))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 1.0, :index 1, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))))
    (testing "charts with transformed sample data"
      (let [[chart] (with-tap-out
                      (view/samples*
                       {}
                       (-> (test-data/samples-with-transformed-values-map)
                           (assoc :viewer :portal))))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 2.0, :index 1, :outlier ""}
                {:elapsed-time 4.0, :index 2, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))))
    (testing "charts the sample data"
      (let [bench-map (-> (test-data/samples-with-outliers-values-map)
                          (assoc :viewer :portal))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers  (analyse/outliers)
            stats     (analyse/stats)
            view      (view/samples)
            [chart]   (with-tap-out
                        (-> bench-map
                            quantiles
                            outliers
                            stats
                            view))]
        (is (= [{:elapsed-time 9.0, :index 0, :outlier ""}
                {:elapsed-time 10.0, :index 1, :outlier ""}
                {:elapsed-time 9.0, :index 2, :outlier ""}
                {:elapsed-time 10.0, :index 3, :outlier ""}
                {:elapsed-time 9.0, :index 4, :outlier ""}
                {:elapsed-time 10.0, :index 5, :outlier ""}
                {:elapsed-time 10000.0, :index 6, :outlier :high-severe}]
               (-> chart :vconcat first :layer first :data :values)))))))

(deftest portal-sample-percentiles-test
  (testing "portal-percentiles"
    (testing "charts the sample data"
      (let [[chart] (with-tap-out
                      (view/sample-percentiles*
                       {}
                       (-> (test-data/samples-with-2-values-map)
                           (assoc :viewer :portal))))]
        (is (= [{:elapsed-time 1.0, :x 0.0, :p 0}
                {:elapsed-time 1.0, :x 1.0, :p 100.0}]
               (-> chart :vconcat first :layer first :data :values)))))))

(deftest portal-histogram-test
  (testing "portal-histogram"
    (testing "charts the sample data"
      (let [bench-map
            (-> (test-data/samples-with-2-values-map)
                (assoc :viewer :portal))
            quantiles      (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers       (analyse/outliers)
            stats          (analyse/stats)
            view-histogrem (view/histogram)
            [chart]        (with-tap-out
                             (-> bench-map
                                 quantiles
                                 outliers
                                 stats
                                 view-histogrem))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 1.0, :index 1, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))))))

(deftest portal-stats-test
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= [[{:metric            "Elapsed Time",
                :mean              "100 ns",
                :min-val           "89.0 ns",
                :mean-minus-3sigma "88.0 ns",
                :mean-plus-3sigma  "112 ns",
                :max-val           "114 ns"}]]
             (with-tap-out
               (view/stats*
                {}
                (-> (test-data/bench-stats-map)
                    (assoc :viewer :portal))))))

      (is (= [[{:metric            "Elapsed Time",
                :mean              "1.00 ns",
                :min-val           "1.00 ns",
                :mean-minus-3sigma "1.00 ns",
                :mean-plus-3sigma  "1.00 ns",
                :max-val           "1.00 ns"}]]
             (let [bench-map
                   (-> (test-data/samples-with-2-values-map)
                       (assoc :viewer :portal))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (with-tap-out
                 (-> bench-map
                     stats
                     view-stats))))))))

(deftest portal-outlier-count-test
  (testing "print-outlier-count"
    (testing "prints via view"
      (is
       (= [[{:low-severe  0,
             :low-mild    2,
             :high-mild   3,
             :high-severe 0,
             :metric      "Elapsed Time"}]]
          (with-tap-out
            ((view/outlier-counts)
             (-> (test-data/outlier-count-map)
                 (assoc :viewer :portal)))))))))

(deftest portal-outlier-significance-test
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [[{:effect :moderate :significance 0.25}]]
             (with-tap-out
               ((view/outlier-significance)
                (-> (test-data/outlier-significance-map)
                    (assoc :viewer :portal)))))))))

(deftest portal-event-stats-test
  (testing "print-event-stats"
    (testing "prints via report"
      (is (= [[{:metric         "ClassLoader",
                :sample-count   "1",
                :loaded-count   "1",
                :unloaded-count "1"}
               {:metric       "JIT compilation",
                :sample-count "1",
                :time-ms      "3.00 ms"}
               {:metric             "Garbage Collector",
                :total-sample-count "1",
                :total-count        "2",
                :total-time-ms      "1.00 ms"}]]
             (let [bench-map   (-> (test-data/samples-for-event-stats-map)
                                   (assoc :viewer :portal))
                   event-stats (analyse/event-stats)
                   view        (view/event-stats)]
               (with-tap-out
                 (-> bench-map
                     event-stats
                     view))))))))
