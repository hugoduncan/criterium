(ns criterium.viewer.portal-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.portal :as portal])
  (:import
   [java.util Queue]))

(set! *unchecked-math*  false)

(defmacro with-tap-out [& body]
  `(let [v# (volatile! [])
         f# (fn [x#]
              (when-not (= ::portal/_ x#)
                (vswap! v# conj x#)))]
     (try
       (add-tap f#)
       ~@body
       (loop []
         (when-not (.isEmpty ^Queue @#'clojure.core/tapq)
           (recur)))
       (loop []
         (when (empty? @v#)
           (recur)))
       (portal/flush)
       @v#
       (finally
         (remove-tap f#)))))

(deftest portal-samples-test
  (testing "portal-samples"
    (testing "charts the sample data"
      (let [[title chart] (with-tap-out
                            (view/samples*
                             :portal
                             {}
                             (:data (test-data/samples-with-2-values-map))))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 1.0, :index 1, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Samples"] title))))
    (testing "charts with transformed sample data"
      (let [[title chart] (with-tap-out
                            (view/samples*
                             :portal
                             {}
                             (:data
                              (test-data/samples-with-transformed-values-map))))]
        (is (= [{:elapsed-time 1.0, :index 0, :outlier ""}
                {:elapsed-time 2.0, :index 1, :outlier ""}
                {:elapsed-time 4.0, :index 2, :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Samples"] title))))
    (testing "charts the sample data"
      (let [bench-map     (:data (test-data/samples-with-outliers-values-map))
            quantiles     (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers      (analyse/outliers)
            stats         (analyse/stats)
            view          (view/samples)
            [title chart] (with-tap-out
                            (->> bench-map
                                 quantiles
                                 outliers
                                 stats
                                 (view :portal)))]
        (is (= [{:elapsed-time 9.0, :index 0, :outlier ""}
                {:elapsed-time 10.0, :index 1, :outlier ""}
                {:elapsed-time 9.0, :index 2, :outlier ""}
                {:elapsed-time 10.0, :index 3, :outlier ""}
                {:elapsed-time 9.0, :index 4, :outlier ""}
                {:elapsed-time 10.0, :index 5, :outlier ""}
                {:elapsed-time 10000.0, :index 6, :outlier :high-severe}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Samples"] title))))))

(deftest portal-sample-percentiles-test
  (testing "portal-percentiles"
    (testing "charts the sample data"
      (let [[title chart] (with-tap-out
                            (view/sample-percentiles*
                             :portal
                             {}
                             (:data (test-data/samples-with-2-values-map))))]
        (is (= [{:elapsed-time 1.0, :x 0.0, :p 0}
                {:elapsed-time 1.0, :x 1.0, :p 100.0}]
               (-> chart :vconcat first :layer first :data :values)))
        (is (= [:b "Percentiles"] title))))))

(deftest portal-histogram-test
  (testing "portal-histogram"
    (testing "charts the sample data"
      (let [data-map
            (:data (test-data/samples-with-outliers-values-map))
            quantiles      (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers       (analyse/outliers)
            stats          (analyse/stats)
            histogram      (analyse/histogram)
            view-histogrem (view/histogram)
            [title chart]  (with-tap-out
                             (->> data-map
                                  quantiles
                                  outliers
                                  stats
                                  histogram
                                  (view-histogrem :portal)))]
        (let [histogram-data (-> chart :vconcat first :layer first :data :values)]
          (is (vector? histogram-data))
          (is (pos? (count histogram-data)))
          (is (every? #(and (contains? % "elapsed-time")
                            (contains? % "end")
                            (contains? % "density"))
                      histogram-data)))
        (is (= [:b "Histogram"] title))))))

(deftest portal-stats-test
  (testing "print-stats"
    (testing "prints via output-view"
      (is (= [[:b "Summary stats"]
              [{:_metric           "Elapsed Time ns",
                :mean              100.0
                :min-val           89.0
                :mean-minus-3sigma 88.0
                :mean-plus-3sigma  112.0
                :max-val           114.0}]]
             (with-tap-out
               (view/stats*
                :portal
                {}
                (:data (test-data/bench-stats-map))))))

      (is (= [[:b "Summary stats"]
              [{:_metric           "Elapsed Time ns",
                :mean              1.00,
                :min-val           1.00,
                :mean-minus-3sigma 1.00,
                :mean-plus-3sigma  1.00,
                :max-val           1.00}]]
             (let [data-map
                   (:data (test-data/samples-with-2-values-map))
                   stats      (analyse/stats)
                   view-stats (view/stats)]
               (with-tap-out
                 (->> data-map
                      stats
                      (view-stats :portal)))))))))

(deftest portal-outlier-count-test
  (testing "print-outlier-count"
    (testing "prints via view"
      (is
       (= [[:b "Outliers"]
           [{:low-severe  0,
             :low-mild    2,
             :high-mild   3,
             :high-severe 0,
             :_metric     "Elapsed Time"}]]
          (with-tap-out
            ((view/outlier-counts)
             :portal
             (:data (test-data/outlier-count-map)))))))))

(deftest portal-outlier-significance-test
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [[:b "Outlier Significance"]
              [{:effect :moderate :significance 0.25}]]
             (with-tap-out
               ((view/outlier-significance)
                :portal
                (:data (test-data/outlier-significance-map)))))))))

(deftest portal-event-stats-test
  (testing "print-event-stats"
    (testing "prints via report"
      (is (= [[:b "Event stats"]
              [{:metric         "ClassLoader",
                :sample-count   "1.0",
                :loaded-count   "1.0",
                :unloaded-count "1.0"}
               {:metric       "JIT compilation",
                :sample-count "1.0",
                :time-ms      "3.00 ms"}
               {:metric             "Garbage Collector",
                :total-sample-count "1.0",
                :total-count        "2.0",
                :total-time-ms      "1.00 ms"}]]
             (let [data-map    (:data (test-data/samples-for-event-stats-map))
                   event-stats (analyse/event-stats)
                   view        (view/event-stats)]
               (with-tap-out
                 (->> data-map
                      event-stats
                      (view :portal)))))))))

;; Tests that the portal viewer handles non-numeric metric values gracefully
;; instead of throwing an exception when coercing to double.
(deftest portal-metrics-non-numeric-test
  (testing "portal-metrics"
    (testing "handles non-numeric metric values"
      (is (= [[{:metric "Elapsed Time" :value "unavailable"}
               {:metric "Expr value" :value nil}]]
             (with-tap-out
               (view/metrics*
                :portal
                {}
                (:data (test-data/samples-with-non-numeric-value-map)))))))))
