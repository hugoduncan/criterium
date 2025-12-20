(ns criterium.analyse.digest-samples-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.analyse.digest-samples]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [approx=]]
   [criterium.types :as types]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have?]]
   [criterium.util.t-digest :as t-digest]))

(defn digest-samples
  [data ^long batch-size]
  {:post [(have? types/digest-samples-map? %)]}
  (let [n (count (first (vals data)))]
    {:type :criterium/digest
     :metric->digest (reduce-kv
                      (fn [res p values]
                        (assoc
                         res p
                         (t-digest/compress
                          (reduce
                           t-digest/add-point
                           (t-digest/new-digest)
                           values))))
                      {}
                      data)
     :transform (if (= batch-size 1)
                  collect-plan/identity-transforms
                  (#'collect-plan/batch-transforms batch-size))
     :num-samples n
     :batch-size batch-size
     :eval-count (* n batch-size)
     :metrics-defs (select-keys
                    (metrics/metrics)
                    (mapv first (keys data)))
     :source-id nil
     :expr-value (ffirst (vals data))}))

(defn transformed-value-fn
  [data-map id]
  (let [transforms (util/get-transforms data-map id)]
    #(util/transform-sample-> % transforms)))

(defn transformed-digest-values
  [data-map id p]
  (let [digest (get (:metric->digest (-> data-map id)) p)
        tform (transformed-value-fn data-map id)]
    (-> digest
        (update :minimum tform)
        (update :maximum tform)
        (update :centroids #(mapv (fn [c] (update c :mean tform)) %)))))

(deftest digest-transform-log-test
  (testing "transform-log"
    (let [raw-data [(Math/exp 1) (Math/exp 2) (Math/exp 3)]

          samples (digest-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/transform-log) data-map)
          digest (-> result
                     :log-samples
                     :metric->digest
                     (get [:elapsed-time]))]
      (testing "puts the log transformed metrics into the result-path"
        (is (= [1.0 2.0 3.0]
               (mapv :mean (:centroids digest)))))
      (testing "does not change original samples"
        (is (= samples (:samples result))))
      (testing "adds transforms for the values"
        (is (approx=
             (mapv (fn [^double v] (/ v 10.0)) raw-data)
             (mapv
              :mean
              (:centroids
               (transformed-digest-values
                result
                :log-samples
                [:elapsed-time])))))
        (is (approx= 1.0 (t-digest/minimum digest)))
        (is (approx= 3.0 (t-digest/maximum digest))))
      (testing "computes correct quantiles from digest"
        ;; With 3 data points [1,2,3], t-digest quantiles cluster at centroids
        (is (approx= 1.0 (t-digest/quantile digest 0.0)) "min quantile")
        (is (approx= 1.0 (t-digest/quantile digest 0.25)) "Q1")
        (is (approx= 2.0 (t-digest/quantile digest 0.5)) "median")
        (is (approx= 3.0 (t-digest/quantile digest 0.75)) "Q3")
        (is (approx= 3.0 (t-digest/quantile digest 1.0)) "max quantile"))
      (testing "doesn't transform event-metrics "
        (is (not (contains? (:log-samples result) [:compilation])))))))

(deftest digest-quantiles-test
  (testing "quantiles"
    (let [raw-data [10 20 30]
          samples (digest-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/quantiles {:quantiles [0.025 0.975]})
                  data-map)
          tform (transformed-value-fn data-map :samples)]
      (testing "puts the quantiles into the result-path"
        (let [qs [0.25 0.5 0.75 0.025 0.975]
              vs (-> result :quantiles util/quantiles :elapsed-time)]
          (is (approx= [10 20 30 10 30] (mapv vs qs)))
          (is (approx=
               [1.0 2.0 3.0 1.0 3.0]
               (mapv
                tform
                (-> result :quantiles util/quantiles :elapsed-time vals))))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :quantiles)))
        (is (= [:elapsed-time]
               (->> result :quantiles util/quantiles keys)))))))

(deftest digest-outliers-test
  ;; Tests outlier detection on digest samples.
  ;; Verifies threshold calculation from quantiles and outlier classification.
  (testing "Outliers"
    (testing "with obvious outlier"
      (let [raw-data  [9 10 9 10 9 10 10000]
            samples   (digest-samples
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
                   :outlier-counts)))))
    (testing "with normal distribution"
      (let [;; Generate normally distributed data (mean=100, stddev=10)
            ;; Using Box-Muller approximation via pre-computed values
            normal-data [80.5 85.2 88.7 91.3 93.8 96.1 98.2 100.0
                         101.8 103.9 106.2 108.7 111.3 114.8 119.5]
            samples     (digest-samples
                         {[:elapsed-time]         normal-data
                          [:compilation :time-ms] (repeat (count normal-data) 0)}
                         1)
            data-map    {:samples samples}
            result      (-> data-map
                            ((analyse/quantiles {:quantiles []}))
                            ((analyse/outliers)))
            qs          (-> result :quantiles util/quantiles :elapsed-time)
            ^double q1  (get qs 0.25)
            ^double q3  (get qs 0.75)
            iqr         (- q3 q1)
            outlier-map (-> result :outliers util/outliers :elapsed-time)
            thresholds  (:thresholds outlier-map)]
        (testing "calculates thresholds from digest quantiles"
          (is (approx= (- q1 (* 3.0 iqr)) (nth thresholds 0))
              "low-severe threshold")
          (is (approx= (- q1 (* 1.5 iqr)) (nth thresholds 1))
              "low-mild threshold")
          (is (approx= (+ q3 (* 1.5 iqr)) (nth thresholds 2))
              "high-mild threshold")
          (is (approx= (+ q3 (* 3.0 iqr)) (nth thresholds 3))
              "high-severe threshold"))
        (testing "detects no outliers in well-behaved normal data"
          (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 0}
                 (:outlier-counts outlier-map))))))))

(deftest digest-stats-test
  (testing "stats"
    (let [raw-data [1 2 3]
          samples (digest-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/stats) data-map)
          expected {:min-val 1.0,
                    :max-val 3.0,
                    :mean 2.0,
                    :mean-plus-3sigma 5.0,
                    :variance 1.0,
                    :mean-minus-3sigma -1.0
                    :n 3.0}
          actual (-> result :stats util/stats :elapsed-time)]
      (testing "puts the stats into the result-path"
        (doseq [k (keys expected)]
          (is (approx= (expected k) (actual k)) (pr-str k))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :stats)))
        (is (= [:elapsed-time]
               (->> result :stats util/stats keys))))))
  (testing "stats variance"
    (let [raw-data [1 1 1 5 5 5 9 9 9]
          samples (digest-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   1)
          data-map {:samples samples}
          result ((analyse/stats) data-map)]
      (testing "calculates sample variance"
        (is (approx= 12.0 (:variance (->> result :stats util/stats :elapsed-time))))))
    (let [raw-data [1 1 1 5 5 5 9 9 9]
          samples (digest-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/stats) data-map)]
      (testing "scales with batch size"
        (let [v (:variance (->> result :stats util/stats :elapsed-time))]
          (is (approx= 12.0 v))
          (is (approx= 1.20 (util/transform-sample->
                             v
                             (util/get-transforms result :stats))))))))
  (testing "excludes outliers"
    (let [raw-data [9 10 9 10 9 10 10000]
          samples (digest-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   1)
          data-map {:samples samples}
          quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
          outliers (analyse/outliers)
          stats (analyse/stats)
          result (-> data-map
                     quantiles
                     outliers
                     stats)
          smap (->> result :stats util/stats :elapsed-time)
          smap' (util/transform-vals->
                 (->> result :stats util/stats :elapsed-time)
                 (util/get-transforms result :stats))]
      (testing "calculates sample variance"
        (is (approx= 9.5 (:mean smap)) "mean")
        (is (approx= 0.3 (:variance smap)) "variance")
        (is (approx= 9 (:min-val smap)) "min")
        (is (approx= 10 (:max-val smap)) "max")
        (is (approx= 11.14316767 (:mean-plus-3sigma smap)) "mean+3sigma")
        (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap))
            "mean-3sigma")
        (is (= 6.0 (:n smap)) "n")

        (is (approx= 9.5 (:mean smap')) "mean")
        (is (approx= 0.3 (:variance smap')) "variance")
        (is (approx= 9 (:min-val smap')) "min")
        (is (approx= 10 (:max-val smap')) "max")
        (is (approx= 11.14316767 (:mean-plus-3sigma smap')) "mean+3sigma")
        (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap'))
            "mean-3sigma")))

    (testing "scales with batch size"
      (let [raw-data [9 10 9 10 9 10 10000]
            samples (digest-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] [0 0 0]}
                     2)
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            data-map {:samples samples}
            outliers (analyse/outliers)
            stats (analyse/stats)
            result (-> data-map quantiles outliers stats)
            smap (-> result :stats util/stats :elapsed-time)
            smap' (util/transform-vals->
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
