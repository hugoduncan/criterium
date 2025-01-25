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
    {:type           :criterium/digest
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

(defn transformed-digest-values
  [data-map id p]
  (let [m          (-> data-map id)
        transforms (util/get-transforms data-map id)
        digest     (get (:metric->digest m) p)
        tform      #(util/transform-sample-> % transforms)]
    (-> digest
        (update :minimum tform)
        (update :maximum tform)
        (update :centroids #(mapv (fn [c] (update c :mean tform)) %)))))

(deftest digest-transform-log-test
  (testing "transform-log"
    (let [raw-data [(Math/exp 1) (Math/exp 2) (Math/exp 3)]

          samples  (digest-samples
                    {[:elapsed-time]         raw-data
                     [:compilation :time-ms] [0 0 0]}
                    10)
          data-map {:samples samples}
          result   ((analyse/transform-log) data-map)
          digest   (-> result
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
      (testing "doesn't transform event-metrics "
        (is (not (contains? (:log-samples result) [:compilation])))))))
