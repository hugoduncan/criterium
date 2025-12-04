(ns criterium.collect-plan-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.measured :as measured]
   [criterium.types :as types]))

(deftest one-shot-test
  (testing "one-shot"
    (let [measured  (measured/measured
                     (fn [] [])
                     (fn [_ _] [1000000 1]))
          collector (collector/collector
                     {:stages     [:compilation :memory]
                      :terminator :elapsed-time})
          data-map  (collect-plan/collect
                     (collect-plan-config/collect-plan-config
                      :one-shot
                      {})
                     collector
                     measured)]
      (is (map? data-map))
      (is (types/collected-metrics-map? (:samples data-map)))
      (is (vector? ((:metric->values (:samples data-map)) [:elapsed-time])))
      (is (= 1
             (count ((:metric->values (:samples data-map)) [:elapsed-time]))))
      (is (every? vector? (vals (:metric->values (:samples data-map)))))
      (is (= 1 (:expr-value (:samples data-map)))))))

(deftest full-test
  (testing "full sampling"
    (let [measured  (measured/measured
                     (fn [] [])
                     (fn [_ _] [1000000 1]))
          collector (collector/collector {:stages     [:compilation :memory]
                                          :terminator :elapsed-time})
          data-map  (collect-plan/collect
                     (collect-plan-config/collect-plan-config
                      :with-jit-warmup
                      {})
                     collector
                     measured)]
      (is (map? data-map))
      (is (types/collected-metrics-map? (:samples data-map)))
      (is (vector? ((:metric->values (:samples data-map)) [:elapsed-time])))
      (is (<= 10
              (count ((:metric->values (:samples data-map)) [:elapsed-time]))))
      (is (every? vector? (vals (:metric->values (:samples data-map)))))
      (is (= 1 (:expr-value (:samples data-map)))))))
