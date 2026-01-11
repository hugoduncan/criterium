(ns criterium.collect-plan-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   criterium.array-core.interface
   [criterium.collect-plan :as collect-plan]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.measured :as measured])
  (:import
   [criterium.array_core.interface ITypedArray]))

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
      (is (= :criterium/metrics-samples (:type (:samples data-map))))
      (is (instance? ITypedArray
                     ((:metric->values (:samples data-map)) [:elapsed-time])))
      (is (= 1
             (arr/length ((:metric->values (:samples data-map)) [:elapsed-time]))))
      (is (every? #(instance? ITypedArray %)
                  (vals (:metric->values (:samples data-map)))))
      (is (= 1 (:expr-value (:samples data-map)))))))

(deftest ^:slow full-test
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
      (is (= :criterium/metrics-samples (:type (:samples data-map))))
      (is (instance? ITypedArray
                     ((:metric->values (:samples data-map)) [:elapsed-time])))
      (is (<= 10
              (arr/length ((:metric->values (:samples data-map)) [:elapsed-time]))))
      (is (every? #(instance? ITypedArray %)
                  (vals (:metric->values (:samples data-map)))))
      (is (= 1 (:expr-value (:samples data-map)))))))
