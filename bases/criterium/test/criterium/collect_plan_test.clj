(ns criterium.collect-plan-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.measured :as measured]))

(deftest one-shot-test
  (testing "one-shot"
    (let [measured (measured/measured
                    (fn [] [])
                    (fn [_ _] [1000000 1]))
          pipeline (collector/collector {:stages     [:compilation :memory]
                                         :terminator :elapsed-time})
          sampled  (collect-plan/collect
                    (collect-plan-config/one-shot-collect-plan {})
                    (:metrics-configs pipeline)
                    pipeline
                    measured)]
      (is (map? sampled))
      (is (vector? (get (:samples sampled) [:elapsed-time])))
      (is (= 1 (count (get (:samples sampled) [:elapsed-time]))))
      (is (every? vector? (vals (:samples sampled))))
      (is (= 1 (:expr-value sampled))))))

(deftest full-test
  (testing "full sampling"
    (let [measured (measured/measured
                    (fn [] [])
                    (fn [_ _] [1000000 1]))
          pipeline (collector/collector {:stages     [:compilation :memory]
                                         :terminator :elapsed-time})
          sampled  (collect-plan/collect
                    (collect-plan-config/full-collect-plan {})
                    (:metrics-configs pipeline)
                    pipeline
                    measured)]
      (is (map? sampled))
      (is (vector? (get (:samples sampled) [:elapsed-time])))
      (is (<= 10 (count (get (:samples sampled) [:elapsed-time]))))
      (is (every? vector? (vals (:samples sampled))))
      (is (= 1 (:expr-value sampled))))))
