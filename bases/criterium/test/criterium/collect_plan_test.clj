(ns criterium.collect-plan-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   criterium.array.interface
   [criterium.collect-plan :as collect-plan]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.measured :as measured])
  (:import
   [criterium.array.interface ITypedArray]))

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

;;; Tests for :num-warmup-samples option in :one-shot collect-plan
;; Verifies that warmup invocations execute before measurement.

(deftest one-shot-num-warmup-test
  ;; Tests :num-warmup-samples option for :one-shot collect-plan.
  ;; Uses an atom counter to verify the number of invocations.
  (let [collector (collector/collector
                   {:stages     []
                    :terminator :elapsed-time})]
    (testing ":one-shot with :num-warmup-samples"
      (testing "when :num-warmup-samples is 0 (default)"
        (testing "invokes measured exactly once"
          (let [counter  (atom 0)
                measured (measured/measured
                          (fn [] [])
                          (fn [_ _]
                            (swap! counter inc)
                            [1000 @counter]))]
            (collect-plan/collect
             (collect-plan-config/collect-plan-config :one-shot {})
             collector
             measured)
            (is (= 1 @counter)
                "Expected 1 invocation (measurement only)"))))
      (testing "when :num-warmup-samples is 1"
        (testing "invokes measured twice (1 warmup + 1 measurement)"
          (let [counter  (atom 0)
                measured (measured/measured
                          (fn [] [])
                          (fn [_ _]
                            (swap! counter inc)
                            [1000 @counter]))]
            (collect-plan/collect
             (collect-plan-config/collect-plan-config :one-shot {:num-warmup-samples 1})
             collector
             measured)
            (is (= 2 @counter)
                "Expected 2 invocations (1 warmup + 1 measurement)"))))
      (testing "when :num-warmup-samples is 3"
        (testing "invokes measured four times (3 warmup + 1 measurement)"
          (let [counter  (atom 0)
                measured (measured/measured
                          (fn [] [])
                          (fn [_ _]
                            (swap! counter inc)
                            [1000 @counter]))]
            (collect-plan/collect
             (collect-plan-config/collect-plan-config :one-shot {:num-warmup-samples 3})
             collector
             measured)
            (is (= 4 @counter)
                "Expected 4 invocations (3 warmup + 1 measurement)"))))
      (testing "returns result from measurement invocation (not warmup)"
        (let [counter  (atom 0)
              measured (measured/measured
                        (fn [] [])
                        (fn [_ _]
                          (swap! counter inc)
                          [1000 @counter]))
              result   (collect-plan/collect
                        (collect-plan-config/collect-plan-config
                         :one-shot
                         {:num-warmup-samples 2})
                        collector
                        measured)]
          (is (= 3 (:expr-value (:samples result)))
              "expr-value should be from the 3rd invocation (after 2 warmups)"))))))
