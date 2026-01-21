(ns criterium.collect-plan.config-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan.config :as config]))

;; Tests for collect-plan-config :one-shot method
;; Verifies :num-warmup-samples option handling and defaults.

(deftest collect-plan-config-one-shot-test
  (testing ":one-shot collect-plan-config"
    (testing "returns default :num-warmup-samples of 0 when not specified"
      (let [config (config/collect-plan-config :one-shot {})]
        (is (= 0 (:num-warmup-samples config)))))
    (testing "returns provided :num-warmup-samples value"
      (let [config (config/collect-plan-config :one-shot {:num-warmup-samples 5})]
        (is (= 5 (:num-warmup-samples config)))))
    (testing "returns default :max-gc-attempts when not specified"
      (let [config (config/collect-plan-config :one-shot {})]
        (is (= 3 (:max-gc-attempts config)))))
    (testing "returns provided :max-gc-attempts value"
      (let [config (config/collect-plan-config :one-shot {:max-gc-attempts 10})]
        (is (= 10 (:max-gc-attempts config)))))
    (testing "includes :scheme-type :one-shot"
      (let [config (config/collect-plan-config :one-shot {})]
        (is (= :one-shot (:scheme-type config)))))))
