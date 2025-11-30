(ns criterium.bench.config-test
    (:require
     [clojure.test :refer [deftest is testing]]
     [criterium.bench-plans :as bench-plans]
     [criterium.bench.config :as bench-config]
     [criterium.collect-plan.config :as collect-plan-config]
     [criterium.collector-configs :as collector-configs]
     [criterium.measured :as measured]))

(deftest config-map-test
         (let [measured (measured/expr 1)]
              (measured/invoke measured (measured/args measured) 1)
              (testing "config-map provides defaults"
                       (is (= (merge
                               bench-plans/default-with-warmup
                               {:collector-config
                                (->>
                                 collector-configs/default-collector-config
                                 (collect-plan-config/ensure-pipeline-stages
                                  :with-jit-warmup))
                                :collect-plan
                                (collect-plan-config/collect-plan-config
                                 :with-jit-warmup {})
                                :return-value [:samples :expr-value]})
                              (bench-config/config-map {}))))
              (testing "config-map can specify the pipeline stages"
                       (is (= (-> (merge
                                   bench-plans/default-with-warmup
                                   {:collector-config
                                    (->>
                                     {:stages     [:class-loader
                                                   :compilation
                                                   :garbage-collector
                                                   :measured-args]
                                      :terminator :elapsed-time}
                                     (collect-plan-config/ensure-pipeline-stages
                                      :with-jit-warmup))
                                    :collect-plan
                                    (collect-plan-config/collect-plan-config
                                     :with-jit-warmup {})
                                    :return-value [:samples :expr-value]}))
                              (bench-config/config-map
                               {:metric-ids [:class-loader
                                             :compilation
                                             :garbage-collector]}))))
              (testing "config-map can specify the sample scheme"
                       (is (= (-> (merge
                                   bench-plans/default-one-shot
                                   {:collector-config
                                    (->>
                                     {:stages     []
                                      :terminator :elapsed-time}
                                     (collect-plan-config/ensure-pipeline-stages
                                      :one-shot))
                                    :collect-plan
                                    (collect-plan-config/collect-plan-config
                                     :one-shot {})
                                    :return-value [:samples :expr-value]}))
                              (bench-config/config-map {:collect-plan :one-shot}))))))
