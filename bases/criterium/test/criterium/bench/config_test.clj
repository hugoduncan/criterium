(ns criterium.bench.config-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.bench.config :as bench-config]
   [criterium.benchmarks :as benchmarks]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.measured :as measured]))

(deftest config-map-test
  (let [measured (measured/expr 1)]
    (measured/invoke measured (measured/args measured) 1)
    (testing "config-map provides defaults"
      (is (= (-> bench-config/default-config
                 (#'collect-plan-config/ensure-pipeline-stages))
             (bench-config/config-map {}))))
    (testing "config-map can specify the pipeline stages"
      (is (= (-> (assoc bench-config/default-config
                        :collector-config
                        {:stages     (mapv
                                      collector/maybe-var-get-stage
                                      [:class-loader
                                       :compilation
                                       :garbage-collector
                                       :measured-args])
                         :terminator (collector/maybe-var-get-stage
                                      :elapsed-time)}))
             (bench-config/config-map
              {:metric-ids [:class-loader
                            :compilation
                            :garbage-collector]}))))
    (testing "config-map can specify the sample scheme"
      (is (= (-> bench-config/default-config
                 (assoc-in [:collector-config :stages] [])
                 (assoc
                  :benchmark benchmarks/one-shot
                  :collect-plan
                  (collect-plan-config/one-shot-collect-plan {})))
             (bench-config/config-map {:collect-plan :one-shot}))))))
