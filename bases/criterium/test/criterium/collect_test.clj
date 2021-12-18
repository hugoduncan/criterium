(ns criterium.collect-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.collect :as collect]
   [criterium.collector :as collector]
   [criterium.measured :as measured]))

(deftest full-zero-garbage-test
  (testing "full sampling"
    (let [measured              (measured/measured
                                 (fn [] [:b])
                                 (fn [_ _] [10000000 1]))
          pipeline              (collector/collector
                                 {:stages     [:garbage-collector :compilation]
                                  :terminator :elapsed-time})
          ;; run pipeline for JIT
          _                     (dotimes [_ 10000]
                                  (collect/collect-arrays
                                   pipeline
                                   measured
                                   1
                                   10))
          [allocations sampled] (agent/with-allocation-tracing
                                  (collect/collect-arrays
                                   pipeline
                                   measured
                                   1
                                   10))
          {:keys [freed-bytes]} (->> allocations
                                     (filterv (agent/allocation-on-thread?))
                                     agent/allocations-summary)]
      (is (= 10 (alength ^objects (:samples sampled))))
      (is (zero? freed-bytes)
          (->> allocations
               (filterv (agent/allocation-on-thread?))
               (filterv agent/allocation-freed?)))
      (is (some? sampled) "hold onto samples reference until this point"))))
