(ns criterium.trigger-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.benchmark :as benchmark]
   [criterium.benchmarks]
   [criterium.trigger :as trigger]))

(deftest triggrt-test
  (let [trigger (trigger/trigger)]
    (trigger/fire! trigger)
    (is (pos? (:last-triggered @trigger)))
    (is (empty? (:samples @trigger)))
    (trigger/fire! trigger)
    (is (pos? (:last-triggered @trigger)))
    (is (= 1 (count (:samples @trigger))))
    (trigger/fire! trigger)
    (is (pos? (:last-triggered @trigger)))
    (is (= 2 (count (:samples @trigger))))
    (let [sampled (trigger/samples! trigger)]
      (is (= 1 (:batch-size sampled)))
      (is (= 2 (:eval-count sampled)))
      (is (map? (:samples sampled)))
      (is (pos? (first ((:samples sampled) [:elapsed-time]))))
      (let [sampled ((benchmark/->benchmark {:analyse [:stats]})
                     sampled)]
        (is (pos? (-> sampled :stats :elapsed-time :mean)))))))
