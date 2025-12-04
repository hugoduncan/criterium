(ns criterium.collect-plan.impl-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.collect-plan.impl :as impl]))

(deftest limit-samples-test
  (let [s (with-out-str
            (let [[nw nm] (impl/limit-samples
                           10000
                           1000 100
                           0
                           10000
                           0)]
              (is (= 1000 nw))
              (is (= 100 nm))))]
    (is (= "" s)))
  (let [s (with-out-str
            (let [[nw nm] (impl/limit-samples
                           10000
                           1000 100
                           5000
                           5000
                           10000)]
              (is (= 500 nw))
              (is (= 50 nm))))]
    (is (str/includes? s "time required for full JIT is 1.50e-05s"))
    (is (str/includes? s "limited to 1.00e-05s"))
    (is (str/includes? s "pass `:limit-time-s 1.50e-05` to improve accuracy"))
    (is (str/includes? s "consider benchmarks at a lower level"))))
