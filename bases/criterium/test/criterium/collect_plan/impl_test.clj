(ns criterium.collect-plan.impl-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.collect-plan.impl :as impl]))

(deftest limit-samples-test
  (let [s (with-out-str
            (let [{:keys [num-warmup-samples num-measure-samples]}
                  (impl/limit-samples
                   10000
                   1000 100
                   0
                   10000
                   0)]
              (is (= 1000 num-warmup-samples))
              (is (= 100 num-measure-samples))))]
    (is (= "" s)))
  (let [s (with-out-str
            (let [{:keys [num-warmup-samples num-measure-samples]}
                  (impl/limit-samples
                   10000
                   1000 100
                   5000
                   5000
                   10000)]
              (is (= 500 num-warmup-samples))
              (is (= 50 num-measure-samples))))]
    (is (str/includes? s "time required for full JIT is 1.50e-05s"))
    (is (str/includes? s "limited to 1.00e-05s"))
    (is (str/includes? s "pass `:limit-time-s 1.50e-05` to improve accuracy"))
    (is (str/includes? s "consider benchmarks at a lower level"))))
