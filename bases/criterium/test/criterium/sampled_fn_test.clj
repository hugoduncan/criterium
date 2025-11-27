(ns criterium.sampled-fn-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.collector-configs :as collector-configs]
   [criterium.jvm :as jvm]
   [criterium.sampled-fn :as sampled-fn]
   [criterium.sampler :as sampler]
   [criterium.util.t-digest :as t-digest]))

(defn- ten-micros
  []
  (jvm/wait 10000))

(deftest sampled-fn-test
  (with-redefs
    [ten-micros (sampled-fn/sample-fn
                 ten-micros
                 collector-configs/default-collector-config)]
    (dotimes [_ 10000]
      (ten-micros))

    (let [samples (sampler/samples-map ten-micros)
          digest  (get-in samples [:metric->digest [:elapsed-time]])]
      (is (< 10000 (t-digest/quantile digest 0.5) 10500)))))
