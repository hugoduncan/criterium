(ns criterium.agent-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.agent :as agent]))

(dotimes [_ 100]
  (agent/with-allocation-tracing 1))

(deftest with-allocation-tracing-test
  (let [[allocs rv] (agent/with-allocation-tracing 1)]
    (is (= 1 rv))
    (is (empty? allocs))))
