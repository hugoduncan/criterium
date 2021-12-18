(ns criterium.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.core :as core]))

(deftest bench-test
  (let [s (with-out-str
            (core/bench 1 :target-execution-time 1))]
    (is s)))
