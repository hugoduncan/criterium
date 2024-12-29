(ns criterium.platform-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.platform :as platform]))

(deftest platform-point-estimates-test
  (testing "platform-point-estimates"
    (let [res (platform/platform-point-estimates {:limit-time-s 0.001})]
      (is (= [:latency
              :granularity
              :constant-long
              :constant-double
              :constant-object
              :constant-nil]
             (keys res)))
      (is (every? double? (vals res))))))
