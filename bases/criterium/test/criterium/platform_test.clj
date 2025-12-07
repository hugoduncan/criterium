(ns criterium.platform-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.platform :as platform]))

(deftest platform-point-estimates-test
  ;; Verifies that platform-point-estimates returns timing characterisation data
  ;; for the platform with expected structure.
  (testing "platform-point-estimates"
    (testing "returns a vector of stat maps"
      (let [res (platform/platform-point-estimates {:limit-time-s 0.001})]
        (is (= ["latency"
                "granularity"
                "constant-long"
                "constant-double"
                "constant-object"
                "constant-nil"]
               (mapv :name res)))
        (is (every? #(and (contains? % :min-ns)
                          (contains? % :mean-ns))
                    res))
        (is (every? #(double? (:min-ns %)) res))
        (is (every? #(double? (:mean-ns %)) res))))))
