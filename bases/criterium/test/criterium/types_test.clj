(ns criterium.types-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.types :as types]))

;; Tests for allocation-trace? type predicate.
;; Validates the allocation trace type used by the allocation
;; tracking pipeline.

(deftest allocation-trace?-test
  (testing "allocation-trace?"
    (testing "returns true for valid allocation trace"
      (is (true? (types/allocation-trace?
                  {:type :criterium/allocation-trace
                   :records []
                   :thread-id 1
                   :eval-count 100
                   :elapsed-time 1.5e9}))))
    (testing "returns true when records is non-empty"
      (is (true? (types/allocation-trace?
                  {:type :criterium/allocation-trace
                   :records [{:object-type "Ljava/lang/String;"
                              :object-size 48}]
                   :thread-id 1
                   :eval-count 100
                   :elapsed-time 1.5e9}))))
    (testing "returns true with extra keys"
      (is (true? (types/allocation-trace?
                  {:type :criterium/allocation-trace
                   :records []
                   :thread-id 1
                   :eval-count 100
                   :elapsed-time 1.5e9
                   :extra-key "allowed"}))))
    (testing "returns false for wrong type"
      (is (false? (types/allocation-trace?
                   {:type :other
                    :records []
                    :thread-id 1
                    :eval-count 100
                    :elapsed-time 1.5e9}))))
    (testing "returns false for missing type"
      (is (false? (types/allocation-trace?
                   {:records []
                    :thread-id 1
                    :eval-count 100
                    :elapsed-time 1.5e9}))))
    (testing "returns false for missing :records"
      (is (false? (types/allocation-trace?
                   {:type :criterium/allocation-trace
                    :thread-id 1
                    :eval-count 100
                    :elapsed-time 1.5e9}))))
    (testing "returns false for missing :thread-id"
      (is (false? (types/allocation-trace?
                   {:type :criterium/allocation-trace
                    :records []
                    :eval-count 100
                    :elapsed-time 1.5e9}))))
    (testing "returns false for missing :eval-count"
      (is (false? (types/allocation-trace?
                   {:type :criterium/allocation-trace
                    :records []
                    :thread-id 1
                    :elapsed-time 1.5e9}))))
    (testing "returns false for missing :elapsed-time"
      (is (false? (types/allocation-trace?
                   {:type :criterium/allocation-trace
                    :records []
                    :thread-id 1
                    :eval-count 100}))))
    (testing "returns false for non-map"
      (is (false? (types/allocation-trace? nil)))
      (is (false? (types/allocation-trace? "allocation-trace")))
      (is (false? (types/allocation-trace? [:criterium/allocation-trace]))))))
