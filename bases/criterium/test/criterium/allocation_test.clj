(ns criterium.allocation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.allocation :as allocation]
   [criterium.types :as types]))

;; Tests for criterium.allocation namespace.
;; Validates the allocation tracing wrapper that bridges the native agent
;; to the criterium pipeline architecture.

(deftest trace?-test
  (testing "trace?"
    (testing "is the same as types/allocation-trace?"
      (is (= allocation/trace? types/allocation-trace?)))
    (testing "returns true for valid allocation trace"
      (is (true? (allocation/trace?
                  {:type :criterium/allocation-trace
                   :records []
                   :thread-id 1
                   :eval-count 100
                   :elapsed-time 1.5e9}))))
    (testing "returns false for invalid trace"
      (is (false? (allocation/trace? {:type :other})))
      (is (false? (allocation/trace? nil))))))

(deftest filter-thread-test
  (testing "filter-thread"
    (let [records [{:thread 1 :object-type "String"}
                   {:thread 2 :object-type "Long"}
                   {:thread 1 :object-type "Vector"}
                   {:thread 3 :object-type "Map"}]]
      (testing "filters records by specified thread-id"
        (is (= [{:thread 1 :object-type "String"}
                {:thread 1 :object-type "Vector"}]
               (vec (allocation/filter-thread records 1)))))
      (testing "returns empty for non-matching thread"
        (is (= [] (vec (allocation/filter-thread records 999)))))
      (testing "returns all matching records"
        (is (= 1 (count (allocation/filter-thread records 2))))
        (is (= 1 (count (allocation/filter-thread records 3))))))))

(deftest with-allocation-trace-test
  (testing "with-allocation-trace"
    (testing "returns result from body"
      (let [[_trace result] (allocation/with-allocation-trace {}
                              (+ 1 2))]
        (is (= 3 result))))
    (testing "returns nil trace when agent not attached"
      ;; Without agent, we get [nil result]
      (let [[trace result] (allocation/with-allocation-trace {}
                             :test-value)]
        ;; Agent may or may not be attached in test environment
        (is (= :test-value result))
        (when trace
          (is (allocation/trace? trace)))))
    (testing "uses default eval-count of 1"
      (let [[trace _] (allocation/with-allocation-trace {}
                        nil)]
        (when trace
          (is (= 1 (:eval-count trace))))))
    (testing "accepts custom eval-count"
      (let [[trace _] (allocation/with-allocation-trace {:eval-count 100}
                        nil)]
        (when trace
          (is (= 100 (:eval-count trace))))))
    (testing "captures thread-id"
      (let [current-thread (.getId (Thread/currentThread))
            [trace _] (allocation/with-allocation-trace {}
                        nil)]
        (when trace
          (is (= current-thread (:thread-id trace))))))
    (testing "measures elapsed-time"
      (let [[trace _] (allocation/with-allocation-trace {}
                        (Thread/sleep 10))]
        (when trace
          (is (pos? (:elapsed-time trace)))
          ;; Should be at least 10ms = 10_000_000 ns
          (is (>= (:elapsed-time trace) 10000000)))))))
