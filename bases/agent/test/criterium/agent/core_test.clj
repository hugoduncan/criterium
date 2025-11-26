(ns criterium.agent.core-test
    "Tests for the criterium.agent.core namespace.

  Tests cover internal implementation details including:
  - State management and transitions
  - Command protocol implementation
  - Allocation tracking and filtering
  - GC interaction and marker handling

  Test organization:
  - Unit tests for internal functions
  - State transition tests
  - Error condition tests
  - Thread safety tests
  - Performance tests"
    (:require
     [clojure.test :refer [deftest is testing use-fixtures]]
     [criterium.agent.core :as agent-core]
     [criterium.jvm :as jvm]))

;; Test Fixtures

(defn cleanup-fixture
      "Cleanup fixture to ensure consistent test environment."
      [f]
      (try
       (f)
       (finally
        (System/gc)
        (Thread/sleep 100))))

(use-fixtures :each cleanup-fixture)

;; Helper Functions

(defn with-timeout
      "Run body with timeout protection."
      [timeout-ms f]
      (let [future (future (f))]
           (try
            (deref future timeout-ms :timeout)
            (finally
             (future-cancel future)))))

(defmacro when-agent-attached [& body]
          `(if (agent-core/attached?)
               (do ~@body)
               (is true)))

;; JIT warm up for performance sensitive tests
(dotimes [_ 10000] (agent-core/agent-state))

;; Unit Tests

(deftest agent-state-test
         (testing "Basic state access"
                  (is (keyword? (agent-core/agent-state))
                      "Agent state should be a keyword")
                  (is (contains? #{:not-attached :passive
                                   :allocation-tracing-starting :allocation-tracing-active
                                   :allocation-tracing-stopping :allocation-tracing-flushing
                                   :allocation-tracing-flushed :allocation-tracing-reporting
                                   :allocation-tracing-reported}
                                 (agent-core/agent-state))
                      "Agent state should be a valid state keyword"))

         (testing "State caching and updates"
                  (let [initial-state (agent-core/agent-state)
                        second-state (agent-core/agent-state)]
                       (is (= initial-state second-state)
                           "State should be stable between reads"))))

(deftest agent-command-test
         (when-agent-attached
          (testing "Basic command sending"
                   (is (nil? (agent-core/agent-command :ping)))
                   (is (nil? (agent-core/agent-command :sync-state))))
          (testing "Invalid commands"
                   (is (thrown? IllegalArgumentException
                                (agent-core/agent-command :invalid-command))
                       "Invalid commands should throw exceptions"))))

(deftest allocation-tracking-test
         (testing "Start/stop cycle"
                  (when-agent-attached
                   (agent-core/allocation-tracing-start!)
                   (is (agent-core/allocation-tracing-active?)
                       "Tracing should be active after start")

                   (agent-core/allocation-tracing-stop!)
                   (is (not (agent-core/allocation-tracing-active?))
                       "Tracing should be inactive after stop")))

         (testing "Marker allocations"
                  (when-agent-attached
                   (agent-core/allocation-tracing-start!)
                   (agent-core/allocation-start-marker)
                   (let [records @agent-core/records]
                        (is (not-any? #(= (:call-method %) "allocation_start_marker") records)
                            "Marker allocations should be filtered"))
                   (agent-core/allocation-tracing-stop!))))

(deftest allocation-filtering-test
         (testing "Thread filtering"
                  (let [current-thread (jvm/current-thread-id)
                        other-thread (inc current-thread)
                        pred (agent-core/allocation-on-thread? current-thread)]
                       (is (pred {:thread current-thread})
                           "Should match current thread")
                       (is (not (pred {:thread other-thread}))
                           "Should not match other thread")))

         (testing "Freed allocation detection"
                  (is (agent-core/allocation-freed? {:freed 1})
                      "Should detect freed allocation")
                  (is (not (agent-core/allocation-freed? {:freed 0}))
                      "Should detect non-freed allocation")))

;; Integration Tests

(deftest full-allocation-cycle-test
         (testing "Complete allocation tracking cycle"
                  (when-agent-attached
                   (let [result (with-timeout 5000
                                              #(do
                                                (agent-core/allocation-tracing-start!)
                                                (try
                                                 (let [_ (Object.)]
                                                      (System/gc)
                                                      (Thread/sleep 100)
                                                      (System/gc))
                                                 (finally
                                                  (agent-core/allocation-tracing-stop!)
                                                  (agent-core/collect-allocation-records)))
                                                @agent-core/records))]
                        (is (not= :timeout result)
                            "Allocation cycle should complete within timeout")
                        (when-not (= :timeout result)
                                  (is (seq result)
                                      "Should capture allocations")
                                  (is (some #(= (:object-type %) "java.lang.Object") result)
                                      "Should capture Object allocation"))))))

;; Performance Tests

(deftest ^:performance state-access-performance
  ;; Verifies agent-state has acceptable overhead for benchmarking use.
  ;; Uses MethodHandle internally for fast invocation.
  (testing "state-access-performance"
    (testing "completes 1M reads in under 10 seconds"
      (let [start-time (System/nanoTime)
            _results (dotimes [_ 1000000]
                       (agent-core/agent-state))
            elapsed (/ (- (System/nanoTime) start-time) 1e6)]
        (is (< elapsed 10000)
            (str "1M state reads took " elapsed "ms, expected < 10000ms"))))))

;; Agent Loading API Tests

;; NOTE: Tests for pid, agent-path, and load-agent! have been moved to
;; criterium.agent.runtime-test since those functions were refactored to
;; the runtime namespace to avoid compile-time dependencies on Agent classes.
