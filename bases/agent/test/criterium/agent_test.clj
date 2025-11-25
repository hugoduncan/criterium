(ns criterium.agent-test
    "Tests for the criterium.agent namespace.

  Tests cover four main areas:
  1. Agent Attachment - Verifying agent initialization and status
  2. Allocation Tracking - Testing allocation capture functionality
  3. Thread Filtering - Testing thread-specific allocation filtering
  4. Results Analysis - Testing allocation summary and statistics

  Test organization:
  - Unit tests verify individual function behavior
  - Integration tests verify interaction between components
  - Zero-garbage tests verify allocation behavior

  Note: Some tests require the native agent to be properly attached."
    (:require
     [clojure.test :refer [deftest is testing use-fixtures]]
     [criterium.agent :as agent]
     [criterium.agent.core :as agent-core]
     [criterium.jvm :as jvm]))

;; Test Fixtures

(defn cleanup-fixture
      "Cleanup fixture to ensure consistent test environment."
      [f]
      (try
       (f)
       (finally
      ;; Force GC to clean up any test allocations
        (jvm/run-finalization-and-force-gc!))))

(use-fixtures :each cleanup-fixture)

;; Helper Functions

(defn make-test-allocation
      "Creates a test allocation by constructing a string."
      []
      (str "test-allocation-" (rand-int 1000)))

;; Unit Tests

(deftest attached?-test
  ;; Test that attached? works with both loading methods
         (testing "attached?"
                  (testing "returns boolean"
                           (is (boolean? (agent/attached?))
                               "Should return a boolean indicating attachment status"))

                  (testing "detects agent via -agentpath"
                    ;; When agent is loaded via -agentpath, state will be set by native library
                           (with-redefs [agent-core/agent-state (constantly :passive)]
                                        (is (agent/attached?)
                                            "Should detect agent loaded via -agentpath")))

                  (testing "detects agent via load-agent!"
                    ;; After load-agent!, state is also updated by native library
                           (with-redefs [agent-core/agent-state (constantly :passive)]
                                        (is (agent/attached?)
                                            "Should detect agent loaded via load-agent!")))

                  (testing "returns false when not attached"
                           (with-redefs [agent-core/agent-state (constantly :not-attached)]
                                        (is (not (agent/attached?))
                                            "Should return false when agent not loaded")))))

(deftest loaded?-test
  ;; Test that loaded? is an alias for attached? and works with both loading methods
         (testing "loaded?"
                  (testing "returns boolean"
                           (is (boolean? (agent/loaded?))
                               "Should return a boolean indicating loaded status"))

                  (testing "matches attached? behavior"
                           (is (= (agent/attached?) (agent/loaded?))
                               "loaded? should match attached? result"))

                  (testing "detects agent via -agentpath"
                    ;; Both attached? and loaded? should work for -agentpath loading
                           (with-redefs [agent-core/agent-state (constantly :passive)]
                                        (is (agent/loaded?)
                                            "Should detect agent loaded via -agentpath")
                                        (is (= (agent/attached?) (agent/loaded?))
                                            "loaded? and attached? should agree")))

                  (testing "detects agent via load-agent!"
                    ;; Both attached? and loaded? should work for programmatic loading
                           (with-redefs [agent-core/agent-state (constantly :passive)]
                                        (is (agent/loaded?)
                                            "Should detect agent loaded via load-agent!")
                                        (is (= (agent/attached?) (agent/loaded?))
                                            "loaded? and attached? should agree")))

                  (testing "returns false when not attached"
                           (with-redefs [agent-core/agent-state (constantly :not-attached)]
                                        (is (not (agent/loaded?))
                                            "Should return false when agent not loaded")
                                        (is (= (agent/attached?) (agent/loaded?))
                                            "loaded? and attached? should agree")))))

(deftest jvm-opts-test
  ;; Test that jvm-opts returns correct JVM argument format
         (testing "jvm-opts"
                  (testing "returns vector"
                           (let [opts (agent/jvm-opts)]
                                (is (vector? opts)
                                    "Should return a vector")))

                  (testing "returns -agentpath argument when agent available"
                           (with-redefs [criterium.agent.runtime/agent-path (constantly "/tmp/test-agent.so")]
                                        (let [opts (agent/jvm-opts)]
                                             (is (= 1 (count opts))
                                                 "Should return single argument")
                                             (is (= "-agentpath:/tmp/test-agent.so" (first opts))
                                                 "Should format -agentpath with agent path"))))

                  (testing "returns empty vector when agent unavailable"
                           (with-redefs [criterium.agent.runtime/agent-path (constantly nil)]
                                        (let [opts (agent/jvm-opts)]
                                             (is (= [] opts)
                                                 "Should return empty vector when agent unavailable"))))

                  (testing "handles platform-specific extensions"
                           (with-redefs [criterium.agent.runtime/agent-path (constantly "/tmp/criterium-agent-linux-x64-abc123.so")]
                                        (let [opts (agent/jvm-opts)]
                                             (is (= "-agentpath:/tmp/criterium-agent-linux-x64-abc123.so" (first opts))
                                                 "Should handle .so extension")))

                           (with-redefs [criterium.agent.runtime/agent-path (constantly "/tmp/criterium-agent-macos-x64-abc123.dylib")]
                                        (let [opts (agent/jvm-opts)]
                                             (is (= "-agentpath:/tmp/criterium-agent-macos-x64-abc123.dylib" (first opts))
                                                 "Should handle .dylib extension"))))))

(deftest with-allocation-tracing-test
         (testing "Basic tracing functionality"
                  (let [[allocs rv] (agent/with-allocation-tracing 1)]
                       (is (= 1 rv)
                           "Should return the result value unchanged")
                       (when (agent/attached?)
                             (is (vector? allocs)
                                 "Should return allocations as a vector"))))

         (testing "Tracing with allocations"
                  (when (agent/attached?)
                        (let [[allocs rv] (agent/with-allocation-tracing
                                           (make-test-allocation))]
                             (is (string? rv)
                                 "Should return the created string")
                             (is (seq allocs)
                                 "Should capture string allocation")
                             (is (some #(= (:object-type %) "java.lang.String")
                                       allocs)
                                 "Should include string allocation record"))))

         (testing "Nested tracing calls"
                  (let [[outer-allocs outer-rv]
                        (agent/with-allocation-tracing
                         (let [[inner-allocs inner-rv]
                               (agent/with-allocation-tracing 1)]
                              (is (= 1 inner-rv))
                              (when (agent/attached?)
                                    (is (vector? inner-allocs)))
                              2))]
                       (is (= 2 outer-rv))
                       (when (agent/attached?)
                             (is (vector? outer-allocs)))))

         (testing "Exception handling"
                  (is (thrown? Exception
                               (agent/with-allocation-tracing
                                (throw (Exception. "test exception")))))))

(deftest allocation-on-thread?-test
         (testing "Thread filtering predicate"
                  (let [current-thread (jvm/current-thread-id)
                        pred (agent/allocation-on-thread?)]
                       (is (fn? pred)
                           "Should return a predicate function")
                       (is (pred {:thread current-thread})
                           "Should match current thread")
                       (is (not (pred {:thread (inc current-thread)}))
                           "Should not match other threads")))

         (testing "Explicit thread ID"
                  (let [test-thread 12345
                        pred (agent/allocation-on-thread? test-thread)]
                       (is (pred {:thread test-thread})
                           "Should match specified thread")
                       (is (not (pred {:thread 0}))
                           "Should not match other threads"))))

(deftest allocation-freed?-test
         (testing "Freed allocation detection"
                  (is (agent/allocation-freed? {:freed 1})
                      "Should identify freed allocation")
                  (is (not (agent/allocation-freed? {:freed 0}))
                      "Should identify non-freed allocation")))

(deftest allocations-summary-test
         (testing "Empty allocation summary"
                  (let [summary (agent/allocations-summary [])]
                       (is (= {:num-allocated 0
                               :num-freed 0
                               :allocated-bytes 0
                               :freed-bytes 0}
                              summary)
                           "Should return zero summary for empty input")))

         (testing "Allocation summary with records"
                  (let [records [{:object_size 100 :freed 1}
                                 {:object_size 200 :freed 0}
                                 {:object_size 300 :freed 1}]
                        summary (agent/allocations-summary records)]
                       (is (= {:num-allocated 3
                               :num-freed 2
                               :allocated-bytes 600
                               :freed-bytes 400}
                              summary)
                           "Should correctly summarize allocation records"))))

;; Integration Tests

(deftest allocation-tracking-integration-test
         (testing "End-to-end allocation tracking"
                  (if (agent/attached?)
                      (let [[allocs result] (agent/with-allocation-tracing
                                             (make-test-allocation))
                            current-thread (jvm/current-thread-id)
                            thread-allocs (filter (agent/allocation-on-thread?) allocs)
                            summary (agent/allocations-summary thread-allocs)]
                           (is (string? result)
                               "Should complete allocation operation")
                           (is (pos? (:num-allocated summary))
                               "Should capture allocations")
                           (is (every? #(= current-thread (:thread %)) thread-allocs)
                               "Should correctly filter thread allocations"))
                      (is true))))

;; Warmup for allocation tests
(dotimes [i 100]
         (agent/with-allocation-tracing 1))
