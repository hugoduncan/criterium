(ns criterium.agent-test
  "Tests for the criterium.agent namespace.

  Tests cover five main areas:
  1. Agent Attachment - Verifying agent initialization and status
  2. Allocation Tracking - Testing allocation capture functionality
  3. Call Tracing - Testing method call tracing and call tree building
  4. Thread Filtering - Testing thread-specific allocation filtering
  5. Results Analysis - Testing allocation summary and statistics

  Test organization:
  - Unit tests verify individual function behavior
  - Integration tests verify interaction between components
  - Zero-garbage tests verify allocation behavior

  Note: Some tests require the native agent to be properly attached."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.agent.core :as agent-core]
   [criterium.agent.runtime :as runtime]
   [criterium.jvm :as jvm]))

;;; Test Helpers

(defmacro with-gc-cleanup
  "Wraps body in try/finally that forces GC cleanup after execution.
  Used for allocation-related tests to ensure consistent state."
  [& body]
  `(try
     ~@body
     (finally
       (jvm/run-finalization-and-force-gc!))))

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
      (with-redefs [runtime/loaded? (constantly true)
                    agent-core/agent-state (constantly :passive)]
        (is (agent/loaded?)
            "Should detect agent loaded via -agentpath")
        (is (= (agent/attached?) (agent/loaded?))
            "loaded? and attached? should agree")))

    (testing "detects agent via load-agent!"
                    ;; Both attached? and loaded? should work for programmatic loading
      (with-redefs [runtime/loaded? (constantly true)
                    agent-core/agent-state (constantly :passive)]
        (is (agent/loaded?)
            "Should detect agent loaded via load-agent!")
        (is (= (agent/attached?) (agent/loaded?))
            "loaded? and attached? should agree")))

    (testing "returns false when not attached"
      (with-redefs [runtime/loaded? (constantly false)
                    agent-core/agent-state (constantly :not-attached)]
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
  ;; Tests allocation tracing macro behavior and nested tracing
  (with-gc-cleanup
    (testing "with-allocation-tracing"
      (testing "returns the result value unchanged"
        (let [[allocs rv] (agent/with-allocation-tracing 1)]
          (is (= 1 rv))
          (when (agent/attached?)
            (is (vector? allocs)))))

      (testing "when agent attached"
        (testing "captures string allocation"
          (when (agent/attached?)
            (let [[allocs rv] (agent/with-allocation-tracing
                                (make-test-allocation))]
              (is (string? rv))
              (is (seq allocs))
              (is (some #(= (:object-type %) "java.lang.String")
                        allocs))))))

      (testing "with nested tracing calls"
        (testing "returns outer value and captures allocations"
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
              (is (vector? outer-allocs))))))

      (testing "propagates exceptions"
        (is (thrown? Exception
                     (agent/with-allocation-tracing
                       (throw (Exception. "test exception")))))))))

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
    (is (agent/allocation-freed? {:freed true})
        "Should identify freed allocation")
    (is (not (agent/allocation-freed? {:freed false}))
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
    (let [records [{:object_size 100 :freed true}
                   {:object_size 200 :freed false}
                   {:object_size 300 :freed true}]
          summary (agent/allocations-summary records)]
      (is (= {:num-allocated 3
              :num-freed 2
              :allocated-bytes 600
              :freed-bytes 400}
             summary)
          "Should correctly summarize allocation records"))))

;; Integration Tests

(deftest allocation-tracking-integration-test
  ;; Tests end-to-end allocation tracking with thread filtering and summary
  (with-gc-cleanup
    (testing "allocation-tracking-integration"
      (testing "when agent attached"
        (testing "captures and summarizes thread allocations"
          (if (agent/attached?)
            (let [[allocs result] (agent/with-allocation-tracing
                                    (make-test-allocation))
                  current-thread  (jvm/current-thread-id)
                  thread-allocs   (filter (agent/allocation-on-thread?) allocs)
                  summary         (agent/allocations-summary thread-allocs)]
              (is (string? result))
              (is (pos? (:num-allocated summary)))
              (is (every? #(= current-thread (:thread %)) thread-allocs)))
            (is true)))))))

;;; Call Tracing Tests

(defn simple-computation
  "A simple function to trace."
  [x]
  (+ x 1))

(defn nested-computation
  "A function that calls other functions."
  [x]
  (simple-computation (simple-computation x)))

(deftest with-call-tracing-test
  ;; Tests call tracing macro behavior
  (testing "with-call-tracing"
    (testing "returns the result value unchanged"
      (let [[_call-tree rv] (agent/with-call-tracing 42)]
        (is (= 42 rv)
            "Should return body result as second element")))

    (testing "when agent not attached"
      (testing "returns nil call-tree gracefully"
        (with-redefs [agent/attached? (constantly false)]
          (let [[call-tree rv] (agent/with-call-tracing
                                 (simple-computation 1))]
            (is (nil? call-tree)
                "Should return nil when agent not attached")
            (is (= 2 rv)
                "Should still execute body")))))

    (testing "when agent attached"
      (when (agent/attached?)
        (testing "captures method calls"
          (let [[call-tree rv] (agent/with-call-tracing
                                 (nested-computation 1))]
            (is (= 3 rv)
                "Should return body result")
            (is (map? call-tree)
                "Should return call tree map")
            (when (map? call-tree)
              (is (contains? call-tree :children)
                  "Call tree should have :children key")
              (is (vector? (:children call-tree))
                  ":children should be a vector"))))

        (testing "call tree node structure"
          (let [[call-tree _] (agent/with-call-tracing
                                (simple-computation 1))]
            (when (map? call-tree)
              (is (contains? call-tree :class)
                  "Node should have :class")
              (is (contains? call-tree :method)
                  "Node should have :method")
              (is (contains? call-tree :call-count)
                  "Node should have :call-count")
              (is (contains? call-tree :children)
                  "Node should have :children")
              (is (contains? call-tree :file)
                  "Node should have :file")
              (is (contains? call-tree :line)
                  "Node should have :line"))))))

    (testing "propagates exceptions"
      (is (thrown? Exception
                   (agent/with-call-tracing
                     (throw (Exception. "test exception"))))))))

(deftest call-tracing-nested-test
  ;; Tests nested call tracing behavior
  (testing "with-call-tracing nested calls"
    (testing "handles nested tracing"
      (let [[outer-tree outer-rv]
            (agent/with-call-tracing
              (let [[inner-tree inner-rv]
                    (agent/with-call-tracing
                      (simple-computation 1))]
                (is (= 2 inner-rv))
                (when (agent/attached?)
                  (is (or (nil? inner-tree) (map? inner-tree))))
                3))]
        (is (= 3 outer-rv))
        (when (agent/attached?)
          (is (or (nil? outer-tree) (map? outer-tree))))))))

;; Warmup for allocation tests
(dotimes [_ 100]
  (agent/with-allocation-tracing 1))
