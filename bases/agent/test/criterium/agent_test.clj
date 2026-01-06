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
   [clojure.string :as str]
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

(deftest call-tracing-wrapper-tree-test
  ;; Tests that with-call-tracing returns a tree rooted at the run-traced* wrapper
  ;; and contains the expected user function calls.
  (testing "with-call-tracing wrapper tree"
    (when (agent/attached?)
      (testing "returns tree rooted at user's anonymous fn"
        (let [[call-tree rv] (agent/with-call-tracing
                               (simple-computation 5))]
          (is (= 6 rv))
          (is (map? call-tree)
              "Should return a call tree")
          (when (map? call-tree)
            ;; The root should be the user's anonymous fn (created by the macro)
            ;; which is inside run-traced* - but run-traced* itself is filtered out
            (is (string? (:class call-tree))
                "Root should have :class")
            ;; The tree should contain simple-computation
            (letfn [(find-class [node class-prefix]
                      (or (and (:class node)
                               (str/starts-with? (:class node) class-prefix))
                          (some #(find-class % class-prefix) (:children node))))]
              (is (find-class call-tree "criterium.agent_test$simple_computation")
                  "Tree should contain simple-computation call"))))))))

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

;;; Call Tree Filter Tests

(def sample-call-tree
  "Sample call tree for filter testing."
  {:class "myapp.Core"
   :method "main"
   :file "Core.java"
   :line 10
   :call-count 1
   :children
   [{:class "myapp.Service"
     :method "process"
     :file "Service.java"
     :line 20
     :call-count 5
     :children
     [{:class "java.util.ArrayList"
       :method "add"
       :file nil
       :line -1
       :call-count 10
       :children
       [{:class "java.util.Arrays"
         :method "copyOf"
         :file nil
         :line -1
         :call-count 2
         :children []}]}
      {:class "clojure.core$map"
       :method "invoke"
       :file "core.clj"
       :line 100
       :call-count 3
       :children
       [{:class "clojure.lang.LazySeq"
         :method "seq"
         :file nil
         :line -1
         :call-count 3
         :children []}]}]}
    {:class "sun.misc.Unsafe"
     :method "allocate"
     :file nil
     :line -1
     :call-count 1
     :children []}]})

(deftest filter-call-tree-test
  ;; Tests call tree filtering with various filter options.
  ;; Validates :exclude-packages, :stop-at-packages, and :max-depth filters.
  (testing "filter-call-tree"
    (testing "with nil input"
      (is (nil? (agent/filter-call-tree nil {}))
          "Should return nil for nil input")
      (is (nil? (agent/filter-call-tree nil {:max-depth 2}))
          "Should return nil for nil input with options"))

    (testing "with empty options"
      (is (= sample-call-tree (agent/filter-call-tree sample-call-tree {}))
          "Should return unchanged tree with empty options"))

    (testing ":exclude-packages"
      (testing "removes matching nodes and promotes children"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:exclude-packages #{"java."}})]
          (is (= "myapp.Core" (:class result)))
          ;; java.util.ArrayList should be removed, its children promoted
          (let [service (first (:children result))]
            (is (= "myapp.Service" (:class service)))
            ;; java.util.ArrayList removed, java.util.Arrays promoted up
            ;; but java.util.Arrays is also excluded, so it and its children removed
            (let [children-classes (set (map :class (:children service)))]
              (is (not (contains? children-classes "java.util.ArrayList")))
              (is (not (contains? children-classes "java.util.Arrays")))))))

      (testing "excludes root when it matches"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:exclude-packages #{"myapp."}})]
          ;; Root matches, so children are promoted
          ;; First promoted child should be myapp.Service (also excluded)
          ;; Eventually we get to non-matching children or nil
          (is (or (nil? result)
                  (and (:class result)
                       (not (str/starts-with? (:class result) "myapp.")))))))

      (testing "handles multiple exclude prefixes"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:exclude-packages #{"java." "sun."}})]
          (let [root-children (:children result)]
            ;; sun.misc.Unsafe should be removed
            (is (not (some #(= "sun.misc.Unsafe" (:class %)) root-children)))))))

    (testing ":stop-at-packages"
      (testing "truncates children at matching nodes"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:stop-at-packages #{"clojure.core"}})]
          ;; Find the clojure.core$map node
          (let [service (first (:children result))
                clj-map (first (filter #(str/starts-with?
                                         (:class %) "clojure.core")
                                       (:children service)))]
            (is (some? clj-map)
                "clojure.core node should exist")
            (is (= [] (:children clj-map))
                "clojure.core node should have no children"))))

      (testing "keeps non-matching nodes unchanged"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:stop-at-packages #{"clojure.core"}})]
          ;; java.util.ArrayList should still have children
          (let [service (first (:children result))
                arraylist (first (filter #(= "java.util.ArrayList" (:class %))
                                         (:children service)))]
            (when arraylist
              (is (seq (:children arraylist))
                  "Non-matching node should keep children"))))))

    (testing ":max-depth"
      (testing "depth 1 returns only root"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:max-depth 1})]
          (is (= "myapp.Core" (:class result)))
          (is (= [] (:children result)))))

      (testing "depth 2 returns root and immediate children"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:max-depth 2})]
          (is (= "myapp.Core" (:class result)))
          (is (= 2 (count (:children result))))
          (is (every? #(= [] (:children %)) (:children result)))))

      (testing "depth 3 includes grandchildren"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:max-depth 3})]
          (let [service (first (:children result))]
            (is (= 2 (count (:children service))))
            ;; Grandchildren should exist but have no children
            (is (every? #(= [] (:children %))
                        (:children service)))))))

    (testing "combined filters"
      (testing ":exclude-packages with :max-depth"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:exclude-packages #{"sun."}
                       :max-depth 2})]
          (is (= "myapp.Core" (:class result)))
          ;; sun.misc.Unsafe should be excluded
          (is (not (some #(= "sun.misc.Unsafe" (:class %))
                         (:children result))))
          ;; Children should have no children due to depth limit
          (is (every? #(= [] (:children %)) (:children result)))))

      (testing ":stop-at-packages with :exclude-packages"
        (let [result (agent/filter-call-tree
                      sample-call-tree
                      {:exclude-packages #{"java."}
                       :stop-at-packages #{"clojure.core"}})]
          ;; java.* nodes excluded
          (let [service (first (:children result))]
            (is (not (some #(str/starts-with? (:class %) "java.")
                           (:children service))))
            ;; clojure.core node truncated
            (let [clj-map (first (filter #(str/starts-with?
                                           (:class %) "clojure.core")
                                         (:children service)))]
              (when clj-map
                (is (= [] (:children clj-map)))))))))))

(deftest jdk-filter-test
  ;; Tests the predefined JDK filter.
  (testing "jdk-filter"
    (testing "is a valid filter map"
      (is (map? agent/jdk-filter))
      (is (contains? agent/jdk-filter :exclude-packages))
      (is (set? (:exclude-packages agent/jdk-filter))))

    (testing "excludes JDK packages"
      (let [prefixes (:exclude-packages agent/jdk-filter)]
        (is (contains? prefixes "java."))
        (is (contains? prefixes "javax."))
        (is (contains? prefixes "jdk."))
        (is (contains? prefixes "sun."))
        (is (contains? prefixes "com.sun."))))

    (testing "filters sample tree correctly"
      (let [result (agent/filter-call-tree sample-call-tree agent/jdk-filter)]
        ;; Should not contain any JDK classes
        (letfn [(contains-jdk? [node]
                  (or (some #(str/starts-with? (:class node) %)
                            (:exclude-packages agent/jdk-filter))
                      (some contains-jdk? (:children node))))]
          (is (not (contains-jdk? result))
              "Result should not contain JDK classes"))))))

(deftest clojure-core-boundary-filter-test
  ;; Tests the predefined Clojure core boundary filter.
  (testing "clojure-core-boundary-filter"
    (testing "is a valid filter map"
      (is (map? agent/clojure-core-boundary-filter))
      (is (contains? agent/clojure-core-boundary-filter :stop-at-packages))
      (is (set? (:stop-at-packages agent/clojure-core-boundary-filter))))

    (testing "stops at clojure.core and clojure.lang"
      (let [prefixes (:stop-at-packages agent/clojure-core-boundary-filter)]
        (is (contains? prefixes "clojure.core"))
        (is (contains? prefixes "clojure.lang."))))

    (testing "filters sample tree correctly"
      (let [result (agent/filter-call-tree
                    sample-call-tree
                    agent/clojure-core-boundary-filter)]
        ;; Find clojure.core$map - it should exist but have no children
        (let [service (first (:children result))
              clj-map (first (filter #(str/starts-with?
                                       (:class %) "clojure.core")
                                     (:children service)))]
          (is (some? clj-map)
              "clojure.core node should exist")
          (is (= [] (:children clj-map))
              "clojure.core node should have empty children"))))))

;; Warmup for allocation tests
(dotimes [_ 100]
  (agent/with-allocation-tracing 1))
