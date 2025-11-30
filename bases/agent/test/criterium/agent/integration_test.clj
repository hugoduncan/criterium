(ns criterium.agent.integration-test
  "Integration tests for agent extraction, loading, and JNI interface.

  Tests verify the complete end-to-end flow:
  - Extraction of bundled agent binary to temp directory
  - Programmatic loading of agent via VirtualMachine.attach
  - JNI interface functionality after programmatic loading
  - Allocation tracking capabilities

  Test organization:
  - Extraction flow tests (agent-path)
  - Loading flow tests (load-agent!)
  - JNI interface verification tests
  - Full integration smoke tests

  Note: These tests require the agent binary to be present in
  resources/native/{platform}/ and cannot run if the agent is
  already loaded via -agentpath."
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.agent.core :as agent-core]
   [criterium.agent.platform :as platform]
   [criterium.agent.runtime :as runtime]))

(defmacro when-agent-binary-available
  "Run tests only if agent binary exists for current platform."
  [& body]
  `(if-let [platform# (platform/detect)]
     (let [resource-path# (platform/resource-path platform#)]
       (if (io/resource resource-path#)
         (do ~@body)
         (is true "Agent binary not available for current platform")))
     (is true "Platform not supported")))

(defmacro when-agent-not-attached
  "Run tests only if agent is not currently attached.

  This is necessary because VirtualMachine.attach can only load an agent
  once per JVM. If the agent is already loaded via -agentpath, these tests
  will be skipped."
  [& body]
  `(if (agent-core/attached?)
     (is true "Agent already attached, skipping programmatic load test")
     (do ~@body)))

(deftest ^:requires-agent extraction-flow-test
         ;; Agent extraction to temp directory
  (testing "agent-path extraction flow"
    (when-agent-binary-available
     (testing "extracts agent to temp directory"
       (let [path (runtime/agent-path)]
         (is (some? path)
             "Should return path to extracted agent")
         (when path
           (is (.exists (io/file path))
               "Extracted file should exist")
           (is (.isFile (io/file path))
               "Path should point to a file")
           (is (.canRead (io/file path))
               "File should be readable")
           (is (.canExecute (io/file path))
               "File should be executable"))))

     (testing "returns same path on repeated calls"
       (let [path1 (runtime/agent-path)
             path2 (runtime/agent-path)]
         (is (= path1 path2)
             "Multiple calls should return same path")))

     (testing "extracted file has correct naming"
       (let [path (runtime/agent-path)
             platform (platform/detect)]
         (when (and path platform)
           (let [filename (.getName (io/file path))]
             (is (re-matches
                  (re-pattern
                   (str "criterium-agent-"
                        platform
                        "-[a-f0-9]{64}"
                        "\\."
                        (platform/extension platform)))
                  filename)
                 "Filename should match expected pattern"))))))))

(deftest ^:requires-agent loading-flow-test
  ;; Programmatic agent loading
  (testing "load-agent! loading flow"
    (when-agent-binary-available
     (when-agent-not-attached
      (testing "loads agent successfully"
        (is (nil? (runtime/load-agent!))
            "load-agent! should return nil on success")
        (is (agent-core/attached?)
            "Agent should be attached after load-agent!")
        (is (agent/loaded?)
            "loaded? should return true after load-agent!"))

      (testing "throws IllegalStateException on second load attempt"
        (when (agent-core/attached?)
          (is (thrown? IllegalStateException
                       (runtime/load-agent!))
              "Should throw when agent already loaded")))))))

(deftest ^:requires-agent jni-interface-test
         ;; JNI interface verification
  (testing "JNI interface after programmatic loading"
    (when-agent-binary-available
     (when-agent-not-attached
      (runtime/load-agent!))

     (when (agent-core/attached?)
       (testing "agent state is accessible"
         (let [state (agent-core/agent-state)]
           (is (keyword? state)
               "Agent state should be a keyword")
           (is (not= state :not-attached)
               "Agent should not be in :not-attached state")))

       (testing "agent commands are executable"
         (is (number? (agent-core/agent-command :ping))
             "Ping command should return numeric state"))

       (testing "allocation tracking can be started and stopped"
         (agent-core/allocation-tracing-start!)
         (is (agent-core/allocation-tracing-active?)
             "Tracing should be active after start")

         (agent-core/allocation-tracing-stop!)
         (is (not (agent-core/allocation-tracing-active?))
             "Tracing should be inactive after stop"))))))

(deftest ^:requires-agent allocation-tracking-smoke-test
  ;; Smoke test for full allocation tracking
  (testing "allocation tracking after agent extraction and loading"
    (when-agent-binary-available
     (when-agent-not-attached
      (runtime/load-agent!))

     (if (agent-core/attached?)
       (testing "captures allocations"
         (reset! agent-core/records [])
         (agent-core/allocation-tracing-start!)
         (try
            ;; Allocate some objects
           (let [_ (Object.)
                 _ (String. "test")
                 _ (java.util.ArrayList.)]
             (System/gc)
             (Thread/sleep 50))
           (finally
             (agent-core/allocation-tracing-stop!)
             (agent-core/collect-allocation-records)))

         (let [records @agent-core/records]
           (is (seq records)
               "Should capture some allocations")
           (when (seq records)
             (is (every? map? records)
                 "Each record should be a map")
             (is (some #(= (:object-type %) "java.lang.Object") records)
                 "Should capture Object allocation"))))
       (is true "no agent attached")))))

(deftest ^:requires-agent concurrent-extraction-test
  ;; Concurrent extraction safety
  (testing "concurrent agent-path calls"
    (when-agent-binary-available
     (testing "multiple threads extract safely"
       (let [paths (doall
                    (pmap (fn [_] (runtime/agent-path))
                          (range 10)))]
         (is (every? some? paths)
             "All threads should get a path")
         (is (apply = paths)
             "All threads should get the same path"))))))
