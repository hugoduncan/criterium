(ns criterium.agent.degradation-test
    "Tests for graceful degradation when agent unavailable or platform unsupported.

  Tests verify that allocation tracking degrades gracefully when:
  - Platform is unsupported (returns nil, continues without error)
  - Agent binary is missing from resources
  - Agent fails to load for any reason
  - Allocation tracking is attempted without agent

  Test organization:
  - Platform detection graceful degradation
  - Agent extraction graceful degradation
  - Allocation tracking graceful degradation
  - Warning logging verification"
    (:require
     [clojure.string :as str]
     [clojure.test :refer [deftest is testing]]
     [criterium.agent :as agent]
     [criterium.agent.loader :as loader]
     [criterium.agent.platform :as platform]
     [criterium.agent.runtime :as runtime]))

(deftest unsupported-platform-test
  ;; Test graceful handling of unsupported platforms
         (testing "unsupported platform detection"
                  (testing "returns nil for unsupported platform"
                           (with-redefs [platform/detect (constantly nil)]
                                        (is (nil? (loader/extract-agent))
                                            "Should return nil when platform unsupported")))

                  (testing "agent-path returns nil for unsupported platform"
                           (with-redefs [loader/extract-agent (constantly nil)]
                                        (is (nil? (runtime/agent-path))
                                            "Should return nil when extraction fails")))

                  (testing "jvm-opts returns empty vector for unsupported platform"
                           (with-redefs [runtime/agent-path (constantly nil)]
                                        (is (= [] (agent/jvm-opts))
                                            "Should return empty vector when agent unavailable")))))

(deftest missing-resources-test
  ;; Test graceful handling when agent binary resources are missing
         (testing "missing agent binary"
                  (testing "agent-path returns nil when binary missing"
                           (with-redefs [loader/extract-agent (constantly nil)]
                                        (is (nil? (runtime/agent-path))
                                            "Should return nil when extraction fails")))

                  (testing "load-agent! throws when agent unavailable"
                           (with-redefs [runtime/loaded? (constantly false)
                                         runtime/agent-path (constantly nil)]
                                        (is (thrown? RuntimeException (runtime/load-agent!))
                                            "Should throw when agent unavailable for platform")))))

(deftest allocation-tracking-without-agent-test
  ;; Test that allocation tracking degrades gracefully when agent unavailable
         (testing "with-allocation-tracing without agent"
                  (testing "returns nil allocations when agent not attached"
                           (with-redefs [agent/attached? (constantly false)]
                                        (let [[allocs result] (agent/with-allocation-tracing
                                                               (+ 1 2 3))]
                                             (is (nil? allocs)
                                                 "Should return nil allocations when agent not attached")
                                             (is (= 6 result)
                                                 "Should still return correct result"))))

                  (testing "returns correct result even without tracking"
                           (with-redefs [agent/attached? (constantly false)]
                                        (let [[allocs result] (agent/with-allocation-tracing
                                                               (str "test" 123))]
                                             (is (nil? allocs)
                                                 "Should return nil allocations")
                                             (is (= "test123" result)
                                                 "Should return correct string result"))))

                  (testing "handles exceptions even without agent"
                           (with-redefs [agent/attached? (constantly false)]
                                        (is (thrown? Exception
                                                     (agent/with-allocation-tracing
                                                      (throw (Exception. "test"))))
                                            "Should propagate exceptions even without agent")))))

(deftest warning-logging-test
  ;; Test that warnings are logged when agent unavailable
         (testing "agent-path logs warning on extraction failure"
                  (let [warnings (atom [])]
                       (with-redefs [loader/extract-agent (fn []
                                                              (throw (Exception. "Platform unsupported")))
                                     println (fn [& args]
                                                 (swap! warnings conj (str/join " " args)))]
                                    (is (nil? (runtime/agent-path))
                                        "Should return nil on extraction failure")
                                    (is (= 1 (count @warnings))
                                        "Should log exactly one warning")
                                    (is (str/includes? (first @warnings) "WARNING")
                                        "Warning should include WARNING prefix")
                                    (is (str/includes? (first @warnings) "Failed to extract agent")
                                        "Warning should mention extraction failure")))))

(deftest jvm-opts-degradation-test
  ;; Test jvm-opts graceful degradation
         (testing "jvm-opts with unavailable agent"
                  (testing "returns empty vector when agent-path returns nil"
                           (with-redefs [runtime/agent-path (constantly nil)]
                                        (is (= [] (agent/jvm-opts))
                                            "Should return empty vector")))

                  (testing "returns proper opts when agent available"
                           (with-redefs [runtime/agent-path (constantly "/tmp/test-agent.so")]
                                        (let [opts (agent/jvm-opts)]
                                             (is (= 1 (count opts))
                                                 "Should return single argument")
                                             (is (= "-agentpath:/tmp/test-agent.so" (first opts))
                                                 "Should format -agentpath correctly"))))))

(deftest load-agent-degradation-test
  ;; Test load-agent! graceful degradation
         (testing "load-agent! error handling"
                  (testing "throws RuntimeException when platform unsupported"
                           (with-redefs [runtime/loaded? (constantly false)
                                         runtime/agent-path (constantly nil)]
                                        (let [ex (is (thrown? RuntimeException
                                                              (runtime/load-agent!))
                                                     "Should throw RuntimeException")]
                                             (when ex
                                                   (is (str/includes? (.getMessage ^Exception ex) "not available")
                                                       "Error message should indicate unavailability")))))

                  (testing "throws IllegalStateException when already loaded"
                           (with-redefs [runtime/loaded? (constantly true)]
                                        (let [ex (is (thrown? IllegalStateException
                                                              (runtime/load-agent!))
                                                     "Should throw IllegalStateException")]
                                             (when ex
                                                   (is (str/includes? (.getMessage ^Exception ex) "already loaded")
                                                       "Error message should indicate already loaded")))))))

(deftest allocations-summary-degradation-test
  ;; Test allocations-summary works with empty results
         (testing "allocations-summary with empty allocations"
                  (testing "handles nil allocations"
                           (let [summary (agent/allocations-summary [])]
                                (is (= {:num-allocated 0
                                        :num-freed 0
                                        :allocated-bytes 0
                                        :freed-bytes 0}
                                       summary)
                                    "Should return zero summary for empty input")))

                  (testing "works when agent unavailable"
                           (with-redefs [agent/attached? (constantly false)]
                                        (let [[allocs _result] (agent/with-allocation-tracing
                                                                (+ 1 2))
                                              summary (agent/allocations-summary (or allocs []))]
                                             (is (= {:num-allocated 0
                                                     :num-freed 0
                                                     :allocated-bytes 0
                                                     :freed-bytes 0}
                                                    summary)
                                                 "Should handle nil allocations gracefully"))))))
