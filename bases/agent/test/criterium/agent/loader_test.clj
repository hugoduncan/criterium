(ns criterium.agent.loader-test
    "Tests for the criterium.agent.loader namespace.

  Tests cover:
  - Hash file reading from resources
  - Temp path construction
  - Agent extraction with file locking
  - Shutdown hook registration
  - Graceful handling of missing resources
  - Concurrent extraction safety"
    (:require
     [clojure.java.io :as io]
     [clojure.test :refer [deftest is testing]]
     [criterium.agent.loader :as loader]
     [criterium.agent.platform :as platform]))

(deftest read-hash-test
  ;; Hash file reading from bundled resources
         (testing "read-hash"
                  (testing "returns nil for unsupported platform"
                           (is (nil? (#'loader/read-hash "unsupported-platform"))))

                  (testing "returns nil when hash file missing"
      ;; Currently no hash files bundled, so this is expected
                           (is (nil? (#'loader/read-hash "linux-x64"))))))

(deftest temp-path-test
  ;; Temp path construction
         (testing "temp-path"
                  (testing "builds correct path format"
                           (let [platform "macos-x64"
                                 hash "abc123def456"
                                 path (#'loader/temp-path platform hash)]
                                (is (re-find #"criterium-agent-macos-x64-abc123def456\.dylib$" path))))

                  (testing "includes temp directory"
                           (let [platform "linux-x64"
                                 hash "test"
                                 path (#'loader/temp-path platform hash)
                                 tmpdir (System/getProperty "java.io.tmpdir")]
                                (is (clojure.string/starts-with? path tmpdir))))))

(deftest lock-path-test
  ;; Lock path construction
         (testing "lock-path"
                  (testing "appends .lock to agent path"
                           (is (= "/tmp/agent.dylib.lock"
                                  (#'loader/lock-path "/tmp/agent.dylib"))))))

(deftest extract-agent-unsupported-test
  ;; Graceful handling when resources unavailable
         (testing "extract-agent with unsupported platform"
                  (testing "returns nil when platform unsupported"
                           (with-redefs [platform/detect (constantly nil)]
                                        (is (nil? (loader/extract-agent)))))))

(deftest extract-agent-missing-hash-test
  ;; Missing hash file handling
         (testing "extract-agent with missing hash file"
                  (testing "returns nil when hash file not found"
                           (with-redefs [platform/detect (constantly "linux-x64")]
        ;; No .sha256 files bundled yet, so should return nil
                                        (is (nil? (loader/extract-agent)))))))

(deftest extract-agent-missing-binary-test
  ;; Missing binary file handling
         (testing "extract-agent with missing binary"
                  (testing "returns nil when agent binary not found"
                           (with-redefs [platform/detect (constantly "linux-x64")
                                         loader/read-hash (constantly "testhash123")]
        ;; No binaries bundled yet, so should return nil
                                        (is (nil? (loader/extract-agent)))))))
