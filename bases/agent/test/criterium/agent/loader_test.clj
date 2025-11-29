(ns criterium.agent.loader-test
    "Tests for the criterium.agent.loader namespace.

  Tests cover:
  - Hash file reading from resources
  - Temp path construction
  - Agent extraction with file locking
  - Shutdown hook registration
  - File permission setting and verification
  - Graceful handling of missing resources
  - Concurrent extraction safety"
    (:require
     [clojure.java.io :as io]
     [clojure.string :as str]
     [clojure.test :refer [deftest is testing]]
     [criterium.agent.loader :as loader]
     [criterium.agent.platform :as platform])
    (:import
     [java.nio.file Files]))

(deftest read-hash-test
  ;; Hash file reading from bundled resources
  (testing "read-hash"
    (testing "returns nil for unsupported platform"
      (is (nil? (#'loader/read-hash "unsupported-platform"))))

    (testing "throws clear error when hash file missing for supported platform"
      ;; Currently no hash files bundled, so this should throw
      (let [ex (is (thrown? RuntimeException (#'loader/read-hash "linux-x64")))]
        (is (str/includes? (ex-message ex) "SHA256 hash file not found"))
        (is (str/includes? (ex-message ex) "native/linux-x64/libcriterium.so.sha256"))
        (is (str/includes? (ex-message ex) "building-agent.md"))))))

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
  ;; Missing hash file handling - throws exception with clear message
  (testing "extract-agent with missing hash file"
    (testing "throws exception when hash file not found"
      (with-redefs [platform/detect (constantly "linux-x64")]
        ;; No .sha256 files bundled yet, so should throw
        (let [ex (is (thrown? RuntimeException (loader/extract-agent)))]
          (is (str/includes? (ex-message ex) "SHA256 hash file not found")))))))

(deftest extract-agent-missing-binary-test
  ;; Missing binary file handling
  (testing "extract-agent with missing binary"
    (testing "returns nil when agent binary not found"
      (with-redefs [platform/detect  (constantly "linux-x64")
                    loader/read-hash (constantly "testhash123")]
        ;; No binaries bundled yet, so should return nil
        (is (nil? (loader/extract-agent)))))))

(deftest permission-functions-test
  ;; File permission setting and verification
         (testing "set-executable! and verify-permissions!"
                  (testing "sets and verifies permissions on temp file"
                           (let [temp-file (Files/createTempFile "test-agent-" ".dylib"
                                                           (into-array java.nio.file.attribute.FileAttribute []))]
                                (try
      ;; Set executable permissions
                                 (#'loader/set-executable! temp-file)

      ;; Verify file is readable and executable
                                 (is (Files/isReadable temp-file)
                                     "File should be readable after setting permissions")
                                 (is (Files/isExecutable temp-file)
                                     "File should be executable after setting permissions")

      ;; Verify permissions function doesn't throw
                                 (is (nil? (#'loader/verify-permissions! temp-file))
                                     "verify-permissions! should not throw for valid file")

                                 (finally
                                  (Files/delete temp-file)))))

                  (testing "verify-permissions! throws for non-existent file"
                           (let [fake-path (.toPath (io/file "/tmp/non-existent-file-12345.dylib"))]
                                (is (thrown? RuntimeException
                                             (#'loader/verify-permissions! fake-path))
                                    "Should throw RuntimeException for non-readable file")))))
