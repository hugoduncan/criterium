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
        (is
         (str/includes?
          (ex-message ex)
          "criterium/agent/linux-x64/libcriterium.so.sha256"))
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
  ;; Missing binary file handling - now a structured failure, not a silent nil
  (testing "extract-agent with missing binary"
    (testing "throws structured :resolve-binary error when agent binary not found"
      (with-redefs [platform/detect (constantly "linux-x64")
                    loader/read-hash (constantly "testhash123")]
        ;; No binaries bundled in dev, so this must surface a failure
        (let [ex (is (thrown? clojure.lang.ExceptionInfo (loader/extract-agent)))]
          (is (= :resolve-binary (:stage (ex-data ex))))
          (is (true? (:criterium.agent/extraction-failure (ex-data ex))))
          (is (str/includes? (ex-message ex) "agent binary not found")))))))

(deftest verify-hash-test
  ;; SHA-256 integrity verification of extracted bytes
  (testing "verify-hash!"
    (let [tmp (Files/createTempFile
               "vh-" ".bin"
               (into-array java.nio.file.attribute.FileAttribute []))]
      (try
        (spit (.toFile tmp) "hello criterium")
        (let [good (#'loader/compute-sha256 tmp)]
          (testing "passes for matching hash"
            (is (nil? (#'loader/verify-hash! tmp good {}))))
          (testing "is a no-op when no expected hash provided"
            (is (nil? (#'loader/verify-hash! tmp nil {})))
            (is (nil? (#'loader/verify-hash! tmp "" {}))))
          (testing "throws :verify-hash on mismatch"
            (let [ex (is (thrown? clojure.lang.ExceptionInfo
                                  (#'loader/verify-hash! tmp "deadbeef" {})))]
              (is (= :verify-hash (:stage (ex-data ex))))
              (is (str/includes? (ex-message ex) "SHA-256 mismatch")))))
        (finally
          (Files/delete tmp))))))

(deftest extract-with-lock-success-test
  ;; End-to-end extraction of a real classpath resource (no hash check).
  ;; Exercises lock-dir-create, open-lock, acquire-lock, create-temp, copy,
  ;; atomic-move, set-executable and verify-permissions stages on a real FS.
  (testing "extract-with-lock"
    (let [resource "criterium/agent/platform.clj" ; always on the classpath
          target (str (System/getProperty "java.io.tmpdir")
                      "/criterium-extract-ok-" (System/nanoTime) ".so")]
      (is (some? (io/resource resource)) "test resource must be on classpath")
      (try
        (testing "copies resource and returns true"
          (is (true? (#'loader/extract-with-lock resource target "linux-x64" nil)))
          (is (.exists (io/file target)))
          (is (Files/isExecutable (.toPath (io/file target)))))
        (finally
          (.delete (io/file target))
          (.delete (io/file (str target ".lock"))))))))

(deftest extract-with-lock-hash-mismatch-test
  ;; A wrong expected hash fails with :verify-hash and leaves no target behind.
  (testing "extract-with-lock with bad hash"
    (let [resource "criterium/agent/platform.clj"
          target (str (System/getProperty "java.io.tmpdir")
                      "/criterium-extract-bad-" (System/nanoTime) ".so")]
      (try
        (let [ex (is (thrown? clojure.lang.ExceptionInfo
                              (#'loader/extract-with-lock
                               resource target "linux-x64" "deadbeef")))]
          (is (= :verify-hash (:stage (ex-data ex))))
          (is (not (.exists (io/file target)))
              "no partially-published target should be left behind"))
        (finally
          (.delete (io/file target))
          (.delete (io/file (str target ".lock"))))))))

(deftest extract-with-lock-copy-failure-test
  ;; A missing resource surfaces as a :copy stage failure (not a silent false).
  (testing "extract-with-lock with missing resource"
    (let [target (str (System/getProperty "java.io.tmpdir")
                      "/criterium-extract-copyfail-" (System/nanoTime) ".so")]
      (try
        (let [ex (is (thrown? clojure.lang.ExceptionInfo
                              (#'loader/extract-with-lock
                               "no/such/resource.bin" target "linux-x64" nil)))]
          (is (= :copy (:stage (ex-data ex)))))
        (finally
          (.delete (io/file target))
          (.delete (io/file (str target ".lock"))))))))

(deftest extract-agent-persistence-test
  ;; Part A: extraction is persistent by default; cleanup is opt-in.
  ;; Exercises the reuse branch (target already present) so no real bundled
  ;; binary is required.
  (testing "extract-agent cleanup registration"
    (with-redefs [platform/detect (constantly "macos-x64")
                  loader/read-hash (constantly "persisttest")]
      (let [target-path (#'loader/temp-path "macos-x64" "persisttest")
            target-file (io/file target-path)
            agents-atom (deref #'loader/extracted-agents)]
        (try
          (spit target-file "stub-agent-bytes")
          (testing "default extraction does NOT register file for deletion"
            (reset! agents-atom #{})
            (is (= target-path (loader/extract-agent)))
            (is (not (contains? @agents-atom target-path))
                "default extraction must be persistent (no cleanup registration)"))
          (testing "cleanup-on-exit? true registers file for deletion"
            (reset! agents-atom #{})
            (is (= target-path (loader/extract-agent {:cleanup-on-exit? true})))
            (is (contains? @agents-atom target-path)
                "cleanup-on-exit? true must register the file for shutdown deletion"))
          (finally
            (reset! agents-atom #{})
            (.delete target-file)))))))

(deftest permission-functions-test
  ;; File permission setting and verification
  (testing "set-executable! and verify-permissions!"
    (testing "sets and verifies permissions on temp file"
      (let [temp-file (Files/createTempFile "test-agent-" ".dylib"
                                            (into-array
                                             java.nio.file.attribute.FileAttribute
                                             []))]
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
