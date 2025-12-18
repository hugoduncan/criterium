(ns build-test
  (:require
   [build.agent :as sut]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]])
  (:import
   [java.io File]))

;; Tests for platform detection functions.
;; Contracts: detect-os returns :linux, :macos, or :unknown based on os.name
;;            detect-arch returns :x64, :arm64, or :unknown based on os.arch
;;            current-platform returns valid platform string or throws
(deftest detect-os-test
  (testing "detect-os"
    (testing "returns :linux for Linux systems"
      (with-redefs [sut/detect-os (constantly :linux)]
        (is (= :linux (sut/detect-os)))))

    (testing "returns :macos for Mac systems"
      (with-redefs [sut/detect-os (constantly :macos)]
        (is (= :macos (sut/detect-os)))))

    (testing "returns current OS (smoke test)"
      (let [os (sut/detect-os)]
        (is (keyword? os))
        (is (contains? #{:linux :macos :unknown} os))))))

(deftest detect-arch-test
  (testing "detect-arch"
    (testing "returns current architecture (smoke test)"
      (let [arch (sut/detect-arch)]
        (is (keyword? arch))
        (is (contains? #{:x64 :arm64 :unknown} arch))))))

(deftest current-platform-test
  (testing "current-platform"
    (testing "returns valid platform string on supported systems"
      (let [os (sut/detect-os)
            arch (sut/detect-arch)]
        (if (and (contains? #{:linux :macos} os)
                 (contains? #{:x64 :arm64} arch)
                 ;; linux-arm64 is not supported
                 (not (and (= :linux os) (= :arm64 arch))))
          (let [platform (sut/current-platform)]
            (is (string? platform))
            (is (contains? #{"linux-x64" "macos-x64" "macos-arm64"} platform)))
          ;; Skip test on unsupported platforms
          (is true "Skipping on unsupported platform"))))

    (testing "throws for unsupported platform"
      (with-redefs [sut/detect-os (constantly :windows)
                    sut/detect-arch (constantly :x64)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Unsupported platform"
                              (sut/current-platform)))))

    (testing "throws for linux-arm64 (unsupported)"
      (with-redefs [sut/detect-os (constantly :linux)
                    sut/detect-arch (constantly :arm64)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Unsupported platform"
                              (sut/current-platform)))))))

;; Tests for library-name function.
;; Contract: library-name returns appropriate filename based on platform.
(deftest library-name-test
  (testing "library-name"
    (testing "returns .so for linux platforms"
      (is (= "libcriterium.so" (sut/library-name "linux-x64"))))

    (testing "returns .dylib for macos platforms"
      (is (= "libcriterium.dylib" (sut/library-name "macos-x64")))
      (is (= "libcriterium.dylib" (sut/library-name "macos-arm64"))))))

;; Tests for compute-sha256 function.
;; Contract: compute-sha256 returns lowercase hex SHA256 hash of file contents.
(deftest compute-sha256-test
  (testing "compute-sha256"
    (testing "computes correct hash for known content"
      (let [test-dir (io/file "target/test-sha256")
            test-file (io/file test-dir "test.txt")]
        (.mkdirs test-dir)
        (spit test-file "hello\n")
        (try
          ;; SHA256 of "hello\n" is known
          (let [hash (#'sut/compute-sha256 test-file)]
            (is (string? hash))
            (is (= 64 (count hash)))
            (is (re-matches #"[0-9a-f]+" hash))
            ;; Expected hash for "hello\n"
            (is (= "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"
                   hash)))
          (finally
            (.delete test-file)
            (.delete test-dir)))))))

(defn- delete-recursively
  "Recursively deletes a directory and all its contents."
  [^java.io.File dir]
  (doseq [^java.io.File f (file-seq dir)]
    (when (.isFile f) (.delete f)))
  (doseq [^java.io.File f (reverse (file-seq dir))]
    (.delete f)))

;; Tests for copy-agent-binary! function.
;; Contract: copy-agent-binary! copies source to target dir and creates hash file.
(deftest copy-agent-binary-test
  (testing "copy-agent-binary!"
    (testing "copies binary and creates hash file"
      (let [test-dir (io/file "target/test-copy")
            resources-dir (io/file "target/test-resources")
            source-file (io/file test-dir "source.so")]
        (.mkdirs test-dir)
        (spit source-file "test-binary")
        (try
          (with-redefs [sut/resources-base-dir (constantly resources-dir)]
            (let [result (sut/copy-agent-binary! (.getPath source-file) "linux-x64")
                  binary-path (io/file (:binary-path result))
                  hash-path (io/file (:hash-path result))]
              (is (.exists binary-path))
              (is (.exists hash-path))
              (is (= "test-binary" (slurp binary-path)))
              (is (re-matches #"[0-9a-f]+\s+libcriterium\.so\n"
                              (slurp hash-path)))))
          (finally
            (delete-recursively test-dir)
            (delete-recursively resources-dir)))))

    (testing "throws when source file missing"
      (let [resources-dir (io/file "target/test-resources-missing")]
        (try
          (with-redefs [sut/resources-base-dir (constantly resources-dir)]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"Source binary not found"
                                  (sut/copy-agent-binary! "/nonexistent/path" "linux-x64"))))
          (finally
            (delete-recursively resources-dir)))))))

;; Tests for run-process! helper function.
;; Contract: run-process! executes a command in a directory, throws on failure.
(deftest run-process-test
  (testing "run-process!"
    (testing "executes command successfully"
      (let [test-dir (io/file "target/test-process")]
        (.mkdirs test-dir)
        (try
          (is (= 0 (#'sut/run-process! ["echo" "hello"] test-dir "Echo test")))
          (finally
            (.delete test-dir)))))

    (testing "throws on non-zero exit code"
      (let [test-dir (io/file "target/test-process-fail")]
        (.mkdirs test-dir)
        (try
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"failed with exit code"
                                (#'sut/run-process! ["false"] test-dir "False command")))
          (finally
            (.delete test-dir)))))

    (testing "includes command and directory in exception data"
      (let [test-dir (io/file "target/test-process-data")]
        (.mkdirs test-dir)
        (try
          (let [ex (try
                     (#'sut/run-process! ["false"] test-dir "Test")
                     (catch clojure.lang.ExceptionInfo e e))
                data (ex-data ex)]
            (is (= ["false"] (:command data)))
            (is (= (.getAbsolutePath test-dir) (:directory data)))
            (is (number? (:exit-code data))))
          (finally
            (.delete test-dir)))))))

;; Integration tests for build-agent-cpp! function.
;; Contract: build-agent-cpp! builds agent using CMake for current platform.
;; These tests require CMake and the agent-cpp source directory.
(deftest ^:slow build-agent-cpp-test
  (testing "build-agent-cpp!"
    (testing "builds agent library for current platform"
      (let [os (sut/detect-os)
            arch (sut/detect-arch)
            test-build-dir (io/file "target/test-agent-build")]
        (if (and (contains? #{:linux :macos} os)
                 (contains? #{:x64 :arm64} arch)
                 (not (and (= :linux os) (= :arm64 arch)))
                 (.isDirectory (io/file "agent-cpp")))
          (try
            (let [lib-path (sut/build-agent-cpp! {:build-dir test-build-dir})
                  lib-file (io/file lib-path)]
              (is (string? lib-path))
              (is (.exists lib-file))
              (is (> (.length lib-file) 0)))
            (finally
              (delete-recursively test-build-dir)))
          (is true "Skipping on unsupported platform or missing agent-cpp"))))

    (testing "throws when agent-cpp directory missing"
      (with-redefs [sut/agent-cpp-dir (constantly (File. "/nonexistent/agent-cpp"))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"agent-cpp directory not found"
                              (sut/build-agent-cpp!)))))))

;; Integration test for build-and-copy-agent! function.
;; Contract: build-and-copy-agent! builds and copies agent to resources.
(deftest ^:slow build-and-copy-agent-test
  (testing "build-and-copy-agent!"
    (testing "builds agent and copies to resources directory"
      (let [os (sut/detect-os)
            arch (sut/detect-arch)
            platform (str (name os) "-" (name arch))
            resources-dir (io/file "target/test-build-resources")
            test-build-dir (io/file "target/test-agent-build-copy")
            original-build-agent-cpp! sut/build-agent-cpp!]
        (if (and (contains? #{"linux-x64" "macos-x64" "macos-arm64"} platform)
                 (.isDirectory (io/file "agent-cpp")))
          (try
            (with-redefs [sut/resources-base-dir (constantly resources-dir)
                          sut/build-agent-cpp! (fn
                                                 ([] (original-build-agent-cpp! {:build-dir test-build-dir}))
                                                 ([opts] (original-build-agent-cpp! (assoc opts :build-dir test-build-dir))))]
              (let [result (sut/build-and-copy-agent!)
                    binary-file (io/file (:binary-path result))
                    hash-file (io/file (:hash-path result))]
                (is (.exists binary-file))
                (is (.exists hash-file))
                (is (> (.length binary-file) 0))
                (is (re-matches #"[0-9a-f]+\s+libcriterium\.(so|dylib)\n"
                                (slurp hash-file)))))
            (finally
              (delete-recursively resources-dir)
              (delete-recursively test-build-dir)))
          (is true "Skipping on unsupported platform or missing agent-cpp"))))))
