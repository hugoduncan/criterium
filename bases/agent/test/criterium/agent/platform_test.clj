(ns criterium.agent.platform-test
  "Tests for the criterium.agent.platform namespace.

  Tests cover platform detection logic including:
  - OS name and architecture mapping
  - Platform identifier generation
  - File extension detection
  - Resource path construction
  - Graceful handling of unsupported platforms

  Test organization:
  - Unit tests with mocked system properties
  - Edge case tests for unsupported platforms
  - Integration tests for current platform"
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent.platform :as platform]))

(defn with-system-properties
  "Execute function with temporarily overridden system properties."
  [props f]
  (let [original-props (into {}
                             (map (fn [k] [k (System/getProperty k)])
                                  (keys props)))]
    (try
      (doseq [[k v] props]
        (if v
          (System/setProperty k v)
          (System/clearProperty k)))
      (f)
      (finally
        (doseq [[k v] original-props]
          (if v
            (System/setProperty k v)
            (System/clearProperty k)))))))

(defmacro with-props
  "Execute body with temporarily overridden system properties."
  [props & body]
  `(with-system-properties ~props (fn [] ~@body)))

(deftest detect-linux-x64
  (testing "Linux on x86_64"
    (with-props {"os.name" "Linux" "os.arch" "x86_64"}
      (is (= "linux-x64" (platform/detect)))))

  (testing "Linux on amd64"
    (with-props {"os.name" "Linux" "os.arch" "amd64"}
      (is (= "linux-x64" (platform/detect))))))

(deftest detect-macos-x64
  (testing "macOS on x86_64"
    (with-props {"os.name" "Mac OS X" "os.arch" "x86_64"}
      (is (= "macos-x64" (platform/detect)))))

  (testing "macOS on amd64"
    (with-props {"os.name" "Mac OS X" "os.arch" "amd64"}
      (is (= "macos-x64" (platform/detect))))))

(deftest detect-macos-arm64
         ;; Test macOS on Apple Silicon (ARM64) detection.
         ;; Verifies correct mapping for aarch64 architecture.
  (testing "detect"
    (testing "returns macos-arm64 for aarch64 architecture"
      (with-props {"os.name" "Mac OS X" "os.arch" "aarch64"}
        (is (= "macos-arm64" (platform/detect)))))))

(deftest detect-unsupported-platforms
  (testing "Windows"
    (with-props {"os.name" "Windows 10" "os.arch" "x86_64"}
      (is (nil? (platform/detect))
          "Windows should not be supported")))

  (testing "Unsupported architecture"
    (with-props {"os.name" "Linux" "os.arch" "sparc"}
      (is (nil? (platform/detect))
          "SPARC should not be supported")))

  (testing "Unsupported OS"
    (with-props {"os.name" "FreeBSD" "os.arch" "x86_64"}
      (is (nil? (platform/detect))
          "FreeBSD should not be supported")))

  (testing "Missing os.name property"
    (with-props {"os.name" nil "os.arch" "x86_64"}
      (is (nil? (platform/detect))
          "Should handle missing os.name gracefully")))

  (testing "Missing os.arch property"
    (with-props {"os.name" "Linux" "os.arch" nil}
      (is (nil? (platform/detect))
          "Should handle missing os.arch gracefully"))))

(deftest extension-test
  (testing "Linux extension"
    (is (= "so" (platform/extension "linux-x64"))))

  (testing "macOS extension"
    (is (= "dylib" (platform/extension "macos-x64"))))

  (testing "macOS ARM64 extension"
    (is (= "dylib" (platform/extension "macos-arm64"))))

  (testing "Nil platform"
    (is (nil? (platform/extension nil))
        "Should return nil for nil platform"))

  (testing "Invalid platform"
    (is (nil? (platform/extension "windows-x64"))
        "Should return nil for unsupported platform")))

(deftest resource-path-test
  (testing "Linux resource path"
    (is (= "criterium/agent/linux-x64/libcriterium.so"
           (platform/resource-path "linux-x64"))))

  (testing "macOS resource path"
    (is (= "criterium/agent/macos-x64/libcriterium.dylib"
           (platform/resource-path "macos-x64"))))

  (testing "macOS ARM64 resource path"
    (is (= "criterium/agent/macos-arm64/libcriterium.dylib"
           (platform/resource-path "macos-arm64"))))

  (testing "Nil platform"
    (is (nil? (platform/resource-path nil))
        "Should return nil for nil platform"))

  (testing "Unsupported platform"
    (is (nil? (platform/resource-path "windows-x64"))
        "Should return nil for unsupported platform")))

(deftest current-platform-test
  (testing "Current platform detection"
    (let [platform (platform/detect)]
      (is (or (nil? platform)
              (contains? #{"linux-x64" "macos-x64" "macos-arm64"} platform))
          "Current platform should be nil or a supported platform"))

    (testing "Current platform has valid extension"
      (when-let [platform (platform/detect)]
        (is (contains? #{"so" "dylib"} (platform/extension platform))
            "Current platform should have valid extension")))

    (testing "Current platform has valid resource path"
      (when-let [platform (platform/detect)]
        (let [path (platform/resource-path platform)]
          (is (string? path))
          (is (re-matches #"criterium/agent/[^/]+/libcriterium\.(so|dylib)" path)
              "Resource path should match expected pattern"))))))
