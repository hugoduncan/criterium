(ns build.version-test
  (:require
   [build.version :as version]
   [clojure.test :refer [deftest is testing]]))

(deftest version->version-map-test
  ;; Test version->version-map parsing and validation
  (testing "version->version-map"
    (testing "parses valid version strings correctly"
      (is
       (=
        {:major 1
         :minor 2
         :incremental 3
         :qualifier 'ALPHA}
        (version/version->version-map "1.2.3-ALPHA")))
      (is
       (=
        {:major 0
         :minor 5
         :incremental 246
         :qualifier 'ALPHA}
        (version/version->version-map "0.5.246-ALPHA")))
      (is
       (= {:major 1 :minor 0 :incremental 0 :qualifier nil}
          (version/version->version-map "1.0.0"))))

    (testing "rejects invalid version formats"
      (is (thrown? AssertionError
                   (version/version->version-map "1.2")))
      (is (thrown? AssertionError
                   (version/version->version-map
                    "1.2.3.4")))
      (is (thrown? AssertionError
                   (version/version->version-map "a.b.c")))
      (is (thrown? AssertionError
                   (version/version->version-map
                    "1.2.3-")))
      (is (thrown? AssertionError
                   (version/version->version-map
                    "1.2.three")))
      (is (thrown? AssertionError
                   (version/version->version-map "")))
      (is (thrown? AssertionError
                   (version/version->version-map
                    "not-a-version"))))))

(deftest version-map->version-test
  ;; Test version map to string conversion
  (testing "version-map->version"
    (testing "converts version maps to strings correctly"
      (is (= "1.2.3-ALPHA"
             (version/version-map->version
              {:major 1
               :minor 2
               :incremental 3
               :qualifier 'ALPHA})))
      (is (= "1.0.0"
             (version/version-map->version
              {:major 1
               :minor 0
               :incremental 0
               :qualifier nil})))
      (is (= "1.0.0"
             (version/version-map->version
              {:major 1 :minor 0 :incremental 0}))))))

(deftest interpolate-version-test
  ;; Test version template interpolation
  (testing "interpolate-version"
    (testing "interpolates git-rev-count placeholder"
      (let [result (version/interpolate-version
                    "0.5.{{git-rev-count}}-ALPHA")]
        (is (string? result))
        (is (re-matches #"0\.5\.\d+-ALPHA" result))))))

(deftest expand-version-test
  ;; Test full version expansion from coordinates
  (testing "expand-version"
    (testing "expands version templates from project coordinates"
      (let [result (version/expand-version
                    {:version "0.5.{{git-rev-count}}-ALPHA"})]
        (is (map? result))
        (is (re-matches #"0\.5\.\d+-ALPHA" (:version result)))))

    (testing "returns static versions unchanged in result map"
      (is (= {:version "1.2.3-ALPHA"}
             (version/expand-version {:version "1.2.3-ALPHA"}))))))
