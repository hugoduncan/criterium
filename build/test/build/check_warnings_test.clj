(ns build.check-warnings-test
  (:require
   [build.check-warnings :as sut]
   [clojure.test :refer [deftest is testing]]))

;; Tests for parse-warnings and check-namespace-warnings functionality.
;; Uses synthetic warning output and a test fixture namespace.

(deftest parse-warnings-test
  (testing "parse-warnings"
    (testing "extracts reflection warnings"
      (let [output "Reflection warning, test.clj:10:5 - call to method foo can't be resolved"
            result (#'sut/parse-warnings output)]
        (is (= 1 (count result)))
        (is (= :reflection (:type (first result))))
        (is (= output (:message (first result))))))

    (testing "extracts boxed math warnings"
      (let [output "Boxed math warning, test.clj:20:3 - call: public static java.lang.Number clojure.lang.Numbers.add(java.lang.Object, java.lang.Object)"
            result (#'sut/parse-warnings output)]
        (is (= 1 (count result)))
        (is (= :boxed-math (:type (first result))))
        (is (= output (:message (first result))))))

    (testing "extracts multiple warnings"
      (let [output "Reflection warning, test.clj:10:5 - call to method foo
Boxed math warning, test.clj:20:3 - call: add
Some other output that is not a warning
Reflection warning, test.clj:30:1 - reference to field bar"
            result (#'sut/parse-warnings output)]
        (is (= 3 (count result)))
        (is (= [:reflection :boxed-math :reflection] (mapv :type result)))))

    (testing "returns empty vector for no warnings"
      (let [output "Some normal output\nNo warnings here"]
        (is (= [] (#'sut/parse-warnings output)))))))

(deftest check-namespace-warnings-test
  ;; Tests check-namespace-warnings by checking known namespaces
  (testing "check-namespace-warnings"
    (testing "returns namespace in result"
      (let [result (sut/check-namespace-warnings 'clojure.string)]
        (is (= 'clojure.string (:namespace result)))))

    (testing "returns empty warnings for clean namespace"
      ;; clojure.string is a well-maintained namespace without warnings
      (let [result (sut/check-namespace-warnings 'clojure.string)]
        (is (nil? (:warnings result)))
        (is (nil? (:error result)))))

    (testing "handles non-existent namespace gracefully"
      (let [result (sut/check-namespace-warnings 'this.namespace.does.not.exist)]
        (is (= 'this.namespace.does.not.exist (:namespace result)))
        (is (some? (:error result)))))

    (testing "captures warnings from namespace with known issues"
      ;; Use a dedicated test fixture that won't be pre-loaded by kaocha hooks
      (let [result (sut/check-namespace-warnings 'build.fixtures.warning-examples)]
        (is (= 'build.fixtures.warning-examples (:namespace result)))
        (is (vector? (:warnings result)))
        (is (pos? (count (:warnings result)))
            "Expected warnings from build.fixtures.warning-examples")
        (is (some #(= :boxed-math (:type %)) (:warnings result))
            "Expected boxed-math warnings")
        (is (some #(= :reflection (:type %)) (:warnings result))
            "Expected reflection warnings")))))
