(ns criterium.util.invariant-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.util.invariant :refer [have have?]]))

(deftest have?-test
  (testing "have?"
    (testing "with a truthy predicate"
      (is (true? (have? :data {:data 1}))
          "returns true")
      (is (true? (have? {:data 1}))
          "returns true"))
    (testing "with an untruthy predicate"
      (is (thrown?
           AssertionError
           (have? :missing {:data 1}))
          "throws"))
    (testing "with no predicate and an untruthy value"
      (is (thrown?
           AssertionError
           (have? nil))
          "throws")
      (is (thrown?
           AssertionError
           (have? false))
          "throws"))))

(deftest have-test
  (testing "have"
    (testing "with a truthy predicate"
      (is (= {:data 1} (have :data {:data 1}))
          "returns its argument")
      (is (= {:data 1} (have {:data 1}))
          "returns its argument"))
    (testing "with an untruthy predicate"
      (is (thrown?
           AssertionError
           (have :missing {:data 1}))
          "throws"))
    (testing "with no predicate and an untruthy value"
      (is (thrown?
           AssertionError
           (have nil))
          "throws")
      (is (thrown?
           AssertionError
           (have false))
          "throws"))))
