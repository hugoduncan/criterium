(ns criterium.transducer.impl-test
  ;; Tests for transducer implementation types.
  ;; Contracts: deftype constructors and type predicates.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.transducer.impl :as impl])
  (:import
   [criterium.transducer.impl
    ArraySetter
    DoubleRange
    LongRange]))

(deftest array-setter-type-test
  (testing "ArraySetter"
    (testing "->ArraySetter constructor"
      (testing "creates an ArraySetter instance"
        (is (instance? ArraySetter (impl/->ArraySetter 0)))))
    (testing "prim-set-at"
      (testing "returns an ArraySetter instance"
        (is (instance? ArraySetter (impl/prim-set-at)))))))

(deftest long-range-type-test
  (testing "LongRange"
    (testing "->LongRange constructor"
      (testing "creates a LongRange instance"
        (is (instance? LongRange (impl/->LongRange 0 10)))))
    (testing "ops range method"
      (testing "returns a LongRange for long args"
        (is (instance? LongRange (.range impl/ops 0 10)))))))

(deftest double-range-type-test
  (testing "DoubleRange"
    (testing "->DoubleRange constructor"
      (testing "creates a DoubleRange instance"
        (is (instance? DoubleRange (impl/->DoubleRange 0.0 10.0 1.0)))))
    (testing "ops range method"
      (testing "returns a DoubleRange for double args"
        (is (instance? DoubleRange (.range impl/ops 0.0 10.0 1.0)))))))
