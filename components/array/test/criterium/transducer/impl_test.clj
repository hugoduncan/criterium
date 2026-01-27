(ns criterium.transducer.impl-test
  ;; Tests for transducer implementation types.
  ;; Contracts: deftype constructors and type predicates.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.transducer :as tr]
   [criterium.transducer.impl :as impl])
  (:import
   [criterium.transducer.impl
    ArraySetter
    DoubleRange
    LongRange]
   [criterium.transducer.interfaces
    IODLOReducible
    IOLDOReducible]))

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

;;; Object accumulator reduce tests

(definterface ISumAccumulator
  (^criterium.transducer.impl_test.ISumAccumulator addDouble [^double x])
  (^criterium.transducer.impl_test.ISumAccumulator addLong [^long x])
  (^double getSum []))

(deftype SumAccumulator [^:unsynchronized-mutable ^double sum]
  ISumAccumulator
  (addDouble [this x]
    (set! sum (+ sum x))
    this)
  (addLong [this x]
    (set! sum (+ sum (double x)))
    this)
  (getSum [_] sum))

(deftest object-accumulator-reduce-test
  ;; Tests tr/reduce with object accumulators over primitive arrays.
  ;; Contracts: IPrimOps.reduce with IODLOReducible and IOLDOReducible.
  (testing "tr/reduce with object accumulator"
    (testing "over double array"
      (testing "accumulates correctly"
        (let [data (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
              acc  (SumAccumulator. 0.0)
              rf   (fn [^ISumAccumulator acc ^double x] (.addDouble acc x))
              result (tr/reduce rf acc ^IODLOReducible data)]
          (is (identical? result acc) "returns the accumulator")
          (is (== 15.0 (.getSum ^ISumAccumulator result)) "sum is correct"))))
    (testing "over long array"
      (testing "accumulates correctly"
        (let [data (arr/->long-array (long-array [1 2 3 4 5]))
              acc  (SumAccumulator. 0.0)
              rf   (fn [^ISumAccumulator acc ^long x] (.addLong acc x))
              result (tr/reduce rf acc ^IOLDOReducible data)]
          (is (identical? result acc) "returns the accumulator")
          (is (== 15.0 (.getSum ^ISumAccumulator result)) "sum is correct"))))))
