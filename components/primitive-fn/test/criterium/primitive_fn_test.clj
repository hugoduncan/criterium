(ns criterium.primitive-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.primitive-fn :as pf])
  (:import
   [criterium.primitive_fn.interface DB LB]))

;; Tests for bfn and defbfn macros - verifies they create functions
;; implementing both IFn and the appropriate primitive interface (DB/LB).

(pf/defbfn test-double-pred
  "Test double predicate."
  [^double x]
  (> x 0.0))

(pf/defbfn test-long-pred
  [^long n]
  (pos? n))

(deftest bfn-test
  (testing "bfn"
    (testing "with ^double argument"
      (testing "creates a function implementing DB"
        (let [f (pf/bfn [^double x] (> x 0.0))]
          (is (instance? DB f))))
      (testing "returns correct boolean via invoke"
        (let [f (pf/bfn [^double x] (> x 0.0))]
          (is (true? (f 1.0)))
          (is (false? (f -1.0)))))
      (testing "returns correct boolean via invokePrim"
        (let [f (pf/bfn [^double x] (> x 0.0))]
          (is (true? (.invokePrim ^DB f 1.0)))
          (is (false? (.invokePrim ^DB f -1.0))))))
    (testing "with ^long argument"
      (testing "creates a function implementing LB"
        (let [f (pf/bfn [^long n] (pos? n))]
          (is (instance? LB f))))
      (testing "returns correct boolean via invoke"
        (let [f (pf/bfn [^long n] (pos? n))]
          (is (true? (f 1)))
          (is (false? (f -1)))))
      (testing "returns correct boolean via invokePrim"
        (let [f (pf/bfn [^long n] (pos? n))]
          (is (true? (.invokePrim ^LB f 1)))
          (is (false? (.invokePrim ^LB f -1))))))
    (testing "with unhinted argument"
      (testing "throws an exception"
        (is (try
              (eval '(criterium.primitive-fn/bfn [x] (pos? x)))
              false
              (catch clojure.lang.Compiler$CompilerException e
                (let [cause (ex-cause e)]
                  (and (instance? clojure.lang.ExceptionInfo cause)
                       (re-find #"bfn requires \^double or \^long hint"
                                (ex-message cause)))))))))))

(deftest defbfn-test
  (testing "defbfn"
    (testing "with ^double argument"
      (testing "creates a function implementing DB"
        (is (instance? DB test-double-pred)))
      (testing "returns correct boolean via invoke"
        (is (true? (test-double-pred 1.0)))
        (is (false? (test-double-pred -1.0))))
      (testing "returns correct boolean via invokePrim"
        (is (true? (.invokePrim ^DB test-double-pred 1.0)))
        (is (false? (.invokePrim ^DB test-double-pred -1.0))))
      (testing "attaches docstring metadata"
        (is (= "Test double predicate." (:doc (meta #'test-double-pred))))))
    (testing "with ^long argument"
      (testing "creates a function implementing LB"
        (is (instance? LB test-long-pred)))
      (testing "returns correct boolean via invoke"
        (is (true? (test-long-pred 1)))
        (is (false? (test-long-pred -1))))
      (testing "returns correct boolean via invokePrim"
        (is (true? (.invokePrim ^LB test-long-pred 1)))
        (is (false? (.invokePrim ^LB test-long-pred -1))))
      (testing "works without docstring"
        (is (nil? (:doc (meta #'test-long-pred))))))))
