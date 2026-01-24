(ns criterium.primitive-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.primitive-fn :as pf])
  (:import
   [criterium.primitive_fn.interface DB DDB LB LLB]))

;; Tests for bfn and defbfn macros - verifies they create functions
;; implementing both IFn and the appropriate primitive interface.

(pf/defbfn test-double-pred
  "Test double predicate."
  [^double x]
  (> x 0.0))

(pf/defbfn test-long-pred
  [^long n]
  (pos? n))

(pf/defbfn test-double-cmp
  "Test double comparison."
  [^double a ^double b]
  (< a b))

(pf/defbfn test-long-cmp
  [^long a ^long b]
  (< a b))

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
    (testing "with two ^double arguments"
      (testing "creates a function implementing DDB"
        (let [f (pf/bfn [^double a ^double b] (< a b))]
          (is (instance? DDB f))))
      (testing "returns correct boolean via invoke"
        (let [f (pf/bfn [^double a ^double b] (< a b))]
          (is (true? (f 1.0 2.0)))
          (is (false? (f 2.0 1.0)))))
      (testing "returns correct boolean via invokePrim"
        (let [f (pf/bfn [^double a ^double b] (< a b))]
          (is (true? (.invokePrim ^DDB f 1.0 2.0)))
          (is (false? (.invokePrim ^DDB f 2.0 1.0))))))
    (testing "with two ^long arguments"
      (testing "creates a function implementing LLB"
        (let [f (pf/bfn [^long a ^long b] (< a b))]
          (is (instance? LLB f))))
      (testing "returns correct boolean via invoke"
        (let [f (pf/bfn [^long a ^long b] (< a b))]
          (is (true? (f 1 2)))
          (is (false? (f 2 1)))))
      (testing "returns correct boolean via invokePrim"
        (let [f (pf/bfn [^long a ^long b] (< a b))]
          (is (true? (.invokePrim ^LLB f 1 2)))
          (is (false? (.invokePrim ^LLB f 2 1))))))
    (testing "with unhinted argument"
      (testing "throws an exception"
        (is (try
              (eval '(criterium.primitive-fn/bfn [x] (pos? x)))
              false
              (catch clojure.lang.Compiler$CompilerException e
                (let [cause (ex-cause e)]
                  (and (instance? clojure.lang.ExceptionInfo cause)
                       (re-find #"bfn requires \^double or \^long hint"
                                (ex-message cause)))))))))
    (testing "with mismatched type hints"
      (testing "throws an exception"
        (is (try
              (eval '(criterium.primitive-fn/bfn [^double a ^long b] (< a b)))
              false
              (catch clojure.lang.Compiler$CompilerException e
                (let [cause (ex-cause e)]
                  (and (instance? clojure.lang.ExceptionInfo cause)
                       (re-find #"matching type hints"
                                (ex-message cause)))))))))
    (testing "with wrong argument count"
      (testing "throws an exception"
        (is (try
              (eval '(criterium.primitive-fn/bfn [^double a ^double b ^double c]
                                                 (< a (+ b c))))
              false
              (catch clojure.lang.Compiler$CompilerException e
                (let [cause (ex-cause e)]
                  (and (instance? clojure.lang.ExceptionInfo cause)
                       (re-find #"1 or 2 arguments"
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
        (is (nil? (:doc (meta #'test-long-pred))))))
    (testing "with two ^double arguments"
      (testing "creates a function implementing DDB"
        (is (instance? DDB test-double-cmp)))
      (testing "returns correct boolean via invoke"
        (is (true? (test-double-cmp 1.0 2.0)))
        (is (false? (test-double-cmp 2.0 1.0))))
      (testing "returns correct boolean via invokePrim"
        (is (true? (.invokePrim ^DDB test-double-cmp 1.0 2.0)))
        (is (false? (.invokePrim ^DDB test-double-cmp 2.0 1.0))))
      (testing "attaches docstring metadata"
        (is (= "Test double comparison." (:doc (meta #'test-double-cmp))))))
    (testing "with two ^long arguments"
      (testing "creates a function implementing LLB"
        (is (instance? LLB test-long-cmp)))
      (testing "returns correct boolean via invoke"
        (is (true? (test-long-cmp 1 2)))
        (is (false? (test-long-cmp 2 1))))
      (testing "returns correct boolean via invokePrim"
        (is (true? (.invokePrim ^LLB test-long-cmp 1 2)))
        (is (false? (.invokePrim ^LLB test-long-cmp 2 1))))
      (testing "works without docstring"
        (is (nil? (:doc (meta #'test-long-cmp))))))))
