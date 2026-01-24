(ns criterium.primitive-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.primitive-fn :as pf])
  (:import
   [clojure.lang IFn$DDD IFn$LLL]
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

;; Tests for primitive wrapper functions - verifies the wrapped functions
;; return correct values and support primitive invocation.

(deftest double-arithmetic-wrappers-test
  (testing "double arithmetic wrappers"
    (testing "dadd"
      (testing "returns the sum of two doubles"
        (is (= 5.0 (pf/dadd 2.0 3.0))))
      (testing "implements IFn$DDD for primitive invocation"
        (is (instance? IFn$DDD pf/dadd))
        (is (= 5.0 (.invokePrim ^IFn$DDD pf/dadd 2.0 3.0)))))
    (testing "dadd-unchecked"
      (testing "returns the sum of two doubles"
        (is (= 5.0 (pf/dadd-unchecked 2.0 3.0)))))
    (testing "dsubtract"
      (testing "returns the difference of two doubles"
        (is (= 2.0 (pf/dsubtract 5.0 3.0)))))
    (testing "dmultiply"
      (testing "returns the product of two doubles"
        (is (= 6.0 (pf/dmultiply 2.0 3.0)))))
    (testing "ddivide"
      (testing "returns the quotient of two doubles"
        (is (= 2.5 (pf/ddivide 5.0 2.0)))))
    (testing "dmin"
      (testing "returns the minimum of two doubles"
        (is (= 2.0 (pf/dmin 2.0 3.0)))
        (is (= 2.0 (pf/dmin 3.0 2.0)))))
    (testing "dmax"
      (testing "returns the maximum of two doubles"
        (is (= 3.0 (pf/dmax 2.0 3.0)))
        (is (= 3.0 (pf/dmax 3.0 2.0)))))))

(deftest long-arithmetic-wrappers-test
  (testing "long arithmetic wrappers"
    (testing "ladd"
      (testing "returns the sum of two longs"
        (is (= 5 (pf/ladd 2 3))))
      (testing "implements IFn$LLL for primitive invocation"
        (is (instance? IFn$LLL pf/ladd))
        (is (= 5 (.invokePrim ^IFn$LLL pf/ladd 2 3)))))
    (testing "lsubtract"
      (testing "returns the difference of two longs"
        (is (= 2 (pf/lsubtract 5 3)))))
    (testing "lmultiply"
      (testing "returns the product of two longs"
        (is (= 6 (pf/lmultiply 2 3)))))
    (testing "ldivide"
      (testing "returns the integer quotient of two longs"
        (is (= 2 (pf/ldivide 5 2)))
        (is (= 2 (pf/ldivide 4 2)))))
    (testing "lmin"
      (testing "returns the minimum of two longs"
        (is (= 2 (pf/lmin 2 3)))
        (is (= 2 (pf/lmin 3 2)))))
    (testing "lmax"
      (testing "returns the maximum of two longs"
        (is (= 3 (pf/lmax 2 3)))
        (is (= 3 (pf/lmax 3 2)))))))

(deftest double-predicate-wrappers-test
  (testing "double predicate wrappers"
    (testing "dpos?"
      (testing "returns true for positive doubles"
        (is (true? (pf/dpos? 1.0)))
        (is (true? (pf/dpos? 0.001))))
      (testing "returns false for non-positive doubles"
        (is (false? (pf/dpos? 0.0)))
        (is (false? (pf/dpos? -1.0))))
      (testing "implements DB for primitive invocation"
        (is (instance? DB pf/dpos?))
        (is (true? (.invokePrim ^DB pf/dpos? 1.0)))))
    (testing "dneg?"
      (testing "returns true for negative doubles"
        (is (true? (pf/dneg? -1.0)))
        (is (true? (pf/dneg? -0.001))))
      (testing "returns false for non-negative doubles"
        (is (false? (pf/dneg? 0.0)))
        (is (false? (pf/dneg? 1.0)))))
    (testing "dzero?"
      (testing "returns true for zero"
        (is (true? (pf/dzero? 0.0))))
      (testing "returns false for non-zero doubles"
        (is (false? (pf/dzero? 1.0)))
        (is (false? (pf/dzero? -1.0)))))))

(deftest long-predicate-wrappers-test
  (testing "long predicate wrappers"
    (testing "lpos?"
      (testing "returns true for positive longs"
        (is (true? (pf/lpos? 1)))
        (is (true? (pf/lpos? 100))))
      (testing "returns false for non-positive longs"
        (is (false? (pf/lpos? 0)))
        (is (false? (pf/lpos? -1))))
      (testing "implements LB for primitive invocation"
        (is (instance? LB pf/lpos?))
        (is (true? (.invokePrim ^LB pf/lpos? 1)))))
    (testing "lneg?"
      (testing "returns true for negative longs"
        (is (true? (pf/lneg? -1)))
        (is (true? (pf/lneg? -100))))
      (testing "returns false for non-negative longs"
        (is (false? (pf/lneg? 0)))
        (is (false? (pf/lneg? 1)))))
    (testing "lzero?"
      (testing "returns true for zero"
        (is (true? (pf/lzero? 0))))
      (testing "returns false for non-zero longs"
        (is (false? (pf/lzero? 1)))
        (is (false? (pf/lzero? -1)))))
    (testing "lodd?"
      (testing "returns true for odd longs"
        (is (true? (pf/lodd? 1)))
        (is (true? (pf/lodd? 3)))
        (is (true? (pf/lodd? -1))))
      (testing "returns false for even longs"
        (is (false? (pf/lodd? 0)))
        (is (false? (pf/lodd? 2)))
        (is (false? (pf/lodd? -2)))))
    (testing "leven?"
      (testing "returns true for even longs"
        (is (true? (pf/leven? 0)))
        (is (true? (pf/leven? 2)))
        (is (true? (pf/leven? -2))))
      (testing "returns false for odd longs"
        (is (false? (pf/leven? 1)))
        (is (false? (pf/leven? 3)))
        (is (false? (pf/leven? -1)))))))
