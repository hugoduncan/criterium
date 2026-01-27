(ns criterium.array.util-test
  ;; Tests the definterface+ macro which extends definterface to support
  ;; interface inheritance via gen-interface's :extends option.
  ;; Contracts: macro without extends behaves like definterface,
  ;; macro with single extend, macro with multiple extends,
  ;; type hints propagate correctly through interface hierarchies.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array.util :refer [definterface+]]))

;;; Test interface definitions

(definterface+ ITestBase
  (^long baseMethod [^double x]))

(definterface+ ITestExtendsSingle [ITestBase]
  (^double singleMethod [^long y]))

(definterface+ ITestAnotherBase
  (anotherBaseMethod []))

(definterface+ ITestExtendsMultiple [ITestBase ITestAnotherBase]
  (^String multiMethod [^long a ^double b]))

(definterface+ ITestNoMethods [ITestBase])

;;; Test implementations

(deftype TestBaseImpl []
  ITestBase
  (baseMethod [_ x] (long x)))

(deftype TestSingleExtendImpl []
  ITestExtendsSingle
  #_{:clj-kondo/ignore [:unresolved-protocol-method]}
  (baseMethod [_ x] (long x))
  (singleMethod [_ y] (double y)))

(deftype TestMultiExtendImpl []
  ITestExtendsMultiple
  #_{:clj-kondo/ignore [:unresolved-protocol-method]}
  (baseMethod [_ x] (long x))
  #_{:clj-kondo/ignore [:unresolved-protocol-method]}
  (anotherBaseMethod [_] :another)
  (multiMethod [_ a b] (str a "-" b)))

(deftype TestMarkerImpl []
  ITestNoMethods
  #_{:clj-kondo/ignore [:unresolved-protocol-method]}
  (baseMethod [_ x] (long x)))

;;; Tests

(deftest definterface+-without-extends-test
  (testing "definterface+ without extends"
    (testing "creates interface with method signatures"
      (is (.isInterface ITestBase)))
    (testing "implementation can be instantiated and method called"
      (let [impl (TestBaseImpl.)]
        (is (= 42 (.baseMethod impl 42.5)))))))

(deftest definterface+-with-single-extends-test
  (testing "definterface+ with single extends"
    (testing "creates interface that extends parent"
      (is (.isInterface ITestExtendsSingle))
      (is (isa? ITestExtendsSingle ITestBase)))
    (testing "implementation satisfies both interfaces"
      (let [impl (TestSingleExtendImpl.)]
        (is (instance? ITestBase impl))
        (is (instance? ITestExtendsSingle impl))))
    (testing "both parent and child methods are callable"
      (let [impl (TestSingleExtendImpl.)]
        (is (= 10 (.baseMethod impl 10.5)))
        (is (= 20.0 (.singleMethod impl 20)))))))

(deftest definterface+-with-multiple-extends-test
  (testing "definterface+ with multiple extends"
    (testing "creates interface that extends all parents"
      (is (.isInterface ITestExtendsMultiple))
      (is (isa? ITestExtendsMultiple ITestBase))
      (is (isa? ITestExtendsMultiple ITestAnotherBase)))
    (testing "implementation satisfies all interfaces"
      (let [impl (TestMultiExtendImpl.)]
        (is (instance? ITestBase impl))
        (is (instance? ITestAnotherBase impl))
        (is (instance? ITestExtendsMultiple impl))))
    (testing "all methods are callable"
      (let [impl (TestMultiExtendImpl.)]
        (is (= 5 (.baseMethod impl 5.0)))
        (is (= :another (.anotherBaseMethod impl)))
        (is (= "10-3.14" (.multiMethod impl 10 3.14)))))))

(deftest definterface+-marker-interface-test
  (testing "definterface+ with extends but no methods (marker interface)"
    (testing "creates marker interface extending parent"
      (is (.isInterface ITestNoMethods))
      (is (isa? ITestNoMethods ITestBase)))
    (testing "implementation satisfies both interfaces"
      (let [impl (TestMarkerImpl.)]
        (is (instance? ITestBase impl))
        (is (instance? ITestNoMethods impl))))
    (testing "parent method is callable"
      (let [impl (TestMarkerImpl.)]
        (is (= 99 (.baseMethod impl 99.0)))))))

(deftest definterface+-type-hints-test
  (testing "definterface+ type hints"
    (testing "primitive return types work correctly"
      (let [base-impl (TestBaseImpl.)
            single-impl (TestSingleExtendImpl.)]
        (is (= Long (type (.baseMethod base-impl 1.0))))
        (is (= Double (type (.singleMethod single-impl 1))))))
    (testing "object return types work correctly"
      (let [impl (TestMultiExtendImpl.)]
        (is (= String (type (.multiMethod impl 1 2.0))))))))
