(ns criterium.array-allocation-test
  ;; Tests that primitive array operations avoid boxing during iteration.
  ;; Validates that typed arrays don't allocate per-element during iteration.
  ;;
  ;; These tests measure freed-bytes during execution. They should be ZERO
  ;; for properly implemented primitive operations.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.array :as arr]
   [criterium.primitive-fn :as pfn]))

;;; Test Data - created at namespace load time

(def ^:private array-size 10)

(def ^:private double-arr
  (arr/->double-array
   (double-array (range 1.0 (double (inc (long array-size)))))))

(def ^:private long-arr
  (arr/->long-array
   (long-array (range 1 (inc (long array-size))))))

(def ^:private object-arr
  (arr/->object-array
   (object-array (map keyword (map str (range array-size))))))

;; Expected values for array= tests - create once at load time
(def ^:private expected-doubles
  (vec (range 1.0 (double (inc (long array-size))))))
(def ^:private expected-longs
  (vec (range 1 (inc (long array-size)))))
(def ^:private expected-objects
  (vec (map keyword (map str (range array-size)))))

;;; Test Helpers

(defn- count-freed
  "Count freed objects from allocations list without creating intermediate collections.
  Uses reduce with a primitive long accumulator."
  ^long [allocs]
  (let [on-thread? (agent/allocation-on-thread?)]
    (reduce (fn ^long [^long acc alloc]
              (if (and (on-thread? alloc) (:freed alloc))
                (unchecked-inc acc)
                acc))
            0
            allocs)))

(defmacro assert-zero-freed
  "Verifies the body produces zero freed objects.
  Runs body once and checks freed count is zero.
  Holds reference to result to prevent it from being counted as freed."
  [desc & body]
  `(when (agent/attached?)
     (testing ~desc
       ;; allow jvm to alloc functinn stats
       (agent/with-allocation-tracing ~@body)
       ;; now verify
       (let [[allocs# result#] (agent/with-allocation-tracing ~@body)
             freed#            (count-freed allocs#)]
         ;; Reference result to keep it alive during count-freed
         (is (zero? freed#)
             (str "Expected zero freed objects, got " freed# " " allocs#))
         (identity result#)))))

;;; Primitive-returning fold operations

(deftest fold-double-no-boxing-test
  (testing "fold-double"
    (assert-zero-freed "produces no garbage"
                       (arr/fold-double double-arr pfn/dadd 0.0))))

(deftest fold-long-no-boxing-test
  (testing "fold-long"
    (assert-zero-freed "produces no garbage"
                       (arr/fold-long long-arr pfn/ladd 0))))

(deftest fold-double-skip-no-boxing-test
  (testing "fold-double-skip"
    (assert-zero-freed "produces no garbage"
                       (arr/fold-double-skip double-arr 5 pfn/dadd 0.0))))

(deftest indexed-fold-double-no-boxing-test
  (testing "indexed-fold-double"
    (let [f (fn ^double [^double acc ^long _i ^double v] (+ acc v))]
      (assert-zero-freed "produces no garbage"
                         (arr/indexed-fold-double
                          double-arr
                          f
                          0.0)))))

;;; Sum operations

(deftest sum-double-no-boxing-test
  (testing "sum-double"
    (assert-zero-freed "produces no garbage"
                       (arr/sum-double double-arr))))

(deftest sum-long-no-boxing-test
  (testing "sum-long"
    (assert-zero-freed "produces no garbage"
                       (arr/sum-long long-arr))))

;;; Accessor operations

(deftest get-double-no-boxing-test
  (testing "get-double"
    (assert-zero-freed "produces no garbage"
                       (arr/get-double double-arr 5))))

(deftest get-long-no-boxing-test
  (testing "get-long"
    (assert-zero-freed "produces no garbage"
                       (arr/get-long long-arr 5))))

(deftest first-double-no-boxing-test
  (testing "first-double"
    (assert-zero-freed "produces no garbage"
                       (arr/first-double double-arr))))

(deftest last-double-no-boxing-test
  (testing "last-double"
    (assert-zero-freed "produces no garbage"
                       (arr/last-double double-arr))))

(deftest first-long-no-boxing-test
  (testing "first-long"
    (assert-zero-freed "produces no garbage"
                       (arr/first-long long-arr))))

(deftest last-long-no-boxing-test
  (testing "last-long"
    (assert-zero-freed "produces no garbage"
                       (arr/last-long long-arr))))

;;; Polymorphic accessors

(deftest length-no-boxing-test
  (testing "length"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/length double-arr))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/length long-arr))
    (assert-zero-freed "ObjectArray produces no garbage"
                       (arr/length object-arr))))

(deftest elem-type-no-boxing-test
  (testing "elem-type"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/elem-type double-arr))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/elem-type long-arr))
    (assert-zero-freed "ObjectArray produces no garbage"
                       (arr/elem-type object-arr))))

(deftest first-element-no-boxing-test
  (testing "first-element"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/first-element double-arr))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/first-element long-arr))
    (assert-zero-freed "ObjectArray produces no garbage"
                       (arr/first-element object-arr))))

(deftest sum-no-boxing-test
  (testing "sum"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/sum double-arr))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/sum long-arr))))

(deftest get-at-no-boxing-test
  (testing "get-at"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/get-at double-arr 5))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/get-at long-arr 5))
    (assert-zero-freed "ObjectArray produces no garbage"
                       (arr/get-at object-arr 5))))

;;; Predicate operations

(deftest dany-no-boxing-test
  (testing "dany?"
    (assert-zero-freed "produces no garbage"
                       (arr/dany? double-arr pfn/dpos?))))

(deftest lany-no-boxing-test
  (testing "lany?"
    (assert-zero-freed "produces no garbage"
                       (arr/lany? long-arr pfn/lpos?))))

;;; Array-creating operations

(deftest dmap-no-boxing-test
  (let [f (fn ^double [^double x] (* x 2.0))]
    (testing "dmap"
      (assert-zero-freed "produces no garbage"
                         (arr/dmap double-arr f)))))

(deftest dmap-indexed-no-boxing-test
  (let [f (fn ^double [^long _i ^double x] (* x 2.0))]
    (testing "dmap-indexed"
      (assert-zero-freed "produces no garbage"
                         (arr/dmap-indexed double-arr f)))))

(deftest lmap-no-boxing-test
  (let [f (fn ^long [^long x] (* x 2))]
    (testing "lmap"
      (assert-zero-freed "produces no garbage"
                         (arr/lmap long-arr f)))))

(deftest lmap-indexed-no-boxing-test
  (let [f (fn ^long [^long _i ^long x] (* x 2))]
    (testing "lmap-indexed"
      (assert-zero-freed "produces no garbage"
                         (arr/lmap-indexed long-arr f)))))

(deftest filter-indices-no-boxing-test
  (testing "filter-indices"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/filter-indices double-arr #{1 2 3}))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/filter-indices long-arr #{1 2 3}))))

(deftest sorted-no-boxing-test
  (testing "sorted"
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/sorted double-arr))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/sorted long-arr))))

;;; Equality operations

(deftest array=-no-boxing-test
  (testing "array="
    (assert-zero-freed "DoubleArray produces no garbage"
                       (arr/array= double-arr expected-doubles))
    (assert-zero-freed "LongArray produces no garbage"
                       (arr/array= long-arr expected-longs))
    (assert-zero-freed "ObjectArray produces no garbage"
                       (arr/array= object-arr expected-objects))))
