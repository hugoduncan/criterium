(ns criterium.array-allocation-test
  ;; Tests that primitive array operations avoid boxing during iteration.
  ;;
  ;; Two categories of tests:
  ;; 1. Zero-allocation: Operations that should allocate nothing (folds, primitive accessors)
  ;; 2. Zero-garbage: Operations that allocate a result but no garbage (dmap, sorted)
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

(defn- count-allocations
  "Count allocations on current thread."
  ^long [allocs]
  (let [on-thread? (agent/allocation-on-thread?)]
    (reduce (fn ^long [^long acc alloc]
              (if (on-thread? alloc)
                (unchecked-inc acc)
                acc))
            0
            allocs)))

(defn- count-freed
  "Count freed objects on current thread."
  ^long [allocs]
  (let [on-thread? (agent/allocation-on-thread?)]
    (reduce (fn ^long [^long acc alloc]
              (if (and (on-thread? alloc) (:freed alloc))
                (unchecked-inc acc)
                acc))
            0
            allocs)))

(defmacro assert-zero-allocation
  "Verifies the body allocates nothing.
  For primitive operations that should never box."
  [& body]
  `(when (agent/attached?)
     ;; warmup to trigger JIT
     (agent/with-allocation-tracing ~@body)
     ;; verify zero allocations
     (let [[allocs# result#] (agent/with-allocation-tracing ~@body)
           allocated#        (count-allocations allocs#)]
       (is (zero? allocated#)
           (str "Expected zero allocations, got " allocated# " " allocs#))
       (identity result#))))

(defmacro assert-zero-garbage
  "Verifies the body produces zero garbage (freed objects).
  For operations that allocate a result but no temporary objects."
  [& body]
  `(when (agent/attached?)
     ;; warmup
     (agent/with-allocation-tracing ~@body)
     ;; verify zero freed
     (let [[allocs# result#] (agent/with-allocation-tracing ~@body)
           freed#            (count-freed allocs#)]
       (is (zero? freed#)
           (str "Expected zero garbage, got " freed# " freed objects"))
       (identity result#))))

;;; Primitive-returning fold operations (zero-allocation)

(deftest fold-double-zero-allocation-test
  (testing "fold-double"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/fold-double double-arr pfn/dadd 0.0)))))

(deftest fold-long-zero-allocation-test
  (testing "fold-long"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/fold-long long-arr pfn/ladd 0)))))

(deftest fold-double-skip-zero-allocation-test
  (testing "fold-double-skip"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/fold-double-skip double-arr 5 pfn/dadd 0.0)))))

(deftest indexed-fold-double-zero-allocation-test
  (testing "indexed-fold-double"
    (let [f (fn ^double [^double acc ^long _i ^double v] (+ acc v))]
      (testing "allocates nothing"
        (assert-zero-allocation
         (arr/indexed-fold-double double-arr f 0.0))))))

;;; Sum operations (zero-allocation)

(deftest sum-double-zero-allocation-test
  (testing "sum-double"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/sum-double double-arr)))))

(deftest sum-long-zero-allocation-test
  (testing "sum-long"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/sum-long long-arr)))))

;;; Accessor operations (zero-allocation)

(deftest get-double-zero-allocation-test
  (testing "get-double"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/get-double double-arr 5)))))

(deftest get-long-zero-allocation-test
  (testing "get-long"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/get-long long-arr 5)))))

(deftest first-double-zero-allocation-test
  (testing "first-double"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/first-double double-arr)))))

(deftest last-double-zero-allocation-test
  (testing "last-double"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/last-double double-arr)))))

(deftest first-long-zero-allocation-test
  (testing "first-long"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/first-long long-arr)))))

(deftest last-long-zero-allocation-test
  (testing "last-long"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/last-long long-arr)))))

;;; Polymorphic accessors (zero-allocation - returns primitive or cached keyword)

(deftest length-zero-allocation-test
  (testing "length"
    (testing "DoubleArray allocates nothing"
      (assert-zero-allocation (arr/length double-arr)))
    (testing "LongArray allocates nothing"
      (assert-zero-allocation (arr/length long-arr)))
    (testing "ObjectArray allocates nothing"
      (assert-zero-allocation (arr/length object-arr)))))

(deftest elem-type-zero-allocation-test
  (testing "elem-type"
    (testing "DoubleArray allocates nothing"
      (assert-zero-allocation (arr/elem-type double-arr)))
    (testing "LongArray allocates nothing"
      (assert-zero-allocation (arr/elem-type long-arr)))
    (testing "ObjectArray allocates nothing"
      (assert-zero-allocation (arr/elem-type object-arr)))))

;;; Polymorphic accessors (zero-garbage - boxes return value)

(deftest first-element-zero-garbage-test
  (testing "first-element"
    (testing "DoubleArray produces no garbage"
      (assert-zero-garbage (arr/first-element double-arr)))
    (testing "LongArray produces no garbage"
      (assert-zero-garbage (arr/first-element long-arr)))
    (testing "ObjectArray produces no garbage"
      (assert-zero-garbage (arr/first-element object-arr)))))

(deftest sum-zero-garbage-test
  (testing "sum"
    (testing "DoubleArray produces no garbage"
      (assert-zero-garbage (arr/sum double-arr)))
    (testing "LongArray produces no garbage"
      (assert-zero-garbage (arr/sum long-arr)))))

(deftest get-at-zero-garbage-test
  (testing "get-at"
    (testing "DoubleArray produces no garbage"
      (assert-zero-garbage (arr/get-at double-arr 5)))
    (testing "LongArray produces no garbage"
      (assert-zero-garbage (arr/get-at long-arr 5)))
    (testing "ObjectArray produces no garbage"
      (assert-zero-garbage (arr/get-at object-arr 5)))))

;;; Predicate operations (zero-allocation - returns boolean)

(deftest dany-zero-allocation-test
  (testing "dany?"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/dany? double-arr pfn/dpos?)))))

(deftest lany-zero-allocation-test
  (testing "lany?"
    (testing "allocates nothing"
      (assert-zero-allocation
       (arr/lany? long-arr pfn/lpos?)))))

;;; Array-creating operations (zero-garbage - allocates result array only)

(deftest dmap-zero-garbage-test
  (let [f (fn ^double [^double x] (* x 2.0))]
    (testing "dmap"
      (testing "produces no garbage"
        (assert-zero-garbage (arr/dmap double-arr f))))))

(deftest dmap-indexed-zero-garbage-test
  (let [f (fn ^double [^long _i ^double x] (* x 2.0))]
    (testing "dmap-indexed"
      (testing "produces no garbage"
        (assert-zero-garbage (arr/dmap-indexed double-arr f))))))

(deftest lmap-zero-garbage-test
  (let [f (fn ^long [^long x] (* x 2))]
    (testing "lmap"
      (testing "produces no garbage"
        (assert-zero-garbage (arr/lmap long-arr f))))))

(deftest lmap-indexed-zero-garbage-test
  (let [f (fn ^long [^long _i ^long x] (* x 2))]
    (testing "lmap-indexed"
      (testing "produces no garbage"
        (assert-zero-garbage (arr/lmap-indexed long-arr f))))))

(deftest filter-indices-zero-garbage-test
  (testing "filter-indices"
    (testing "DoubleArray produces no garbage"
      (assert-zero-garbage (arr/filter-indices double-arr #{1 2 3})))
    (testing "LongArray produces no garbage"
      (assert-zero-garbage (arr/filter-indices long-arr #{1 2 3})))))

(deftest sorted-zero-garbage-test
  (testing "sorted"
    (testing "DoubleArray produces no garbage"
      (assert-zero-garbage (arr/sorted double-arr)))
    (testing "LongArray produces no garbage"
      (assert-zero-garbage (arr/sorted long-arr)))))

;;; Equality operations (zero-allocation - returns boolean)

(deftest array=-zero-allocation-test
  (testing "array="
    (testing "DoubleArray allocates nothing"
      (assert-zero-allocation (arr/array= double-arr expected-doubles)))
    (testing "LongArray allocates nothing"
      (assert-zero-allocation (arr/array= long-arr expected-longs)))
    (testing "ObjectArray allocates nothing"
      (assert-zero-allocation (arr/array= object-arr expected-objects)))))
