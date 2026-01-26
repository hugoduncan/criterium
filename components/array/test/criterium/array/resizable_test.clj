(ns criterium.array.resizable-test
  ;; Tests the resizable array types that provide mutable-size primitive
  ;; array storage for algorithms where size isn't known upfront.
  ;; Contracts: type construction with capacity/size, resize operations,
  ;; interface method implementations respecting current size,
  ;; conversion to fixed arrays.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr])
  (:import
   [criterium.array.interfaces IIndexed IIndexedSet]
   [criterium.array.resizable ResizableDoubleArray ResizableLongArray ResizableObjectArray]))

;;; ResizableDoubleArray tests

(deftest resizable-double-array-construction-test
  ;; Tests construction of ResizableDoubleArray with capacity and initial size.
  ;; Contracts: capacity-only creates array with size=capacity,
  ;; capacity+size creates with specified size, validates bounds.
  (testing "resizable-double-array"
    (testing "with capacity only"
      (testing "creates array with size equal to capacity"
        (let [^ResizableDoubleArray arr (arr/resizable-double-array 5)]
          (is (= 5 (arr/length arr)))
          (is (= 5 (arr/capacity arr)))))
      (testing "creates array with zero capacity"
        (let [^ResizableDoubleArray arr (arr/resizable-double-array 0)]
          (is (= 0 (arr/length arr)))
          (is (= 0 (arr/capacity arr))))))
    (testing "with capacity and initial-size"
      (testing "creates array with specified size less than capacity"
        (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
          (is (= 3 (arr/length arr)))
          (is (= 10 (arr/capacity arr)))))
      (testing "creates array with size equal to capacity"
        (let [^ResizableDoubleArray arr (arr/resizable-double-array 5 5)]
          (is (= 5 (arr/length arr)))
          (is (= 5 (arr/capacity arr)))))
      (testing "creates array with zero initial size"
        (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 0)]
          (is (= 0 (arr/length arr)))
          (is (= 10 (arr/capacity arr)))))
      (testing "throws on negative initial-size"
        (is (thrown-with-msg? IllegalArgumentException
                              #"initial-size must be between 0 and capacity"
                              (arr/resizable-double-array 5 -1))))
      (testing "throws on initial-size exceeding capacity"
        (is (thrown-with-msg? IllegalArgumentException
                              #"initial-size must be between 0 and capacity"
                              (arr/resizable-double-array 5 6)))))))

(deftest resizable-double-array-predicate-test
  ;; Tests the resizable-double-array? type predicate.
  ;; Contracts: returns true only for ResizableDoubleArray instances.
  (testing "resizable-double-array?"
    (testing "returns true for ResizableDoubleArray"
      (is (arr/resizable-double-array? (arr/resizable-double-array 5))))
    (testing "returns false for DoubleArray"
      (is (not (arr/resizable-double-array?
                (arr/->double-array (double-array [1.0]))))))
    (testing "returns false for other types"
      (is (not (arr/resizable-double-array? [1.0 2.0])))
      (is (not (arr/resizable-double-array? nil))))))

(deftest resizable-double-array-resize-test
  ;; Tests the resize! operation on ResizableDoubleArray.
  ;; Contracts: shrinking works, growing beyond capacity throws,
  ;; resize to same size works, resize to zero works.
  (testing "resize!"
    (testing "shrinks the array"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 10)]
        (arr/resize! arr 5)
        (is (= 5 (arr/length arr)))
        (is (= 10 (arr/capacity arr)))))
    (testing "resizes to zero"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5)]
        (arr/resize! arr 0)
        (is (= 0 (arr/length arr)))))
    (testing "resizes to same size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5 3)]
        (arr/resize! arr 3)
        (is (= 3 (arr/length arr)))))
    (testing "can grow back up to capacity after shrinking"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 10)]
        (arr/resize! arr 3)
        (is (= 3 (arr/length arr)))
        (arr/resize! arr 8)
        (is (= 8 (arr/length arr)))))
    (testing "throws on negative size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5)]
        (is (thrown-with-msg? IllegalArgumentException
                              #"new-size must be between 0 and capacity"
                              (arr/resize! arr -1)))))
    (testing "throws on size exceeding capacity"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5)]
        (is (thrown-with-msg? IllegalArgumentException
                              #"new-size must be between 0 and capacity"
                              (arr/resize! arr 6)))))))

(deftest resizable-double-array-elem-type-test
  ;; Tests that ResizableDoubleArray reports correct element type.
  (testing "elem-type"
    (testing "returns :double"
      (is (= :double (arr/elem-type (arr/resizable-double-array 5)))))))

(deftest resizable-double-array-fold-test
  ;; Tests fold operations on ResizableDoubleArray respect current size.
  ;; Contracts: fold only iterates over active elements, not capacity.
  (testing "fold operations"
    (testing "fold respects current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        ;; Set values within size
        (dotimes [i 5]
          (.setDouble arr i (double i)))
        ;; Set values beyond size via underlying array (to verify fold ignores them)
        (dotimes [i 5]
          (aset ^doubles (.array arr) (+ i 5) (double (+ i 100))))
        ;; Fold should only see indices 0-4
        (is (= [0.0 1.0 2.0 3.0 4.0]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "fold-double respects current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (dotimes [i 5]
          (.setDouble arr i (double i)))
        ;; Set values beyond size via underlying array
        (dotimes [i 5]
          (aset ^doubles (.array arr) (+ i 5) (double (+ i 100))))
        ;; Sum of 0+1+2+3+4 = 10
        (is (= 10.0 (arr/fold-double arr
                                     (fn ^double [^double a ^double b]
                                       (+ a b))
                                     0.0)))))
    (testing "dfold respects current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        (dotimes [i 3]
          (.setDouble arr i (double (* i 10))))
        ;; Set values beyond size via underlying array
        (dotimes [i 7]
          (aset ^doubles (.array arr) (+ i 3) 999.0))
        (is (= [0.0 10.0 20.0]
               (arr/dfold arr (fn [acc ^double v] (conj acc v)) [])))))
    (testing "fold on resized array uses new size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 10)]
        (dotimes [i 10]
          (.setDouble arr i (double i)))
        (arr/resize! arr 3)
        (is (= [0.0 1.0 2.0]
               (arr/fold arr #(conj %1 %2) [])))))))

(deftest resizable-double-array-map-test
  ;; Tests map operations on ResizableDoubleArray return fixed arrays.
  ;; Contracts: dmap/dmapIndexed return DoubleArray sized to current size.
  (testing "dmap"
    (testing "returns DoubleArray with size elements"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)
            _                         (dotimes [i 5]
                                        (.setDouble arr i (double (inc i))))
            result                    (arr/dmap arr (fn ^double [^double x] (* x 2.0)))]
        (is (arr/double-array? result))
        (is (= 5 (arr/length result)))
        (is (= [2.0 4.0 6.0 8.0 10.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))
  (testing "dmapIndexed"
    (testing "returns DoubleArray with size elements"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)
            _                         (dotimes [i 3]
                                        (.setDouble arr i 1.0))
            ;; Map each element to its index
            result                    (arr/dmap-indexed arr
                                                        (fn ^double [^long i ^double _x]
                                                          (double i)))]
        (is (arr/double-array? result))
        (is (= 3 (arr/length result)))
        (is (= [0.0 1.0 2.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))))

(deftest resizable-double-array-fill!-test
  ;; Tests fill! operation on ResizableDoubleArray.
  ;; Contracts: fill! sets elements 0 to size-1 to the specified value,
  ;; returns array for chaining, handles empty arrays and zero values.
  (testing "fill!"
    (testing "fills all elements within current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (dotimes [i 5]
          (.setDouble arr i (double i)))
        (arr/fill! arr 42.0)
        (is (= [42.0 42.0 42.0 42.0 42.0]
               (arr/dfold arr (fn [acc ^double v] (conj acc v)) [])))))
    (testing "does not modify elements beyond current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        ;; Set value beyond size via underlying array
        (aset ^doubles (.array arr) 5 99.0)
        (arr/fill! arr 1.0)
        ;; Value beyond size should be unchanged
        (is (= 99.0 (aget ^doubles (.array arr) 5)))))
    (testing "handles empty array"
      (let [arr (arr/resizable-double-array 10 0)]
        (is (= arr (arr/fill! arr 5.0)))
        (is (= 0 (arr/length arr)))))
    (testing "handles single element"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5 1)]
        (arr/fill! arr 7.0)
        (is (= [7.0] (arr/dfold arr (fn [acc ^double v] (conj acc v)) [])))))
    (testing "fills with zero"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5 3)]
        (dotimes [i 3]
          (.setDouble arr i (double (inc i))))
        (arr/fill! arr 0.0)
        (is (= [0.0 0.0 0.0]
               (arr/dfold arr (fn [acc ^double v] (conj acc v)) [])))))
    (testing "returns array for chaining"
      (let [arr (arr/resizable-double-array 5 3)]
        (is (identical? arr (arr/fill! arr 1.0)))))))

(deftest resizable-double-array-to-fixed-test
  ;; Tests conversion from ResizableDoubleArray to DoubleArray.
  ;; Contracts: to-fixed creates new DoubleArray with only active elements.
  (testing "to-fixed"
    (testing "creates DoubleArray with current size elements"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (dotimes [i 5]
          (.setDouble arr i (double (* i 2))))
        (let [fixed (arr/to-fixed arr)]
          (is (arr/double-array? fixed))
          (is (= 5 (arr/length fixed)))
          (is (= [0.0 2.0 4.0 6.0 8.0]
                 (arr/dfold fixed (fn [acc ^double v] (conj acc v)) []))))))
    (testing "creates copy not sharing backing array"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 5 5)
            _                         (dotimes [i 5]
                                        (.setDouble arr i (double i)))
            fixed                     (arr/to-fixed arr)]
        ;; Modify original
        (.setDouble arr 0 99.0)
        ;; Fixed should be unchanged
        (is (= 0.0 (arr/get-at fixed 0)))))
    (testing "works with zero size"
      (let [arr   (arr/resizable-double-array 10 0)
            fixed (arr/to-fixed arr)]
        (is (= 0 (arr/length fixed)))))
    (testing "works after resize"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 10)]
        (dotimes [i 10]
          (.setDouble arr i (double i)))
        (arr/resize! arr 3)
        (let [fixed (arr/to-fixed arr)]
          (is (= 3 (arr/length fixed)))
          (is (= [0.0 1.0 2.0]
                 (arr/dfold fixed (fn [acc ^double v] (conj acc v)) []))))))))

(deftest resizable-double-array-sorted-test
  ;; Tests that sorted returns a DoubleArray respecting current size.
  (testing "sorted"
    (testing "returns sorted DoubleArray with size elements"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (.setDouble arr 0 3.0)
        (.setDouble arr 1 1.0)
        (.setDouble arr 2 4.0)
        (.setDouble arr 3 1.0)
        (.setDouble arr 4 5.0)
        ;; Set value beyond size via underlying array (to verify sorted ignores it)
        (aset ^doubles (.array arr) 5 0.0)
        (let [result (arr/sorted arr)]
          (is (arr/double-array? result))
          (is (= 5 (arr/length result)))
          (is (= [1.0 1.0 3.0 4.0 5.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest resizable-double-array-filter-indices-test
  ;; Tests filter-indices respects current size.
  (testing "filter-indices"
    (testing "excludes indices and returns DoubleArray"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (dotimes [i 5]
          (.setDouble arr i (double i)))
        (let [result (arr/filter-indices arr #{1 3})]
          (is (arr/double-array? result))
          (is (= 3 (arr/length result)))
          (is (= [0.0 2.0 4.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest resizable-double-array-fold-skip-test
  ;; Tests fold-skip operations respect current size.
  (testing "fold-double-skip"
    (testing "skips index within current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (dotimes [i 5]
          (.setDouble arr i (double (inc i))))
        ;; Sum 1+2+3+4+5 = 15, skip index 2 (value 3) = 12
        (is (= 12.0 (arr/fold-double-skip
                     arr 2
                     (fn ^double [^double a ^double b] (+ a b))
                     0.0))))))
  (testing "dfold-skip"
    (testing "skips index when collecting"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 4)]
        (dotimes [i 4]
          (.setDouble arr i (double i)))
        (is (= [0.0 2.0 3.0]
               (arr/dfold-skip arr 1
                               (fn [acc ^double v] (conj acc v))
                               [])))))))

(deftest resizable-double-array-indexed-fold-test
  ;; Tests indexed fold operations respect current size.
  (testing "indexed-fold-double"
    (testing "iterates with indices over current size only"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        (dotimes [i 3]
          (.setDouble arr i 1.0))
        ;; Sum of indices: 0+1+2 = 3
        (is (= 3.0 (arr/indexed-fold-double
                    arr
                    (fn ^double [^double acc ^long i ^double _v]
                      (+ acc (double i)))
                    0.0))))))
  (testing "indexed-dfold"
    (testing "collects with indices from current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        (dotimes [i 3]
          (.setDouble arr i (double (* i 10))))
        (is (= [{:i 0 :v 0.0} {:i 1 :v 10.0} {:i 2 :v 20.0}]
               (arr/indexed-dfold
                arr
                (fn [acc ^long i ^double v]
                  (conj acc {:i i :v v}))
                [])))))))

(deftest resizable-double-array-dany-test
  ;; Tests dany predicate respects current size.
  (testing "dany?"
    (testing "checks only within current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        (dotimes [i 3]
          (.setDouble arr i 1.0))
        ;; Set a value beyond size via underlying array (to verify dany ignores it)
        (aset ^doubles (.array arr) 5 99.0)
        ;; Should not find 99.0
        (is (not (arr/dany? arr (fn [^double x] (== x 99.0)))))
        ;; Should find 1.0
        (is (arr/dany? arr (fn [^double x] (== x 1.0))))))))

(deftest resizable-double-array-array-equals-test
  ;; Tests arrayEquals respects current size.
  (testing "array="
    (testing "compares only current size elements"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        (dotimes [i 3]
          (.setDouble arr i (double i)))
        (is (arr/array= arr [0.0 1.0 2.0]))
        (is (not (arr/array= arr [0.0 1.0 2.0 3.0])))))))

(deftest resizable-double-array-bounds-checking-test
  ;; Tests that indexed access methods throw on out-of-bounds access.
  ;; Contracts: getDouble/getLong/getObject and setDouble/setLong/setObject
  ;; throw IndexOutOfBoundsException for indices outside [0, size).
  (testing "IIndexed bounds checking"
    (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
      (dotimes [i 5]
        (.setDouble arr i (double i)))
      (testing "getDouble throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.getDouble arr 5))))
      (testing "getDouble throws on negative index"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index -1 out of bounds for size 5"
                              (.getDouble arr -1))))
      (testing "getLong throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.getLong arr 5))))
      (testing "getObject throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 7 out of bounds for size 5"
                              (.getObject arr 7))))))
  (testing "IIndexedSet bounds checking"
    (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
      (testing "setDouble throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.setDouble arr 5 99.0))))
      (testing "setDouble throws on negative index"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index -1 out of bounds for size 5"
                              (.setDouble arr -1 99.0))))
      (testing "setLong throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 6 out of bounds for size 5"
                              (.setLong arr 6 99))))
      (testing "setObject throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.setObject arr 5 99.0))))))
  (testing "bounds checking respects resized size"
    (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 10)]
      (dotimes [i 10]
        (.setDouble arr i (double i)))
      (arr/resize! arr 3)
      (testing "access at index 3 throws after resize to 3"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 3 out of bounds for size 3"
                              (.getDouble arr 3))))
      (testing "access at index 2 succeeds"
        (is (= 2.0 (.getDouble arr 2)))))))

;;; ResizableLongArray tests

(deftest resizable-long-array-construction-test
  ;; Tests construction of ResizableLongArray with capacity and initial size.
  ;; Contracts: capacity-only creates array with size=capacity,
  ;; capacity+size creates with specified size, validates bounds.
  (testing "resizable-long-array"
    (testing "with capacity only"
      (testing "creates array with size equal to capacity"
        (let [^ResizableLongArray arr (arr/resizable-long-array 5)]
          (is (= 5 (arr/length arr)))
          (is (= 5 (arr/capacity arr)))))
      (testing "creates array with zero capacity"
        (let [^ResizableLongArray arr (arr/resizable-long-array 0)]
          (is (= 0 (arr/length arr)))
          (is (= 0 (arr/capacity arr))))))
    (testing "with capacity and initial-size"
      (testing "creates array with specified size less than capacity"
        (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
          (is (= 3 (arr/length arr)))
          (is (= 10 (arr/capacity arr)))))
      (testing "creates array with size equal to capacity"
        (let [^ResizableLongArray arr (arr/resizable-long-array 5 5)]
          (is (= 5 (arr/length arr)))
          (is (= 5 (arr/capacity arr)))))
      (testing "creates array with zero initial size"
        (let [^ResizableLongArray arr (arr/resizable-long-array 10 0)]
          (is (= 0 (arr/length arr)))
          (is (= 10 (arr/capacity arr)))))
      (testing "throws on negative initial-size"
        (is (thrown-with-msg? IllegalArgumentException
                              #"initial-size must be between 0 and capacity"
                              (arr/resizable-long-array 5 -1))))
      (testing "throws on initial-size exceeding capacity"
        (is (thrown-with-msg? IllegalArgumentException
                              #"initial-size must be between 0 and capacity"
                              (arr/resizable-long-array 5 6)))))))

(deftest resizable-long-array-predicate-test
  ;; Tests the resizable-long-array? type predicate.
  ;; Contracts: returns true only for ResizableLongArray instances.
  (testing "resizable-long-array?"
    (testing "returns true for ResizableLongArray"
      (is (arr/resizable-long-array? (arr/resizable-long-array 5))))
    (testing "returns false for LongArray"
      (is (not (arr/resizable-long-array?
                (arr/->long-array (long-array [1]))))))
    (testing "returns false for other types"
      (is (not (arr/resizable-long-array? [1 2])))
      (is (not (arr/resizable-long-array? nil))))))

(deftest resizable-long-array-resize-test
  ;; Tests the resize! operation on ResizableLongArray.
  ;; Contracts: shrinking works, growing beyond capacity throws,
  ;; resize to same size works, resize to zero works.
  (testing "resize!"
    (testing "shrinks the array"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 10)]
        (arr/resize! arr 5)
        (is (= 5 (arr/length arr)))
        (is (= 10 (arr/capacity arr)))))
    (testing "resizes to zero"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5)]
        (arr/resize! arr 0)
        (is (= 0 (arr/length arr)))))
    (testing "resizes to same size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5 3)]
        (arr/resize! arr 3)
        (is (= 3 (arr/length arr)))))
    (testing "can grow back up to capacity after shrinking"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 10)]
        (arr/resize! arr 3)
        (is (= 3 (arr/length arr)))
        (arr/resize! arr 8)
        (is (= 8 (arr/length arr)))))
    (testing "throws on negative size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5)]
        (is (thrown-with-msg? IllegalArgumentException
                              #"new-size must be between 0 and capacity"
                              (arr/resize! arr -1)))))
    (testing "throws on size exceeding capacity"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5)]
        (is (thrown-with-msg? IllegalArgumentException
                              #"new-size must be between 0 and capacity"
                              (arr/resize! arr 6)))))))

(deftest resizable-long-array-elem-type-test
  ;; Tests that ResizableLongArray reports correct element type.
  (testing "elem-type"
    (testing "returns :long"
      (is (= :long (arr/elem-type (arr/resizable-long-array 5)))))))

(deftest resizable-long-array-fold-test
  ;; Tests fold operations on ResizableLongArray respect current size.
  ;; Contracts: fold only iterates over active elements, not capacity.
  (testing "fold operations"
    (testing "fold respects current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        ;; Set values within size
        (dotimes [i 5]
          (.setLong arr i (long i)))
        ;; Set values beyond size via underlying array (to verify fold ignores them)
        (dotimes [i 5]
          (aset ^longs (.array arr) (+ i 5) (long (+ i 100))))
        (is (= [0 1 2 3 4]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "fold-long respects current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        (dotimes [i 5]
          (.setLong arr i (long i)))
        ;; Set values beyond size via underlying array
        (dotimes [i 5]
          (aset ^longs (.array arr) (+ i 5) (long (+ i 100))))
        ;; Sum of 0+1+2+3+4 = 10
        (is (= 10 (arr/fold-long arr
                                 (fn ^long [^long a ^long b]
                                   (+ a b))
                                 0)))))
    (testing "lfold respects current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
        (dotimes [i 3]
          (.setLong arr i (long (* i 10))))
        ;; Set values beyond size via underlying array
        (dotimes [i 7]
          (aset ^longs (.array arr) (+ i 3) 999))
        (is (= [0 10 20]
               (arr/lfold arr (fn [acc ^long v] (conj acc v)) [])))))
    (testing "fold on resized array uses new size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 10)]
        (dotimes [i 10]
          (.setLong arr i (long i)))
        (arr/resize! arr 3)
        (is (= [0 1 2]
               (arr/fold arr #(conj %1 %2) [])))))))

(deftest resizable-long-array-map-test
  ;; Tests map operations on ResizableLongArray return fixed arrays.
  ;; Contracts: lmap/lmapIndexed return LongArray sized to current size,
  ;; dmap/dmapIndexed return DoubleArray sized to current size.
  (testing "lmap"
    (testing "returns LongArray with size elements"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)
            _                       (dotimes [i 5]
                                      (.setLong arr i (long (inc i))))
            result                  (arr/lmap arr (fn ^long [^long x] (* x 2)))]
        (is (arr/long-array? result))
        (is (= 5 (arr/length result)))
        (is (= [2 4 6 8 10]
               (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))
  (testing "lmapIndexed"
    (testing "returns LongArray with size elements"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)
            _                       (dotimes [i 3]
                                      (.setLong arr i 1))
            result                  (arr/lmap-indexed arr
                                                      (fn ^long [^long i ^long _x]
                                                        i))]
        (is (arr/long-array? result))
        (is (= 3 (arr/length result)))
        (is (= [0 1 2]
               (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))
  (testing "dmap"
    (testing "returns DoubleArray with size elements"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 4)
            _                       (dotimes [i 4]
                                      (.setLong arr i (long (inc i))))
            result                  (arr/dmap arr (fn ^double [^double x] (* x 1.5)))]
        (is (arr/double-array? result))
        (is (= 4 (arr/length result)))
        (is (= [1.5 3.0 4.5 6.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))))

(deftest resizable-long-array-fill!-test
  ;; Tests fill! operation on ResizableLongArray.
  ;; Contracts: fill! sets elements 0 to size-1 to the specified value,
  ;; returns array for chaining, handles empty arrays and zero values.
  (testing "fill!"
    (testing "fills all elements within current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        (dotimes [i 5]
          (.setLong arr i (long i)))
        (arr/fill! arr 42)
        (is (= [42 42 42 42 42]
               (arr/lfold arr (fn [acc ^long v] (conj acc v)) [])))))
    (testing "does not modify elements beyond current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
        ;; Set value beyond size via underlying array
        (aset ^longs (.array arr) 5 99)
        (arr/fill! arr 1)
        ;; Value beyond size should be unchanged
        (is (= 99 (aget ^longs (.array arr) 5)))))
    (testing "handles empty array"
      (let [arr (arr/resizable-long-array 10 0)]
        (is (= arr (arr/fill! arr 5)))
        (is (= 0 (arr/length arr)))))
    (testing "handles single element"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5 1)]
        (arr/fill! arr 7)
        (is (= [7] (arr/lfold arr (fn [acc ^long v] (conj acc v)) [])))))
    (testing "fills with zero"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5 3)]
        (dotimes [i 3]
          (.setLong arr i (long (inc i))))
        (arr/fill! arr 0)
        (is (= [0 0 0]
               (arr/lfold arr (fn [acc ^long v] (conj acc v)) [])))))
    (testing "returns array for chaining"
      (let [arr (arr/resizable-long-array 5 3)]
        (is (identical? arr (arr/fill! arr 1)))))))

(deftest resizable-long-array-to-fixed-test
  ;; Tests conversion from ResizableLongArray to LongArray.
  ;; Contracts: to-fixed creates new LongArray with only active elements.
  (testing "to-fixed"
    (testing "creates LongArray with current size elements"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        (dotimes [i 5]
          (.setLong arr i (long (* i 2))))
        (let [fixed (arr/to-fixed arr)]
          (is (arr/long-array? fixed))
          (is (= 5 (arr/length fixed)))
          (is (= [0 2 4 6 8]
                 (arr/lfold fixed (fn [acc ^long v] (conj acc v)) []))))))
    (testing "creates copy not sharing backing array"
      (let [^ResizableLongArray arr (arr/resizable-long-array 5 5)
            _                       (dotimes [i 5]
                                      (.setLong arr i (long i)))
            fixed                   (arr/to-fixed arr)]
        (.setLong arr 0 99)
        (is (= 0 (arr/get-at fixed 0)))))
    (testing "works with zero size"
      (let [arr   (arr/resizable-long-array 10 0)
            fixed (arr/to-fixed arr)]
        (is (= 0 (arr/length fixed)))))
    (testing "works after resize"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 10)]
        (dotimes [i 10]
          (.setLong arr i (long i)))
        (arr/resize! arr 3)
        (let [fixed (arr/to-fixed arr)]
          (is (= 3 (arr/length fixed)))
          (is (= [0 1 2]
                 (arr/lfold fixed (fn [acc ^long v] (conj acc v)) []))))))))

(deftest resizable-long-array-sorted-test
  ;; Tests that sorted returns a DoubleArray respecting current size.
  (testing "sorted"
    (testing "returns sorted DoubleArray with size elements"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        (.setLong arr 0 3)
        (.setLong arr 1 1)
        (.setLong arr 2 4)
        (.setLong arr 3 1)
        (.setLong arr 4 5)
        ;; Set value beyond size via underlying array (to verify sorted ignores it)
        (aset ^longs (.array arr) 5 0)
        (let [result (arr/sorted arr)]
          (is (arr/double-array? result))
          (is (= 5 (arr/length result)))
          (is (= [1.0 1.0 3.0 4.0 5.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest resizable-long-array-filter-indices-test
  ;; Tests filter-indices respects current size.
  (testing "filter-indices"
    (testing "excludes indices and returns LongArray"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        (dotimes [i 5]
          (.setLong arr i (long i)))
        (let [result (arr/filter-indices arr #{1 3})]
          (is (arr/long-array? result))
          (is (= 3 (arr/length result)))
          (is (= [0 2 4]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))))

(deftest resizable-long-array-fold-skip-test
  ;; Tests fold-skip operations respect current size.
  (testing "fold-double-skip"
    (testing "skips index within current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
        (dotimes [i 5]
          (.setLong arr i (long (inc i))))
        ;; Sum 1+2+3+4+5 = 15, skip index 2 (value 3) = 12
        (is (= 12.0 (arr/fold-double-skip
                     arr 2
                     (fn ^double [^double a ^double b] (+ a b))
                     0.0))))))
  (testing "dfold-skip"
    (testing "skips index when collecting"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 4)]
        (dotimes [i 4]
          (.setLong arr i (long i)))
        (is (= [0.0 2.0 3.0]
               (arr/dfold-skip arr 1
                               (fn [acc ^double v] (conj acc v))
                               [])))))))

(deftest resizable-long-array-indexed-fold-test
  ;; Tests indexed fold operations respect current size.
  (testing "indexed-fold-double"
    (testing "iterates with indices over current size only"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
        (dotimes [i 3]
          (.setLong arr i 1))
        ;; Sum of indices: 0+1+2 = 3
        (is (= 3.0 (arr/indexed-fold-double
                    arr
                    (fn ^double [^double acc ^long i ^double _v]
                      (+ acc (double i)))
                    0.0))))))
  (testing "indexed-dfold"
    (testing "collects with indices from current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
        (dotimes [i 3]
          (.setLong arr i (long (* i 10))))
        (is (= [{:i 0 :v 0.0} {:i 1 :v 10.0} {:i 2 :v 20.0}]
               (arr/indexed-dfold
                arr
                (fn [acc ^long i ^double v]
                  (conj acc {:i i :v v}))
                [])))))))

(deftest resizable-long-array-lany-test
  ;; Tests lany predicate respects current size.
  (testing "lany?"
    (testing "checks only within current size"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
        (dotimes [i 3]
          (.setLong arr i 1))
        ;; Set value beyond size via underlying array (to verify lany ignores it)
        (aset ^longs (.array arr) 5 99)
        (is (not (arr/lany? arr (fn [^long x] (== x 99)))))
        (is (arr/lany? arr (fn [^long x] (== x 1))))))))

(deftest resizable-long-array-array-equals-test
  ;; Tests arrayEquals respects current size.
  (testing "array="
    (testing "compares only current size elements"
      (let [^ResizableLongArray arr (arr/resizable-long-array 10 3)]
        (dotimes [i 3]
          (.setLong arr i (long i)))
        (is (arr/array= arr [0 1 2]))
        (is (not (arr/array= arr [0 1 2 3])))))))

(deftest resizable-long-array-bounds-checking-test
  ;; Tests that indexed access methods throw on out-of-bounds access.
  ;; Contracts: getDouble/getLong/getObject and setDouble/setLong/setObject
  ;; throw IndexOutOfBoundsException for indices outside [0, size).
  (testing "IIndexed bounds checking"
    (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
      (dotimes [i 5]
        (.setLong arr i (long i)))
      (testing "getLong throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.getLong arr 5))))
      (testing "getLong throws on negative index"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index -1 out of bounds for size 5"
                              (.getLong arr -1))))
      (testing "getDouble throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.getDouble arr 5))))
      (testing "getObject throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 7 out of bounds for size 5"
                              (.getObject arr 7))))))
  (testing "IIndexedSet bounds checking"
    (let [^ResizableLongArray arr (arr/resizable-long-array 10 5)]
      (testing "setLong throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.setLong arr 5 99))))
      (testing "setLong throws on negative index"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index -1 out of bounds for size 5"
                              (.setLong arr -1 99))))
      (testing "setDouble throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 6 out of bounds for size 5"
                              (.setDouble arr 6 99.0))))
      (testing "setObject throws on index >= size"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 5 out of bounds for size 5"
                              (.setObject arr 5 99))))))
  (testing "bounds checking respects resized size"
    (let [^ResizableLongArray arr (arr/resizable-long-array 10 10)]
      (dotimes [i 10]
        (.setLong arr i (long i)))
      (arr/resize! arr 3)
      (testing "access at index 3 throws after resize to 3"
        (is (thrown-with-msg? IndexOutOfBoundsException
                              #"index 3 out of bounds for size 3"
                              (.getLong arr 3))))
      (testing "access at index 2 succeeds"
        (is (= 2 (.getLong arr 2)))))))

;;; ResizableObjectArray tests

(deftest resizable-object-array-construction-test
  ;; Tests construction of ResizableObjectArray with capacity and initial size.
  ;; Contracts: capacity-only creates array with size=capacity,
  ;; capacity+size creates with specified size, validates bounds.
  (testing "resizable-object-array"
    (testing "with capacity only"
      (testing "creates array with size equal to capacity"
        (let [^ResizableObjectArray arr (arr/resizable-object-array 5)]
          (is (= 5 (arr/length arr)))
          (is (= 5 (arr/capacity arr)))))
      (testing "creates array with zero capacity"
        (let [^ResizableObjectArray arr (arr/resizable-object-array 0)]
          (is (= 0 (arr/length arr)))
          (is (= 0 (arr/capacity arr))))))
    (testing "with capacity and initial-size"
      (testing "creates array with specified size less than capacity"
        (let [^ResizableObjectArray arr (arr/resizable-object-array 10 3)]
          (is (= 3 (arr/length arr)))
          (is (= 10 (arr/capacity arr)))))
      (testing "creates array with size equal to capacity"
        (let [^ResizableObjectArray arr (arr/resizable-object-array 5 5)]
          (is (= 5 (arr/length arr)))
          (is (= 5 (arr/capacity arr)))))
      (testing "creates array with zero initial size"
        (let [^ResizableObjectArray arr (arr/resizable-object-array 10 0)]
          (is (= 0 (arr/length arr)))
          (is (= 10 (arr/capacity arr)))))
      (testing "throws on negative initial-size"
        (is (thrown-with-msg? IllegalArgumentException
                              #"initial-size must be between 0 and capacity"
                              (arr/resizable-object-array 5 -1))))
      (testing "throws on initial-size exceeding capacity"
        (is (thrown-with-msg? IllegalArgumentException
                              #"initial-size must be between 0 and capacity"
                              (arr/resizable-object-array 5 6)))))))

(deftest resizable-object-array-predicate-test
  ;; Tests the resizable-object-array? type predicate.
  ;; Contracts: returns true only for ResizableObjectArray instances.
  (testing "resizable-object-array?"
    (testing "returns true for ResizableObjectArray"
      (is (arr/resizable-object-array? (arr/resizable-object-array 5))))
    (testing "returns false for ObjectArray"
      (is (not (arr/resizable-object-array?
                (arr/->object-array (object-array [:a]))))))
    (testing "returns false for other types"
      (is (not (arr/resizable-object-array? [:a :b])))
      (is (not (arr/resizable-object-array? nil))))))

(deftest resizable-object-array-resize-test
  ;; Tests the resize! operation on ResizableObjectArray.
  ;; Contracts: shrinking works, growing beyond capacity throws,
  ;; resize to same size works, resize to zero works.
  (testing "resize!"
    (testing "shrinks the array"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 10)]
        (arr/resize! arr 5)
        (is (= 5 (arr/length arr)))
        (is (= 10 (arr/capacity arr)))))
    (testing "resizes to zero"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5)]
        (arr/resize! arr 0)
        (is (= 0 (arr/length arr)))))
    (testing "resizes to same size"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5 3)]
        (arr/resize! arr 3)
        (is (= 3 (arr/length arr)))))
    (testing "can grow back up to capacity after shrinking"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 10)]
        (arr/resize! arr 3)
        (is (= 3 (arr/length arr)))
        (arr/resize! arr 8)
        (is (= 8 (arr/length arr)))))
    (testing "throws on negative size"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5)]
        (is (thrown-with-msg? IllegalArgumentException
                              #"new-size must be between 0 and capacity"
                              (arr/resize! arr -1)))))
    (testing "throws on size exceeding capacity"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5)]
        (is (thrown-with-msg? IllegalArgumentException
                              #"new-size must be between 0 and capacity"
                              (arr/resize! arr 6)))))))

(deftest resizable-object-array-elem-type-test
  ;; Tests that ResizableObjectArray reports correct element type.
  (testing "elem-type"
    (testing "returns :object"
      (is (= :object (arr/elem-type (arr/resizable-object-array 5)))))))

(deftest resizable-object-array-fold-test
  ;; Tests fold operations on ResizableObjectArray respect current size.
  ;; Contracts: fold only iterates over active elements, not capacity.
  (testing "fold operations"
    (testing "fold respects current size"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 5)]
        (dotimes [i 10]
          (aset ^objects (.array arr) i (keyword (str "k" i))))
        (is (= [:k0 :k1 :k2 :k3 :k4]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "fold on resized array uses new size"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 10)]
        (dotimes [i 10]
          (aset ^objects (.array arr) i (keyword (str "k" i))))
        (arr/resize! arr 3)
        (is (= [:k0 :k1 :k2]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "fold on empty array returns init"
      (let [arr (arr/resizable-object-array 10 0)]
        (is (= []
               (arr/fold arr #(conj %1 %2) [])))))))

(deftest resizable-object-array-fill!-test
  ;; Tests fill! operation on ResizableObjectArray.
  ;; Contracts: fill! sets elements 0 to size-1 to the specified value,
  ;; returns array for chaining, handles empty arrays and nil values.
  (testing "fill!"
    (testing "fills all elements within current size"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 5)]
        (dotimes [i 5]
          (aset ^objects (.array arr) i (keyword (str "k" i))))
        (arr/fill! arr :filled)
        (is (= [:filled :filled :filled :filled :filled]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "does not modify elements beyond current size"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 3)]
        ;; Set value beyond size via underlying array
        (aset ^objects (.array arr) 5 :beyond)
        (arr/fill! arr :within)
        ;; Value beyond size should be unchanged
        (is (= :beyond (aget ^objects (.array arr) 5)))))
    (testing "handles empty array"
      (let [arr (arr/resizable-object-array 10 0)]
        (is (= arr (arr/fill! arr :val)))
        (is (= 0 (arr/length arr)))))
    (testing "handles single element"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5 1)]
        (arr/fill! arr :single)
        (is (= [:single] (arr/fold arr #(conj %1 %2) [])))))
    (testing "fills with nil"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5 3)]
        (dotimes [i 3]
          (aset ^objects (.array arr) i (keyword (str "k" i))))
        (arr/fill! arr nil)
        (is (= [nil nil nil]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "returns array for chaining"
      (let [arr (arr/resizable-object-array 5 3)]
        (is (identical? arr (arr/fill! arr :val)))))))

(deftest resizable-object-array-to-fixed-test
  ;; Tests conversion from ResizableObjectArray to ObjectArray.
  ;; Contracts: to-fixed creates new ObjectArray with only active elements.
  (testing "to-fixed"
    (testing "creates ObjectArray with current size elements"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 5)]
        (dotimes [i 5]
          (aset ^objects (.array arr) i (keyword (str "v" i))))
        (let [fixed (arr/to-fixed arr)]
          (is (arr/object-array? fixed))
          (is (= 5 (arr/length fixed)))
          (is (= [:v0 :v1 :v2 :v3 :v4]
                 (arr/fold fixed #(conj %1 %2) []))))))
    (testing "creates copy not sharing backing array"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 5 5)
            _                         (dotimes [i 5]
                                        (aset ^objects (.array arr) i (keyword (str "k" i))))
            fixed                     (arr/to-fixed arr)]
        (aset ^objects (.array arr) 0 :changed)
        (is (= :k0 (aget ^objects (.array ^criterium.array.ObjectArray fixed) 0)))))
    (testing "works with zero size"
      (let [arr   (arr/resizable-object-array 10 0)
            fixed (arr/to-fixed arr)]
        (is (= 0 (arr/length fixed)))))
    (testing "works after resize"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 10)]
        (dotimes [i 10]
          (aset ^objects (.array arr) i (keyword (str "k" i))))
        (arr/resize! arr 3)
        (let [fixed (arr/to-fixed arr)]
          (is (= 3 (arr/length fixed)))
          (is (= [:k0 :k1 :k2]
                 (arr/fold fixed #(conj %1 %2) []))))))))

(deftest resizable-object-array-array-equals-test
  ;; Tests arrayEquals respects current size.
  (testing "array="
    (testing "compares only current size elements"
      (let [^ResizableObjectArray arr (arr/resizable-object-array 10 3)]
        (dotimes [i 3]
          (aset ^objects (.array arr) i (keyword (str "k" i))))
        (is (arr/array= arr [:k0 :k1 :k2]))
        (is (not (arr/array= arr [:k0 :k1 :k2 :k3])))))))

;;; Public API tests for capacity and resize!

(deftest capacity-polymorphic-test
  ;; Tests the capacity function works uniformly with all resizable array types.
  ;; Contracts: capacity returns the maximum capacity for any IResizable.
  (testing "capacity"
    (testing "returns capacity for ResizableDoubleArray"
      (let [arr (arr/resizable-double-array 10 5)]
        (is (= 10 (arr/capacity arr)))))
    (testing "returns capacity for ResizableLongArray"
      (let [arr (arr/resizable-long-array 15 3)]
        (is (= 15 (arr/capacity arr)))))
    (testing "returns capacity for ResizableObjectArray"
      (let [arr (arr/resizable-object-array 20 0)]
        (is (= 20 (arr/capacity arr)))))))

(deftest resize!-polymorphic-test
  ;; Tests the resize! function works uniformly with all resizable array types.
  ;; Contracts: resize! mutates size and returns new size for any IResizable.
  (testing "resize!"
    (testing "resizes ResizableDoubleArray and returns new size"
      (let [arr (arr/resizable-double-array 10 10)]
        (is (= 5 (arr/resize! arr 5)))
        (is (= 5 (arr/length arr)))))
    (testing "resizes ResizableLongArray and returns new size"
      (let [arr (arr/resizable-long-array 10 10)]
        (is (= 3 (arr/resize! arr 3)))
        (is (= 3 (arr/length arr)))))
    (testing "resizes ResizableObjectArray and returns new size"
      (let [arr (arr/resizable-object-array 10 10)]
        (is (= 7 (arr/resize! arr 7)))
        (is (= 7 (arr/length arr)))))))

;;; Tests for IIndexed/IIndexedSet interfaces via interface-typed bindings
;; These tests verify that the interface methods work correctly when called
;; through interface types rather than concrete types.

(deftest resizable-double-array-iindexed-interface-test
  ;; Tests IIndexed interface methods when accessed through interface type.
  ;; Contracts: getDouble/getLong/getObject work when array is typed as IIndexed.
  (testing "IIndexed"
    (testing "getDouble via IIndexed type"
      (let [^IIndexed arr (arr/resizable-double-array 5)]
        (dotimes [i 5]
          (.setDouble ^IIndexedSet arr i (double (* i 10))))
        (is (= 0.0 (.getDouble arr 0)))
        (is (= 20.0 (.getDouble arr 2)))
        (is (= 40.0 (.getDouble arr 4)))))
    (testing "getLong via IIndexed type"
      (let [^IIndexed arr (arr/resizable-double-array 3)]
        (dotimes [i 3]
          (.setDouble ^IIndexedSet arr i (double i)))
        (is (= 0 (.getLong arr 0)))
        (is (= 2 (.getLong arr 2)))))
    (testing "getObject via IIndexed type"
      (let [^IIndexed arr (arr/resizable-double-array 3)]
        (dotimes [i 3]
          (.setDouble ^IIndexedSet arr i (double i)))
        (is (= 1.0 (.getObject arr 1)))))
    (testing "bounds checking via IIndexed type"
      (let [^IIndexed arr (arr/resizable-double-array 5 3)]
        (is (thrown? IndexOutOfBoundsException (.getDouble arr 3)))
        (is (thrown? IndexOutOfBoundsException (.getDouble arr -1)))))))

(deftest resizable-double-array-iindexedset-interface-test
  ;; Tests IIndexedSet interface methods when accessed through interface type.
  ;; Contracts: setDouble/setLong/setObject work when array is typed as IIndexedSet.
  (testing "IIndexedSet"
    (testing "setDouble via IIndexedSet type"
      (let [arr               (arr/resizable-double-array 5)
            ^IIndexedSet iset arr]
        (.setDouble iset 0 1.5)
        (.setDouble iset 2 3.5)
        (is (= 1.5 (.getDouble ^IIndexed arr 0)))
        (is (= 3.5 (.getDouble ^IIndexed arr 2)))))
    (testing "setLong via IIndexedSet type"
      (let [arr               (arr/resizable-double-array 3)
            ^IIndexedSet iset arr]
        (.setLong iset 1 42)
        (is (= 42.0 (.getDouble ^IIndexed arr 1)))))
    (testing "setObject via IIndexedSet type"
      (let [arr               (arr/resizable-double-array 3)
            ^IIndexedSet iset arr]
        (.setObject iset 0 7.5)
        (is (= 7.5 (.getDouble ^IIndexed arr 0)))))
    (testing "bounds checking via IIndexedSet type"
      (let [^IIndexedSet arr (arr/resizable-double-array 5 3)]
        (is (thrown? IndexOutOfBoundsException (.setDouble arr 3 1.0)))
        (is (thrown? IndexOutOfBoundsException (.setDouble arr -1 1.0)))))))

(deftest resizable-long-array-iindexed-interface-test
  ;; Tests IIndexed interface methods when accessed through interface type.
  ;; Contracts: getDouble/getLong/getObject work when array is typed as IIndexed.
  (testing "IIndexed"
    (testing "getDouble via IIndexed type"
      (let [^IIndexed arr (arr/resizable-long-array 5)]
        (dotimes [i 5]
          (.setLong ^IIndexedSet arr i (long (* i 10))))
        (is (= 0.0 (.getDouble arr 0)))
        (is (= 20.0 (.getDouble arr 2)))
        (is (= 40.0 (.getDouble arr 4)))))
    (testing "getLong via IIndexed type"
      (let [^IIndexed arr (arr/resizable-long-array 3)]
        (dotimes [i 3]
          (.setLong ^IIndexedSet arr i (long i)))
        (is (= 0 (.getLong arr 0)))
        (is (= 2 (.getLong arr 2)))))
    (testing "getObject via IIndexed type"
      (let [^IIndexed arr (arr/resizable-long-array 3)]
        (dotimes [i 3]
          (.setLong ^IIndexedSet arr i (long i)))
        (is (= 1 (.getObject arr 1)))))
    (testing "bounds checking via IIndexed type"
      (let [^IIndexed arr (arr/resizable-long-array 5 3)]
        (is (thrown? IndexOutOfBoundsException (.getLong arr 3)))
        (is (thrown? IndexOutOfBoundsException (.getLong arr -1)))))))

(deftest resizable-long-array-iindexedset-interface-test
  ;; Tests IIndexedSet interface methods when accessed through interface type.
  ;; Contracts: setDouble/setLong/setObject work when array is typed as IIndexedSet.
  (testing "IIndexedSet"
    (testing "setDouble via IIndexedSet type"
      (let [arr               (arr/resizable-long-array 5)
            ^IIndexedSet iset arr]
        (.setDouble iset 0 1.0)
        (.setDouble iset 2 3.0)
        (is (= 1 (.getLong ^IIndexed arr 0)))
        (is (= 3 (.getLong ^IIndexed arr 2)))))
    (testing "setLong via IIndexedSet type"
      (let [arr               (arr/resizable-long-array 3)
            ^IIndexedSet iset arr]
        (.setLong iset 1 42)
        (is (= 42 (.getLong ^IIndexed arr 1)))))
    (testing "setObject via IIndexedSet type"
      (let [arr               (arr/resizable-long-array 3)
            ^IIndexedSet iset arr]
        (.setObject iset 0 7)
        (is (= 7 (.getLong ^IIndexed arr 0)))))
    (testing "bounds checking via IIndexedSet type"
      (let [^IIndexedSet arr (arr/resizable-long-array 5 3)]
        (is (thrown? IndexOutOfBoundsException (.setLong arr 3 1)))
        (is (thrown? IndexOutOfBoundsException (.setLong arr -1 1)))))))
