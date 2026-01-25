(ns criterium.array-test
  ;; Tests the TypedArray interfaces and types that provide
  ;; primitive array storage for benchmark samples.
  ;; Contracts: type construction, interface method implementations,
  ;; primitive fold operations, and metric type -> element type mapping.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr])
  (:import
   [criterium.array ResizableDoubleArray]))

(deftest type-predicates-test
  (testing "type predicates"
    (let [da (arr/->double-array (double-array [1.0 2.0]))
          la (arr/->long-array (long-array [1 2]))
          oa (arr/->object-array (object-array [:a :b]))]
      (testing "typed-array? returns true for all typed arrays"
        (is (arr/typed-array? da))
        (is (arr/typed-array? la))
        (is (arr/typed-array? oa)))
      (testing "typed-array? returns false for non-arrays"
        (is (not (arr/typed-array? [1 2 3])))
        (is (not (arr/typed-array? nil)))
        (is (not (arr/typed-array? "string"))))
      (testing "double-array? returns true only for DoubleArray"
        (is (arr/double-array? da))
        (is (not (arr/double-array? la)))
        (is (not (arr/double-array? oa)))
        (is (not (arr/double-array? [1.0]))))
      (testing "long-array? returns true only for LongArray"
        (is (arr/long-array? la))
        (is (not (arr/long-array? da)))
        (is (not (arr/long-array? oa)))
        (is (not (arr/long-array? [1]))))
      (testing "object-array? returns true only for ObjectArray"
        (is (arr/object-array? oa))
        (is (not (arr/object-array? da)))
        (is (not (arr/object-array? la)))
        (is (not (arr/object-array? [:a])))))))

(deftest double-array-test
  (testing "DoubleArray"
    (let [a       (double-array [1.0 2.0 3.0 4.0 5.0])
          wrapped (arr/->double-array a)]
      (testing "returns :double from elem-type"
        (is (= :double (arr/elem-type wrapped))))
      (testing "returns correct count from length"
        (is (= 5 (arr/length wrapped))))
      (testing "folds over values correctly"
        (is (= 15.0 (arr/fold wrapped + 0.0))))
      (testing "fold accumulates in order"
        (is (= [1.0 2.0 3.0 4.0 5.0]
               (arr/fold wrapped #(conj %1 %2) [])))))))

(deftest long-array-test
  (testing "LongArray"
    (let [a       (long-array [10 20 30 40])
          wrapped (arr/->long-array a)]
      (testing "returns :long from elem-type"
        (is (= :long (arr/elem-type wrapped))))
      (testing "returns correct count from length"
        (is (= 4 (arr/length wrapped))))
      (testing "folds over values correctly"
        (is (= 100 (arr/fold wrapped + 0))))
      (testing "fold accumulates in order"
        (is (= [10 20 30 40]
               (arr/fold wrapped #(conj %1 %2) [])))))))

(deftest object-array-test
  (testing "ObjectArray"
    (let [a       (object-array [:a :b :c])
          wrapped (arr/->object-array a)]
      (testing "returns :object from elem-type"
        (is (= :object (arr/elem-type wrapped))))
      (testing "returns correct count from length"
        (is (= 3 (arr/length wrapped))))
      (testing "folds over values correctly"
        (is (= [:a :b :c]
               (arr/fold wrapped #(conj %1 %2) [])))))))

(deftest empty-arrays-test
  (testing "empty arrays"
    (testing "DoubleArray with zero elements"
      (let [wrapped (arr/->double-array (double-array []))]
        (is (= 0 (arr/length wrapped)))
        (is (= 0.0 (arr/fold wrapped + 0.0)))))
    (testing "LongArray with zero elements"
      (let [wrapped (arr/->long-array (long-array []))]
        (is (= 0 (arr/length wrapped)))
        (is (= 0 (arr/fold wrapped + 0)))))
    (testing "ObjectArray with zero elements"
      (let [wrapped (arr/->object-array (object-array []))]
        (is (= 0 (arr/length wrapped)))
        (is (= [] (arr/fold wrapped #(conj %1 %2) [])))))))

(deftest fold-double-test
  (testing "fold-double"
    (testing "sums doubles without boxing"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))]
        (is (= 15.0 (arr/fold-double wrapped
                                     (fn ^double [^double a ^double b]
                                       (+ a b))
                                     0.0)))))
    (testing "works with empty array"
      (let [wrapped (arr/->double-array (double-array []))]
        (is (= 0.0 (arr/fold-double wrapped
                                    (fn ^double [^double a ^double b]
                                      (+ a b))
                                    0.0)))))
    (testing "finds maximum value"
      (let [wrapped (arr/->double-array (double-array [3.0 1.0 4.0 1.0 5.0]))]
        (is (= 5.0 (arr/fold-double wrapped
                                    (fn ^double [^double a ^double b]
                                      (Math/max a b))
                                    Double/NEGATIVE_INFINITY)))))))

(deftest fold-long-test
  (testing "fold-long"
    (testing "sums longs without boxing"
      (let [wrapped (arr/->long-array (long-array [10 20 30 40]))]
        (is (= 100 (arr/fold-long wrapped
                                  (fn ^long [^long a ^long b]
                                    (+ a b))
                                  0)))))
    (testing "works with empty array"
      (let [wrapped (arr/->long-array (long-array []))]
        (is (= 0 (arr/fold-long wrapped
                                (fn ^long [^long a ^long b]
                                  (+ a b))
                                0)))))
    (testing "counts non-zero values"
      (let [wrapped (arr/->long-array (long-array [0 1 0 2 3 0]))]
        (is (= 3 (arr/fold-long wrapped
                                (fn ^long [^long cnt ^long v]
                                  (if (zero? v) cnt (unchecked-inc cnt)))
                                0)))))))

(deftest dfold-test
  (testing "dfold"
    (testing "collects doubles into a vector"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
        (is (= [1.0 2.0 3.0]
               (arr/dfold wrapped
                          (fn [acc ^double v] (conj acc v))
                          [])))))
    (testing "works with empty array"
      (let [wrapped (arr/->double-array (double-array []))]
        (is (= []
               (arr/dfold wrapped
                          (fn [acc ^double v] (conj acc v))
                          [])))))
    (testing "accumulates into a map"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
        (is (= {:sum 6.0 :count 3}
               (arr/dfold wrapped
                          (fn [acc ^double v]
                            (-> acc
                                (update :sum + v)
                                (update :count inc)))
                          {:sum 0.0 :count 0})))))))

(deftest lfold-test
  (testing "lfold"
    (testing "collects longs into a vector"
      (let [wrapped (arr/->long-array (long-array [10 20 30]))]
        (is (= [10 20 30]
               (arr/lfold wrapped
                          (fn [acc ^long v] (conj acc v))
                          [])))))
    (testing "works with empty array"
      (let [wrapped (arr/->long-array (long-array []))]
        (is (= []
               (arr/lfold wrapped
                          (fn [acc ^long v] (conj acc v))
                          [])))))
    (testing "accumulates into a map"
      (let [wrapped (arr/->long-array (long-array [10 20 30]))]
        (is (= {:sum 60 :count 3}
               (arr/lfold wrapped
                          (fn [acc ^long v]
                            (-> acc
                                (update :sum + v)
                                (update :count inc)))
                          {:sum 0 :count 0})))))))

(deftest sorted-test
  ;; Tests the sorted function which returns a new sorted DoubleArray.
  ;; Contracts: returns sorted copy without modifying original, handles edge cases.
  (testing "sorted"
    (testing "with DoubleArray"
      (testing "returns a sorted copy"
        (let [original (arr/->double-array (double-array [3.0 1.0 4.0 1.0 5.0]))
              result   (arr/sorted original)]
          (is (= [1.0 1.0 3.0 4.0 5.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))
      (testing "does not modify original"
        (let [original (arr/->double-array (double-array [3.0 1.0 2.0]))]
          (arr/sorted original)
          (is (= [3.0 1.0 2.0]
                 (arr/dfold original (fn [acc ^double v] (conj acc v)) [])))))
      (testing "works with empty array"
        (let [original (arr/->double-array (double-array []))
              result   (arr/sorted original)]
          (is (= 0 (arr/length result)))))
      (testing "works with single element"
        (let [original (arr/->double-array (double-array [42.0]))
              result   (arr/sorted original)]
          (is (= [42.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))
    (testing "with LongArray"
      (testing "returns a sorted DoubleArray"
        (let [original (arr/->long-array (long-array [3 1 4 1 5]))
              result   (arr/sorted original)]
          (is (= [1.0 1.0 3.0 4.0 5.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))
      (testing "does not modify original"
        (let [original (arr/->long-array (long-array [3 1 2]))]
          (arr/sorted original)
          (is (= [3 1 2]
                 (arr/lfold original (fn [acc ^long v] (conj acc v)) [])))))
      (testing "works with empty array"
        (let [original (arr/->long-array (long-array []))
              result   (arr/sorted original)]
          (is (= 0 (arr/length result)))))
      (testing "works with single element"
        (let [original (arr/->long-array (long-array [42]))
              result   (arr/sorted original)]
          (is (= [42.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest fold-double-skip-test
  (testing "fold-double-skip"
    (testing "skips the specified index"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))]
        (is (= 13.0 (arr/fold-double-skip
                     wrapped 1
                     (fn ^double [^double a ^double b] (+ a b))
                     0.0))
            "Skipping index 1 (value 2.0) gives 1+3+4+5=13")))
    (testing "skips first element"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
        (is (= 5.0 (arr/fold-double-skip
                    wrapped 0
                    (fn ^double [^double a ^double b] (+ a b))
                    0.0)))))
    (testing "skips last element"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
        (is (= 3.0 (arr/fold-double-skip
                    wrapped 2
                    (fn ^double [^double a ^double b] (+ a b))
                    0.0)))))
    (testing "works with single element (skip only element)"
      (let [wrapped (arr/->double-array (double-array [42.0]))]
        (is (= 0.0 (arr/fold-double-skip
                    wrapped 0
                    (fn ^double [^double a ^double b] (+ a b))
                    0.0)))))))

(deftest dfold-skip-test
  (testing "dfold-skip"
    (testing "skips the specified index when collecting"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0]))]
        (is (= [1.0 3.0 4.0]
               (arr/dfold-skip wrapped 1
                               (fn [acc ^double v] (conj acc v))
                               [])))))
    (testing "skips first element"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
        (is (= [2.0 3.0]
               (arr/dfold-skip wrapped 0
                               (fn [acc ^double v] (conj acc v))
                               [])))))
    (testing "skips last element"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
        (is (= [1.0 2.0]
               (arr/dfold-skip wrapped 2
                               (fn [acc ^double v] (conj acc v))
                               [])))))))

(deftest indexed-fold-double-test
  ;; Tests primitive indexed fold that returns double.
  ;; Contracts: fold with index access, primitive performance, both array types.
  (testing "indexed-fold-double"
    (testing "with DoubleArray"
      (testing "sums values using index"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))]
          (is (= 15.0 (arr/indexed-fold-double
                       wrapped
                       (fn ^double [^double acc ^long _i ^double v]
                         (+ acc v))
                       0.0)))))
      (testing "computes weighted sum using index"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
          ;; sum of v * i = 1*0 + 2*1 + 3*2 = 0 + 2 + 6 = 8
          (is (= 8.0 (arr/indexed-fold-double
                      wrapped
                      (fn ^double [^double acc ^long i ^double v]
                        (+ acc (* v (double i))))
                      0.0)))))
      (testing "finds max value"
        (let [wrapped (arr/->double-array (double-array [3.0 1.0 4.0 1.0 5.0]))]
          (is (= 5.0 (arr/indexed-fold-double
                      wrapped
                      (fn ^double [^double acc ^long _i ^double v]
                        (Math/max acc v))
                      Double/NEGATIVE_INFINITY)))))
      (testing "works with empty array"
        (let [wrapped (arr/->double-array (double-array []))]
          (is (= 0.0 (arr/indexed-fold-double
                      wrapped
                      (fn ^double [^double acc ^long _i ^double v]
                        (+ acc v))
                      0.0))))))
    (testing "with LongArray"
      (testing "sums values (as doubles)"
        (let [wrapped (arr/->long-array (long-array [10 20 30 40]))]
          (is (= 100.0 (arr/indexed-fold-double
                        wrapped
                        (fn ^double [^double acc ^long _i ^double v]
                          (+ acc v))
                        0.0)))))
      (testing "computes weighted sum using index"
        (let [wrapped (arr/->long-array (long-array [10 20 30]))]
          ;; sum of v * i = 10*0 + 20*1 + 30*2 = 0 + 20 + 60 = 80
          (is (= 80.0 (arr/indexed-fold-double
                       wrapped
                       (fn ^double [^double acc ^long i ^double v]
                         (+ acc (* v (double i))))
                       0.0))))))))

(deftest indexed-dfold-test
  ;; Tests Object-returning indexed fold.
  ;; Contracts: fold with index access, accumulates into collections, both array types.
  (testing "indexed-dfold"
    (testing "with DoubleArray"
      (testing "collects values with indices into vector of maps"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
          (is (= [{:i 0 :v 1.0} {:i 1 :v 2.0} {:i 2 :v 3.0}]
                 (arr/indexed-dfold
                  wrapped
                  (fn [acc ^long i ^double v]
                    (conj acc {:i i :v v}))
                  [])))))
      (testing "collects only even-indexed values"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))]
          (is (= [1.0 3.0 5.0]
                 (arr/indexed-dfold
                  wrapped
                  (fn [acc ^long i ^double v]
                    (if (even? i) (conj acc v) acc))
                  [])))))
      (testing "accumulates into a map"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
          (is (= {:sum 6.0 :count 3 :last-idx 2}
                 (arr/indexed-dfold
                  wrapped
                  (fn [acc ^long i ^double v]
                    (-> acc
                        (update :sum + v)
                        (update :count inc)
                        (assoc :last-idx i)))
                  {:sum 0.0 :count 0 :last-idx -1})))))
      (testing "works with empty array"
        (let [wrapped (arr/->double-array (double-array []))]
          (is (= []
                 (arr/indexed-dfold
                  wrapped
                  (fn [acc ^long i ^double v]
                    (conj acc {:i i :v v}))
                  []))))))
    (testing "with LongArray"
      (testing "collects values with indices"
        (let [wrapped (arr/->long-array (long-array [10 20 30]))]
          (is (= [{:i 0 :v 10.0} {:i 1 :v 20.0} {:i 2 :v 30.0}]
                 (arr/indexed-dfold
                  wrapped
                  (fn [acc ^long i ^double v]
                    (conj acc {:i i :v v}))
                  [])))))
      (testing "works with empty array"
        (let [wrapped (arr/->long-array (long-array []))]
          (is (= []
                 (arr/indexed-dfold
                  wrapped
                  (fn [acc ^long i ^double v]
                    (conj acc {:i i :v v}))
                  []))))))))

(deftest filter-indices-test
  (testing "filter-indices"
    (testing "excludes specified indices"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0 5.0]))
            result  (arr/filter-indices wrapped #{1 3})]
        (is (= 3 (arr/length result)))
        (is (= [1.0 3.0 5.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))
    (testing "returns copy when exclude set is empty"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))
            result  (arr/filter-indices wrapped #{})]
        (is (= 3 (arr/length result)))
        (is (= [1.0 2.0 3.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))
    (testing "returns empty array when all excluded"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0]))
            result  (arr/filter-indices wrapped #{0 1})]
        (is (= 0 (arr/length result)))))
    (testing "works with single element excluded"
      (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0 4.0]))
            result  (arr/filter-indices wrapped #{2})]
        (is (= [1.0 2.0 4.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))
    (testing "on LongArray"
      (testing "excludes specified indices and returns LongArray"
        (let [wrapped (arr/->long-array (long-array [10 20 30 40 50]))
              result  (arr/filter-indices wrapped #{1 3})]
          (is (= :long (arr/elem-type result)))
          (is (= 3 (arr/length result)))
          (is (= [10 30 50]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) [])))))
      (testing "returns LongArray copy when exclude set is empty"
        (let [wrapped (arr/->long-array (long-array [10 20 30]))
              result  (arr/filter-indices wrapped #{})]
          (is (= :long (arr/elem-type result)))
          (is (= 3 (arr/length result)))
          (is (= [10 20 30]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))))

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
        ;; Set values in the array
        (dotimes [i 10]
          (.setDouble arr i (double i)))
        ;; Fold should only see indices 0-4
        (is (= [0.0 1.0 2.0 3.0 4.0]
               (arr/fold arr #(conj %1 %2) [])))))
    (testing "fold-double respects current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 5)]
        (dotimes [i 10]
          (.setDouble arr i (double i)))
        ;; Sum of 0+1+2+3+4 = 10
        (is (= 10.0 (arr/fold-double arr
                                     (fn ^double [^double a ^double b]
                                       (+ a b))
                                     0.0)))))
    (testing "dfold respects current size"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)]
        (dotimes [i 10]
          (.setDouble arr i (double (* i 10))))
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
            _ (dotimes [i 5]
                (.setDouble arr i (double (inc i))))
            result (arr/dmap arr (fn ^double [^double x] (* x 2.0)))]
        (is (arr/double-array? result))
        (is (= 5 (arr/length result)))
        (is (= [2.0 4.0 6.0 8.0 10.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))
  (testing "dmapIndexed"
    (testing "returns DoubleArray with size elements"
      (let [^ResizableDoubleArray arr (arr/resizable-double-array 10 3)
            _ (dotimes [i 3]
                (.setDouble arr i 1.0))
            ;; Map each element to its index
            result (arr/dmap-indexed arr
                                     (fn ^double [^long i ^double _x]
                                       (double i)))]
        (is (arr/double-array? result))
        (is (= 3 (arr/length result)))
        (is (= [0.0 1.0 2.0]
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))))

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
            _ (dotimes [i 5]
                (.setDouble arr i (double i)))
            fixed (arr/to-fixed arr)]
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
        ;; Set values beyond size (should be ignored)
        (.setDouble arr 5 0.0)
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
        ;; Set a value beyond size
        (.setDouble arr 5 99.0)
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
