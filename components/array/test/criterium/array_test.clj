(ns criterium.array-test
  ;; Tests the fixed TypedArray interfaces and types that provide
  ;; primitive array storage for benchmark samples.
  ;; Contracts: type construction, interface method implementations,
  ;; primitive fold operations, and metric type -> element type mapping.
  ;; See criterium.array.resizable-test for resizable array tests.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]))

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

(deftest fill!-test
  ;; Tests fill! operation on fixed arrays.
  ;; Contracts: fills all elements with specified value, returns array for chaining.
  (testing "fill!"
    (testing "on DoubleArray"
      (testing "fills all elements with specified value"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
          (arr/fill! wrapped 42.0)
          (is (= [42.0 42.0 42.0]
                 (arr/dfold wrapped (fn [acc ^double v] (conj acc v)) [])))))
      (testing "fills with zero"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0 3.0]))]
          (arr/fill! wrapped 0.0)
          (is (= [0.0 0.0 0.0]
                 (arr/dfold wrapped (fn [acc ^double v] (conj acc v)) [])))))
      (testing "returns array for chaining"
        (let [wrapped (arr/->double-array (double-array [1.0 2.0]))]
          (is (identical? wrapped (arr/fill! wrapped 5.0)))))
      (testing "handles empty array"
        (let [wrapped (arr/->double-array (double-array []))]
          (is (identical? wrapped (arr/fill! wrapped 1.0)))
          (is (= 0 (arr/length wrapped))))))
    (testing "on LongArray"
      (testing "fills all elements with specified value"
        (let [wrapped (arr/->long-array (long-array [1 2 3]))]
          (arr/fill! wrapped 42)
          (is (= [42 42 42]
                 (arr/lfold wrapped (fn [acc ^long v] (conj acc v)) [])))))
      (testing "fills with zero"
        (let [wrapped (arr/->long-array (long-array [1 2 3]))]
          (arr/fill! wrapped 0)
          (is (= [0 0 0]
                 (arr/lfold wrapped (fn [acc ^long v] (conj acc v)) [])))))
      (testing "returns array for chaining"
        (let [wrapped (arr/->long-array (long-array [1 2]))]
          (is (identical? wrapped (arr/fill! wrapped 5)))))
      (testing "handles empty array"
        (let [wrapped (arr/->long-array (long-array []))]
          (is (identical? wrapped (arr/fill! wrapped 1)))
          (is (= 0 (arr/length wrapped))))))
    (testing "on ObjectArray"
      (testing "fills all elements with specified value"
        (let [wrapped (arr/->object-array (object-array [:a :b :c]))]
          (arr/fill! wrapped :filled)
          (is (= [:filled :filled :filled]
                 (arr/fold wrapped #(conj %1 %2) [])))))
      (testing "fills with nil"
        (let [wrapped (arr/->object-array (object-array [:a :b :c]))]
          (arr/fill! wrapped nil)
          (is (= [nil nil nil]
                 (arr/fold wrapped #(conj %1 %2) [])))))
      (testing "returns array for chaining"
        (let [wrapped (arr/->object-array (object-array [:a :b]))]
          (is (identical? wrapped (arr/fill! wrapped :x)))))
      (testing "handles empty array"
        (let [wrapped (arr/->object-array (object-array []))]
          (is (identical? wrapped (arr/fill! wrapped :x)))
          (is (= 0 (arr/length wrapped))))))))
