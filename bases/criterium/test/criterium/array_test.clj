(ns criterium.array-test
  ;; Tests the TypedArray interfaces and types that provide
  ;; primitive array storage for benchmark samples.
  ;; Contracts: type construction, interface method implementations,
  ;; primitive fold operations, and metric type -> element type mapping.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]))

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

(deftest metric-type->elem-type-test
  (testing "metric-type->elem-type"
    (testing "maps :quantitative to :double"
      (is (= :double (arr/metric-type->elem-type :quantitative))))
    (testing "maps :event to :long"
      (is (= :long (arr/metric-type->elem-type :event))))
    (testing "maps :nominal to :object"
      (is (= :object (arr/metric-type->elem-type :nominal))))))

(deftest array-for-metric-type-test
  (testing "array-for-metric-type"
    (testing "creates DoubleArray for :quantitative"
      (let [wrapped (arr/array-for-metric-type :quantitative [1.0 2.0 3.0])]
        (is (instance? criterium.array.DoubleArray wrapped))
        (is (= 3 (arr/length wrapped)))
        (is (= 6.0 (arr/fold wrapped + 0.0)))))
    (testing "creates LongArray for :event"
      (let [wrapped (arr/array-for-metric-type :event [10 20 30])]
        (is (instance? criterium.array.LongArray wrapped))
        (is (= 3 (arr/length wrapped)))
        (is (= 60 (arr/fold wrapped + 0)))))
    (testing "creates ObjectArray for :nominal"
      (let [wrapped (arr/array-for-metric-type :nominal [:x :y :z])]
        (is (instance? criterium.array.ObjectArray wrapped))
        (is (= 3 (arr/length wrapped)))
        (is (= [:x :y :z] (arr/fold wrapped #(conj %1 %2) [])))))))

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
  (testing "sorted"
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
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))))

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
               (arr/dfold result (fn [acc ^double v] (conj acc v)) [])))))))
