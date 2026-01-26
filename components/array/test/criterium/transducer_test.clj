(ns criterium.transducer-test
  ;; Tests primitive transducers for high-performance data processing.
  ;; Contracts: primitive range sources, reduce/transduce operations,
  ;; prim-map/prim-filter transformations, array output via into, and
  ;; cross-type reductions (long→double, double→long, and object accumulators).
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.primitive-fn :as pf]
   [criterium.transducer :as t])
  (:import
   [criterium.array.interfaces IIndexed IIndexedSet]
   [criterium.transducer.interfaces
    IDLDReducible ILDLReducible IODLOReducible IOLDOReducible]))

(deftest long-range-test
  (testing "LongRange"
    (testing "implements ILLLReducible"
      (testing "reduces with primitive long accumulator"
        (is (= 10 (t/reduce t/prim-sum 0 (t/range 0 5)))
            "0+1+2+3+4 = 10"))
      (testing "handles empty range"
        (is (= 0 (t/reduce t/prim-sum 0 (t/range 5 5)))))
      (testing "handles single element"
        (is (= 3 (t/reduce t/prim-sum 0 (t/range 3 4))))))
    (testing "implements IOLOReducible"
      (testing "reduces into long array"
        (let [result (t/into
                      (arr/->long-array (long-array 5))
                      identity
                      (t/range 0 5))]
          (is (= [0 1 2 3 4]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))))

(deftest double-range-test
  (testing "DoubleRange"
    (testing "implements IDDDReducible"
      (testing "reduces with primitive double accumulator"
        (is (= 10.0 (t/reduce t/prim-sum 0.0 (t/range 0.0 5.0 1.0)))
            "0+1+2+3+4 = 10"))
      (testing "handles empty range"
        (is (= 0.0 (t/reduce t/prim-sum 0.0 (t/range 5.0 5.0 1.0)))))
      (testing "handles fractional steps"
        (is (= 3.0 (t/reduce t/prim-sum 0.0 (t/range 0.0 2.0 0.5)))
            "0.0+0.5+1.0+1.5 = 3.0")))
    (testing "implements IODOReducible"
      (testing "reduces into double array"
        (let [result (t/into
                      (arr/->double-array (double-array 5))
                      identity
                      (t/range 0.0 5.0 1.0))]
          (is (= [0.0 1.0 2.0 3.0 4.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest prim-sum-test
  (testing "prim-sum"
    (testing "sums longs"
      (is (= 6 (.invokePrim ^clojure.lang.IFn$LLL t/prim-sum 1 5))))
    (testing "sums doubles"
      (is (= 6.5 (.invokePrim ^clojure.lang.IFn$DDD t/prim-sum 1.5 5.0))))))

(deftest prim-map-test
  (testing "prim-map"
    (testing "with long values"
      (testing "applies function during reduce"
        (let [square (fn ^long [^long x] (* x x))]
          (is (= 30 (t/transduce (t/map square)
                                 t/prim-sum
                                 0
                                 (t/range 0 5)))
              "0+1+4+9+16 = 30")))
      (testing "into long array with map"
        (let [square (fn ^long [^long x] (* x x))
              result (t/into
                      (arr/->long-array (long-array 4))
                      (t/map square)
                      (t/range 1 5))]
          (is (= [1 4 9 16]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))
    (testing "with double values"
      (testing "applies function during reduce"
        (let [square (fn ^double [^double x] (* x x))]
          (is (= 30.0 (t/transduce (t/map square)
                                   t/prim-sum
                                   0.0
                                   (t/range 0.0 5.0 1.0)))
              "0+1+4+9+16 = 30")))
      (testing "into double array with map"
        (let [sqrt   (fn ^double [^double x] (Math/sqrt x))
              result (t/into
                      (arr/->double-array (double-array 4))
                      (t/map sqrt)
                      (t/range 1.0 5.0 1.0))]
          (is (= [1.0 (Math/sqrt 2.0) (Math/sqrt 3.0) 2.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest prim-filter-test
  (testing "prim-filter"
    (testing "with long values"
      (testing "filters during reduce"
        (is (= 6 (t/transduce (t/filter pf/leven?)
                              t/prim-sum
                              0
                              (t/range 0 5)))
            "0+2+4 = 6"))
      (testing "into long array with filter"
        (let [result (t/into
                      (arr/->long-array (long-array 2))
                      (t/filter pf/lodd?)
                      (t/range 0 5))]
          (is (= [1 3]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))
    (testing "with double values"
      (testing "filters during reduce"
        (is (= 3.0 (t/transduce (t/filter pf/dpos?)
                                t/prim-sum
                                0.0
                                (t/range -2.0 3.0 1.0)))
            "positive values: 1.0+2.0 = 3.0"))
      (testing "into double array with filter"
        (let [result (t/into
                      (arr/->double-array (double-array 2))
                      (t/filter pf/dpos?)
                      (t/range -2.0 3.0 1.0))]
          (is (= [1.0 2.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest composed-transducers-test
  (testing "composed transducers"
    (testing "map then filter with longs"
      (let [square (fn ^long [^long x] (* x x))
            xform  (comp (t/map square)
                         (t/filter pf/lodd?))]
        (is (= 10 (t/transduce xform t/prim-sum 0 (t/range 0 5)))
            "squares: 0,1,4,9,16 -> odd ones: 1,9 -> sum: 10")))
    (testing "filter then map with longs"
      (let [square (fn ^long [^long x] (* x x))
            xform  (comp (t/filter pf/lodd?)
                         (t/map square))]
        (is (= 10 (t/transduce xform t/prim-sum 0 (t/range 0 5)))
            "odds: 1,3 -> squares: 1,9 -> sum: 10")))
    (testing "map then map with doubles"
      (let [add1   (fn ^double [^double x] (+ x 1.0))
            double (fn ^double [^double x] (* x 2.0))
            xform  (comp (t/map add1)
                         (t/map double))]
        (is (= 12.0 (t/transduce xform t/prim-sum 0.0 (t/range 0.0 3.0 1.0)))
            "(0+1)*2 + (1+1)*2 + (2+1)*2 = 2+4+6 = 12")))))

(deftest in-place-transformation-test
  (testing "in-place transformation"
    (testing "transforms array elements into same array"
      (let [a       (arr/->double-array (double-array [0.0 1.0 2.0 3.0]))
            square  (fn ^double [^double x] (* x x))
            _result (t/into a (t/map square) a)]
        (is (= [0.0 1.0 4.0 9.0]
               (arr/dfold a (fn [acc ^double v] (conj acc v)) [])))))))

(deftest range-macro-test
  (testing "range macro"
    (testing "creates LongRange with two long args"
      (let [r (t/range 0 5)]
        (is (= 10 (t/reduce t/prim-sum 0 r)))))
    (testing "creates DoubleRange with three double args"
      (let [r (t/range 0.0 5.0 1.0)]
        (is (= 10.0 (t/reduce t/prim-sum 0.0 r)))))))

(deftest prim-set-at-test
  (testing "prim-set-at"
    (testing "sets long values sequentially"
      (let [setter (t/prim-set-at)
            arr    (arr/->long-array (long-array 3))]
        (.invokePrim ^clojure.lang.IFn$OLO setter arr 10)
        (.invokePrim ^clojure.lang.IFn$OLO setter arr 20)
        (.invokePrim ^clojure.lang.IFn$OLO setter arr 30)
        (is (= [10 20 30]
               (arr/lfold arr (fn [acc ^long v] (conj acc v)) [])))))
    (testing "sets double values sequentially"
      (let [setter (t/prim-set-at)
            arr    (arr/->double-array (double-array 3))]
        (.invokePrim ^clojure.lang.IFn$ODO setter arr 1.5)
        (.invokePrim ^clojure.lang.IFn$ODO setter arr 2.5)
        (.invokePrim ^clojure.lang.IFn$ODO setter arr 3.5)
        (is (= [1.5 2.5 3.5]
               (arr/dfold arr (fn [acc ^double v] (conj acc v)) [])))))))

(deftest cross-type-transduce-test
  ;; Tests cross-type transductions where input array type differs from
  ;; accumulator type. Contracts: IDLDReducible (long→double),
  ;; ILDLReducible (double→long), compile-time dispatch.
  (testing "cross-type transduce"
    (testing "long array to double accumulator"
      (testing "transforms and sums long array into double result"
        (let [longs  (arr/->long-array (long-array [1 2 3 4 5]))
              to-dbl (fn ^double [^long x] (double x))
              result (t/transduce (t/map to-dbl)
                                  t/prim-sum
                                  0.0
                                  longs)]
          (is (= 15.0 result))))
      (testing "applies function during cross-type reduction"
        (let [longs  (arr/->long-array (long-array [1 2 3 4]))
              add-pt (fn ^double [^long x] (+ (double x) 0.5))
              result (t/transduce (t/map add-pt)
                                  t/prim-sum
                                  0.0
                                  longs)]
          (is (= 12.0 result)
              "(1+0.5)+(2+0.5)+(3+0.5)+(4+0.5) = 12.0")))
      (testing "works with ResizableLongArray"
        (let [^IDLDReducible longs (arr/resizable-long-array 5)
              _      (dotimes [i 5]
                       (.setLong ^IIndexedSet longs i (inc i)))
              to-dbl (fn ^double [^long x] (double x))
              result (t/transduce (t/map to-dbl)
                                  t/prim-sum
                                  0.0
                                  longs)]
          (is (= 15.0 result)))))
    (testing "double array to long accumulator"
      (testing "transforms and accumulates double array into long result"
        (let [dbls   (arr/->double-array (double-array [1.9 2.1 3.5 4.4]))
              to-lng (fn ^long [^double x] (long x))
              result (t/transduce (t/map to-lng)
                                  t/prim-sum
                                  0
                                  dbls)]
          (is (= 10 result)
              "floor: 1+2+3+4 = 10")))
      (testing "works with ResizableDoubleArray"
        (let [^ILDLReducible dbls (arr/resizable-double-array 4)
              _      (dotimes [i 4]
                       (.setDouble ^IIndexedSet dbls i (+ 1.0 i)))
              to-lng (fn ^long [^double x] (long x))
              result (t/transduce (t/map to-lng)
                                  t/prim-sum
                                  0
                                  dbls)]
          (is (= 10 result)
              "1+2+3+4 = 10"))))))

(deftest cross-map-test
  ;; Tests cross-map transducer for cross-type reductions with object accumulators.
  ;; Contracts: IODLOReducible (double→long→object), IOLDOReducible (long→double→object),
  ;; works with fixed and resizable arrays.
  (testing "cross-map"
    (testing "double source → long element → object accumulator"
      (testing "transforms doubles to longs and accumulates into object"
        (let [^IODLOReducible dbls (arr/->double-array
                                    (double-array [0.5 1.5 2.5 3.5]))
              to-long (fn ^long [^double x] (long x))
              result  (atom [])]
          (t/transduce
           (t/cross-map to-long)
           (fn [acc ^long x] (swap! acc conj x) acc)
           result
           dbls)
          (is (= [0 1 2 3] @result))))
      (testing "works with ResizableDoubleArray"
        (let [^IODLOReducible dbls (arr/resizable-double-array 4)
              _       (dotimes [i 4]
                        (.setDouble ^IIndexedSet dbls i (+ 0.5 i)))
              to-long (fn ^long [^double x] (long x))
              result  (atom [])]
          (t/transduce
           (t/cross-map to-long)
           (fn [acc ^long x] (swap! acc conj x) acc)
           result
           dbls)
          (is (= [0 1 2 3] @result))))
      (testing "accumulates into mutable array (bin-counts pattern)"
        (let [^IODLOReducible data (arr/->double-array
                                    (double-array [0.1 0.9 1.1 1.9 2.5]))
              ^criterium.array.resizable.ResizableLongArray counts (arr/resizable-long-array 3)
              _      (arr/fill! counts 0)
              to-bin (fn ^long [^double x] (min 2 (max 0 (long x))))]
          (t/transduce
           (t/cross-map to-bin)
           (fn [c ^long idx]
             (let [^IIndexed indexed c
                   ^IIndexedSet indexed-set c]
               (.setLong indexed-set idx (inc (.getLong indexed idx))))
             c)
           counts
           data)
          (is (arr/array= counts [2 2 1])
              "bins: [0,1)→2, [1,2)→2, [2,3)→1"))))
    (testing "long source → double element → object accumulator"
      (testing "transforms longs to doubles and accumulates into object"
        (let [^IOLDOReducible longs (arr/->long-array (long-array [1 2 3 4]))
              to-dbl (fn ^double [^long x] (+ (double x) 0.5))
              result (atom [])]
          (t/transduce
           (t/cross-map to-dbl)
           (fn [acc ^double x] (swap! acc conj x) acc)
           result
           longs)
          (is (= [1.5 2.5 3.5 4.5] @result))))
      (testing "works with ResizableLongArray"
        (let [^IOLDOReducible longs (arr/resizable-long-array 4)
              _      (dotimes [i 4]
                       (.setLong ^IIndexedSet longs i (inc i)))
              to-dbl (fn ^double [^long x] (+ (double x) 0.5))
              result (atom [])]
          (t/transduce
           (t/cross-map to-dbl)
           (fn [acc ^double x] (swap! acc conj x) acc)
           result
           longs)
          (is (= [1.5 2.5 3.5 4.5] @result)))))))
