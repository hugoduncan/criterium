(ns criterium.transducer-test
  ;; Tests primitive transducers for high-performance data processing.
  ;; Contracts: primitive range sources, reduce/transduce operations,
  ;; prim-map/prim-filter transformations, and array output via into.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.primitive-fn :as pf]
   [criterium.transducer :as t]))

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
          (is (= 30 (t/transduce (t/prim-map square)
                                 t/prim-sum
                                 0
                                 (t/range 0 5)))
              "0+1+4+9+16 = 30")))
      (testing "into long array with map"
        (let [square (fn ^long [^long x] (* x x))
              result (t/into
                      (arr/->long-array (long-array 4))
                      (t/prim-map square)
                      (t/range 1 5))]
          (is (= [1 4 9 16]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))
    (testing "with double values"
      (testing "applies function during reduce"
        (let [square (fn ^double [^double x] (* x x))]
          (is (= 30.0 (t/transduce (t/prim-map square)
                                   t/prim-sum
                                   0.0
                                   (t/range 0.0 5.0 1.0)))
              "0+1+4+9+16 = 30")))
      (testing "into double array with map"
        (let [sqrt (fn ^double [^double x] (Math/sqrt x))
              result (t/into
                      (arr/->double-array (double-array 4))
                      (t/prim-map sqrt)
                      (t/range 1.0 5.0 1.0))]
          (is (= [1.0 (Math/sqrt 2.0) (Math/sqrt 3.0) 2.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest prim-filter-test
  (testing "prim-filter"
    (testing "with long values"
      (testing "filters during reduce"
        (is (= 6 (t/transduce (t/prim-filter pf/leven?)
                              t/prim-sum
                              0
                              (t/range 0 5)))
            "0+2+4 = 6"))
      (testing "into long array with filter"
        (let [result (t/into
                      (arr/->long-array (long-array 2))
                      (t/prim-filter pf/lodd?)
                      (t/range 0 5))]
          (is (= [1 3]
                 (arr/lfold result (fn [acc ^long v] (conj acc v)) []))))))
    (testing "with double values"
      (testing "filters during reduce"
        (is (= 3.0 (t/transduce (t/prim-filter pf/dpos?)
                                t/prim-sum
                                0.0
                                (t/range -2.0 3.0 1.0)))
            "positive values: 1.0+2.0 = 3.0"))
      (testing "into double array with filter"
        (let [result (t/into
                      (arr/->double-array (double-array 2))
                      (t/prim-filter pf/dpos?)
                      (t/range -2.0 3.0 1.0))]
          (is (= [1.0 2.0]
                 (arr/dfold result (fn [acc ^double v] (conj acc v)) []))))))))

(deftest composed-transducers-test
  (testing "composed transducers"
    (testing "map then filter with longs"
      (let [square (fn ^long [^long x] (* x x))
            xform  (comp (t/prim-map square)
                         (t/prim-filter pf/lodd?))]
        (is (= 10 (t/transduce xform t/prim-sum 0 (t/range 0 5)))
            "squares: 0,1,4,9,16 -> odd ones: 1,9 -> sum: 10")))
    (testing "filter then map with longs"
      (let [square (fn ^long [^long x] (* x x))
            xform  (comp (t/prim-filter pf/lodd?)
                         (t/prim-map square))]
        (is (= 10 (t/transduce xform t/prim-sum 0 (t/range 0 5)))
            "odds: 1,3 -> squares: 1,9 -> sum: 10")))
    (testing "map then map with doubles"
      (let [add1   (fn ^double [^double x] (+ x 1.0))
            double (fn ^double [^double x] (* x 2.0))
            xform  (comp (t/prim-map add1)
                         (t/prim-map double))]
        (is (= 12.0 (t/transduce xform t/prim-sum 0.0 (t/range 0.0 3.0 1.0)))
            "(0+1)*2 + (1+1)*2 + (2+1)*2 = 2+4+6 = 12")))))

(deftest in-place-transformation-test
  (testing "in-place transformation"
    (testing "transforms array elements into same array"
      (let [a       (arr/->double-array (double-array [0.0 1.0 2.0 3.0]))
            square  (fn ^double [^double x] (* x x))
            _result (t/into a (t/prim-map square) a)]
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
