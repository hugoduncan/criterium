(ns criterium.typed-samples-test
  ;; Tests the TypedSamples interfaces and types that provide
  ;; primitive array storage for benchmark samples.
  ;; Contracts: type construction, interface method implementations,
  ;; primitive fold operations, and metric type -> element type mapping.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.typed-samples :as ts]))

(deftest double-samples-test
  (testing "DoubleSamples"
    (let [arr     (double-array [1.0 2.0 3.0 4.0 5.0])
          samples (ts/double-samples arr)]
      (testing "returns :double from elem-type"
        (is (= :double (ts/elem-type samples))))
      (testing "returns correct count from sample-count"
        (is (= 5 (ts/sample-count samples))))
      (testing "folds over values correctly"
        (is (= 15.0 (ts/fold samples + 0.0))))
      (testing "fold accumulates in order"
        (is (= [1.0 2.0 3.0 4.0 5.0]
               (ts/fold samples #(conj %1 %2) [])))))))

(deftest long-samples-test
  (testing "LongSamples"
    (let [arr     (long-array [10 20 30 40])
          samples (ts/long-samples arr)]
      (testing "returns :long from elem-type"
        (is (= :long (ts/elem-type samples))))
      (testing "returns correct count from sample-count"
        (is (= 4 (ts/sample-count samples))))
      (testing "folds over values correctly"
        (is (= 100 (ts/fold samples + 0))))
      (testing "fold accumulates in order"
        (is (= [10 20 30 40]
               (ts/fold samples #(conj %1 %2) [])))))))

(deftest object-samples-test
  (testing "ObjectSamples"
    (let [arr     (object-array [:a :b :c])
          samples (ts/object-samples arr)]
      (testing "returns :object from elem-type"
        (is (= :object (ts/elem-type samples))))
      (testing "returns correct count from sample-count"
        (is (= 3 (ts/sample-count samples))))
      (testing "folds over values correctly"
        (is (= [:a :b :c]
               (ts/fold samples #(conj %1 %2) [])))))))

(deftest empty-samples-test
  (testing "empty arrays"
    (testing "DoubleSamples with zero elements"
      (let [samples (ts/double-samples (double-array []))]
        (is (= 0 (ts/sample-count samples)))
        (is (= 0.0 (ts/fold samples + 0.0)))))
    (testing "LongSamples with zero elements"
      (let [samples (ts/long-samples (long-array []))]
        (is (= 0 (ts/sample-count samples)))
        (is (= 0 (ts/fold samples + 0)))))
    (testing "ObjectSamples with zero elements"
      (let [samples (ts/object-samples (object-array []))]
        (is (= 0 (ts/sample-count samples)))
        (is (= [] (ts/fold samples #(conj %1 %2) [])))))))

(deftest metric-type->elem-type-test
  (testing "metric-type->elem-type"
    (testing "maps :quantitative to :double"
      (is (= :double (ts/metric-type->elem-type :quantitative))))
    (testing "maps :event to :long"
      (is (= :long (ts/metric-type->elem-type :event))))
    (testing "maps :nominal to :object"
      (is (= :object (ts/metric-type->elem-type :nominal))))))

(deftest samples-for-metric-type-test
  (testing "samples-for-metric-type"
    (testing "creates DoubleSamples for :quantitative"
      (let [samples (ts/samples-for-metric-type :quantitative [1.0 2.0 3.0])]
        (is (instance? criterium.typed_samples.DoubleSamples samples))
        (is (= 3 (ts/sample-count samples)))
        (is (= 6.0 (ts/fold samples + 0.0)))))
    (testing "creates LongSamples for :event"
      (let [samples (ts/samples-for-metric-type :event [10 20 30])]
        (is (instance? criterium.typed_samples.LongSamples samples))
        (is (= 3 (ts/sample-count samples)))
        (is (= 60 (ts/fold samples + 0)))))
    (testing "creates ObjectSamples for :nominal"
      (let [samples (ts/samples-for-metric-type :nominal [:x :y :z])]
        (is (instance? criterium.typed_samples.ObjectSamples samples))
        (is (= 3 (ts/sample-count samples)))
        (is (= [:x :y :z] (ts/fold samples #(conj %1 %2) [])))))))

(deftest fold-double-test
  (testing "fold-double"
    (testing "sums doubles without boxing"
      (let [samples (ts/double-samples (double-array [1.0 2.0 3.0 4.0 5.0]))]
        (is (= 15.0 (ts/fold-double samples
                                    (fn ^double [^double a ^double b]
                                      (+ a b))
                                    0.0)))))
    (testing "works with empty array"
      (let [samples (ts/double-samples (double-array []))]
        (is (= 0.0 (ts/fold-double samples
                                   (fn ^double [^double a ^double b]
                                     (+ a b))
                                   0.0)))))
    (testing "finds maximum value"
      (let [samples (ts/double-samples (double-array [3.0 1.0 4.0 1.0 5.0]))]
        (is (= 5.0 (ts/fold-double samples
                                   (fn ^double [^double a ^double b]
                                     (Math/max a b))
                                   Double/NEGATIVE_INFINITY)))))))

(deftest fold-long-test
  (testing "fold-long"
    (testing "sums longs without boxing"
      (let [samples (ts/long-samples (long-array [10 20 30 40]))]
        (is (= 100 (ts/fold-long samples
                                 (fn ^long [^long a ^long b]
                                   (+ a b))
                                 0)))))
    (testing "works with empty array"
      (let [samples (ts/long-samples (long-array []))]
        (is (= 0 (ts/fold-long samples
                               (fn ^long [^long a ^long b]
                                 (+ a b))
                               0)))))
    (testing "counts non-zero values"
      (let [samples (ts/long-samples (long-array [0 1 0 2 3 0]))]
        (is (= 3 (ts/fold-long samples
                               (fn ^long [^long cnt ^long v]
                                 (if (zero? v) cnt (unchecked-inc cnt)))
                               0)))))))
