(ns criterium.stats.histogram-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.stats.histogram :as histogram]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

(defn- abs-error
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

;; Tests for histogram computation.
;; Tests verify contract: histogram computes correct bin structure and counts.

(deftest histogram-test
  (testing "histogram"
    (testing "computes valid histogram structure"
      (let [data (darr (range 1 101))
            h (histogram/histogram data)]
        (is (= :criterium/histogram-fixed-width (:type h)))
        (is (= 100 (:n h)))
        (is (= 1.0 (:min h)))
        (is (= 100.0 (:max h)))
        (is (vector? (:counts h)))
        (is (vector? (:centers h)))
        (is (vector? (:density h)))
        (is (number? (:width h)))
        (is (pos? (:num-bins h)))))
    (testing "bin counts sum to sample count"
      (let [data (darr (range 1 101))
            h (histogram/histogram data)]
        (is (= (:n h) (reduce + (:counts h))))))
    (testing "density sums to approximately 1"
      (let [data (darr (range 1 101))
            h (histogram/histogram data)
            density-sum (reduce + (:density h))]
        (is (< (abs-error 1.0 density-sum) 0.01))))
    (testing "throws on empty input"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"empty"
           (histogram/histogram (darr [])))))
    (testing "throws on constant values"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"same"
           (histogram/histogram (darr (repeat 100 5.0))))))
    (testing "accepts precomputed IQR"
      (let [data (darr (range 1 101))
            iqr 25.0
            h (histogram/histogram data iqr)]
        (is (= :criterium/histogram-fixed-width (:type h)))))))
