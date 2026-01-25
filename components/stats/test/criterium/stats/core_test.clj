(ns criterium.stats.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.stats.core :as core]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

;; Tests for core statistics functions.
;; Tests verify contract: each function computes expected statistical values.

(deftest mean-test
  (testing "mean"
    (testing "returns arithmetic mean of data"
      (is (= 1.0 (core/mean (darr (repeat 20 1)))))
      (is (= 3.0 (core/mean (darr (range 0 7)))))
      (is (= 50.0 (core/mean (darr (range 0 101))))))))

(deftest sum-test
  (testing "sum"
    (testing "returns sum of data points"
      (is (= 20.0 (core/sum (darr (take 20 (repeatedly (constantly 1)))))))
      (is (= 21.0 (core/sum (darr (range 0 7))))))))

(deftest sum-of-squares-test
  (testing "sum-of-squares"
    (testing "returns sum of squared data points"
      (is (= 20.0 (core/sum-of-squares (darr (take 20 (repeatedly (constantly 1)))))))
      (is (= 80.0 (core/sum-of-squares (darr (take 20 (repeatedly (constantly 2)))))))
      (is (= 91.0 (core/sum-of-squares (darr (range 0 7))))))))

(deftest variance-test
  (testing "variance"
    (testing "returns zero for constant data"
      (is (= 0.0 (core/variance (darr (take 20 (repeatedly (constantly 1))))))))
    (testing "with df=0 returns population variance"
      (is (= 4.0 (core/variance (darr (range 0 7)) 0)))
      (is (= 850.0 (core/variance (darr (range 0 101)) 0))))
    (testing "with df=1 returns sample variance"
      (is (= 858.5 (core/variance (darr (range 0 101)) 1))))))

(deftest median-test
  (testing "median"
    (testing "returns median for odd count"
      (is (= [5.0 nil nil]
             (core/median (darr [1 2 5 7 8])))))
    (testing "returns median for even count"
      (is (= [3.5 nil nil]
             (core/median (darr [1 2 2 5 7 8])))))))

(deftest quartiles-test
  (testing "quartiles"
    (testing "returns [q1 median q3] for sorted data"
      (is (= [2.0 5.0 7.0]
             (core/quartiles (darr [1 2 5 7 8]))))
      (is (= [2.0 3.5 7.0]
             (core/quartiles (darr [1 2 2 5 7 8])))))))

(deftest quantile-test
  (testing "quantile"
    (testing "returns exact data points at quantile boundaries"
      (is (== 2 (core/quantile 0.25 (darr [1 2 5 7 8]))))
      (is (== 5 (core/quantile 0.5 (darr [1 2 5 7 8]))))
      (is (== 7 (core/quantile 0.75 (darr [1 2 5 7 8])))))
    (testing "interpolates between data points"
      (is (= 2.0 (core/quantile 0.25 (darr [1 2 2 5 7 8]))))
      (is (= 3.5 (core/quantile 0.5 (darr [1 2 2 5 7 8]))))
      (is (= 6.5 (core/quantile 0.75 (darr [1 2 2 5 7 8])))))
    (testing "handles edge quantiles"
      (is (== 5 (core/quantile 0.05 (darr (range 0 101)))))
      (is (== 95 (core/quantile 0.95 (darr (range 0 101))))))))

(deftest skewness-test
  ;; Tests the skewness function returns 0.0 for constant data (zero variance).
  ;; This prevents NaN values that would break bootstrap sorting.
  (testing "skewness"
    (testing "returns 0.0 for constant data"
      (is (= 0.0 (core/skewness (darr (repeat 50 1.0)))))
      (is (= 0.0 (core/skewness (darr (repeat 50 1.0)) 1)))
      (is (= 0.0 (core/skewness (darr (repeat 50 1.0)) 2)))
      (is (= 0.0 (core/skewness (darr (repeat 50 1.0)) 3))))
    (testing "returns finite value for non-constant data"
      (is (Double/isFinite (core/skewness (darr [1 2 3 4 5 6 7 8 9 10])))))))

(deftest kurtosis-test
  ;; Tests the kurtosis function returns 0.0 for constant data (zero variance).
  ;; This prevents NaN values that would break bootstrap sorting.
  (testing "kurtosis"
    (testing "returns 0.0 for constant data"
      (is (= 0.0 (core/kurtosis (darr (repeat 50 1.0)))))
      (is (= 0.0 (core/kurtosis (darr (repeat 50 1.0)) 1)))
      (is (= 0.0 (core/kurtosis (darr (repeat 50 1.0)) 2)))
      (is (= 0.0 (core/kurtosis (darr (repeat 50 1.0)) 3))))
    (testing "returns finite value for non-constant data"
      (is (Double/isFinite (core/kurtosis (darr [1 2 3 4 5 6 7 8 9 10])))))))

(deftest cv-test
  (testing "cv"
    (testing "returns coefficient of variation (std dev / mean)"
      ;; For data [2 4 6 8]: mean=5, var=20/3, sd=sqrt(20/3)≈2.582
      ;; CV = 2.582/5 ≈ 0.5164
      (is (< (Math/abs (- (core/cv (darr [2 4 6 8]))
                          (/ (Math/sqrt (/ 20.0 3)) 5.0)))
             1e-10)))
    (testing "returns NaN for single element"
      (is (Double/isNaN (core/cv (darr [5.0])))))
    (testing "returns NaN for empty collection"
      (is (Double/isNaN (core/cv (darr [])))))
    (testing "returns NaN when mean is zero"
      (is (Double/isNaN (core/cv (darr [-1.0 1.0])))))))
