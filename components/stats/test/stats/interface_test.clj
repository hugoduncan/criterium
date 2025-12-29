(ns stats.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [random.interface :as random]
   [stats.interface :as stats]))

;; Tests for stats component functions.
;; Tests verify contract: each function computes expected statistical values.

(deftest mean-test
  (testing "mean"
    (testing "returns arithmetic mean of data"
      (is (= 1.0 (stats/mean (repeat 20 1))))
      (is (= 3.0 (stats/mean (range 0 7))))
      (is (= 50.0 (stats/mean (range 0 101)))))))

(deftest sum-test
  (testing "sum"
    (testing "returns sum of data points"
      (is (= 20 (stats/sum (take 20 (repeatedly (constantly 1))))))
      (is (= 21 (stats/sum (range 0 7)))))))

(deftest sum-of-squares-test
  (testing "sum-of-squares"
    (testing "returns sum of squared data points"
      (is (= 20.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 1))))))
      (is (= 80.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 2))))))
      (is (= 91.0 (stats/sum-of-squares (range 0 7)))))))

(deftest variance-test
  (testing "variance"
    (testing "returns zero for constant data"
      (is (= 0.0 (stats/variance (take 20 (repeatedly (constantly 1)))))))
    (testing "with df=0 returns population variance"
      (is (= 4.0 (stats/variance (range 0 7) 0)))
      (is (= 850.0 (stats/variance (range 0 101) 0))))
    (testing "with df=1 returns sample variance"
      (is (= 858.5 (stats/variance (range 0 101) 1))))))

(deftest median-test
  (testing "median"
    (testing "returns median and partitions for odd count"
      (is (= [5 [1 2] [7 8]]
             (stats/median [1 2 5 7 8]))))
    (testing "returns median and partitions for even count"
      (is (= [3.5 [1 2 2] [5 7 8]]
             (stats/median [1 2 2 5 7 8]))))))

(deftest quartiles-test
  (testing "quartiles"
    (testing "returns [q1 median q3] for sorted data"
      (is (= [1.5 5 7.5]
             (stats/quartiles [1 2 5 7 8])))
      (is (= [2 3.5 7]
             (stats/quartiles [1 2 2 5 7 8]))))))

(deftest quantile-test
  (testing "quantile"
    (testing "returns exact data points at quantile boundaries"
      (is (= 2 (stats/quantile 0.25 [1 2 5 7 8])))
      (is (= 5 (stats/quantile 0.5 [1 2 5 7 8])))
      (is (= 7 (stats/quantile 0.75 [1 2 5 7 8]))))
    (testing "interpolates between data points"
      (is (= 2.0 (stats/quantile 0.25 [1 2 2 5 7 8])))
      (is (= 3.5 (stats/quantile 0.5 [1 2 2 5 7 8])))
      (is (= 6.5 (stats/quantile 0.75 [1 2 2 5 7 8]))))
    (testing "handles edge quantiles"
      (is (= 5 (stats/quantile 0.05 (range 0 101))))
      (is (= 95 (stats/quantile 0.95 (range 0 101)))))))

(deftest boxplot-outlier-thresholds-test
  (testing "boxplot-outlier-thresholds"
    (testing "returns [severe-low mild-low mild-high severe-high]"
      (is (= [-4.0 -1.0 7.0 10.0] (stats/boxplot-outlier-thresholds 2.0 4.0))))))

(defspec sample-uniform-range-test 50
  (testing "sample-uniform"
    (testing "returns values in [0..n)]"
      (prop/for-all
       [t gen/nat]
       (every?
        #(<= 0 % t)
        (stats/sample-uniform 100 t (random/well-rng-1024a)))))))

(defspec sample-uniform-count-test 50
  (testing "sample-uniform"
    (testing "returns the correct number of samples"
      (prop/for-all
       [t gen/nat]
       (= t
          (count (stats/sample-uniform t 1 (random/well-rng-1024a))))))))

(deftest confidence-interval-test
  (testing "confidence-interval"
    (testing "returns 95% confidence interval around mean"
      (let [[low high] (stats/confidence-interval 100.0 25.0)]
        (is (< low 100.0))
        (is (> high 100.0))
        (is (< (- 100.0 low) 10.0))
        (is (< (- high 100.0) 10.0))))))
