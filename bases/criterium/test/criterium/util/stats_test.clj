(ns criterium.util.stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]))

(deftest mean-test
  (is (= 1.0 (stats/mean (repeat 20 1))))
  (is (= 3.0 (stats/mean (range 0 7))))
  (is (= 50.0 (stats/mean (range 0 101)))))

(deftest sum-test
  (is (= 20 (stats/sum (take 20 (repeatedly (constantly 1))))))
  (is (= 21 (stats/sum (range 0 7)))))

(deftest sum-of-squares-test
  (is (= 20.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 1))))))
  (is (= 80.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 2))))))
  (is (= 91.0 (stats/sum-of-squares (range 0 7)))))

(deftest variance-test
  (is (= 0.0 (stats/variance (take 20 (repeatedly (constantly 1))))))
  (is (= 4.0 (stats/variance (range 0 7) 0)))
  (is (= 850.0 (stats/variance (range 0 101) 0))) ; R: mean((y-mean(y))^2)
  (is (= 858.5 (stats/variance (range 0 101) 1)))) ; R: var(y)

(deftest median-test
  ;; R: median(vs)
  (is (= [5 [1 2] [7 8]]
         (stats/median [1 2 5 7 8])))
  (is (= [3.5 [1 2 2] [5 7 8]]
         (stats/median [1 2 2 5 7 8]))))

(deftest quartiles-test
  ;; R: quantile(vs, prob=c(.25,0.5,.75))
  (is (= [1.5 5 7.5]
         (stats/quartiles [1 2 5 7 8])))
  (is (= [2 3.5 7]
         (stats/quartiles [1 2 2 5 7 8]))))

(deftest quantile-test
  (testing "exact data points"
    ;; R: quantile(c(1,2,5,7,8), prob=c(.25,0.5,.75))
    (is (= 2 (stats/quantile 0.25 [1 2 5 7 8])))
    (is (= 5 (stats/quantile 0.5 [1 2 5 7 8])))
    (is (= 7 (stats/quantile 0.75 [1 2 5 7 8]))))
  (testing "interpolated data points"
    ;; R: quantile(c(1,2,2,5,7,8), prob=c(.25,0.5,.75))
    (is (= 2.0 (stats/quantile 0.25 [1 2 2 5 7 8])))
    (is (= 3.5 (stats/quantile 0.5 [1 2 2 5 7 8])))
    (is (= 6.5 (stats/quantile 0.75 [1 2 2 5 7 8])))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec sample-uniform-test
  (testing "sample-uniform"
    (testing "returns values in [0..n)]"
      (prop/for-all
       [t gen/nat]
       (every?
        #(<= 0 % t)
        (stats/sample-uniform 100 t (well/well-rng-1024a)))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec sample-uniform-count-test
  (testing "sample-uniform"
    (testing "returns the correct number of samples"
      (prop/for-all
       [t gen/nat]
       (= t
          (count (stats/sample-uniform t 1 (well/well-rng-1024a))))))))

(deftest boxplot-outlier-thresholds-test
  (is (= [-4.0 -1.0 7.0 10.0] (stats/boxplot-outlier-thresholds 2.0 4.0))))

(deftest quantiles-test
  (let [max-error 1.5e-7]
    (test-max-error 1.0 (stats/quantile 0.5 [0 1 2]) max-error)
    (test-max-error 1.5 (stats/quantile 0.5 [0 1 2 3]) max-error)
    (test-max-error 1.0 (stats/quantile 0.25 [0 1 1.5 2 3]) max-error)
    (test-max-error 2.0 (stats/quantile 0.75 [0 1 1.5 2 3]) max-error))
  (is (= 5 (stats/quantile 0.05 (range 0 101))))
  (is (= 95 (stats/quantile 0.95 (range 0 101)))))
