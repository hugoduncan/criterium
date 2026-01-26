(ns criterium.stats.sampling-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.random.interface :as random]
   [criterium.stats.sampling :as sampling]))

;; Tests for sampling functions.
;; Tests verify contract: samples are generated within expected ranges and counts.

(defspec sample-uniform-range-test 50
  (testing "sample-uniform"
    (testing "returns values in [0..n)]"
      (prop/for-all
       [t gen/nat]
       (every?
        #(<= 0 % t)
        (sampling/sample-uniform 100 t (random/make-well-rng-1024a)))))))

(defspec sample-uniform-count-test 50
  (testing "sample-uniform"
    (testing "returns the correct number of samples"
      (prop/for-all
       [t gen/nat]
       (= t
          (count (sampling/sample-uniform t 1 (random/make-well-rng-1024a))))))))

(deftest confidence-interval-test
  (testing "confidence-interval"
    (testing "returns 95% confidence interval around mean"
      (let [[low high] (sampling/confidence-interval 100.0 25.0)
            low (double low)
            high (double high)]
        (is (< low 100.0))
        (is (> high 100.0))
        (is (< (- 100.0 low) 10.0))
        (is (< (- high 100.0) 10.0))))))
