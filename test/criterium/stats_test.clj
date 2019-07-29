(ns criterium.stats-test
  (:use clojure.test
        criterium.stats)
  (:require criterium.well))

(defmacro test-max-error [expected actual max-error]
  `(is (< (Math/abs (- ~expected ~actual)) ~max-error)))

(deftest mean-test
  (is (= 1 (mean (take 20 (repeatedly (constantly 1))))))
  (is (= 3 (mean (range 0 7)))))

(deftest sum-test
  (is (= 20 (sum (take 20 (repeatedly (constantly 1))))))
  (is (= 21 (sum (range 0 7)))))

(deftest sum-of-squares-test
  (is (= 20.0 (sum-of-squares (take 20 (repeatedly (constantly 1))))))
  (is (= 80.0 (sum-of-squares (take 20 (repeatedly (constantly 2))))))
  (is (= 91.0 (sum-of-squares (range 0 7)))))

(deftest variance-test
  (is (= 0.0 (variance (take 20 (repeatedly (constantly 1))))))
  (is (= 4.0 (variance (range 0 7) 0))))

(deftest median-test
  (is (= [5 [1 2] [7 8]]
         (median [1 2 5 7 8])))
  (is (= [7/2 [1 2 2] [5 7 8]]
         (median [1 2 2 5 7 8]))))

(deftest quartiles-test
  (is (= [3/2 5 15/2]
         (quartiles [1 2 5 7 8])))
  (is (= [2 7/2 7]
         (quartiles [1 2 2 5 7 8]))))

(deftest boxplot-outlier-thresholds-test
  (is (= [-4.0 -1.0 7.0 10.0] (boxplot-outlier-thresholds 2.0 4.0))))

(deftest bootstrap-estimate-test
  (is (= [1 0.0 [1.0 1.0]]
         (bootstrap-estimate (take 20 (repeatedly (constantly 1))))))
  (let [[m s [l u]] (bootstrap-estimate (take 1000000 (repeatedly rand)))]
    (is (test-max-error 0.5 m 1e-2))
    (is (test-max-error 0.0 l 0.2))
    (is (test-max-error 1.0 u 0.2))
    (is (test-max-error 0.0833 s 0.2))))

(deftest bootstrap-estimate-scale-test
  (is (= [1e-9 [1e-8 1e-8]]
         (scale-bootstrap-estimate [1 1 [10 10]] 1e-9))))

;; Values from R, qnorm (with options(digits=15))
(deftest normal-quantile-test
  (is (pos? (normal-quantile 0.5001)))
  (is (neg? (normal-quantile 0.4999)))
  (is (< 2e-8 (- (normal-quantile 0.999) (normal-quantile 0.001))))
  (let [max-error 1.0e-7]
    (is (= 0.0 (normal-quantile 0.5)))
    (is (test-max-error 1.2815515655446 (normal-quantile 0.9) max-error))
    (is (test-max-error 0.674489750196082 (normal-quantile 0.75) max-error))
    (is (test-max-error -1.03643338949379 (normal-quantile 0.15) max-error))
    (is (test-max-error -2.32634787404084 (normal-quantile 0.01) max-error))))


;; Values from R, erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
(deftest erf-test
  (let [max-error 1.5e-7]
    (test-max-error 0.999999984582742 (erf 4) max-error)
    (test-max-error 0.995322265018953 (erf 2) max-error)
    (test-max-error 0.842700792949715 (erf 1) max-error)
    (test-max-error 0.112462916018285 (erf 0.1) max-error)
    (test-max-error 0.0112834155558497 (erf 0.01) max-error)))

;; Values from R, pnorm
(deftest normal-cdf-test
  (let [max-error 1.5e-7]
    (test-max-error 0.99865010196837 (normal-cdf 3.0) max-error)
    (test-max-error 0.977249868051821 (normal-cdf 2.0) max-error)
    (test-max-error 0.841344746068543 (normal-cdf 1.0) max-error)
    (test-max-error 0.691462461274013 (normal-cdf 0.5) max-error)
    (test-max-error 0.5 (normal-cdf 0.0) max-error)
    (test-max-error 0.158655253931457 (normal-cdf -1.0) max-error)
    (test-max-error 0.00134989803163009 (normal-cdf -3.0) max-error)))

(deftest quantiles-test
  (let [max-error 1.5e-7]
    (test-max-error 1.0 (quantile 0.5 [0 1 2]) max-error)
    (test-max-error 1.5 (quantile 0.5 [0 1 2 3]) max-error)
    (test-max-error 1.0 (quantile 0.25 [0 1 1.5 2 3]) max-error)
    (test-max-error 2.0 (quantile 0.75 [0 1 1.5 2 3]) max-error))
  (is (= 5.0 (quantile 0.05 (range 0 101))))
  (is (= 95.0 (quantile 0.95 (range 0 101)))))
