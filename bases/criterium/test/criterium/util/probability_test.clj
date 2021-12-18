(ns criterium.util.probability-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.probability :as probability]))

;; Values from R, qnorm (with options(digits=15))
(deftest normal-quantile-test
  (is (pos? (probability/normal-quantile 0.5001)))
  (is (neg? (probability/normal-quantile 0.4999)))
  (is (< 2e-8 (- (probability/normal-quantile 0.999)
                 (probability/normal-quantile 0.001))))
  (let [max-error 1.0e-7]
    (is (= 0.0 (probability/normal-quantile 0.5)))
    (is (test-max-error
         1.2815515655446
         (probability/normal-quantile 0.9)
         max-error))
    (is (test-max-error
         0.674489750196082
         (probability/normal-quantile 0.75)
         max-error))
    (is (test-max-error
         -1.03643338949379
         (probability/normal-quantile 0.15)
         max-error))
    (is (test-max-error
         -2.32634787404084
         (probability/normal-quantile 0.01)
         max-error))))

;; Values from R, erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
(deftest erf-test
  (let [max-error 1.5e-7]
    (test-max-error 0.999999984582742 (probability/erf 4.0) max-error)
    (test-max-error 0.995322265018953 (probability/erf 2.0) max-error)
    (test-max-error 0.842700792949715 (probability/erf 1.0) max-error)
    (test-max-error 0.112462916018285 (probability/erf 0.1) max-error)
    (test-max-error 0.0112834155558497 (probability/erf 0.01) max-error)))

;; Values from R, pnorm
(deftest normal-cdf-test
  (let [max-error 1.5e-7]
    (test-max-error 0.99865010196837 (probability/normal-cdf 3.0) max-error)
    (test-max-error 0.977249868051821 (probability/normal-cdf 2.0) max-error)
    (test-max-error 0.841344746068543 (probability/normal-cdf 1.0) max-error)
    (test-max-error 0.691462461274013 (probability/normal-cdf 0.5) max-error)
    (test-max-error 0.5 (probability/normal-cdf 0.0) max-error)
    (test-max-error 0.158655253931457 (probability/normal-cdf -1.0) max-error)
    (test-max-error
     0.00134989803163009
     (probability/normal-cdf -3.0)
     max-error)))