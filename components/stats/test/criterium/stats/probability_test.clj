(ns criterium.stats.probability-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.stats.probability :as probability]))

;; Tests for probability functions.
;; Tests verify contract: probability functions compute expected values
;; within documented error bounds.

(defn- abs-error
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

;; Values from R, qnorm (with options(digits=15))
(deftest normal-quantile-test
  (testing "normal-quantile"
    (testing "returns 0 at median"
      (is (= 0.0 (probability/normal-quantile 0.5))))
    (testing "returns positive for p > 0.5"
      (is (pos? (probability/normal-quantile 0.5001))))
    (testing "returns negative for p < 0.5"
      (is (neg? (probability/normal-quantile 0.4999))))
    (testing "is monotonic"
      (is (< 2e-8 (- (probability/normal-quantile 0.999)
                     (probability/normal-quantile 0.001)))))
    (let [max-error 1.0e-7]
      (testing "matches R qnorm at 0.9"
        (is (< (abs-error 1.2815515655446 (probability/normal-quantile 0.9))
               max-error)))
      (testing "matches R qnorm at 0.75"
        (is (< (abs-error 0.674489750196082 (probability/normal-quantile 0.75))
               max-error)))
      (testing "matches R qnorm at 0.15"
        (is (< (abs-error -1.03643338949379 (probability/normal-quantile 0.15))
               max-error)))
      (testing "matches R qnorm at 0.01"
        (is (< (abs-error -2.32634787404084 (probability/normal-quantile 0.01))
               max-error))))))

;; Values from R, erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
(deftest erf-test
  (testing "erf"
    (let [max-error 1.5e-7]
      (testing "matches R at x=4.0"
        (is (< (abs-error 0.999999984582742 (probability/erf 4.0)) max-error)))
      (testing "matches R at x=2.0"
        (is (< (abs-error 0.995322265018953 (probability/erf 2.0)) max-error)))
      (testing "matches R at x=1.0"
        (is (< (abs-error 0.842700792949715 (probability/erf 1.0)) max-error)))
      (testing "matches R at x=0.1"
        (is (< (abs-error 0.112462916018285 (probability/erf 0.1)) max-error)))
      (testing "matches R at x=0.01"
        (is (< (abs-error 0.0112834155558497 (probability/erf 0.01)) max-error))))))

;; Values from R, pnorm
(deftest normal-cdf-test
  (testing "normal-cdf"
    (let [max-error 1.5e-7]
      (testing "returns 0.5 at x=0"
        (is (< (abs-error 0.5 (probability/normal-cdf 0.0)) max-error)))
      (testing "matches R pnorm at x=3.0"
        (is (< (abs-error 0.99865010196837 (probability/normal-cdf 3.0)) max-error)))
      (testing "matches R pnorm at x=2.0"
        (is (< (abs-error 0.977249868051821 (probability/normal-cdf 2.0)) max-error)))
      (testing "matches R pnorm at x=1.0"
        (is (< (abs-error 0.841344746068543 (probability/normal-cdf 1.0)) max-error)))
      (testing "matches R pnorm at x=0.5"
        (is (< (abs-error 0.691462461274013 (probability/normal-cdf 0.5)) max-error)))
      (testing "matches R pnorm at x=-1.0"
        (is (< (abs-error 0.158655253931457 (probability/normal-cdf -1.0)) max-error)))
      (testing "matches R pnorm at x=-3.0"
        (is (< (abs-error 0.00134989803163009 (probability/normal-cdf -3.0))
               max-error))))))

(deftest normal-pdf-test
  (testing "normal-pdf"
    (testing "returns function for given mu and sigma"
      (let [pdf (probability/normal-pdf 0.0 1.0)]
        (is (fn? pdf))))
    (testing "standard normal pdf at mean equals 1/sqrt(2*pi)"
      (let [pdf (probability/normal-pdf 0.0 1.0)
            expected (/ 1.0 (Math/sqrt (* 2.0 Math/PI)))]
        (is (< (abs-error expected (pdf 0.0)) 1e-10))))
    (testing "pdf at mu equals 1/(sigma*sqrt(2*pi))"
      (let [mu 5.0
            sigma 2.0
            pdf (probability/normal-pdf mu sigma)
            expected (/ 1.0 (* sigma (Math/sqrt (* 2.0 Math/PI))))]
        (is (< (abs-error expected (pdf mu)) 1e-10))))))
