(ns criterium.util.probability-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.stats.probability :as probability]))

;; Values from R, qnorm (with options(digits=15))
(deftest normal-quantile-test
  (is (pos? (probability/normal-quantile 0.5001)))
  (is (neg? (probability/normal-quantile 0.4999)))
  (is (< 2e-8 (- (double (probability/normal-quantile 0.999))
                 (double (probability/normal-quantile 0.001)))))
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

;; Values from R lgamma() (with options(digits=16))
;; Tests the Lanczos approximation accuracy across input domains.
(deftest log-gamma-test
  (testing "log-gamma"
    (testing "throws for non-positive arguments"
      (is (thrown? IllegalArgumentException (probability/log-gamma 0.0)))
      (is (thrown? IllegalArgumentException (probability/log-gamma -1.0))))

    (testing "matches R lgamma() within tolerance"
      (let [;; 1e-12 tolerance - Lanczos with g=7 provides ~15 digits of precision
            max-error 1e-12
            ;; Reference data from R lgamma()
            ;; Format: [input expected-lgamma]
            test-cases [[0.01  4.599479878042022]    ; small x (reflection formula)
                        [0.1   2.252712651734206]    ; small x (reflection formula)
                        [0.25  1.288022524698077]    ; small x (reflection formula)
                        [0.5   0.5723649429247001]   ; x=0.5 (log(sqrt(pi)))
                        [0.75  0.2032809514312954]
                        [0.99  0.005854806764709779] ; near 1.0
                        [1.0   0.0]                  ; Γ(1) = 1
                        [1.5  -0.1207822376352452]
                        [2.0   0.0]                  ; Γ(2) = 1
                        [2.5   0.2846828704729192]
                        [3.0   0.6931471805599453]   ; log(2)
                        [3.5   1.200973602347074]
                        [4.0   1.791759469228055]    ; log(6)
                        [4.5   2.453736570842442]
                        [5.0   3.178053830347946]    ; log(24)
                        [10.0  12.80182748008147]
                        [50.0  144.5657439463449]
                        [100.0 359.1342053695754]
                        [500.0 2605.115850361734]
                        [1000.0 5905.220423209181]]] ; large x (Stirling region)
        (doseq [[x expected] test-cases]
          (test-max-error expected (probability/log-gamma x) max-error
                          (str "log-gamma(" x ")")))))

    (testing "verifies known identities"
      (let [max-error 1e-12]
        ;; Γ(n) = (n-1)! for positive integers
        (test-max-error (Math/log 1.0) (probability/log-gamma 2.0) max-error)   ; 1! = 1
        (test-max-error (Math/log 2.0) (probability/log-gamma 3.0) max-error)   ; 2! = 2
        (test-max-error (Math/log 6.0) (probability/log-gamma 4.0) max-error)   ; 3! = 6
        (test-max-error (Math/log 24.0) (probability/log-gamma 5.0) max-error)  ; 4! = 24
        (test-max-error (Math/log 120.0) (probability/log-gamma 6.0) max-error) ; 5! = 120
        ;; Γ(1/2) = sqrt(π)
        (test-max-error (* 0.5 (Math/log Math/PI))
                        (probability/log-gamma 0.5)
                        max-error)))))
