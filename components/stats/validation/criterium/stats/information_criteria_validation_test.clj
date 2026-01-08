(ns criterium.stats.information-criteria-validation-test
  "Validation tests for AIC, BIC, and AICc against R reference.

  Tests skip gracefully when R/Rserve is unavailable.

  In R, AIC and BIC are typically computed from fitted model objects.
  We validate by computing log-likelihood from a known model and
  comparing the information criteria values.

  R reference formulas:
    AIC = -2*logLik + 2*k
    BIC = -2*logLik + log(n)*k
    AICc = AIC + (2*k*(k+1))/(n-k-1)"
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.r-validation.r :as r]
   [criterium.stats.interface :as stats]
   [criterium.test.assert :refer [approx=]]))

;;; Test cases for information criteria
;; Each case: [k n log-likelihood description]
(def ic-test-cases
  [[2 100 -150.0 "simple normal model"]
   [3 50 -75.5 "regression with 3 params"]
   [1 200 -300.0 "exponential model"]
   [5 30 -45.0 "complex model, small sample"]
   [10 1000 -2500.0 "many params, large sample"]
   [2 10 -15.0 "minimal sample for AICc"]])

(deftest aic-validation-test
  ;; Validates stats.interface/aic against R's AIC formula.
  ;; R computes: AIC = -2*logLik + 2*k = 2*k - 2*logLik
  (testing "aic"
    (if-not (r/r-available?)
      (do
        (println "Skipping AIC validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[k _n log-lik desc] ic-test-cases]
        (testing (str "for " desc " (k=" k ", logLik=" log-lik ")")
          ;; R's AIC formula: -2*logLik + 2*k
          (let [r-aic (first (r/r-eval (format "2*%d - 2*%f" k log-lik)))
                clj-aic (stats/aic k log-lik)]
            (is (approx= r-aic clj-aic 1e-10)
                (format "AIC mismatch: R=%.15f, clj=%.15f"
                        r-aic clj-aic))))))))

(deftest bic-validation-test
  ;; Validates stats.interface/bic against R's BIC formula.
  ;; R computes: BIC = -2*logLik + log(n)*k = k*log(n) - 2*logLik
  (testing "bic"
    (if-not (r/r-available?)
      (do
        (println "Skipping BIC validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[k n log-lik desc] ic-test-cases]
        (testing (str "for " desc " (k=" k ", n=" n ", logLik=" log-lik ")")
          ;; R's BIC formula: -2*logLik + log(n)*k
          (let [r-bic (first (r/r-eval (format "%d*log(%d) - 2*%f" k n log-lik)))
                clj-bic (stats/bic k n log-lik)]
            (is (approx= r-bic clj-bic 1e-10)
                (format "BIC mismatch: R=%.15f, clj=%.15f"
                        r-bic clj-bic))))))))

(deftest aicc-validation-test
  ;; Validates stats.interface/aicc against R's AICc formula.
  ;; R computes: AICc = AIC + (2*k*(k+1))/(n-k-1)
  ;; Equivalent to: 2*k - 2*logLik + (2*k^2 + 2*k)/(n-k-1)
  (testing "aicc"
    (if-not (r/r-available?)
      (do
        (println "Skipping AICc validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at various parameter combinations"
          (doseq [[k n log-lik desc] ic-test-cases
                  :when (> n (+ k 1))] ; AICc requires n > k + 1
            (testing (str "for " desc " (k=" k ", n=" n ", logLik=" log-lik ")")
              ;; R's AICc formula: AIC + (2*k*(k+1))/(n-k-1)
              (let [r-aicc (first (r/r-eval
                                   (format "(2*%d - 2*%f) + (2*%d*(%d+1))/(%d-%d-1)"
                                           k log-lik k k n k)))
                    clj-aicc (stats/aicc k n log-lik)]
                (is (approx= r-aicc clj-aicc 1e-10)
                    (format "AICc mismatch: R=%.15f, clj=%.15f"
                            r-aicc clj-aicc))))))

        (testing "throws for n <= k + 1"
          (is (thrown? IllegalArgumentException
                       (stats/aicc 5 5 -10.0))
              "should throw when n = k")
          (is (thrown? IllegalArgumentException
                       (stats/aicc 5 6 -10.0))
              "should throw when n = k + 1"))))))

(deftest aic-bic-relationship-test
  ;; Verifies expected relationships between AIC and BIC.
  ;; BIC = AIC + (log(n) - 2) * k
  ;; So BIC > AIC when n >= 8 (since log(8) ≈ 2.08 > 2)
  (testing "AIC and BIC"
    (testing "relationship"
      (testing "BIC penalizes more heavily for n >= 8"
        (let [k 3
              log-lik -100.0]
          (doseq [n [8 10 50 100 1000]]
            (testing (str "at n=" n)
              (let [aic (stats/aic k log-lik)
                    bic (stats/bic k n log-lik)]
                (is (> bic aic)
                    (format "BIC=%.4f should exceed AIC=%.4f for n=%d"
                            bic aic n))
                ;; Verify the relationship: BIC = AIC + (log(n) - 2) * k
                (is (approx= bic (+ aic (* (- (Math/log n) 2.0) k)) 1e-10)
                    "BIC = AIC + (log(n) - 2) * k"))))))

      (testing "AIC > BIC for small n (n < 8)"
        (let [k 3
              log-lik -100.0
              n 5
              aic (stats/aic k log-lik)
              bic (stats/bic k n log-lik)]
          (is (< bic aic)
              (format "BIC=%.4f should be less than AIC=%.4f for n=%d"
                      bic aic n)))))))

(deftest aicc-converges-to-aic-test
  ;; Verifies that AICc → AIC as n → ∞.
  ;; The correction term (2k² + 2k)/(n - k - 1) → 0 as n → ∞.
  (testing "AICc"
    (testing "converges to AIC for large n"
      (let [k 5
            log-lik -500.0
            aic (stats/aic k log-lik)]
        (doseq [n [100 1000 10000 100000]]
          (testing (str "at n=" n)
            (let [aicc (stats/aicc k n log-lik)
                  diff (Math/abs (- aicc aic))
                  ;; Expected correction term: (2k² + 2k)/(n - k - 1)
                  expected-correction (/ (+ (* 2.0 k k) (* 2.0 k))
                                         (- n k 1.0))]
              (is (approx= diff expected-correction 1e-10)
                  (format "AICc - AIC = %.10f should equal correction %.10f"
                          diff expected-correction))
              ;; For n=100000, difference should be very small
              (when (>= n 100000)
                (is (< diff 0.001)
                    (format "AICc should be nearly equal to AIC for n=%d" n))))))))))

(deftest information-criteria-with-actual-model-test
  ;; Tests information criteria using a fitted normal distribution.
  ;; This validates the complete workflow: compute log-likelihood from data,
  ;; then compute IC values.
  ;; Requires MASS package (installed by CI workflow).
  (testing "information criteria"
    (if-not (r/r-available?)
      (do
        (println "Skipping model-based IC validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "for fitted normal distribution"
        ;; Generate data in R, fit model, compute IC
        (r/r-eval "library(MASS)")
        (r/r-eval "
          set.seed(42)
          x <- rnorm(100, mean=5, sd=2)
          fit <- fitdistr(x, 'normal')
          ")
        (let [r-loglik (first (r/r-eval "logLik(fit)"))
              r-k 2  ; normal has 2 parameters (mean, sd)
              r-n 100
              ;; Compute IC in R for comparison
              r-aic (first (r/r-eval "AIC(fit)"))
              r-bic (first (r/r-eval "BIC(fit)"))
              ;; Compute using our functions
              clj-aic (stats/aic r-k r-loglik)
              clj-bic (stats/bic r-k r-n r-loglik)]
          (testing "AIC matches R's AIC()"
            (is (approx= r-aic clj-aic 1e-8)
                (format "AIC mismatch: R=%.10f, clj=%.10f" r-aic clj-aic)))
          (testing "BIC matches R's BIC()"
            (is (approx= r-bic clj-bic 1e-8)
                (format "BIC mismatch: R=%.10f, clj=%.10f" r-bic clj-bic))))))))