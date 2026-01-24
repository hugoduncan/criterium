(ns criterium.stats.chi-squared-validation-test
  "Validation tests for chi-squared CDF against R's pchisq() function.

  The chi-squared distribution is used for Ljung-Box p-values and other
  statistical tests. This validates our implementation matches R across
  a range of degrees of freedom and x values.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.r-validation.r :as r]
   [criterium.stats.chi-squared :as chi-squared]
   [criterium.test.assert :refer [approx=]]))

;;; Test configurations
;; Degrees of freedom commonly used in practice
(def test-dfs [1 2 5 10 15 20 30 50])

;; X values spanning the distribution - from very small to large
(def test-x-values [0.1 0.5 1.0 2.0 5.0 10.0 15.0 20.0 30.0 50.0])

;;; Tests

(deftest chi-squared-cdf-validation-test
  ;; Validates chi-squared/cdf against R's pchisq() function.
  ;; The chi-squared CDF is critical for p-value calculations in
  ;; statistical tests like Ljung-Box.
  (testing "chi-squared/cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping chi-squared CDF validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "matches R pchisq() for various df and x values"
          (doseq [df test-dfs
                  x test-x-values]
            (testing (str "df=" df ", x=" x)
              (let [r-val (first (r/r-eval (str "pchisq(" x ", " df ")")))
                    clj-val (chi-squared/cdf x df)]
                ;; Use 1e-9 relative tolerance - our implementation should be very accurate
                (is (approx= r-val clj-val 1e-9)
                    (format "pchisq(%.1f, %d): R=%.15f, clj=%.15f"
                            (double x) (long df) ^double r-val clj-val))))))

        (testing "at extreme x values for df=20"
          ;; These are relevant for Ljung-Box tests where Q can be large
          (doseq [x [0.01 0.001 100.0 200.0]]
            (testing (str "x=" x)
              (let [r-val (first (r/r-eval (str "pchisq(" x ", 20)")))
                    clj-val (chi-squared/cdf x 20)]
                (is (approx= r-val clj-val 1e-8)
                    (format "pchisq(%.3f, 20): R=%.15f, clj=%.15f"
                            (double x) ^double r-val clj-val))))))

        (testing "at specific Ljung-Box Q statistic values"
          ;; Real Q values from Ljung-Box tests with df=20
          (doseq [q [22.05943 35.0 45.0 60.0]]
            (testing (str "Q=" q)
              (let [r-p (first (r/r-eval (str "1 - pchisq(" q ", 20)")))
                    clj-p (- 1.0 (chi-squared/cdf q 20))]
                (is (approx= r-p clj-p 1e-9)
                    (format "1-pchisq(%.5f, 20): R=%.15f, clj=%.15f"
                            q ^double r-p clj-p))))))))))

(deftest chi-squared-cdf-edge-cases-validation-test
  ;; Validates edge case handling matches R behavior.
  (testing "chi-squared/cdf edge cases"
    (if-not (r/r-available?)
      (do
        (println "Skipping chi-squared edge cases validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at x=0"
          (doseq [df [1 5 10 20]]
            (let [r-val (first (r/r-eval (str "pchisq(0, " df ")")))
                  clj-val (chi-squared/cdf 0.0 (long df))]
              (is (= r-val clj-val 0.0)
                  (format "pchisq(0, %d) should be 0" (long df))))))

        (testing "approaching 1.0 for large x"
          (let [r-val (first (r/r-eval "pchisq(100, 10)"))
                clj-val (chi-squared/cdf 100.0 10)]
            (is (approx= r-val clj-val 1e-12)
                (format "pchisq(100, 10): R=%.15f, clj=%.15f"
                        (double r-val) clj-val))
            (is (> clj-val 0.9999)
                "CDF should be very close to 1.0 for large x")))))))

(deftest chi-squared-cdf-boundary-test
  ;; Validates the transition between series and continued fraction methods.
  ;; The regularized-gamma-p function uses series for x < a+1, CF for x >= a+1.
  ;; For chi-squared with df=20, a=10, boundary is at x=22 (since x/2 < a+1 means x < 22).
  (testing "chi-squared/cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping chi-squared boundary validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "at series/CF boundary for df=20"
        ;; Boundary is where x/2 = a+1 = df/2+1, so x = df+2 = 22
        (doseq [x [21.0 21.5 21.9 21.99 22.0 22.01 22.1 22.5 23.0]]
          (testing (str "x=" x)
            (let [r-val (first (r/r-eval (str "pchisq(" x ", 20)")))
                  clj-val (chi-squared/cdf x 20)]
              (is (approx= r-val clj-val 1e-9)
                  (format "pchisq(%.2f, 20): R=%.15f, clj=%.15f"
                          x (double r-val) clj-val)))))))))
