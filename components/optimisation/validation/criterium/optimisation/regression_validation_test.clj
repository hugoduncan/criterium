(ns criterium.optimisation.regression-validation-test
  "Validation tests for criterium.optimisation.interface/linear-regression against R's lm().

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.optimisation.interface :as optimisation]
   [criterium.test.assert :refer [approx=]]
   [r-validation.r :as r :refer [vec->r-str]]))

(defn near-zero=
  "Check if both values are essentially zero (within abs-tol of 0).
  Useful for comparing residual variance in perfect-fit regressions where
  floating-point artifacts may produce tiny non-zero values."
  [^double a ^double b ^double abs-tol]
  (and (< (Math/abs a) abs-tol)
       (< (Math/abs b) abs-tol)))

;;; Paired data for linear regression tests

(def linear-perfect-xs [1.0 2.0 3.0 4.0 5.0])
(def linear-perfect-ys [2.0 4.0 6.0 8.0 10.0])  ; y = 2x (perfect fit)

(def linear-offset-xs [1.0 2.0 3.0 4.0 5.0])
(def linear-offset-ys [3.0 5.0 7.0 9.0 11.0])  ; y = 2x + 1 (perfect fit with intercept)

(def linear-noise-xs [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0])
(def linear-noise-ys [2.1 3.9 6.2 7.8 10.1 12.0 13.9 16.1 18.0 20.1])  ; y ≈ 2x (with noise)

(def linear-neg-slope-xs [1.0 2.0 3.0 4.0 5.0])
(def linear-neg-slope-ys [10.0 8.0 6.0 4.0 2.0])  ; y = 12 - 2x (negative slope)

(def linear-small-xs [0.001 0.002 0.003 0.004 0.005])
(def linear-small-ys [0.0021 0.0039 0.0061 0.0078 0.0102])  ; small values with noise

(deftest linear-regression-validation-test
  ;; Validates criterium.optimisation.interface/linear-regression against R's lm() function.
  ;; R's lm() computes ordinary least squares regression. linear-regression
  ;; returns {:coeffs [intercept slope] :variance residual-variance :r-sqr r-squared}.
  (testing "linear-regression"
    (if-not (r/r-available?)
      (do
        (println "Skipping linear-regression validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with perfect fit (y = 2x)"
          (let [r-result (r/r-eval
                          (str "m <- lm(" (vec->r-str linear-perfect-ys) " ~ "
                               (vec->r-str linear-perfect-xs) ");"
                               "c(coef(m)[1], coef(m)[2], sum(residuals(m)^2)/(length("
                               (vec->r-str linear-perfect-ys) ")-2), summary(m)$r.squared)"))
                [r-intercept r-slope r-var r-rsq] r-result
                clj-result (optimisation/linear-regression linear-perfect-xs linear-perfect-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            ;; For y = 2x, expected intercept is 0. Use near-zero= for floating-point artifacts.
            (is (near-zero= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15e, clj=%.15e" r-intercept clj-intercept))
            (is (approx= r-slope clj-slope 1e-10)
                (format "slope mismatch: R=%.15f, clj=%.15f" r-slope clj-slope))
            ;; For perfect fit, both variances should be essentially zero.
            ;; Use near-zero= since floating-point artifacts may produce tiny values.
            (is (near-zero= r-var (:variance clj-result) 1e-20)
                (format "variance mismatch: R=%.15e, clj=%.15e" r-var (:variance clj-result)))
            (is (approx= r-rsq (:r-sqr clj-result) 1e-10)
                (format "r-squared mismatch: R=%.15f, clj=%.15f" r-rsq (:r-sqr clj-result)))))

        (testing "with perfect fit and intercept (y = 2x + 1)"
          (let [r-result (r/r-eval
                          (str "m <- lm(" (vec->r-str linear-offset-ys) " ~ "
                               (vec->r-str linear-offset-xs) ");"
                               "c(coef(m)[1], coef(m)[2], sum(residuals(m)^2)/(length("
                               (vec->r-str linear-offset-ys) ")-2), summary(m)$r.squared)"))
                [r-intercept r-slope r-var r-rsq] r-result
                clj-result (optimisation/linear-regression linear-offset-xs linear-offset-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            (is (approx= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15f, clj=%.15f" r-intercept clj-intercept))
            (is (approx= r-slope clj-slope 1e-10)
                (format "slope mismatch: R=%.15f, clj=%.15f" r-slope clj-slope))
            ;; For perfect fit, both variances should be essentially zero.
            (is (near-zero= r-var (:variance clj-result) 1e-20)
                (format "variance mismatch: R=%.15e, clj=%.15e" r-var (:variance clj-result)))
            (is (approx= r-rsq (:r-sqr clj-result) 1e-10)
                (format "r-squared mismatch: R=%.15f, clj=%.15f" r-rsq (:r-sqr clj-result)))))

        (testing "with noisy data"
          (let [r-result (r/r-eval
                          (str "m <- lm(" (vec->r-str linear-noise-ys) " ~ "
                               (vec->r-str linear-noise-xs) ");"
                               "c(coef(m)[1], coef(m)[2], sum(residuals(m)^2)/(length("
                               (vec->r-str linear-noise-ys) ")-2), summary(m)$r.squared)"))
                [r-intercept r-slope r-var r-rsq] r-result
                clj-result (optimisation/linear-regression linear-noise-xs linear-noise-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            (is (approx= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15f, clj=%.15f" r-intercept clj-intercept))
            (is (approx= r-slope clj-slope 1e-10)
                (format "slope mismatch: R=%.15f, clj=%.15f" r-slope clj-slope))
            (is (approx= r-var (:variance clj-result) 1e-10)
                (format "variance mismatch: R=%.15f, clj=%.15f" r-var (:variance clj-result)))
            (is (approx= r-rsq (:r-sqr clj-result) 1e-10)
                (format "r-squared mismatch: R=%.15f, clj=%.15f" r-rsq (:r-sqr clj-result)))))

        (testing "with negative slope"
          ;; y = 10 - x (perfect fit with negative slope)
          (let [r-result (r/r-eval
                          (str "m <- lm(" (vec->r-str linear-neg-slope-ys) " ~ "
                               (vec->r-str linear-neg-slope-xs) ");"
                               "c(coef(m)[1], coef(m)[2], sum(residuals(m)^2)/(length("
                               (vec->r-str linear-neg-slope-ys) ")-2), summary(m)$r.squared)"))
                [r-intercept r-slope r-var r-rsq] r-result
                clj-result (optimisation/linear-regression linear-neg-slope-xs linear-neg-slope-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            (is (approx= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15f, clj=%.15f" r-intercept clj-intercept))
            (is (approx= r-slope clj-slope 1e-10)
                (format "slope mismatch: R=%.15f, clj=%.15f" r-slope clj-slope))
            ;; Perfect fit, both variances should be essentially zero.
            (is (near-zero= r-var (:variance clj-result) 1e-20)
                (format "variance mismatch: R=%.15e, clj=%.15e" r-var (:variance clj-result)))
            (is (approx= r-rsq (:r-sqr clj-result) 1e-10)
                (format "r-squared mismatch: R=%.15f, clj=%.15f" r-rsq (:r-sqr clj-result)))))

        (testing "with small values"
          (let [r-result (r/r-eval
                          (str "m <- lm(" (vec->r-str linear-small-ys) " ~ "
                               (vec->r-str linear-small-xs) ");"
                               "c(coef(m)[1], coef(m)[2], sum(residuals(m)^2)/(length("
                               (vec->r-str linear-small-ys) ")-2), summary(m)$r.squared)"))
                [r-intercept r-slope r-var r-rsq] r-result
                clj-result (optimisation/linear-regression linear-small-xs linear-small-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            (is (approx= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15f, clj=%.15f" r-intercept clj-intercept))
            (is (approx= r-slope clj-slope 1e-10)
                (format "slope mismatch: R=%.15f, clj=%.15f" r-slope clj-slope))
            (is (approx= r-var (:variance clj-result) 1e-10)
                (format "variance mismatch: R=%.15f, clj=%.15f" r-var (:variance clj-result)))
            (is (approx= r-rsq (:r-sqr clj-result) 1e-10)
                (format "r-squared mismatch: R=%.15f, clj=%.15f" r-rsq (:r-sqr clj-result)))))))))
