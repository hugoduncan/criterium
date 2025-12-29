(ns criterium.validation.stats-validation-test
  "Validation tests for criterium.util.stats against R reference implementations.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test.assert :refer [approx=]]
   [criterium.util.stats :as stats]
   [criterium.validation.r :as r :refer [vec->r-str]]))

(defn near-zero=
  "Check if both values are essentially zero (within abs-tol of 0).
  Useful for comparing residual variance in perfect-fit regressions where
  floating-point artifacts may produce tiny non-zero values."
  [^double a ^double b ^double abs-tol]
  (and (< (Math/abs a) abs-tol)
       (< (Math/abs b) abs-tol)))

;;; Test data sets
;; Fixed datasets for reproducible validation

(def simple-integers [1 2 3 4 5 6 7 8 9 10])

(def simple-doubles [1.5 2.3 3.7 4.1 5.9 6.2 7.8 8.4 9.1 10.0])

(def mixed-signs [-5.0 -2.3 0.0 1.5 3.7 8.9])

(def single-value [42.0])

(def two-values [1.0 3.0])

(def large-range [0.001 1000000.0 500000.0 250000.0 750000.0])

;;; Tests

(deftest r-connection-test
  ;; Verifies the R connection helper handles both available and unavailable cases.
  (testing "r-connection"
    (testing "reports availability status"
      (let [available? (r/r-available?)]
        (is (boolean? available?)
            "r-available? should return a boolean")
        (when-not available?
          (println "R/Rserve not available:" (r/unavailable-reason-msg)))))))

(deftest mean-validation-test
  ;; Validates criterium.util.stats/mean against R's mean() function.
  ;; The mean function should produce identical results to R for all test cases.
  (testing "mean"
    (if-not (r/r-available?)
      (do
        (println "Skipping mean validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with simple integers"
          (let [r-mean (first (r/r-eval (str "mean(" (vec->r-str simple-integers) ")")))
                clj-mean (stats/mean simple-integers)]
            (is (approx= r-mean clj-mean 1e-10)
                (format "mean mismatch: R=%.15f, clj=%.15f" r-mean clj-mean))))

        (testing "with simple doubles"
          (let [r-mean (first (r/r-eval (str "mean(" (vec->r-str simple-doubles) ")")))
                clj-mean (stats/mean simple-doubles)]
            (is (approx= r-mean clj-mean 1e-10)
                (format "mean mismatch: R=%.15f, clj=%.15f" r-mean clj-mean))))

        (testing "with mixed positive and negative values"
          (let [r-mean (first (r/r-eval (str "mean(" (vec->r-str mixed-signs) ")")))
                clj-mean (stats/mean mixed-signs)]
            (is (approx= r-mean clj-mean 1e-10)
                (format "mean mismatch: R=%.15f, clj=%.15f" r-mean clj-mean))))

        (testing "with a single value"
          (let [r-mean (first (r/r-eval (str "mean(" (vec->r-str single-value) ")")))
                clj-mean (stats/mean single-value)]
            (is (approx= r-mean clj-mean 1e-10)
                (format "mean mismatch: R=%.15f, clj=%.15f" r-mean clj-mean))))

        (testing "with two values"
          (let [r-mean (first (r/r-eval (str "mean(" (vec->r-str two-values) ")")))
                clj-mean (stats/mean two-values)]
            (is (approx= r-mean clj-mean 1e-10)
                (format "mean mismatch: R=%.15f, clj=%.15f" r-mean clj-mean))))

        (testing "with large range of values"
          (let [r-mean (first (r/r-eval (str "mean(" (vec->r-str large-range) ")")))
                clj-mean (stats/mean large-range)]
            (is (approx= r-mean clj-mean 1e-10)
                (format "mean mismatch: R=%.15f, clj=%.15f" r-mean clj-mean))))))))

(deftest variance-validation-test
  ;; Validates criterium.util.stats/variance against R's var() function.
  ;; R's var() computes sample variance (n-1 denominator) by default,
  ;; which matches criterium's (variance data) or (variance data 1).
  (testing "variance"
    (if-not (r/r-available?)
      (do
        (println "Skipping variance validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "sample variance (df=1)"
          (testing "with simple integers"
            (let [r-var (first (r/r-eval (str "var(" (vec->r-str simple-integers) ")")))
                  clj-var (stats/variance simple-integers)]
              (is (approx= r-var clj-var 1e-10)
                  (format "variance mismatch: R=%.15f, clj=%.15f" r-var clj-var))))

          (testing "with simple doubles"
            (let [r-var (first (r/r-eval (str "var(" (vec->r-str simple-doubles) ")")))
                  clj-var (stats/variance simple-doubles)]
              (is (approx= r-var clj-var 1e-10)
                  (format "variance mismatch: R=%.15f, clj=%.15f" r-var clj-var))))

          (testing "with mixed positive and negative values"
            (let [r-var (first (r/r-eval (str "var(" (vec->r-str mixed-signs) ")")))
                  clj-var (stats/variance mixed-signs)]
              (is (approx= r-var clj-var 1e-10)
                  (format "variance mismatch: R=%.15f, clj=%.15f" r-var clj-var))))

          (testing "with two values"
            (let [r-var (first (r/r-eval (str "var(" (vec->r-str two-values) ")")))
                  clj-var (stats/variance two-values)]
              (is (approx= r-var clj-var 1e-10)
                  (format "variance mismatch: R=%.15f, clj=%.15f" r-var clj-var))))

          (testing "with large range of values"
            (let [r-var (first (r/r-eval (str "var(" (vec->r-str large-range) ")")))
                  clj-var (stats/variance large-range)]
              (is (approx= r-var clj-var 1e-10)
                  (format "variance mismatch: R=%.15f, clj=%.15f" r-var clj-var)))))

        (testing "population variance (df=0)"
          ;; Population variance uses n as denominator instead of (n-1).
          ;; R doesn't have a built-in population variance, so we compute it as:
          ;; var(x) * (n-1) / n
          (testing "with simple integers"
            (let [n (count simple-integers)
                  r-var (first (r/r-eval
                                (str "var(" (vec->r-str simple-integers) ") * "
                                     (dec n) " / " n)))
                  clj-var (stats/variance simple-integers 0)]
              (is (approx= r-var clj-var 1e-10)
                  (format "population variance mismatch: R=%.15f, clj=%.15f"
                          r-var clj-var))))

          (testing "with simple doubles"
            (let [n (count simple-doubles)
                  r-var (first (r/r-eval
                                (str "var(" (vec->r-str simple-doubles) ") * "
                                     (dec n) " / " n)))
                  clj-var (stats/variance simple-doubles 0)]
              (is (approx= r-var clj-var 1e-10)
                  (format "population variance mismatch: R=%.15f, clj=%.15f"
                          r-var clj-var))))

          (testing "with mixed positive and negative values"
            (let [n (count mixed-signs)
                  r-var (first (r/r-eval
                                (str "var(" (vec->r-str mixed-signs) ") * "
                                     (dec n) " / " n)))
                  clj-var (stats/variance mixed-signs 0)]
              (is (approx= r-var clj-var 1e-10)
                  (format "population variance mismatch: R=%.15f, clj=%.15f"
                          r-var clj-var)))))))))

(deftest median-validation-test
  ;; Validates criterium.util.stats/median against R's median() function.
  ;; Note: criterium's median expects sorted data and returns [median lower upper].
  (testing "median"
    (if-not (r/r-available?)
      (do
        (println "Skipping median validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with simple integers"
          (let [sorted (vec (sort simple-integers))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with simple doubles"
          (let [sorted (vec (sort simple-doubles))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with mixed positive and negative values"
          (let [sorted (vec (sort mixed-signs))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with a single value"
          (let [sorted (vec (sort single-value))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with two values"
          (let [sorted (vec (sort two-values))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with large range of values"
          (let [sorted (vec (sort large-range))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with odd number of elements"
          (let [sorted [1 2 3 4 5]
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with even number of elements"
          (let [sorted [1 2 3 4 5 6]
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (first (stats/median sorted))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))))))

(deftest quantile-validation-test
  ;; Validates criterium.util.stats/quantile against R's quantile() function.
  ;; R's default type=7 uses linear interpolation matching criterium's implementation.
  ;; Note: criterium's quantile expects sorted data.
  (testing "quantile"
    (if-not (r/r-available?)
      (do
        (println "Skipping quantile validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with simple integers"
          (let [sorted (vec (sort simple-integers))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with simple doubles"
          (let [sorted (vec (sort simple-doubles))]
            (doseq [q [0.0 0.1 0.25 0.5 0.75 0.9 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with mixed positive and negative values"
          (let [sorted (vec (sort mixed-signs))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with single value"
          (let [sorted (vec (sort single-value))]
            (doseq [q [0.0 0.5 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with two values"
          (let [sorted (vec (sort two-values))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with large range of values"
          (let [sorted (vec (sort large-range))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with arbitrary quantiles"
          (let [sorted (vec (sort simple-doubles))]
            (doseq [q [0.05 0.33 0.67 0.95]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (stats/quantile q sorted)]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))))))

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
  ;; Validates criterium.util.stats/linear-regression against R's lm() function.
  ;; R's lm() computes ordinary least squares regression. Criterium's linear-regression
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
                clj-result (stats/linear-regression linear-perfect-xs linear-perfect-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            (is (approx= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15f, clj=%.15f" r-intercept clj-intercept))
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
                clj-result (stats/linear-regression linear-offset-xs linear-offset-ys)
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
                clj-result (stats/linear-regression linear-noise-xs linear-noise-ys)
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
                clj-result (stats/linear-regression linear-neg-slope-xs linear-neg-slope-ys)
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
                clj-result (stats/linear-regression linear-small-xs linear-small-ys)
                [clj-intercept clj-slope] (:coeffs clj-result)]
            (is (approx= r-intercept clj-intercept 1e-10)
                (format "intercept mismatch: R=%.15f, clj=%.15f" r-intercept clj-intercept))
            (is (approx= r-slope clj-slope 1e-10)
                (format "slope mismatch: R=%.15f, clj=%.15f" r-slope clj-slope))
            (is (approx= r-var (:variance clj-result) 1e-10)
                (format "variance mismatch: R=%.15f, clj=%.15f" r-var (:variance clj-result)))
            (is (approx= r-rsq (:r-sqr clj-result) 1e-10)
                (format "r-squared mismatch: R=%.15f, clj=%.15f" r-rsq (:r-sqr clj-result)))))))))
