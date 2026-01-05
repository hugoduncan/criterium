(ns stats.core-validation-test
  "Validation tests for stats.interface core functions against R reference.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test.assert :refer [approx=]]
   [r-validation.r :as r :refer [vec->r-str]]
   [stats.interface :as stats]))

;;; Test data sets
;; Fixed datasets for reproducible validation

(def simple-integers [1 2 3 4 5 6 7 8 9 10])

(def simple-doubles [1.5 2.3 3.7 4.1 5.9 6.2 7.8 8.4 9.1 10.0])

(def mixed-signs [-5.0 -2.3 0.0 1.5 3.7 8.9])

(def single-value [42.0])

(def two-values [1.0 3.0])

(def large-range [0.001 1000000.0 500000.0 250000.0 750000.0])

;;; Tests

(deftest mean-validation-test
  ;; Validates stats.interface/mean against R's mean() function.
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
  ;; Validates stats.interface/variance against R's var() function.
  ;; R's var() computes sample variance (n-1 denominator) by default,
  ;; which matches stats/variance with df=1.
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
  ;; Validates stats.interface/median against R's median() function.
  ;; Note: stats/median expects sorted data and returns [median lower upper].
  (testing "median"
    (if-not (r/r-available?)
      (do
        (println "Skipping median validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with simple integers"
          (let [sorted (vec (sort simple-integers))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with simple doubles"
          (let [sorted (vec (sort simple-doubles))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with mixed positive and negative values"
          (let [sorted (vec (sort mixed-signs))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with a single value"
          (let [sorted (vec (sort single-value))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with two values"
          (let [sorted (vec (sort two-values))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with large range of values"
          (let [sorted (vec (sort large-range))
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with odd number of elements"
          (let [sorted [1 2 3 4 5]
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))

        (testing "with even number of elements"
          (let [sorted [1 2 3 4 5 6]
                r-med (first (r/r-eval (str "median(" (vec->r-str sorted) ")")))
                clj-med (double (first (stats/median sorted)))]
            (is (approx= r-med clj-med 1e-10)
                (format "median mismatch: R=%.15f, clj=%.15f" r-med clj-med))))))))

(deftest quantile-validation-test
  ;; Validates stats.interface/quantile against R's quantile() function.
  ;; R's default type=7 uses linear interpolation matching our implementation.
  ;; Note: stats/quantile expects sorted data.
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
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with simple doubles"
          (let [sorted (vec (sort simple-doubles))]
            (doseq [q [0.0 0.1 0.25 0.5 0.75 0.9 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with mixed positive and negative values"
          (let [sorted (vec (sort mixed-signs))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with single value"
          (let [sorted (vec (sort single-value))]
            (doseq [q [0.0 0.5 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with two values"
          (let [sorted (vec (sort two-values))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with large range of values"
          (let [sorted (vec (sort large-range))]
            (doseq [q [0.0 0.25 0.5 0.75 1.0]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))

        (testing "with arbitrary quantiles"
          (let [sorted (vec (sort simple-doubles))]
            (doseq [q [0.05 0.33 0.67 0.95]]
              (testing (str "at quantile " q)
                (let [r-q (first (r/r-eval
                                  (str "quantile(" (vec->r-str sorted) ", " q ", type=7)")))
                      clj-q (double (stats/quantile q sorted))]
                  (is (approx= r-q clj-q 1e-10)
                      (format "quantile mismatch at q=%.2f: R=%.15f, clj=%.15f"
                              q r-q clj-q)))))))))))

;;; Skewed test dataset for skewness/kurtosis testing
(def right-skewed [1.0 1.5 2.0 2.5 3.0 3.5 4.0 5.0 7.0 15.0])

(deftest skewness-validation-test
  ;; Validates stats.interface/skewness against R's e1071::skewness() function.
  ;; Tests all three types defined in Joanes & Gill (1998).
  (testing "skewness"
    (if-not (r/r-available?)
      (do
        (println "Skipping skewness validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        ;; Load e1071 package for skewness function
        (r/r-eval "library(e1071)")

        (testing "type 1"
          (testing "with simple integers"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-integers)
                                      ", type=1)")))
                  clj-skew (stats/skewness simple-integers 1)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with simple doubles"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-doubles)
                                      ", type=1)")))
                  clj-skew (stats/skewness simple-doubles 1)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with right-skewed data"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str right-skewed)
                                      ", type=1)")))
                  clj-skew (stats/skewness right-skewed 1)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with mixed positive and negative values"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str mixed-signs)
                                      ", type=1)")))
                  clj-skew (stats/skewness mixed-signs 1)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew)))))

        (testing "type 2 (default, unbiased under normality)"
          (testing "with simple integers"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-integers)
                                      ", type=2)")))
                  clj-skew (stats/skewness simple-integers 2)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with simple doubles"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-doubles)
                                      ", type=2)")))
                  clj-skew (stats/skewness simple-doubles 2)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with right-skewed data"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str right-skewed)
                                      ", type=2)")))
                  clj-skew (stats/skewness right-skewed 2)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "uses type 2 by default"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-doubles)
                                      ", type=2)")))
                  clj-skew (stats/skewness simple-doubles)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "default skewness mismatch: R=%.15f, clj=%.15f"
                          r-skew clj-skew)))))

        (testing "type 3"
          (testing "with simple integers"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-integers)
                                      ", type=3)")))
                  clj-skew (stats/skewness simple-integers 3)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with simple doubles"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str simple-doubles)
                                      ", type=3)")))
                  clj-skew (stats/skewness simple-doubles 3)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew))))

          (testing "with right-skewed data"
            (let [r-skew (first (r/r-eval
                                 (str "e1071::skewness(" (vec->r-str right-skewed)
                                      ", type=3)")))
                  clj-skew (stats/skewness right-skewed 3)]
              (is (approx= r-skew clj-skew 1e-10)
                  (format "skewness mismatch: R=%.15f, clj=%.15f" r-skew clj-skew)))))))))

(deftest kurtosis-validation-test
  ;; Validates stats.interface/kurtosis against R's e1071::kurtosis() function.
  ;; Tests all three types defined in Joanes & Gill (1998).
  ;; Note: e1071::kurtosis returns excess kurtosis (normal = 0), which matches
  ;; our implementation.
  (testing "kurtosis"
    (if-not (r/r-available?)
      (do
        (println "Skipping kurtosis validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        ;; Load e1071 package for kurtosis function
        (r/r-eval "library(e1071)")

        (testing "type 1"
          (testing "with simple integers"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-integers)
                                      ", type=1)")))
                  clj-kurt (stats/kurtosis simple-integers 1)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with simple doubles"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-doubles)
                                      ", type=1)")))
                  clj-kurt (stats/kurtosis simple-doubles 1)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with right-skewed data"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str right-skewed)
                                      ", type=1)")))
                  clj-kurt (stats/kurtosis right-skewed 1)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with mixed positive and negative values"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str mixed-signs)
                                      ", type=1)")))
                  clj-kurt (stats/kurtosis mixed-signs 1)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt)))))

        (testing "type 2 (default, unbiased under normality)"
          (testing "with simple integers"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-integers)
                                      ", type=2)")))
                  clj-kurt (stats/kurtosis simple-integers 2)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with simple doubles"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-doubles)
                                      ", type=2)")))
                  clj-kurt (stats/kurtosis simple-doubles 2)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with right-skewed data"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str right-skewed)
                                      ", type=2)")))
                  clj-kurt (stats/kurtosis right-skewed 2)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "uses type 2 by default"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-doubles)
                                      ", type=2)")))
                  clj-kurt (stats/kurtosis simple-doubles)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "default kurtosis mismatch: R=%.15f, clj=%.15f"
                          r-kurt clj-kurt)))))

        (testing "type 3"
          (testing "with simple integers"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-integers)
                                      ", type=3)")))
                  clj-kurt (stats/kurtosis simple-integers 3)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with simple doubles"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str simple-doubles)
                                      ", type=3)")))
                  clj-kurt (stats/kurtosis simple-doubles 3)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt))))

          (testing "with right-skewed data"
            (let [r-kurt (first (r/r-eval
                                 (str "e1071::kurtosis(" (vec->r-str right-skewed)
                                      ", type=3)")))
                  clj-kurt (stats/kurtosis right-skewed 3)]
              (is (approx= r-kurt clj-kurt 1e-10)
                  (format "kurtosis mismatch: R=%.15f, clj=%.15f" r-kurt clj-kurt)))))))))

(deftest cv-validation-test
  ;; Validates stats.interface/cv against R's sd(x)/mean(x) formula.
  ;; Coefficient of variation is computed as standard deviation / mean.
  (testing "cv"
    (if-not (r/r-available?)
      (do
        (println "Skipping cv validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with simple integers"
          (let [r-cv (first (r/r-eval
                             (str "sd(" (vec->r-str simple-integers)
                                  ") / mean(" (vec->r-str simple-integers) ")")))
                clj-cv (stats/cv simple-integers)]
            (is (approx= r-cv clj-cv 1e-10)
                (format "cv mismatch: R=%.15f, clj=%.15f" r-cv clj-cv))))

        (testing "with simple doubles"
          (let [r-cv (first (r/r-eval
                             (str "sd(" (vec->r-str simple-doubles)
                                  ") / mean(" (vec->r-str simple-doubles) ")")))
                clj-cv (stats/cv simple-doubles)]
            (is (approx= r-cv clj-cv 1e-10)
                (format "cv mismatch: R=%.15f, clj=%.15f" r-cv clj-cv))))

        (testing "with right-skewed data"
          (let [r-cv (first (r/r-eval
                             (str "sd(" (vec->r-str right-skewed)
                                  ") / mean(" (vec->r-str right-skewed) ")")))
                clj-cv (stats/cv right-skewed)]
            (is (approx= r-cv clj-cv 1e-10)
                (format "cv mismatch: R=%.15f, clj=%.15f" r-cv clj-cv))))

        (testing "with large range of values"
          (let [r-cv (first (r/r-eval
                             (str "sd(" (vec->r-str large-range)
                                  ") / mean(" (vec->r-str large-range) ")")))
                clj-cv (stats/cv large-range)]
            (is (approx= r-cv clj-cv 1e-10)
                (format "cv mismatch: R=%.15f, clj=%.15f" r-cv clj-cv))))))))
