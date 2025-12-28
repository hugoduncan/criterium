(ns criterium.validation.stats-validation-test
  "Validation tests for criterium.util.stats against R reference implementations.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.util.stats :as stats]
   [criterium.validation.r :as r]))

;;; Test data sets
;; Fixed datasets for reproducible validation

(def simple-integers [1 2 3 4 5 6 7 8 9 10])

(def simple-doubles [1.5 2.3 3.7 4.1 5.9 6.2 7.8 8.4 9.1 10.0])

(def mixed-signs [-5.0 -2.3 0.0 1.5 3.7 8.9])

(def single-value [42.0])

(def two-values [1.0 3.0])

(def large-range [0.001 1000000.0 500000.0 250000.0 750000.0])

;;; Helper functions

(defn- approx=
  "Check if two numbers are approximately equal within relative tolerance."
  [^double expected ^double actual ^double tolerance]
  (let [diff (Math/abs (- expected actual))
        denom (Math/abs expected)]
    (if (zero? denom)
      (< diff tolerance)
      (< (/ diff denom) tolerance))))

(defn- vec->r-str
  "Convert Clojure vector to R c() syntax."
  [v]
  (str "c(" (clojure.string/join ", " v) ")"))

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
