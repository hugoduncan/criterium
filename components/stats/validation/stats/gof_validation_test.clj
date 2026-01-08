(ns stats.gof-validation-test
  "Validation tests for goodness-of-fit tests against R reference.

  Tests skip gracefully when R/Rserve is unavailable.

  GoF tests validated:
  - ks-test: Kolmogorov-Smirnov test, against R's ks.test()
  - cvm-test: Cramér-von Mises test, against R's goftest::cvm.test()

  Note: We validate the test statistics directly. P-values may differ
  slightly between implementations due to different approximations."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test.assert :refer [approx=]]
   [r-validation.r :as r]
   [stats.interface :as stats]))

;;; Test Data
;; Using fixed data for reproducibility

(def normal-test-data
  "Data that follows approximately normal distribution."
  [0.5 -0.3 1.2 -0.8 0.1 0.9 -0.5 0.7 -0.2 1.0
   0.3 -0.6 0.8 -0.1 0.4 1.1 -0.4 0.6 -0.7 0.2])

(def gamma-test-data
  "Data drawn from gamma(shape=3, scale=2) distribution."
  [5.2 4.1 7.8 3.2 6.5 8.1 4.5 5.9 2.8 7.2
   6.1 3.9 5.5 4.8 6.8 5.1 3.5 7.5 4.2 6.3])

(def uniform-test-data
  "Data drawn from uniform(0, 1) distribution."
  [0.12 0.45 0.78 0.23 0.89 0.34 0.56 0.67 0.01 0.99
   0.11 0.44 0.77 0.22 0.88 0.33 0.55 0.66 0.02 0.98])

;;; Kolmogorov-Smirnov Test Validation

(deftest ks-test-statistic-validation-test
  ;; Validates stats.interface/ks-test-statistic against R's ks.test().
  ;; R: ks.test(x, "pnorm", mean, sd)$statistic
  (testing "ks-test-statistic"
    (if-not (r/r-available?)
      (do
        (println "Skipping K-S statistic validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "against R's ks.test for normal distribution"
          (let [data-str (str "c(" (str/join "," normal-test-data) ")")
                mean-val (/ (reduce + normal-test-data) (count normal-test-data))
                var-val (/ (reduce + (map #(* (- % mean-val) (- % mean-val))
                                          normal-test-data))
                           (dec (count normal-test-data)))
                sd-val (Math/sqrt var-val)
                r-result (r/r-eval (str "ks.test(" data-str
                                        ", 'pnorm', mean=" mean-val
                                        ", sd=" sd-val ")$statistic"))
                r-d (if (map? r-result)
                      (first (vals r-result))
                      (first r-result))
                ;; Use normal CDF for the test
                normal-cdf-fn (fn [x]
                                (stats/normal-cdf (/ (- x mean-val) sd-val)))
                clj-d (stats/ks-test-statistic normal-test-data normal-cdf-fn)]
            (is (approx= r-d clj-d 1e-7)
                (format "D statistic mismatch: R=%.10f, clj=%.10f" r-d clj-d))))

        (testing "against R's ks.test for uniform distribution"
          (let [data-str (str "c(" (str/join "," uniform-test-data) ")")
                r-result (r/r-eval (str "ks.test(" data-str
                                        ", 'punif', min=0, max=1)$statistic"))
                r-d (if (map? r-result)
                      (first (vals r-result))
                      (first r-result))
                ;; Uniform(0,1) CDF: F(x) = x for x in [0,1]
                uniform-cdf-fn (fn [x]
                                 (cond
                                   (<= x 0.0) 0.0
                                   (>= x 1.0) 1.0
                                   :else x))
                clj-d (stats/ks-test-statistic uniform-test-data uniform-cdf-fn)]
            (is (approx= r-d clj-d 1e-10)
                (format "D statistic mismatch: R=%.10f, clj=%.10f" r-d clj-d))))

        (testing "against R's ks.test for gamma distribution"
          (let [data-str (str "c(" (str/join "," gamma-test-data) ")")
                ;; Fit gamma parameters via MLE
                mle-result (stats/gamma-mle gamma-test-data)
                shape (get-in mle-result [:params :shape])
                scale (get-in mle-result [:params :scale])
                rate (/ 1.0 scale)
                ;; R uses rate parameterization
                r-result (r/r-eval (str "ks.test(" data-str
                                        ", 'pgamma', shape=" shape
                                        ", rate=" rate ")$statistic"))
                r-d (if (map? r-result)
                      (first (vals r-result))
                      (first r-result))
                gamma-cdf-fn (stats/gamma-cdf shape scale)
                clj-d (stats/ks-test-statistic gamma-test-data gamma-cdf-fn)]
            (is (approx= r-d clj-d 1e-10)
                (format "D statistic mismatch: R=%.10f, clj=%.10f" r-d clj-d))))))))

(deftest ks-test-pvalue-validation-test
  ;; Validates that p-values are in reasonable agreement with R.
  ;; Note: P-value approximations may differ slightly.
  (testing "ks-test p-value"
    (if-not (r/r-available?)
      (do
        (println "Skipping K-S p-value validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "is reasonable for well-fitting data"
          ;; For data from the true distribution, p-value should not be tiny
          (let [data-str (str "c(" (str/join "," uniform-test-data) ")")
                r-result (r/r-eval (str "ks.test(" data-str ", 'punif')$p.value"))
                r-p (first r-result)
                uniform-cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
                clj-result (stats/ks-test uniform-test-data uniform-cdf-fn)
                clj-p (:p-value clj-result)]
            ;; Both should reject the null at high p (data is from uniform)
            (is (> r-p 0.01) "R p-value should not reject good fit")
            (is (> clj-p 0.01) "Clj p-value should not reject good fit")))

        (testing "detects poor fit"
          ;; Use gamma data but test against wrong distribution (exponential)
          (let [data-str (str "c(" (str/join "," gamma-test-data) ")")
                ;; Fit with wrong distribution: exponential (gamma with shape=1)
                mean-val (/ (reduce + gamma-test-data) (count gamma-test-data))
                rate (/ 1.0 mean-val)
                r-result (r/r-eval (str "ks.test(" data-str
                                        ", 'pexp', rate=" rate ")$p.value"))
                r-p (first r-result)
                ;; Use exponential CDF (gamma with shape=1)
                exp-cdf-fn (stats/gamma-cdf 1.0 mean-val)
                clj-result (stats/ks-test gamma-test-data exp-cdf-fn)
                clj-p (:p-value clj-result)]
            ;; Both should have low p-value (data is from gamma, not exponential)
            (is (< r-p 0.5) "R p-value should indicate poor fit")
            (is (< clj-p 0.5) "Clj p-value should indicate poor fit")))))))

;;; Cramér-von Mises Test Validation

(defn- goftest-available?
  "Check if R's goftest package is available."
  []
  (when (r/r-available?)
    (try
      (r/r-eval "library(goftest)")
      true
      (catch Exception _
        false))))

(deftest cvm-test-statistic-validation-test
  ;; Validates stats.interface/cvm-test-statistic against R's goftest::cvm.test().
  ;; R: goftest::cvm.test(x, "pnorm", mean, sd)$statistic
  (testing "cvm-test-statistic"
    (if-not (goftest-available?)
      (do
        (println "Skipping CvM statistic validation: R/goftest not available")
        (is true "Skipped - R/goftest unavailable"))
      (do
        (testing "against R's cvm.test for uniform distribution"
          (let [data-str (str "c(" (str/join "," uniform-test-data) ")")
                r-result (r/r-eval (str "cvm.test(" data-str
                                        ", 'punif', min=0, max=1)$statistic"))
                r-w2 (if (map? r-result)
                       (first (vals r-result))
                       (first r-result))
                uniform-cdf-fn (fn [x]
                                 (cond
                                   (<= x 0.0) 0.0
                                   (>= x 1.0) 1.0
                                   :else x))
                clj-w2 (stats/cvm-test-statistic uniform-test-data uniform-cdf-fn)]
            (is (approx= r-w2 clj-w2 1e-10)
                (format "W² statistic mismatch: R=%.10f, clj=%.10f" r-w2 clj-w2))))

        (testing "against R's cvm.test for gamma distribution"
          (let [data-str (str "c(" (str/join "," gamma-test-data) ")")
                mle-result (stats/gamma-mle gamma-test-data)
                shape (get-in mle-result [:params :shape])
                scale (get-in mle-result [:params :scale])
                rate (/ 1.0 scale)
                r-result (r/r-eval (str "cvm.test(" data-str
                                        ", 'pgamma', shape=" shape
                                        ", rate=" rate ")$statistic"))
                r-w2 (if (map? r-result)
                       (first (vals r-result))
                       (first r-result))
                gamma-cdf-fn (stats/gamma-cdf shape scale)
                clj-w2 (stats/cvm-test-statistic gamma-test-data gamma-cdf-fn)]
            (is (approx= r-w2 clj-w2 1e-10)
                (format "W² statistic mismatch: R=%.10f, clj=%.10f" r-w2 clj-w2))))))))

(deftest cvm-test-pvalue-validation-test
  ;; Validates that CvM p-values are in reasonable agreement with R.
  (testing "cvm-test p-value"
    (if-not (goftest-available?)
      (do
        (println "Skipping CvM p-value validation: R/goftest not available")
        (is true "Skipped - R/goftest unavailable"))
      (do
        (testing "is reasonable for well-fitting data"
          (let [data-str (str "c(" (str/join "," uniform-test-data) ")")
                r-result (r/r-eval (str "cvm.test(" data-str ", 'punif')$p.value"))
                r-p (first r-result)
                uniform-cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
                clj-result (stats/cvm-test uniform-test-data uniform-cdf-fn)
                clj-p (:p-value clj-result)]
            (is (> r-p 0.01) "R p-value should not reject good fit")
            (is (> clj-p 0.01) "Clj p-value should not reject good fit")))

        (testing "detects poor fit"
          (let [data-str (str "c(" (str/join "," gamma-test-data) ")")
                mean-val (/ (reduce + gamma-test-data) (count gamma-test-data))
                rate (/ 1.0 mean-val)
                r-result (r/r-eval (str "cvm.test(" data-str
                                        ", 'pexp', rate=" rate ")$p.value"))
                r-p (first r-result)
                exp-cdf-fn (stats/gamma-cdf 1.0 mean-val)
                clj-result (stats/cvm-test gamma-test-data exp-cdf-fn)
                clj-p (:p-value clj-result)]
            (is (< r-p 0.5) "R p-value should indicate poor fit")
            (is (< clj-p 0.5) "Clj p-value should indicate poor fit")))))))

;;; Additional property tests

(deftest gof-tests-basic-properties-test
  ;; Tests basic properties of GoF tests without R.
  (testing "ks-test"
    (testing "returns expected keys"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/ks-test uniform-test-data cdf-fn)]
        (is (contains? result :statistic))
        (is (contains? result :p-value))
        (is (contains? result :n))))

    (testing "D statistic is in [0, 1]"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/ks-test uniform-test-data cdf-fn)]
        (is (<= 0 (:statistic result) 1))))

    (testing "p-value is in [0, 1]"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/ks-test uniform-test-data cdf-fn)]
        (is (<= 0 (:p-value result) 1))))

    (testing "n equals sample size"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/ks-test uniform-test-data cdf-fn)]
        (is (= (count uniform-test-data) (:n result))))))

  (testing "cvm-test"
    (testing "returns expected keys"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/cvm-test uniform-test-data cdf-fn)]
        (is (contains? result :statistic))
        (is (contains? result :p-value))
        (is (contains? result :n))))

    (testing "W² statistic is non-negative"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/cvm-test uniform-test-data cdf-fn)]
        (is (>= (:statistic result) 0))))

    (testing "p-value is in [0, 1]"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/cvm-test uniform-test-data cdf-fn)]
        (is (<= 0 (:p-value result) 1))))

    (testing "n equals sample size"
      (let [cdf-fn (fn [x] (cond (<= x 0) 0.0 (>= x 1) 1.0 :else x))
            result (stats/cvm-test uniform-test-data cdf-fn)]
        (is (= (count uniform-test-data) (:n result)))))))

(deftest gof-tests-with-distributions-test
  ;; Tests GoF functions work with the distribution CDF functions.
  (testing "integration with distribution functions"
    (testing "ks-test with gamma-cdf"
      (let [shape 3.0
            scale 2.0
            cdf-fn (stats/gamma-cdf shape scale)
            result (stats/ks-test gamma-test-data cdf-fn)]
        (is (number? (:statistic result)))
        (is (number? (:p-value result)))))

    (testing "ks-test with weibull-cdf"
      (let [shape 2.0
            scale 5.0
            cdf-fn (stats/weibull-cdf shape scale)
            result (stats/ks-test gamma-test-data cdf-fn)]
        (is (number? (:statistic result)))
        (is (number? (:p-value result)))))

    (testing "ks-test with lognormal-cdf"
      (let [mu 1.0
            sigma 0.5
            cdf-fn (stats/lognormal-cdf mu sigma)
            result (stats/ks-test gamma-test-data cdf-fn)]
        (is (number? (:statistic result)))
        (is (number? (:p-value result)))))

    (testing "cvm-test with gamma-cdf"
      (let [shape 3.0
            scale 2.0
            cdf-fn (stats/gamma-cdf shape scale)
            result (stats/cvm-test gamma-test-data cdf-fn)]
        (is (number? (:statistic result)))
        (is (number? (:p-value result)))))

    (testing "cvm-test with inverse-gaussian-cdf"
      (let [mu 5.0
            lambda 10.0
            cdf-fn (stats/inverse-gaussian-cdf mu lambda)
            result (stats/cvm-test gamma-test-data cdf-fn)]
        (is (number? (:statistic result)))
        (is (number? (:p-value result)))))))
