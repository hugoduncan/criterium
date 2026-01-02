(ns stats.bootstrap-validation-test
  "Validation tests for stats.interface bootstrap functions against R's boot package.

  Bootstrap methods involve random sampling, so exact matching is not possible.
  Instead, we validate:
  - Jackknife (deterministic) - exact match
  - Bootstrap estimates - statistical similarity within expected bounds
  - BCa CI - coverage and width comparisons

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test.assert :refer [approx=]]
   [r-validation.r :as r :refer [vec->r-str]]
   [random.interface :as random]
   [stats.interface :as stats])
  (:import
   [stats.bootstrap BcaEstimate]))

;;; Test data sets
;; Fixed datasets for reproducible validation

(def simple-data [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0])

;; Normal-like symmetric data (mean ≈ 50, sd ≈ 10)
(def normal-data
  [35.2 42.1 45.8 47.3 48.9 50.1 51.2 52.8 54.6 58.3
   61.7 38.4 44.2 46.5 49.0 50.5 52.1 53.9 56.2 63.1])

;; Skewed data (exponential-like, tests BCa bias correction)
(def skewed-data
  [0.1 0.3 0.5 0.8 1.2 1.8 2.5 3.5 5.0 8.0
   0.2 0.4 0.7 1.0 1.5 2.1 3.0 4.2 6.5 12.0])

;; Small sample for edge case testing
(def small-data [2.0 4.0 6.0 8.0 10.0])

;;; Helper functions

(defn- within-factor?
  "Check if two values are within a multiplicative factor of each other.
  For comparing bootstrap CI widths where exact match is not expected."
  ^Boolean [^double expected ^double actual ^double factor]
  (let [ratio (if (zero? expected)
                (if (zero? actual) 1.0 Double/POSITIVE_INFINITY)
                (/ actual expected))]
    (and (>= ratio (/ 1.0 factor))
         (<= ratio factor))))

(defn- ci-contains?
  "Check if a confidence interval [lower upper] contains a value."
  ^Boolean [[^double lower ^double upper] ^double value]
  (and (<= lower value) (<= value upper)))

(defn- deterministic-rng-factory
  "Create an RNG factory that produces deterministic but different sequences.
  Each call to the returned factory uses a different seed based on a counter,
  ensuring reproducible test results across platforms."
  [base-seed]
  (let [counter (atom 0)]
    #(random/well-rng-1024a (+ base-seed (swap! counter inc)))))

;;; Jackknife validation (deterministic - exact match)

(deftest jackknife-validation-test
  ;; Validates stats.interface/jacknife against R's leave-one-out implementation.
  ;; Jackknife is deterministic, so results should match exactly.
  (testing "jacknife"
    (if-not (r/r-available?)
      (do
        (println "Skipping jackknife validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "computes leave-one-out means"
          ;; Jackknife mean: for each i, compute mean of data without element i
          (let [data (vec (sort simple-data))
                clj-jack (stats/jacknife data stats/mean)
                ;; R: sapply(1:length(x), function(i) mean(x[-i]))
                r-jack (r/r-eval
                        (str "x <- " (vec->r-str data) "; "
                             "sapply(1:length(x), function(i) mean(x[-i]))"))]
            (is (= (count clj-jack) (count r-jack))
                (format "jackknife sample count mismatch: clj=%d, R=%d"
                        (count clj-jack) (count r-jack)))
            (doseq [[i [clj-val r-val]] (map-indexed vector (map vector clj-jack r-jack))]
              (is (approx= clj-val r-val 1e-10)
                  (format "jackknife[%d] mismatch: clj=%.15f, R=%.15f"
                          i clj-val r-val)))))

        (testing "computes leave-one-out variances"
          (let [data (vec (sort simple-data))
                clj-jack (stats/jacknife data stats/variance)
                r-jack (r/r-eval
                        (str "x <- " (vec->r-str data) "; "
                             "sapply(1:length(x), function(i) var(x[-i]))"))]
            (is (= (count clj-jack) (count r-jack))
                (format "jackknife sample count mismatch: clj=%d, R=%d"
                        (count clj-jack) (count r-jack)))
            (doseq [[i [clj-val r-val]] (map-indexed vector (map vector clj-jack r-jack))]
              (is (approx= clj-val r-val 1e-10)
                  (format "jackknife variance[%d] mismatch: clj=%.15f, R=%.15f"
                          i clj-val r-val)))))

        (testing "with normal-like data"
          (let [data (vec (sort normal-data))
                clj-jack (stats/jacknife data stats/mean)
                r-jack (r/r-eval
                        (str "x <- " (vec->r-str data) "; "
                             "sapply(1:length(x), function(i) mean(x[-i]))"))]
            (doseq [[i [clj-val r-val]] (map-indexed vector (map vector clj-jack r-jack))]
              (is (approx= clj-val r-val 1e-10)
                  (format "jackknife[%d] mismatch: clj=%.15f, R=%.15f"
                          i clj-val r-val)))))

        (testing "with skewed data"
          (let [data (vec (sort skewed-data))
                clj-jack (stats/jacknife data stats/mean)
                r-jack (r/r-eval
                        (str "x <- " (vec->r-str data) "; "
                             "sapply(1:length(x), function(i) mean(x[-i]))"))]
            (doseq [[i [clj-val r-val]] (map-indexed vector (map vector clj-jack r-jack))]
              (is (approx= clj-val r-val 1e-10)
                  (format "jackknife[%d] mismatch: clj=%.15f, R=%.15f"
                          i clj-val r-val)))))))))

;;; Bootstrap estimate validation (statistical comparison)

(deftest bootstrap-estimate-validation-test
  ;; Validates bootstrap mean estimates converge to the true sample mean.
  ;; Since RNGs differ, we compare statistical properties rather than exact values.
  (testing "bootstrap-estimate"
    (if-not (r/r-available?)
      (do
        (println "Skipping bootstrap-estimate validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "mean estimate converges to sample mean"
          ;; Both R and Clojure bootstrap means should be close to the original sample mean
          (let [data (vec (sort normal-data))
                true-mean (stats/mean data)
                boot-size 1000
                ;; Clojure bootstrap
                clj-samples (stats/bootstrap-sample
                             data stats/mean boot-size random/well-rng-1024a)
                clj-boot-mean (stats/mean clj-samples)
                ;; R bootstrap
                r-boot-mean (first (r/r-eval
                                    (str "set.seed(42); x <- " (vec->r-str data) "; "
                                         "boot_means <- replicate(" boot-size ", "
                                         "mean(sample(x, replace=TRUE))); "
                                         "mean(boot_means)")))
                ;; Standard error of mean: sd/sqrt(n)
                se (/ (Math/sqrt (stats/variance data)) (Math/sqrt (count data)))
                ;; Bootstrap mean should be within 2 SE of true mean
                tolerance (* 2 se)]
            (is (< (Math/abs (- ^double clj-boot-mean true-mean)) tolerance)
                (format "Clojure bootstrap mean %.4f not within 2 SE of true mean %.4f (SE=%.4f)"
                        clj-boot-mean true-mean se))
            (is (< (Math/abs (- ^double r-boot-mean true-mean)) tolerance)
                (format "R bootstrap mean %.4f not within 2 SE of true mean %.4f (SE=%.4f)"
                        r-boot-mean true-mean se))))

        (testing "variance estimate is reasonable"
          ;; Bootstrap variance of the mean should approximate SE²
          (let [data (vec (sort normal-data))
                n (count data)
                boot-size 1000
                ;; Analytical SE² = var(data) / n
                analytical-var (/ (stats/variance data) n)
                ;; Clojure bootstrap variance
                clj-samples (stats/bootstrap-sample
                             data stats/mean boot-size random/well-rng-1024a)
                clj-boot-var (stats/variance clj-samples)
                ;; R bootstrap variance
                r-boot-var (first (r/r-eval
                                   (str "set.seed(42); x <- " (vec->r-str data) "; "
                                        "boot_means <- replicate(" boot-size ", "
                                        "mean(sample(x, replace=TRUE))); "
                                        "var(boot_means)")))]
            ;; Bootstrap variance should be within factor of 2 of analytical
            (is (within-factor? analytical-var clj-boot-var 2.0)
                (format "Clojure bootstrap var %.6f not within 2x of analytical %.6f"
                        clj-boot-var analytical-var))
            (is (within-factor? analytical-var r-boot-var 2.0)
                (format "R bootstrap var %.6f not within 2x of analytical %.6f"
                        r-boot-var analytical-var))))))))

;;; BCa CI validation (statistical comparison)

(deftest bca-ci-validation-test
  ;; Validates BCa confidence intervals against R's boot.ci.
  ;; Since RNGs differ, we compare CI widths and coverage rather than exact bounds.
  (testing "bca-nonparametric"
    (if-not (r/r-available?)
      (do
        (println "Skipping BCa CI validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "CI contains sample statistic"
          ;; Both R and Clojure BCa CIs should contain the sample mean
          (let [data (vec (sort normal-data))
                true-mean (stats/mean data)
                boot-size 1000
                alpha [0.025 0.5 0.975]
                ;; Clojure BCa
                clj-bca (stats/bca-nonparametric
                         data stats/mean boot-size alpha random/well-rng-1024a)
                [clj-ci _ _ _ _] clj-bca
                [clj-lower _clj-median clj-upper] clj-ci
                ;; R BCa using boot package
                r-result (r/r-eval
                          (str "library(boot); set.seed(42); "
                               "x <- " (vec->r-str data) "; "
                               "b <- boot(x, function(d,i) mean(d[i]), R=" boot-size "); "
                               "ci <- boot.ci(b, type='bca', conf=0.95); "
                               "c(ci$bca[4], ci$bca[5])"))
                [r-lower r-upper] r-result]
            ;; Both CIs should contain the true mean
            (is (ci-contains? [clj-lower clj-upper] true-mean)
                (format "Clojure BCa CI [%.4f, %.4f] does not contain mean %.4f"
                        clj-lower clj-upper true-mean))
            (is (ci-contains? [r-lower r-upper] true-mean)
                (format "R BCa CI [%.4f, %.4f] does not contain mean %.4f"
                        r-lower r-upper true-mean))
            ;; CI widths should be similar (within factor of 2)
            (let [clj-width (- ^double clj-upper ^double clj-lower)
                  r-width (- ^double r-upper ^double r-lower)]
              (is (within-factor? r-width clj-width 2.0)
                  (format "CI widths differ too much: clj=%.4f, R=%.4f"
                          clj-width r-width)))))

        (testing "handles skewed data appropriately"
          ;; BCa should adjust for skewness - CIs may be asymmetric
          ;; Use deterministic RNG to ensure reproducible z0 calculation
          (let [data (vec (sort skewed-data))
                true-mean (stats/mean data)
                boot-size 1000
                alpha [0.025 0.5 0.975]
                ;; Clojure BCa with deterministic RNG for reproducibility
                clj-bca (stats/bca-nonparametric
                         data stats/mean boot-size alpha
                         (deterministic-rng-factory 42))
                [clj-ci clj-z0 _clj-acc _ _] clj-bca
                [clj-lower _ clj-upper] clj-ci
                ;; R BCa
                r-result (r/r-eval
                          (str "library(boot); set.seed(42); "
                               "x <- " (vec->r-str data) "; "
                               "b <- boot(x, function(d,i) mean(d[i]), R=" boot-size "); "
                               "ci <- boot.ci(b, type='bca', conf=0.95); "
                               "c(ci$bca[4], ci$bca[5])"))
                [r-lower r-upper] r-result]
            ;; For skewed data, z0 (bias) should be non-zero
            (is (not (zero? ^double clj-z0))
                "BCa z0 should be non-zero for skewed data")
            ;; Both CIs should contain the true mean
            (is (ci-contains? [clj-lower clj-upper] true-mean)
                (format "Clojure BCa CI [%.4f, %.4f] does not contain mean %.4f"
                        clj-lower clj-upper true-mean))
            (is (ci-contains? [r-lower r-upper] true-mean)
                (format "R BCa CI [%.4f, %.4f] does not contain mean %.4f"
                        r-lower r-upper true-mean))))

        (testing "with small sample"
          ;; BCa should still work with small samples, though CIs will be wider
          (let [data (vec (sort small-data))
                true-mean (stats/mean data)
                boot-size 1000
                alpha [0.025 0.5 0.975]
                ;; Clojure BCa
                clj-bca (stats/bca-nonparametric
                         data stats/mean boot-size alpha random/well-rng-1024a)
                [clj-ci _ _ _ _] clj-bca
                [clj-lower _ clj-upper] clj-ci
                ;; R BCa
                r-result (r/r-eval
                          (str "library(boot); set.seed(42); "
                               "x <- " (vec->r-str data) "; "
                               "b <- boot(x, function(d,i) mean(d[i]), R=" boot-size "); "
                               "ci <- boot.ci(b, type='bca', conf=0.95); "
                               "c(ci$bca[4], ci$bca[5])"))
                [r-lower r-upper] r-result]
            ;; Both CIs should contain the true mean
            (is (ci-contains? [clj-lower clj-upper] true-mean)
                (format "Clojure BCa CI [%.4f, %.4f] does not contain mean %.4f"
                        clj-lower clj-upper true-mean))
            (is (ci-contains? [r-lower r-upper] true-mean)
                (format "R BCa CI [%.4f, %.4f] does not contain mean %.4f"
                        r-lower r-upper true-mean))))))))

;;; Bootstrap-bca wrapper validation

(deftest bootstrap-bca-validation-test
  ;; Validates the bootstrap-bca wrapper function produces valid BcaEstimate records.
  (testing "bootstrap-bca"
    (if-not (r/r-available?)
      (do
        (println "Skipping bootstrap-bca validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "returns BcaEstimate with valid structure"
          (let [data (vec (sort normal-data))
                boot-size 500
                alpha [0.5 0.025 0.975]
                result (stats/bootstrap-bca
                        data stats/mean boot-size alpha random/well-rng-1024a)]
            (is (instance? BcaEstimate result)
                "Result should be BcaEstimate record")
            (is (number? (:point-estimate result))
                "point-estimate should be a number")
            (is (vector? (:estimate-quantiles result))
                "estimate-quantiles should be a vector")
            (is (= 2 (count (:estimate-quantiles result)))
                "Should have 2 quantile estimates (for 0.025 and 0.975)")))

        (testing "point estimate matches sample statistic"
          (let [data (vec (sort normal-data))
                true-mean (stats/mean data)
                boot-size 500
                alpha [0.5 0.025 0.975]
                result (stats/bootstrap-bca
                        data stats/mean boot-size alpha random/well-rng-1024a)
                ;; Point estimate (at alpha=0.5) should be close to true mean
                point-est (:point-estimate result)
                se (/ (Math/sqrt (stats/variance data)) (Math/sqrt (count data)))
                tolerance (* 3 se)]
            (is (< (Math/abs (- ^double point-est true-mean)) tolerance)
                (format "Point estimate %.4f not within 3 SE of true mean %.4f"
                        point-est true-mean))))))))
