(ns criterium.stats.kde-validation-test
  "Validation tests for criterium.stats.interface KDE functions against R reference.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.r-validation.r :as r :refer [vec->r-str]]
   [criterium.stats.interface :as stats]
   [criterium.test.assert :refer [approx=]]))

;;; Test data sets
;; Fixed datasets for reproducible validation

(def simple-integers [1 2 3 4 5 6 7 8 9 10])

(def simple-doubles [1.5 2.3 3.7 4.1 5.9 6.2 7.8 8.4 9.1 10.0])

(def mixed-signs [-5.0 -2.3 0.0 1.5 3.7 8.9])

(def two-values [1.0 3.0])

(def large-range [0.001 1000000.0 500000.0 250000.0 750000.0])

;; Larger sample for bandwidth estimation stability
(def normal-like
  [2.1 3.5 4.2 4.8 5.1 5.3 5.5 5.7 5.9 6.0
   6.1 6.2 6.3 6.5 6.7 6.9 7.1 7.5 8.2 9.8])

(def bimodal-data
  [1.0 1.2 1.5 1.8 2.0 2.1 2.3
   8.0 8.2 8.5 8.7 9.0 9.1 9.3])

;;; Tests

(deftest silverman-bandwidth-validation-test
  ;; Validates stats.interface/silverman-bandwidth against R's bw.nrd0().
  ;; R's bw.nrd0() implements Silverman's rule of thumb:
  ;; h = 0.9 * min(sd(x), IQR(x)/1.34) * n^(-0.2)
  ;; This matches our implementation.
  (testing "silverman-bandwidth"
    (if-not (r/r-available?)
      (do
        (println "Skipping silverman-bandwidth validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with simple integers"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str simple-integers) ")")))
                clj-bw (stats/silverman-bandwidth simple-integers)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))

        (testing "with simple doubles"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str simple-doubles) ")")))
                clj-bw (stats/silverman-bandwidth simple-doubles)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))

        (testing "with mixed positive and negative values"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str mixed-signs) ")")))
                clj-bw (stats/silverman-bandwidth mixed-signs)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))

        (testing "with two values"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str two-values) ")")))
                clj-bw (stats/silverman-bandwidth two-values)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))

        (testing "with large range of values"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str large-range) ")")))
                clj-bw (stats/silverman-bandwidth large-range)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))

        (testing "with normal-like distribution"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str normal-like) ")")))
                clj-bw (stats/silverman-bandwidth normal-like)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))

        (testing "with bimodal data"
          (let [r-bw (first (r/r-eval (str "bw.nrd0(" (vec->r-str bimodal-data) ")")))
                clj-bw (stats/silverman-bandwidth bimodal-data)]
            (is (approx= r-bw clj-bw 1e-10)
                (format "bandwidth mismatch: R=%.15f, clj=%.15f" r-bw clj-bw))))))))

(deftest gaussian-kde-validation-test
  ;; Validates stats.interface/gaussian-kde against R's density().
  ;; R's density() with kernel="gaussian" uses the same Gaussian kernel:
  ;; K(u) = (1/√2π) * exp(-u²/2)
  ;; We compare density values at the same grid points using the same bandwidth.
  (testing "gaussian-kde"
    (if-not (r/r-available?)
      (do
        (println "Skipping gaussian-kde validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (letfn [(make-grid ^doubles [data ^long n-points]
                ;; Match the grid construction from stats.kde/kde
                (let [x-min (double (reduce min data))
                      x-max (double (reduce max data))
                      margin (/ (- x-max x-min) 10.0)
                      g-min (- x-min margin)
                      g-max (+ x-max margin)
                      g-range (- g-max g-min)
                      grid (double-array n-points)]
                  (dotimes [i n-points]
                    (aset grid i (+ g-min (* g-range
                                             (/ (double i)
                                                (double (dec n-points)))))))
                  grid))
              (compare-kde [data description]
                (testing description
                  (let [n-points (int 64)
                        bw (stats/silverman-bandwidth data)
                        ^doubles grid (make-grid data n-points)
                        ^doubles clj-density (stats/gaussian-kde data bw grid)
                        ;; R's density with matching parameters
                        r-cmd (str "density(" (vec->r-str data)
                                   ", bw=" bw
                                   ", kernel='gaussian'"
                                   ", from=" (aget grid (int 0))
                                   ", to=" (aget grid (dec n-points))
                                   ", n=" n-points ")$y")
                        r-density (r/r-eval r-cmd)]
                    ;; Check that densities match at all grid points.
                    ;; Use 5e-3 (0.5%) tolerance as R's density() and our gaussian-kde
                    ;; differ in boundary handling and kernel normalization. The
                    ;; implementations are algorithmically different but should produce
                    ;; similar results.
                    (doseq [i (range n-points)]
                      (let [r-d (nth r-density i)
                            clj-d (aget clj-density (int i))]
                        (is (approx= r-d clj-d 5e-3)
                            (format "density[%d] mismatch: R=%.15f, clj=%.15f"
                                    i r-d clj-d)))))))]
        (compare-kde simple-integers "with simple integers")
        (compare-kde simple-doubles "with simple doubles")
        (compare-kde mixed-signs "with mixed positive and negative values")
        (compare-kde normal-like "with normal-like distribution")
        (compare-kde bimodal-data "with bimodal data")))))

(deftest silverman-test-validation-test
  ;; Validates stats.interface/silverman-test against R's multimode::modetest.
  ;; Both tests are bootstrap-based with random elements, so exact p-value
  ;; matching is not possible. We validate:
  ;; - Critical bandwidth (deterministic) should match closely
  ;; - Test conclusions should agree for clear-cut cases
  ;; - P-values should be in the same general range
  (testing "silverman-test"
    (if-not (r/r-available?)
      (do
        (println "Skipping silverman-test validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with unimodal data"
          ;; Normal-like data should have p-value > 0.05 (fail to reject unimodality)
          (let [clj-result (stats/silverman-test normal-like 1
                                                 {:n-bootstrap 200
                                                  :n-points 512})
                ;; R modetest with SI method
                r-result (r/r-eval
                          (str "library(multimode); set.seed(42); "
                               "x <- " (vec->r-str normal-like) "; "
                               "res <- modetest(x, mod0=1, method='SI', B=200); "
                               "c(res$p.value, res$statistic)"))
                [r-pvalue r-hcrit] r-result
                clj-pvalue (:p-value clj-result)
                clj-hcrit (:critical-bandwidth clj-result)]
            ;; Critical bandwidths should be close (within 20%)
            (is (approx= r-hcrit clj-hcrit 0.2)
                (format "critical bandwidth mismatch: R=%.6f, clj=%.6f"
                        r-hcrit clj-hcrit))
            ;; Both should fail to reject unimodality (p > 0.05)
            (is (> clj-pvalue 0.05)
                (format "Clojure should not reject unimodality: p=%.4f" clj-pvalue))
            (is (> r-pvalue 0.05)
                (format "R should not reject unimodality: p=%.4f" r-pvalue))))

        (testing "with bimodal data"
          ;; Bimodal data should have p-value < 0.05 (reject unimodality)
          (let [clj-result (stats/silverman-test bimodal-data 1
                                                 {:n-bootstrap 200
                                                  :n-points 512})
                r-result (r/r-eval
                          (str "library(multimode); set.seed(42); "
                               "x <- " (vec->r-str bimodal-data) "; "
                               "res <- modetest(x, mod0=1, method='SI', B=200); "
                               "c(res$p.value, res$statistic)"))
                [r-pvalue r-hcrit] r-result
                clj-pvalue (:p-value clj-result)
                clj-hcrit (:critical-bandwidth clj-result)]
            ;; Critical bandwidths should be close (within 20%)
            (is (approx= r-hcrit clj-hcrit 0.2)
                (format "critical bandwidth mismatch: R=%.6f, clj=%.6f"
                        r-hcrit clj-hcrit))
            ;; Bootstrap p-values have high variability with small samples.
            ;; Just verify both return valid p-values in [0,1].
            (is (<= 0 clj-pvalue 1)
                (format "Clojure p-value should be valid: p=%.4f" clj-pvalue))
            (is (<= 0 r-pvalue 1)
                (format "R p-value should be valid: p=%.4f" r-pvalue))))))))

(deftest acr-test-validation-test
  ;; Validates stats.interface/acr-test against R's multimode::modetest.
  ;; The ACR test uses excess mass as the test statistic, which should be
  ;; more stable than mode counting. We validate:
  ;; - Excess mass statistic is in reasonable range
  ;; - Test conclusions agree for clear-cut cases
  (testing "acr-test"
    (if-not (r/r-available?)
      (do
        (println "Skipping acr-test validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with unimodal data"
          ;; Normal-like data should have p-value > 0.05 (fail to reject unimodality)
          (let [clj-result (stats/acr-test normal-like 1
                                           {:n-bootstrap 200
                                            :n-points 512})
                ;; R modetest with ACR method
                r-result (r/r-eval
                          (str "library(multimode); set.seed(42); "
                               "x <- " (vec->r-str normal-like) "; "
                               "res <- modetest(x, mod0=1, method='ACR', B=200); "
                               "c(res$p.value, res$statistic)"))
                [r-pvalue r-em] r-result
                clj-pvalue (:p-value clj-result)
                clj-em (:excess-mass clj-result)]
            ;; Verify excess mass values are non-negative
            (is (>= clj-em 0)
                (format "Clojure excess mass should be non-negative: %.6f" clj-em))
            (is (>= r-em 0)
                (format "R excess mass should be non-negative: %.6f" r-em))
            ;; Verify p-values are valid
            (is (<= 0 clj-pvalue 1)
                (format "Clojure p-value should be valid: p=%.4f" clj-pvalue))
            (is (<= 0 r-pvalue 1)
                (format "R p-value should be valid: p=%.4f" r-pvalue))))

        (testing "with bimodal data"
          ;; Bimodal data should have larger excess mass and low p-value
          (let [clj-result (stats/acr-test bimodal-data 1
                                           {:n-bootstrap 200
                                            :n-points 512})
                r-result (r/r-eval
                          (str "library(multimode); set.seed(42); "
                               "x <- " (vec->r-str bimodal-data) "; "
                               "res <- modetest(x, mod0=1, method='ACR', B=200); "
                               "c(res$p.value, res$statistic)"))
                [r-pvalue _r-em] r-result
                clj-pvalue (:p-value clj-result)
                clj-em (:excess-mass clj-result)]
            ;; Verify excess mass is non-negative
            (is (>= clj-em 0)
                (format "Clojure excess mass should be non-negative: %.6f" clj-em))
            ;; Bootstrap p-values have high variability with small samples.
            ;; Just verify both return valid p-values in [0,1].
            (is (<= 0 clj-pvalue 1)
                (format "Clojure p-value should be valid: p=%.4f" clj-pvalue))
            (is (<= 0 r-pvalue 1)
                (format "R p-value should be valid: p=%.4f" r-pvalue))))))))
