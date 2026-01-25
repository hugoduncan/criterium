(ns criterium.validation.kde-stats-validation-test
  "Validation tests for KDE-based statistics integration against R reference.

  Tests validate criterium's trapezoidal integration for computing
  density-weighted mean and variance by comparing against R's integration
  on the SAME grid/density data from criterium's KDE.

  This validates the integration math, not the KDE computation itself.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse.metrics-samples :as metrics-samples]
   [criterium.r-validation.r :as r :refer [vec->r-str]]
   [criterium.test.assert :refer [approx=]]
   [criterium.stats.kde :as kde]))

;;; Test data sets
;; Reuse datasets from kde_validation_test for consistency

(def simple-doubles [1.5 2.3 3.7 4.1 5.9 6.2 7.8 8.4 9.1 10.0])

(def normal-like
  [2.1 3.5 4.2 4.8 5.1 5.3 5.5 5.7 5.9 6.0
   6.1 6.2 6.3 6.5 6.7 6.9 7.1 7.5 8.2 9.8])

(def bimodal-data
  [1.0 1.2 1.5 1.8 2.0 2.1 2.3
   8.0 8.2 8.5 8.7 9.0 9.1 9.3])

(def skewed-data
  [1.0 1.1 1.2 1.3 1.5 1.8 2.0 2.5 3.0 4.0 6.0 10.0])

;;; Helper functions

(defn- r-kde-stats-from-grid
  "Compute KDE-based statistics in R using trapezoidal integration on provided grid.
  Takes the grid and density arrays from criterium's KDE output.
  Returns [mean variance] computed from density-weighted integration."
  [grid density]
  (let [;; R code to compute density-weighted mean and variance on provided grid
        r-code (str
                "grid <- " (vec->r-str (vec grid)) ";"
                "dens <- " (vec->r-str (vec density)) ";"
                "dx <- diff(grid)[1];"
                ;; Trapezoidal integration for mean: ∫ x·f(x) dx
                "xf <- grid * dens;"
                "mean_kde <- sum((xf[-1] + xf[-length(xf)]) * dx / 2);"
                ;; Trapezoidal integration for variance: ∫ (x-μ)²·f(x) dx
                "sq_dev <- (grid - mean_kde)^2 * dens;"
                "var_kde <- sum((sq_dev[-1] + sq_dev[-length(sq_dev)]) * dx / 2);"
                "c(mean_kde, var_kde)")]
    (r/r-eval r-code)))

(defn- clj-kde-stats
  "Compute KDE-based statistics using criterium's implementation.
  Returns map with :mean, :variance, :grid, :density."
  [data n-points]
  (let [kde-result (kde/kde data {:n-points n-points})
        stats (#'metrics-samples/kde-stats-for-metric kde-result)]
    {:mean (:mean stats)
     :variance (:variance stats)
     :grid (:grid kde-result)
     :density (:density kde-result)}))

;;; Tests

(deftest kde-stats-mean-validation-test
  ;; Validates criterium's KDE-based mean integration against R's trapezoidal integration.
  ;; Both compute ∫ x·f(x) dx using trapezoidal rule over the SAME grid/density.
  (testing "kde-stats mean"
    (if-not (r/r-available?)
      (do
        (println "Skipping kde-stats mean validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (let [n-points 512]
        (testing "with simple doubles"
          (let [clj-stats (clj-kde-stats simple-doubles n-points)
                [r-mean _] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            ;; Use 0.1% tolerance - should match closely since same grid/density
            (is (approx= r-mean (:mean clj-stats) 0.001)
                (format "mean mismatch: R=%.10f, clj=%.10f" r-mean (:mean clj-stats)))))

        (testing "with normal-like distribution"
          (let [clj-stats (clj-kde-stats normal-like n-points)
                [r-mean _] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            (is (approx= r-mean (:mean clj-stats) 0.001)
                (format "mean mismatch: R=%.10f, clj=%.10f" r-mean (:mean clj-stats)))))

        (testing "with bimodal data"
          (let [clj-stats (clj-kde-stats bimodal-data n-points)
                [r-mean _] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            (is (approx= r-mean (:mean clj-stats) 0.001)
                (format "mean mismatch: R=%.10f, clj=%.10f" r-mean (:mean clj-stats)))))

        (testing "with skewed data"
          (let [clj-stats (clj-kde-stats skewed-data n-points)
                [r-mean _] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            (is (approx= r-mean (:mean clj-stats) 0.001)
                (format "mean mismatch: R=%.10f, clj=%.10f" r-mean (:mean clj-stats)))))))))

(deftest kde-stats-variance-validation-test
  ;; Validates criterium's KDE-based variance integration against R's trapezoidal integration.
  ;; Both compute ∫ (x-μ)²·f(x) dx using trapezoidal rule over the SAME grid/density.
  (testing "kde-stats variance"
    (if-not (r/r-available?)
      (do
        (println "Skipping kde-stats variance validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (let [n-points 512]
        (testing "with simple doubles"
          (let [clj-stats (clj-kde-stats simple-doubles n-points)
                [_ r-var] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            ;; Use 0.1% tolerance - should match closely since same grid/density
            (is (approx= r-var (:variance clj-stats) 0.001)
                (format "variance mismatch: R=%.10f, clj=%.10f" r-var (:variance clj-stats)))))

        (testing "with normal-like distribution"
          (let [clj-stats (clj-kde-stats normal-like n-points)
                [_ r-var] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            (is (approx= r-var (:variance clj-stats) 0.001)
                (format "variance mismatch: R=%.10f, clj=%.10f" r-var (:variance clj-stats)))))

        (testing "with bimodal data"
          (let [clj-stats (clj-kde-stats bimodal-data n-points)
                [_ r-var] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            (is (approx= r-var (:variance clj-stats) 0.001)
                (format "variance mismatch: R=%.10f, clj=%.10f" r-var (:variance clj-stats)))))

        (testing "with skewed data"
          (let [clj-stats (clj-kde-stats skewed-data n-points)
                [_ r-var] (r-kde-stats-from-grid (:grid clj-stats) (:density clj-stats))]
            (is (approx= r-var (:variance clj-stats) 0.001)
                (format "variance mismatch: R=%.10f, clj=%.10f" r-var (:variance clj-stats)))))))))

(deftest kde-stats-consistency-test
  ;; Validates that KDE-based statistics are consistent with sample statistics
  ;; for distributions where they should converge (unimodal, symmetric).
  ;; This test doesn't require R.
  (testing "kde-stats consistency with sample statistics"
    (let [n-points 512]
      (testing "mean converges to sample mean for normal-like data"
        (let [sample-mean (/ (double (reduce + normal-like)) (count normal-like))
              clj-stats (clj-kde-stats normal-like n-points)]
          ;; KDE mean should be within 5% of sample mean for unimodal data
          (is (approx= sample-mean (:mean clj-stats) 0.05)
              (format "KDE mean should approximate sample mean: sample=%.10f, kde=%.10f"
                      sample-mean (:mean clj-stats)))))

      (testing "variance converges to sample variance for normal-like data"
        (let [sample-mean (/ (double (reduce + normal-like)) (count normal-like))
              sample-var (/ (double (reduce + (map #(Math/pow (- ^double % sample-mean) 2) normal-like)))
                            (dec (count normal-like)))
              clj-stats (clj-kde-stats normal-like n-points)]
          ;; KDE variance should be within 20% of sample variance
          ;; (KDE smoothing tends to increase apparent variance slightly)
          (is (approx= sample-var (:variance clj-stats) 0.20)
              (format "KDE variance should approximate sample variance: sample=%.10f, kde=%.10f"
                      sample-var (:variance clj-stats))))))))
