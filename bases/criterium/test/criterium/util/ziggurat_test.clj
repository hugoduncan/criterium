(ns criterium.util.ziggurat-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.test-utils :refer [abs-error autocorrelation variance-ratio]]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]
   [criterium.util.ziggurat :as ziggurat]))

(defspec random-normal-zig-test-property 10
  (prop/for-all
   [random-seed gen/small-integer]
   (let [random-source  (java.util.Random. random-seed)
         values         (->> #(.nextDouble random-source)
                             repeatedly
                             ziggurat/random-normal-zig
                             (take 10000)
                             vec)
         mean           (stats/mean values)
         variance       (stats/variance values)
         mean-error     (abs-error mean 0.0)
         variance-error (abs-error variance 1.0)
         mean-tol       1e-1
         variance-tol   15e-1]
     (is (< mean-error mean-tol))
     (is (< variance-error variance-tol))
     (and (< mean-error mean-tol)
          (< variance-error variance-tol)))))

;;; Autocorrelation tests
;; Verify that ziggurat with WELL RNG produces normal samples with negligible
;; autocorrelation and correct marginal distribution. The ziggurat algorithm
;; consumes 2-3+ uniform values per output, which can amplify RNG correlations.

(deftest ziggurat-well-rng-autocorrelation-test
  ;; Verify ziggurat produces independent normal samples when using WELL RNG.
  ;; With n=100,000, SE ≈ 1/√n ≈ 0.003, so threshold of 0.02 is conservative.
  (testing "random-normal-zig with WELL RNG"
    (let [seed    42
          n       100000
          samples (vec (take n (ziggurat/random-normal-zig
                                (well/well-rng-1024a seed))))]
      (testing "has negligible autocorrelation at multiple lags"
        (let [lags [1 2 5 10]]
          (doseq [lag lags]
            (let [ac (autocorrelation samples lag)]
              (is (< (Math/abs ac) 0.02)
                  (format "lag %d: autocorrelation %.4f exceeds threshold"
                          lag ac))))))

      (testing "has batch-sum variance ratio near 1.0"
        (let [batch-size 500
              ;; Normal distribution has variance 1.0
              ratio      (variance-ratio samples batch-size 1.0)]
          (is (< 0.9 ratio 1.1)
              (format "variance ratio %.3f outside [0.9, 1.1]" ratio))))

      (testing "has correct marginal distribution"
        (let [mean     (stats/mean samples)
              variance (stats/variance samples)]
          (is (< (Math/abs mean) 0.02)
              (format "mean %.4f exceeds tolerance 0.02" mean))
          (is (< (Math/abs (- variance 1.0)) 0.1)
              (format "variance %.4f not within 0.1 of 1.0" variance)))))))
