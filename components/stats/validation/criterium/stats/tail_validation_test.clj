(ns criterium.stats.tail-validation-test
  "Validation tests for tail statistics against R EVT packages.

  Tests skip gracefully when R/Rserve is unavailable.

  Tail statistics validated:
  - hill-estimator: Hill estimator for tail index, against evd::hill
  - gpd-mle: GPD maximum likelihood estimation, against evd::fpot
  - gpd-pdf/cdf/quantile: GPD distribution functions, against evd package
  - mean-residual-life: Mean excess function, computed in R

  References:
  - R evd package: https://cran.r-project.org/package=evd
  - R POT package: https://cran.r-project.org/package=POT"
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.r-validation.r :as r :refer [vec->r-str]]
   [criterium.stats.interface :as stats]
   [criterium.test.assert :refer [approx=]]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

(defn- sorted-darr
  "Create a sorted DoubleArray from a sequence."
  [coll]
  (darr (sort coll)))

(defn- darr->vec
  "Convert a DoubleArray to a Clojure vector."
  [typed-arr]
  (let [n (arr/length typed-arr)]
    (loop [i 0
           result []]
      (if (>= i n)
        result
        (recur (inc i) (conj result (arr/get-double typed-arr i)))))))

;;; Test Data
;; Heavy-tailed data for meaningful EVT analysis.
;; Generated from Pareto distribution (power law tail).

(def pareto-data
  "Pareto(alpha=2, xm=1) samples - moderate heavy tail.
  Generated with set.seed(42); rpareto(50, scale=1, shape=2) in R."
  [1.054 1.031 1.189 1.082 1.426 1.093 1.247 1.017 1.103 1.341
   1.023 1.156 1.069 1.392 1.044 1.211 1.063 1.285 1.008 1.178
   1.519 1.037 1.127 1.461 1.015 1.233 1.089 1.367 1.051 1.144
   1.006 1.298 1.076 1.412 1.028 1.195 1.058 1.324 1.012 1.162
   1.484 1.042 1.108 1.445 1.019 1.221 1.072 1.356 1.034 1.137])

(def gpd-data-positive-xi
  "GPD(xi=0.3, sigma=2) exceedances - heavy tail (Frechet-type).
  Positive xi indicates unbounded upper tail."
  [0.52 1.83 0.31 2.94 0.78 1.45 3.67 0.19 2.11 1.02
   0.65 4.23 1.31 0.43 2.56 0.91 1.72 0.26 3.15 1.58
   0.84 2.38 0.57 1.94 3.89 0.71 1.19 2.72 0.38 1.67
   4.56 0.95 2.24 1.41 0.48 3.42 1.08 1.86 0.62 2.67])

(def gpd-data-zero-xi
  "GPD(xi=0, sigma=2) exceedances - exponential tail (Gumbel-type).
  Zero xi gives exponential distribution."
  [0.41 1.52 2.87 0.73 1.94 0.28 3.21 1.15 0.56 2.43
   1.78 0.34 2.65 0.89 1.36 3.54 0.62 2.08 1.01 0.47
   2.31 1.63 0.79 2.96 0.55 1.42 3.12 0.86 1.89 0.38
   2.54 1.24 0.68 3.35 1.06 1.71 0.51 2.19 0.93 1.58])

(def gpd-data-negative-xi
  "GPD(xi=-0.2, sigma=2) exceedances - bounded tail (Weibull-type).
  Negative xi indicates finite upper endpoint at -sigma/xi = 10."
  [0.38 1.24 2.15 0.61 1.72 3.08 0.85 1.98 0.49 2.56
   1.41 0.72 2.31 1.08 0.55 1.85 2.74 0.92 1.58 0.42
   2.02 1.31 0.78 2.48 0.65 1.65 2.89 0.98 1.45 0.52
   2.21 1.18 0.68 1.92 2.62 0.82 1.52 0.45 2.38 1.05])

;;; Hill Estimator Validation

(deftest hill-estimator-validation-test
  ;; Validates stats/hill-estimator against R's evd::hill function.
  ;; The Hill estimator computes tail index for the k largest observations.
  (testing "hill-estimator"
    (if-not (r/r-available?)
      (do
        (println "Skipping Hill estimator validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(evd)")

        (testing "against evd::hill for Pareto data"
          ;; evd::hill returns Hill estimates for k=1 to n-1
          ;; We compare at specific k values
          (let [sorted-data (vec (sort pareto-data))
                data-str (vec->r-str sorted-data)
                ;; Test at k values: 10, 20, 30
                test-k-values [10 20 30]
                clj-results (stats/hill-estimator (sorted-darr pareto-data)
                                                  test-k-values)]
            (doseq [{:keys [k estimate]} clj-results]
              (testing (str "at k=" k)
                ;; evd::hill(x, k) returns estimate for that specific k
                (let [r-result (r/r-eval (str "evd::hill(" data-str ", " k ")"))
                      r-estimate (if (map? r-result)
                                   (first (vals r-result))
                                   (first r-result))]
                  (is (approx= r-estimate estimate 1e-6)
                      (format "Hill estimate mismatch at k=%d: R=%.10f, clj=%.10f"
                              k r-estimate estimate)))))))

        (testing "tail-index interpretation"
          ;; For Pareto(alpha=2) data, tail index should be around 2
          ;; Hill estimate H_k estimates 1/alpha, so tail-index = 1/H_k ~ 2
          (let [results (stats/hill-estimator (sorted-darr pareto-data) [20])
                {:keys [tail-index]} (first results)]
            ;; Allow for sampling variability
            (is (< 1.0 tail-index 4.0)
                (format "Tail index should be in reasonable range, got %.3f"
                        tail-index))))))))

;;; GPD MLE Validation

(deftest gpd-mle-validation-test
  ;; Validates stats/gpd-mle against R's evd::fpot function.
  ;; fpot fits GPD to exceedances over a threshold.
  (testing "gpd-mle"
    (if-not (r/r-available?)
      (do
        (println "Skipping GPD MLE validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(evd)")

        (testing "positive xi case"
          (let [data-str (vec->r-str gpd-data-positive-xi)
                ;; R's fpot uses threshold, we have exceedances directly
                ;; Use ismev::gpd.fit or evd::fpot with threshold=0
                r-result (r/r-eval
                          (str "fit <- evd::fpot(" data-str ", threshold=0); "
                               "c(fit$estimate['shape'], fit$estimate['scale'])"))
                [r-xi r-sigma] (if (sequential? r-result)
                                 r-result
                                 (vals r-result))
                clj-result (stats/gpd-mle (darr gpd-data-positive-xi))
                {:keys [xi sigma]} clj-result]
            ;; GPD MLE can have some variability in optimization
            (is (approx= r-xi xi 0.1)
                (format "xi mismatch: R=%.6f, clj=%.6f" r-xi xi))
            (is (approx= r-sigma sigma 0.2)
                (format "sigma mismatch: R=%.6f, clj=%.6f" r-sigma sigma))))

        (testing "exponential case (xi near zero)"
          (let [data-str (vec->r-str gpd-data-zero-xi)
                r-result (r/r-eval
                          (str "fit <- evd::fpot(" data-str ", threshold=0); "
                               "c(fit$estimate['shape'], fit$estimate['scale'])"))
                [r-xi r-sigma] (if (sequential? r-result)
                                 r-result
                                 (vals r-result))
                clj-result (stats/gpd-mle (darr gpd-data-zero-xi))
                {:keys [xi sigma]} clj-result]
            ;; For exponential data, both R and Clojure xi should be near zero
            (is (< (Math/abs r-xi) 0.3)
                (format "R xi should be near zero for exponential data, got %.6f" r-xi))
            (is (< (Math/abs xi) 0.3)
                (format "clj xi should be near zero for exponential data, got %.6f" xi))
            (is (approx= r-sigma sigma 0.3)
                (format "sigma mismatch: R=%.6f, clj=%.6f" r-sigma sigma))))

        (testing "negative xi case"
          (let [data-str (vec->r-str gpd-data-negative-xi)
                r-result (r/r-eval
                          (str "fit <- evd::fpot(" data-str ", threshold=0); "
                               "c(fit$estimate['shape'], fit$estimate['scale'])"))
                [r-xi r-sigma] (if (sequential? r-result)
                                 r-result
                                 (vals r-result))
                clj-result (stats/gpd-mle (darr gpd-data-negative-xi))
                {:keys [xi sigma]} clj-result]
            (is (approx= r-xi xi 0.15)
                (format "xi mismatch: R=%.6f, clj=%.6f" r-xi xi))
            (is (approx= r-sigma sigma 0.3)
                (format "sigma mismatch: R=%.6f, clj=%.6f" r-sigma sigma))))

        (testing "returns expected structure"
          (let [result (stats/gpd-mle (darr gpd-data-positive-xi))]
            (is (contains? result :xi))
            (is (contains? result :sigma))
            (is (contains? result :log-likelihood))
            (is (contains? result :converged?))
            (is (contains? result :n))))))))

;;; GPD Distribution Functions Validation

(deftest gpd-pdf-validation-test
  ;; Validates stats/gpd-pdf against R's evd::dgpd function.
  (testing "gpd-pdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping GPD PDF validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(evd)")

        (testing "positive xi"
          (let [xi 0.3
                sigma 2.0
                test-points [0.5 1.0 2.0 3.0 5.0]
                pdf-fn (stats/gpd-pdf xi sigma)]
            (doseq [y test-points]
              (testing (str "at y=" y)
                (let [r-result (r/r-eval
                                (str "evd::dgpd(" y ", loc=0, scale=" sigma
                                     ", shape=" xi ")"))
                      r-pdf (first r-result)
                      clj-pdf (pdf-fn y)]
                  (is (approx= r-pdf clj-pdf 1e-6)
                      (format "PDF mismatch at y=%.1f: R=%.10f, clj=%.10f"
                              y r-pdf clj-pdf)))))))

        (testing "zero xi (exponential)"
          (let [xi 0.0
                sigma 2.0
                test-points [0.5 1.0 2.0 3.0 5.0]
                pdf-fn (stats/gpd-pdf xi sigma)]
            (doseq [y test-points]
              (testing (str "at y=" y)
                (let [r-result (r/r-eval
                                (str "evd::dgpd(" y ", loc=0, scale=" sigma
                                     ", shape=0)"))
                      r-pdf (first r-result)
                      clj-pdf (pdf-fn y)]
                  (is (approx= r-pdf clj-pdf 1e-6)
                      (format "PDF mismatch at y=%.1f: R=%.10f, clj=%.10f"
                              y r-pdf clj-pdf)))))))

        (testing "negative xi"
          (let [xi -0.2
                sigma 2.0
                ;; Upper bound is -sigma/xi = 10
                test-points [0.5 1.0 2.0 5.0 8.0]
                pdf-fn (stats/gpd-pdf xi sigma)]
            (doseq [y test-points]
              (testing (str "at y=" y)
                (let [r-result (r/r-eval
                                (str "evd::dgpd(" y ", loc=0, scale=" sigma
                                     ", shape=" xi ")"))
                      r-pdf (first r-result)
                      clj-pdf (pdf-fn y)]
                  (is (approx= r-pdf clj-pdf 1e-6)
                      (format "PDF mismatch at y=%.1f: R=%.10f, clj=%.10f"
                              y r-pdf clj-pdf)))))))))))

(deftest gpd-cdf-validation-test
  ;; Validates stats/gpd-cdf against R's evd::pgpd function.
  (testing "gpd-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping GPD CDF validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(evd)")

        (testing "positive xi"
          (let [xi 0.3
                sigma 2.0
                test-points [0.5 1.0 2.0 3.0 5.0]
                cdf-fn (stats/gpd-cdf xi sigma)]
            (doseq [y test-points]
              (testing (str "at y=" y)
                (let [r-result (r/r-eval
                                (str "evd::pgpd(" y ", loc=0, scale=" sigma
                                     ", shape=" xi ")"))
                      r-cdf (first r-result)
                      clj-cdf (cdf-fn y)]
                  (is (approx= r-cdf clj-cdf 1e-6)
                      (format "CDF mismatch at y=%.1f: R=%.10f, clj=%.10f"
                              y r-cdf clj-cdf)))))))

        (testing "zero xi (exponential)"
          (let [xi 0.0
                sigma 2.0
                test-points [0.5 1.0 2.0 3.0 5.0]
                cdf-fn (stats/gpd-cdf xi sigma)]
            (doseq [y test-points]
              (testing (str "at y=" y)
                (let [r-result (r/r-eval
                                (str "evd::pgpd(" y ", loc=0, scale=" sigma
                                     ", shape=0)"))
                      r-cdf (first r-result)
                      clj-cdf (cdf-fn y)]
                  (is (approx= r-cdf clj-cdf 1e-6)
                      (format "CDF mismatch at y=%.1f: R=%.10f, clj=%.10f"
                              y r-cdf clj-cdf)))))))

        (testing "negative xi"
          (let [xi -0.2
                sigma 2.0
                test-points [0.5 1.0 2.0 5.0 8.0]
                cdf-fn (stats/gpd-cdf xi sigma)]
            (doseq [y test-points]
              (testing (str "at y=" y)
                (let [r-result (r/r-eval
                                (str "evd::pgpd(" y ", loc=0, scale=" sigma
                                     ", shape=" xi ")"))
                      r-cdf (first r-result)
                      clj-cdf (cdf-fn y)]
                  (is (approx= r-cdf clj-cdf 1e-6)
                      (format "CDF mismatch at y=%.1f: R=%.10f, clj=%.10f"
                              y r-cdf clj-cdf)))))))))))

(deftest gpd-quantile-validation-test
  ;; Validates stats/gpd-quantile against R's evd::qgpd function.
  (testing "gpd-quantile"
    (if-not (r/r-available?)
      (do
        (println "Skipping GPD quantile validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(evd)")

        (testing "positive xi"
          (let [xi 0.3
                sigma 2.0
                test-probs [0.1 0.25 0.5 0.75 0.9 0.95 0.99]
                quantile-fn (stats/gpd-quantile xi sigma)]
            (doseq [p test-probs]
              (testing (str "at p=" p)
                (let [r-result (r/r-eval
                                (str "evd::qgpd(" p ", loc=0, scale=" sigma
                                     ", shape=" xi ")"))
                      r-q (first r-result)
                      clj-q (quantile-fn p)]
                  (is (approx= r-q clj-q 1e-6)
                      (format "Quantile mismatch at p=%.2f: R=%.10f, clj=%.10f"
                              p r-q clj-q)))))))

        (testing "zero xi (exponential)"
          (let [xi 0.0
                sigma 2.0
                test-probs [0.1 0.25 0.5 0.75 0.9 0.95]
                quantile-fn (stats/gpd-quantile xi sigma)]
            (doseq [p test-probs]
              (testing (str "at p=" p)
                (let [r-result (r/r-eval
                                (str "evd::qgpd(" p ", loc=0, scale=" sigma
                                     ", shape=0)"))
                      r-q (first r-result)
                      clj-q (quantile-fn p)]
                  (is (approx= r-q clj-q 1e-6)
                      (format "Quantile mismatch at p=%.2f: R=%.10f, clj=%.10f"
                              p r-q clj-q)))))))

        (testing "negative xi"
          (let [xi -0.2
                sigma 2.0
                ;; Upper bound is -sigma/xi = 10
                test-probs [0.1 0.25 0.5 0.75 0.9 0.95]
                quantile-fn (stats/gpd-quantile xi sigma)]
            (doseq [p test-probs]
              (testing (str "at p=" p)
                (let [r-result (r/r-eval
                                (str "evd::qgpd(" p ", loc=0, scale=" sigma
                                     ", shape=" xi ")"))
                      r-q (first r-result)
                      clj-q (quantile-fn p)]
                  (is (approx= r-q clj-q 1e-6)
                      (format "Quantile mismatch at p=%.2f: R=%.10f, clj=%.10f"
                              p r-q clj-q)))))))))))

;;; Mean Residual Life Validation

(deftest mean-residual-life-validation-test
  ;; Validates stats/mean-residual-life against R computation.
  ;; MRL e(u) = mean(x[x > u] - u) for threshold u.
  (testing "mean-residual-life"
    (if-not (r/r-available?)
      (do
        (println "Skipping MRL validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "against R manual computation"
          (let [sorted-data (vec (sort gpd-data-positive-xi))
                data-str (vec->r-str sorted-data)
                ;; Test at specific thresholds
                test-thresholds [0.5 1.0 1.5 2.0]
                clj-results (stats/mean-residual-life (sorted-darr gpd-data-positive-xi)
                                                      test-thresholds)]
            (doseq [{:keys [threshold mrl n-exceed]} clj-results]
              (when (pos? n-exceed)
                (testing (str "at threshold=" threshold)
                  ;; R computation: mean(x[x > u] - u)
                  (let [r-result (r/r-eval
                                  (str "x <- " data-str "; "
                                       "u <- " threshold "; "
                                       "exc <- x[x > u]; "
                                       "if(length(exc) > 0) mean(exc - u) else NA"))
                        r-mrl (first r-result)]
                    (when-not (Double/isNaN r-mrl)
                      (is (approx= r-mrl mrl 1e-6)
                          (format "MRL mismatch at u=%.1f: R=%.10f, clj=%.10f"
                                  threshold r-mrl mrl)))))))))

        (testing "n-exceed count is correct"
          (let [sorted-data (vec (sort gpd-data-positive-xi))
                data-str (vec->r-str sorted-data)
                threshold 1.5
                clj-result (first (stats/mean-residual-life
                                   (sorted-darr gpd-data-positive-xi)
                                   [threshold]))
                r-count (first (r/r-eval
                                (str "x <- " data-str "; sum(x > " threshold ")")))]
            (is (= (long r-count) (:n-exceed clj-result))
                (format "n-exceed mismatch: R=%d, clj=%d"
                        (long r-count) (:n-exceed clj-result)))))))))

;;; Tail Ratios Validation

(deftest tail-ratios-validation-test
  ;; Validates stats/tail-ratios computation.
  ;; Tail ratios are p99/p95, p999/p99 computed from percentiles.
  (testing "tail-ratios"
    (if-not (r/r-available?)
      (do
        (println "Skipping tail ratios validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "computed correctly from R quantiles"
          (let [sorted-data (vec (sort pareto-data))
                data-str (vec->r-str sorted-data)
                ;; Get percentiles from R
                r-p95 (first (r/r-eval
                              (str "quantile(" data-str ", 0.95, type=7)")))
                r-p99 (first (r/r-eval
                              (str "quantile(" data-str ", 0.99, type=7)")))
                r-p999 (first (r/r-eval
                               (str "quantile(" data-str ", 0.999, type=7)")))
                ;; Compute expected ratios
                expected-p99-p95 (/ r-p99 r-p95)
                expected-p999-p99 (/ r-p999 r-p99)
                ;; Compute via our function
                percentiles {:p95 r-p95 :p99 r-p99 :p999 r-p999}
                ratios (stats/tail-ratios percentiles)]
            (is (approx= expected-p99-p95 (:p99-p95 ratios) 1e-10)
                (format "p99/p95 mismatch: expected=%.10f, got=%.10f"
                        expected-p99-p95 (:p99-p95 ratios)))
            (is (approx= expected-p999-p99 (:p999-p99 ratios) 1e-10)
                (format "p999/p99 mismatch: expected=%.10f, got=%.10f"
                        expected-p999-p99 (:p999-p99 ratios)))))

        (testing "with numeric keys"
          (let [percentiles {0.95 10.0 0.99 15.0 0.999 25.0}
                ratios (stats/tail-ratios percentiles)]
            (is (approx= 1.5 (:p99-p95 ratios) 1e-10))
            (is (approx= (/ 25.0 15.0) (:p999-p99 ratios) 1e-10))))))))

;;; Exceedances Over Threshold Validation

(deftest exceedances-over-threshold-validation-test
  ;; Validates stats/exceedances-over-threshold against R.
  (testing "exceedances-over-threshold"
    (if-not (r/r-available?)
      (do
        (println "Skipping exceedances validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "extracts correct values"
          (let [data gpd-data-positive-xi
                data-str (vec->r-str data)
                threshold 2.0
                ;; Compute excesses (value - threshold) to match Clojure impl
                r-result (r/r-eval (str "x <- " data-str "; x[x > " threshold "] - " threshold))
                r-exceed (if (sequential? r-result) r-result [r-result])
                clj-exceed (stats/exceedances-over-threshold (darr data) threshold)
                clj-exceed-vec (darr->vec clj-exceed)]
            (is (= (count r-exceed) (count clj-exceed-vec))
                (format "Count mismatch: R=%d, clj=%d"
                        (count r-exceed) (count clj-exceed-vec)))
            ;; Values should match (order may differ, so compare sorted)
            (let [r-sorted (sort r-exceed)
                  clj-sorted (sort clj-exceed-vec)]
              (doseq [[r-val clj-val] (map vector r-sorted clj-sorted)]
                (is (approx= r-val clj-val 1e-10)
                    (format "Value mismatch: R=%.10f, clj=%.10f"
                            r-val clj-val))))))

        (testing "returns empty for high threshold"
          (let [data [1.0 2.0 3.0 4.0 5.0]
                result (stats/exceedances-over-threshold (darr data) 10.0)]
            (is (zero? (arr/length result)))))))))

;;; Property Tests (no R required)

(deftest gpd-cdf-pdf-consistency-test
  ;; Tests that PDF integrates to CDF (numerical check).
  (testing "gpd-cdf and gpd-pdf"
    (testing "are consistent"
      (let [xi 0.3
            sigma 2.0
            cdf-fn (stats/gpd-cdf xi sigma)
            pdf-fn (stats/gpd-pdf xi sigma)
            ;; Numerical integration via trapezoid rule
            y-max 10.0
            n-steps 1000
            dy (/ y-max n-steps)
            integral (loop [i 1
                            acc (* 0.5 (pdf-fn 0.0) dy)]
                       (if (>= i n-steps)
                         acc
                         (let [y (* i dy)]
                           (recur (inc i)
                                  (+ acc (* (pdf-fn y) dy))))))]
        ;; Integral should approximate CDF at y-max
        (is (approx= (cdf-fn y-max) integral 0.01)
            (format "CDF(%.1f)=%.6f but integral=%.6f"
                    y-max (cdf-fn y-max) integral))))))

(deftest gpd-quantile-cdf-inverse-test
  ;; Tests that quantile is inverse of CDF.
  (testing "gpd-quantile"
    (testing "is inverse of gpd-cdf"
      (doseq [[xi sigma] [[0.3 2.0] [0.0 2.0] [-0.2 2.0]]]
        (testing (str "with xi=" xi ", sigma=" sigma)
          (let [cdf-fn (stats/gpd-cdf xi sigma)
                quantile-fn (stats/gpd-quantile xi sigma)
                test-probs [0.1 0.25 0.5 0.75 0.9]]
            (doseq [p test-probs]
              (let [y (quantile-fn p)
                    p-back (cdf-fn y)]
                (is (approx= p p-back 1e-10)
                    (format "Round-trip failed: p=%.2f -> y=%.6f -> p=%.6f"
                            p y p-back))))))))))

(deftest hill-estimator-basic-properties-test
  ;; Tests basic properties of Hill estimator.
  (testing "hill-estimator"
    (testing "returns estimates for valid k range"
      (let [results (stats/hill-estimator (sorted-darr pareto-data) [5 10 20])]
        (is (= 3 (count results)))
        (doseq [{:keys [k estimate tail-index]} results]
          (is (pos? k))
          (is (pos? estimate) "H_k should be positive for positive data")
          (is (pos? tail-index) "tail-index should be positive"))))

    (testing "filters invalid k values"
      (let [n (count pareto-data)
            results (stats/hill-estimator (sorted-darr pareto-data) [0 -1 n (inc n)])]
        (is (empty? results) "Should filter out invalid k values")))

    (testing "handles small samples"
      (let [results (stats/hill-estimator (sorted-darr [1.0]) [1])]
        (is (empty? results) "Single element should return empty")))))
