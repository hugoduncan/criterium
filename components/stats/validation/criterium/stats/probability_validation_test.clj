(ns criterium.stats.probability-validation-test
  "Validation tests for criterium.stats.probability functions against R reference.

  Tests skip gracefully when R/Rserve is unavailable.

  Distributions tested:
  - Normal: qnorm, pnorm
  - Gamma: dgamma, pgamma (shape, scale parameterization)
  - Weibull: dweibull, pweibull
  - Log-normal: dlnorm, plnorm
  - Inverse Gaussian: dinvgauss, pinvgauss (requires statmod package)"
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.primitive-fn :as prim]
   [criterium.r-validation.r :as r]
   [criterium.stats.probability :as stats]
   [criterium.test.assert :refer [approx=]]))

;;; Test quantiles
;; Standard quantile points to test across the distribution
(def standard-quantiles [0.001 0.01 0.05 0.1 0.25 0.5 0.75 0.9 0.95 0.99 0.999])

;;; Test x values for CDF
;; Central values where CDF is not too small or too close to 1
(def central-x-values [-2.0 -1.5 -1.0 -0.5 0.0 0.5 1.0 1.5 2.0])
;; Tail values where CDF is very small or very close to 1
(def tail-x-values [-4.0 -3.0 3.0 4.0])

;;; Tests

(deftest normal-quantile-validation-test
  ;; Validates stats.interface/normal-quantile against R's qnorm() function.
  ;; R's qnorm(p) returns the quantile of the standard normal distribution at probability p.
  ;; The Wichura AS241 algorithm is highly accurate (machine precision for central values).
  (testing "normal-quantile"
    (if-not (r/r-available?)
      (do
        (println "Skipping normal-quantile validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at standard probability values"
          (doseq [p standard-quantiles]
            (testing (str "at p=" p)
              (let [r-q (first (r/r-eval (str "qnorm(" p ")")))
                    clj-q (stats/normal-quantile p)]
                (is (approx= r-q clj-q 1e-10)
                    (format "normal-quantile mismatch at p=%.3f: R=%.15f, clj=%.15f"
                            p r-q clj-q))))))

        (testing "at the median (p=0.5)"
          (let [r-q (first (r/r-eval "qnorm(0.5)"))
                clj-q (stats/normal-quantile 0.5)]
            ;; Median of standard normal should be exactly 0
            (is (approx= r-q clj-q 1e-14)
                (format "median mismatch: R=%.15f, clj=%.15f" r-q clj-q))
            (is (approx= 0.0 clj-q 1e-14)
                "median of standard normal should be 0")))

        (testing "at symmetric probability pairs"
          ;; qnorm(p) = -qnorm(1-p) for any p
          (doseq [^double p [0.1 0.25 0.05 0.01]]
            (testing (str "p=" p " and p=" (- 1.0 p))
              (let [q-low (stats/normal-quantile p)
                    q-high (stats/normal-quantile (- 1.0 p))]
                (is (approx= (- q-low) q-high 1e-10)
                    (format "symmetry mismatch: q(%.2f)=%.15f, q(%.2f)=%.15f"
                            p q-low (- 1.0 p) q-high))))))

        (testing "at extreme probabilities"
          (doseq [p [0.0001 0.00001 0.9999 0.99999]]
            (testing (str "at p=" p)
              (let [r-q (first (r/r-eval (str "qnorm(" p ")")))
                    clj-q (stats/normal-quantile p)]
                ;; Slightly larger tolerance for extreme values
                (is (approx= r-q clj-q 1e-8)
                    (format "normal-quantile mismatch at p=%.5f: R=%.15f, clj=%.15f"
                            p r-q clj-q))))))))))

(deftest normal-cdf-validation-test
  ;; Validates stats.interface/normal-cdf against R's pnorm() function.
  ;; R's pnorm(x) returns the cumulative probability P(X < x) for standard normal.
  ;; Note: The implementation uses polynomial erf approximation with max error 1.5e-7.
  (testing "normal-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping normal-cdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at central x values"
          ;; For central values, CDF is not too small or too close to 1,
          ;; so relative tolerance works well.
          (doseq [x central-x-values]
            (testing (str "at x=" x)
              (let [r-p (first (r/r-eval (str "pnorm(" x ")")))
                    clj-p (stats/normal-cdf x)]
                ;; 1e-5 tolerance accounts for erf approximation propagation
                (is (approx= r-p clj-p 1e-5)
                    (format "normal-cdf mismatch at x=%.1f: R=%.15f, clj=%.15f"
                            x r-p clj-p))))))

        (testing "at tail x values"
          ;; For tail values, CDF is very small or very close to 1.
          ;; Use absolute tolerance since relative error is misleading.
          (doseq [x tail-x-values]
            (testing (str "at x=" x)
              (let [r-p (double (first (r/r-eval (str "pnorm(" x ")"))))
                    clj-p (stats/normal-cdf x)
                    abs-diff (Math/abs (- r-p clj-p))]
                ;; Absolute error should be < 1e-6 (well within erf max error)
                (is (< abs-diff 1e-6)
                    (format "normal-cdf mismatch at x=%.1f: R=%.15f, clj=%.15f, diff=%.2e"
                            x r-p clj-p abs-diff))))))

        (testing "at x=0 (the median)"
          (let [r-p (first (r/r-eval "pnorm(0)"))
                clj-p (stats/normal-cdf 0.0)]
            ;; CDF at 0 should be exactly 0.5
            (is (approx= r-p clj-p 1e-10)
                (format "median CDF mismatch: R=%.15f, clj=%.15f" r-p clj-p))
            (is (approx= 0.5 clj-p 1e-10)
                "CDF at 0 should be 0.5")))

        (testing "symmetry around the mean"
          ;; pnorm(x) + pnorm(-x) = 1 for any x
          (doseq [^double x [0.5 1.0 2.0 3.0]]
            (testing (str "x=" x " and x=" (- x))
              (let [p-pos (stats/normal-cdf x)
                    p-neg (stats/normal-cdf (- x))]
                (is (approx= 1.0 (+ p-pos p-neg) 1e-10)
                    (format "symmetry mismatch: pnorm(%.1f)=%.15f, pnorm(%.1f)=%.15f, sum=%.15f"
                            x p-pos (- x) p-neg (+ p-pos p-neg)))))))

        (testing "at extreme x values"
          ;; For extreme values (|x| >= 5), CDF is extremely small or close to 1.
          ;; Use absolute tolerance since these are edge cases.
          (doseq [x [-5.0 -6.0 5.0 6.0]]
            (testing (str "at x=" x)
              (let [r-p (double (first (r/r-eval (str "pnorm(" x ")"))))
                    clj-p (stats/normal-cdf x)
                    abs-diff (Math/abs (- r-p clj-p))]
                ;; Absolute error should be < 1e-6
                (is (< abs-diff 1e-6)
                    (format "normal-cdf mismatch at x=%.1f: R=%.15f, clj=%.15f, diff=%.2e"
                            x r-p clj-p abs-diff))))))))))

(deftest normal-cdf-quantile-inverse-test
  ;; Verifies that normal-cdf and normal-quantile are inverses.
  ;; pnorm(qnorm(p)) ≈ p and qnorm(pnorm(x)) ≈ x
  (testing "normal-cdf and normal-quantile"
    (testing "are inverses"
      (testing "pnorm(qnorm(p)) = p"
        (doseq [p [0.1 0.25 0.5 0.75 0.9 0.95 0.99]]
          (testing (str "at p=" p)
            (let [x (stats/normal-quantile p)
                  p-back (stats/normal-cdf x)]
              ;; Due to erf approximation, use 1e-6 tolerance
              (is (approx= p p-back 1e-6)
                  (format "inverse mismatch: p=%.2f, qnorm(p)=%.15f, pnorm(qnorm(p))=%.15f"
                          p x p-back))))))

      (testing "qnorm(pnorm(x)) = x"
        (doseq [x [-2.0 -1.0 0.0 1.0 2.0]]
          (testing (str "at x=" x)
            (let [p (stats/normal-cdf x)
                  x-back (stats/normal-quantile p)]
              ;; Due to erf approximation, use 1e-5 tolerance
              (is (approx= x x-back 1e-5)
                  (format "inverse mismatch: x=%.1f, pnorm(x)=%.15f, qnorm(pnorm(x))=%.15f"
                          x p x-back)))))))))

;;; Gamma Distribution Validation

;; Test parameters for gamma distribution
(def gamma-params [[1.0 1.0]    ; exponential
                   [2.0 1.0]    ; shape > 1
                   [0.5 1.0]    ; shape < 1
                   [3.0 2.0]    ; different scale
                   [5.0 0.5]]) ; higher shape, smaller scale

;; Test x values for gamma (positive only)
(def gamma-x-values [0.1 0.5 1.0 2.0 3.0 5.0 10.0])

(deftest gamma-pdf-validation-test
  ;; Validates stats.interface/gamma-pdf against R's dgamma().
  ;; R uses shape and scale parameterization by default.
  (testing "gamma-pdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping gamma-pdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[shape scale] gamma-params]
        (testing (str "with shape=" shape ", scale=" scale)
          (let [pdf-fn (stats/gamma-pdf shape scale)]
            (doseq [x gamma-x-values]
              (testing (str "at x=" x)
                (let [r-d (first (r/r-eval
                                  (format "dgamma(%s, shape=%s, scale=%s)"
                                          x shape scale)))
                      clj-d (pdf-fn x)]
                  (is (approx= r-d clj-d 1e-10)
                      (format "gamma-pdf mismatch: R=%.15f, clj=%.15f"
                              r-d clj-d)))))))))))

(deftest gamma-cdf-validation-test
  ;; Validates stats.interface/gamma-cdf against R's pgamma().
  (testing "gamma-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping gamma-cdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[shape scale] gamma-params]
        (testing (str "with shape=" shape ", scale=" scale)
          (let [cdf-fn (stats/gamma-cdf shape scale)]
            (doseq [x gamma-x-values]
              (testing (str "at x=" x)
                (let [r-p (first (r/r-eval
                                  (format "pgamma(%s, shape=%s, scale=%s)"
                                          x shape scale)))
                      clj-p (cdf-fn x)]
                  ;; Looser tolerance due to regularized-gamma-p approximation
                  (is (approx= r-p clj-p 1e-2)
                      (format "gamma-cdf mismatch: R=%.15f, clj=%.15f"
                              r-p clj-p)))))))))))

(deftest gamma-cdf-pdf-consistency-test
  ;; Verifies that gamma-cdf and gamma-pdf are consistent.
  ;; The PDF should be the derivative of the CDF (tested numerically).
  (testing "gamma-cdf and gamma-pdf"
    (testing "are consistent"
      (let [shape 2.0
            scale 1.5
            pdf-fn (stats/gamma-pdf shape scale)
            cdf-fn (stats/gamma-cdf shape scale)
            h 1e-6]
        (doseq [^double x [0.5 1.0 2.0 3.0]]
          (testing (str "at x=" x)
            (let [numerical-deriv (/ (- (prim/invoke-dd cdf-fn (+ x h))
                                        (prim/invoke-dd cdf-fn (- x h)))
                                     (* 2.0 h))
                  pdf-value (pdf-fn x)]
              (is (approx= numerical-deriv pdf-value 1e-4)
                  (format "CDF derivative != PDF: deriv=%.10f, pdf=%.10f"
                          numerical-deriv pdf-value)))))))))

;;; Weibull Distribution Validation

;; Test parameters for Weibull distribution
(def weibull-params [[1.0 1.0]    ; exponential
                     [2.0 1.0]    ; shape > 1 (increasing hazard)
                     [0.5 1.0]    ; shape < 1 (decreasing hazard)
                     [3.0 2.0]    ; Rayleigh-like
                     [5.0 0.5]])  ; high shape, small scale

;; Test x values for Weibull (positive only)
(def weibull-x-values [0.1 0.5 1.0 2.0 3.0 5.0])

(deftest weibull-pdf-validation-test
  ;; Validates stats.interface/weibull-pdf against R's dweibull().
  ;; R uses shape and scale parameterization.
  (testing "weibull-pdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping weibull-pdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[shape scale] weibull-params]
        (testing (str "with shape=" shape ", scale=" scale)
          (let [pdf-fn (stats/weibull-pdf shape scale)]
            (doseq [x weibull-x-values]
              (testing (str "at x=" x)
                (let [r-d (first (r/r-eval
                                  (format "dweibull(%s, shape=%s, scale=%s)"
                                          x shape scale)))
                      clj-d (pdf-fn x)]
                  (is (approx= r-d clj-d 1e-10)
                      (format "weibull-pdf mismatch: R=%.15f, clj=%.15f"
                              r-d clj-d)))))))))))

(deftest weibull-cdf-validation-test
  ;; Validates stats.interface/weibull-cdf against R's pweibull().
  (testing "weibull-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping weibull-cdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[shape scale] weibull-params]
        (testing (str "with shape=" shape ", scale=" scale)
          (let [cdf-fn (stats/weibull-cdf shape scale)]
            (doseq [x weibull-x-values]
              (testing (str "at x=" x)
                (let [r-p (first (r/r-eval
                                  (format "pweibull(%s, shape=%s, scale=%s)"
                                          x shape scale)))
                      clj-p (cdf-fn x)]
                  (is (approx= r-p clj-p 1e-10)
                      (format "weibull-cdf mismatch: R=%.15f, clj=%.15f"
                              r-p clj-p)))))))))))

(deftest weibull-cdf-pdf-consistency-test
  ;; Verifies that weibull-cdf and weibull-pdf are consistent.
  (testing "weibull-cdf and weibull-pdf"
    (testing "are consistent"
      (let [shape 2.0
            scale 1.5
            pdf-fn (stats/weibull-pdf shape scale)
            cdf-fn (stats/weibull-cdf shape scale)
            h 1e-6]
        (doseq [^double x [0.5 1.0 2.0 3.0]]
          (testing (str "at x=" x)
            (let [numerical-deriv (/ (- (prim/invoke-dd cdf-fn (+ x h))
                                        (prim/invoke-dd cdf-fn (- x h)))
                                     (* 2.0 h))
                  pdf-value (pdf-fn x)]
              (is (approx= numerical-deriv pdf-value 1e-4)
                  (format "CDF derivative != PDF: deriv=%.10f, pdf=%.10f"
                          numerical-deriv pdf-value)))))))))

;;; Log-normal Distribution Validation

;; Test parameters for log-normal distribution
(def lognormal-params [[0.0 1.0]     ; standard log-normal
                       [1.0 0.5]     ; shifted, smaller variance
                       [-0.5 1.5]    ; negative mu, larger variance
                       [2.0 0.25]])  ; larger mean, tight

;; Test x values for log-normal (positive only)
(def lognormal-x-values [0.1 0.5 1.0 2.0 5.0 10.0])

(deftest lognormal-pdf-validation-test
  ;; Validates stats.interface/lognormal-pdf against R's dlnorm().
  ;; R uses meanlog and sdlog parameterization.
  (testing "lognormal-pdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping lognormal-pdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[mu sigma] lognormal-params]
        (testing (str "with mu=" mu ", sigma=" sigma)
          (let [pdf-fn (stats/lognormal-pdf mu sigma)]
            (doseq [x lognormal-x-values]
              (testing (str "at x=" x)
                (let [r-d (first (r/r-eval
                                  (format "dlnorm(%s, meanlog=%s, sdlog=%s)"
                                          x mu sigma)))
                      clj-d (pdf-fn x)]
                  (is (approx= r-d clj-d 1e-10)
                      (format "lognormal-pdf mismatch: R=%.15f, clj=%.15f"
                              r-d clj-d)))))))))))

(deftest lognormal-cdf-validation-test
  ;; Validates stats.interface/lognormal-cdf against R's plnorm().
  ;; Uses erf approximation so tolerance is slightly looser.
  (testing "lognormal-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping lognormal-cdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (doseq [[mu sigma] lognormal-params]
        (testing (str "with mu=" mu ", sigma=" sigma)
          (let [cdf-fn (stats/lognormal-cdf mu sigma)]
            (doseq [x lognormal-x-values]
              (testing (str "at x=" x)
                (let [^double r-p (first
                                   (r/r-eval
                                    (format "plnorm(%s, meanlog=%s, sdlog=%s)"
                                            x mu sigma)))
                      clj-p       (prim/invoke-dd cdf-fn x)]
                  ;; Looser relative tolerance for erf approximation (1%)
                  ;; and accept underflow (both values < 1e-15)
                  (is (or (approx= r-p clj-p 0.01)
                          (and (< r-p 1e-15) (< clj-p 1e-15)))
                      (format "lognormal-cdf mismatch: R=%.15f, clj=%.15f"
                              r-p clj-p)))))))))))

(deftest lognormal-cdf-pdf-consistency-test
  ;; Verifies that lognormal-cdf and lognormal-pdf are consistent.
  ;; Uses looser tolerance (2e-3) due to erf approximation in normal-cdf.
  (testing "lognormal-cdf and lognormal-pdf"
    (testing "are consistent"
      (let [mu     0.0
            sigma  1.0
            pdf-fn (stats/lognormal-pdf mu sigma)
            cdf-fn (stats/lognormal-cdf mu sigma)
            h      1e-6]
        (doseq [^double x [0.5 1.0 2.0 3.0]]
          (testing (str "at x=" x)
            (let [numerical-deriv (/ (- (prim/invoke-dd cdf-fn (+ x h))
                                        (prim/invoke-dd cdf-fn (- x h)))
                                     (* 2.0 h))
                  pdf-value       (pdf-fn x)]
              ;; Looser tolerance due to erf approximation error propagation
              ;; The erf polynomial has max error 1.5e-7 which propagates
              (is (approx= numerical-deriv pdf-value 2e-3)
                  (format "CDF derivative != PDF: deriv=%.10f, pdf=%.10f"
                          numerical-deriv pdf-value)))))))))

;;; Inverse Gaussian Distribution Validation

;; Test parameters for inverse Gaussian distribution
(def inverse-gaussian-params [[1.0 1.0]     ; standard
                              [2.0 1.0]     ; larger mean
                              [1.0 2.0]     ; larger shape (tighter)
                              [0.5 0.5]     ; small
                              [3.0 5.0]])   ; larger values

;; Test x values for inverse Gaussian (positive only)
(def inverse-gaussian-x-values [0.1 0.5 1.0 2.0 3.0 5.0])

(deftest inverse-gaussian-pdf-validation-test
  ;; Validates stats.interface/inverse-gaussian-pdf against R's dinvgauss().
  ;; Requires statmod package (installed by CI workflow).
  (testing "inverse-gaussian-pdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping inverse-gaussian-pdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(statmod)")
        (doseq [[mu lambda] inverse-gaussian-params]
          (testing (str "with mu=" mu ", lambda=" lambda)
            (let [pdf-fn (stats/inverse-gaussian-pdf mu lambda)]
              (doseq [x inverse-gaussian-x-values]
                (testing (str "at x=" x)
                  (let [r-d (first (r/r-eval
                                    (format "dinvgauss(%s, mean=%s, shape=%s)"
                                            x mu lambda)))
                        clj-d (pdf-fn x)]
                    (is (approx= r-d clj-d 1e-10)
                        (format "inverse-gaussian-pdf mismatch: R=%.15f, clj=%.15f"
                                r-d clj-d))))))))))))

(deftest inverse-gaussian-cdf-validation-test
  ;; Validates stats.interface/inverse-gaussian-cdf against R's pinvgauss().
  ;; Requires statmod package (installed by CI workflow).
  ;; Uses erf approximation so slightly looser tolerance.
  (testing "inverse-gaussian-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping inverse-gaussian-cdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(statmod)")
        (doseq [[mu lambda] inverse-gaussian-params]
          (testing (str "with mu=" mu ", lambda=" lambda)
            (let [cdf-fn (stats/inverse-gaussian-cdf mu lambda)]
              (doseq [x inverse-gaussian-x-values]
                (testing (str "at x=" x)
                  (let [r-p (first (r/r-eval
                                    (format "pinvgauss(%s, mean=%s, shape=%s)"
                                            x mu lambda)))
                        clj-p (cdf-fn x)]
                    ;; Looser tolerance for erf approximation (~1% relative error for small values)
                    (is (approx= r-p clj-p 1e-2)
                        (format "inverse-gaussian-cdf mismatch: R=%.15f, clj=%.15f"
                                r-p clj-p))))))))))))

(deftest inverse-gaussian-cdf-pdf-consistency-test
  ;; Verifies that inverse-gaussian-cdf and inverse-gaussian-pdf are consistent.
  ;; Uses looser tolerance (2e-3) due to erf approximation in normal-cdf.
  (testing "inverse-gaussian-cdf and inverse-gaussian-pdf"
    (testing "are consistent"
      (let [mu     1.0
            lambda 1.0
            pdf-fn (stats/inverse-gaussian-pdf mu lambda)
            cdf-fn (stats/inverse-gaussian-cdf mu lambda)
            h      1e-6]
        (doseq [^double x [0.5 1.0 2.0 3.0]]
          (testing (str "at x=" x)
            (let [numerical-deriv (/ (- (prim/invoke-dd cdf-fn (+ x h))
                                        (prim/invoke-dd cdf-fn (- x h)))
                                     (* 2.0 h))
                  pdf-value       (pdf-fn x)]
              ;; Looser tolerance due to erf approximation error propagation
              ;; The erf polynomial has max error 1.5e-7 which propagates
              (is (approx= numerical-deriv pdf-value 2e-3)
                  (format "CDF derivative != PDF: deriv=%.10f, pdf=%.10f"
                          numerical-deriv pdf-value)))))))))

;;; Regularized Gamma Function Validation

(deftest regularized-gamma-p-validation-test
  ;; Validates stats.interface/regularized-gamma-p against R's pgamma().
  ;; P(a, x) = pgamma(x, shape=a, scale=1)
  (testing "regularized-gamma-p"
    (if-not (r/r-available?)
      (do
        (println "Skipping regularized-gamma-p validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at various (a, x) values"
          (doseq [[a x] [[1.0 1.0] [2.0 1.0] [0.5 2.0] [3.0 5.0] [5.0 3.0]
                         [10.0 5.0] [1.0 10.0] [0.5 0.1]]]
            (testing (str "at a=" a ", x=" x)
              (let [r-p (first (r/r-eval (format "pgamma(%s, shape=%s, scale=1)"
                                                 x a)))
                    clj-p (stats/regularized-gamma-p a x)]
                ;; Looser tolerance due to series approximation
                (is (approx= r-p clj-p 1e-2)
                    (format "regularized-gamma-p mismatch: R=%.15f, clj=%.15f"
                            r-p clj-p))))))

        (testing "at x=0"
          (is (= 0.0 (stats/regularized-gamma-p 1.0 0.0))
              "P(a, 0) should be 0"))

        (testing "approaches 1 for large x"
          (let [p (stats/regularized-gamma-p 2.0 100.0)]
            (is (> p 0.9999999)
                (format "P(2, 100) should be very close to 1, got: %.15f" p))))))))
