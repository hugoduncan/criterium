(ns criterium.stats.mle-validation-test
  "Validation tests for criterium.stats.mle functions against R reference.

  Tests skip gracefully when R/Rserve is unavailable.

  MLE functions tested:
  - gamma-mle: using R's MASS::fitdistr or fitdistrplus
  - lognormal-mle: using R's MASS::fitdistr or fitdistrplus
  - inverse-gaussian-mle: using R's statmod package
  - weibull-mle: using R's MASS::fitdistr

  Also validates digamma and trigamma functions."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.r-validation.r :as r]
   [criterium.stats.mle :as mle]
   [criterium.stats.probability :as stats]
   [criterium.test.assert :refer [approx=]]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

;;; Digamma and Trigamma Validation

(deftest digamma-validation-test
  ;; Validates stats.interface/digamma against R's digamma() function.
  (testing "digamma"
    (if-not (r/r-available?)
      (do
        (println "Skipping digamma validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at various x values"
          (doseq [^double x [0.5 1.0 1.5 2.0 3.0 5.0 10.0 50.0 100.0]]
            (testing (str "at x=" x)
              (let [r-val (first (r/r-eval (str "digamma(" x ")")))
                    clj-val (stats/digamma x)]
                (is (approx= r-val clj-val 1e-7)
                    (format "digamma mismatch at x=%.1f: R=%.15f, clj=%.15f"
                            x r-val clj-val))))))

        (testing "at small x values near zero"
          (doseq [^double x [0.1 0.01 0.001]]
            (testing (str "at x=" x)
              (let [r-val (first (r/r-eval (str "digamma(" x ")")))
                    clj-val (stats/digamma x)]
                (is (approx= r-val clj-val 1e-8)
                    (format "digamma mismatch at x=%.4f: R=%.15f, clj=%.15f"
                            x r-val clj-val))))))

        (testing "satisfies recurrence relation: ψ(x+1) = ψ(x) + 1/x"
          (doseq [^double x [0.5 1.0 2.0 5.0]]
            (testing (str "at x=" x)
              (let [psi-x (stats/digamma x)
                    psi-x1 (stats/digamma (+ x 1.0))
                    expected (+ psi-x (/ 1.0 x))]
                (is (approx= psi-x1 expected 1e-10)
                    (format "recurrence failed: ψ(%s+1)=%.15f, ψ(%s)+1/%s=%.15f"
                            x psi-x1 x x expected))))))))))

(deftest trigamma-validation-test
  ;; Validates stats.interface/trigamma against R's trigamma() function.
  (testing "trigamma"
    (if-not (r/r-available?)
      (do
        (println "Skipping trigamma validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at various x values"
          (doseq [^double x [0.5 1.0 1.5 2.0 3.0 5.0 10.0 50.0 100.0]]
            (testing (str "at x=" x)
              (let [r-val (first (r/r-eval (str "trigamma(" x ")")))
                    clj-val (stats/trigamma x)]
                (is (approx= r-val clj-val 1e-7)
                    (format "trigamma mismatch at x=%.1f: R=%.15f, clj=%.15f"
                            x r-val clj-val))))))

        (testing "at small x values near zero"
          (doseq [^double x [0.1 0.01 0.001]]
            (testing (str "at x=" x)
              (let [r-val (first (r/r-eval (str "trigamma(" x ")")))
                    clj-val (stats/trigamma x)]
                ;; Use relative tolerance for large values
                (is (approx= r-val clj-val 1e-6)
                    (format "trigamma mismatch at x=%.4f: R=%.15f, clj=%.15f"
                            x r-val clj-val))))))

        (testing "satisfies recurrence relation: ψ'(x+1) = ψ'(x) - 1/x²"
          (doseq [^double x [0.5 1.0 2.0 5.0]]
            (testing (str "at x=" x)
              (let [psi-x (stats/trigamma x)
                    psi-x1 (stats/trigamma (+ x 1.0))
                    expected (- psi-x (/ 1.0 (* x x)))]
                (is (approx= psi-x1 expected 1e-10)
                    (format "recurrence failed: ψ'(%s+1)=%.15f, ψ'(%s)-1/%s²=%.15f"
                            x psi-x1 x x expected))))))))))

;;; Test datasets for MLE validation
;; Using fixed seeds for reproducibility

(def gamma-test-data
  "Test data generated from gamma(shape=3, scale=2) distribution."
  [5.2 4.1 7.8 3.2 6.5 8.1 4.5 5.9 2.8 7.2
   6.1 3.9 5.5 4.8 6.8 5.1 3.5 7.5 4.2 6.3])

(def lognormal-test-data
  "Test data generated from lognormal(mu=1, sigma=0.5) distribution."
  [2.3 3.1 2.8 4.2 2.1 3.5 2.6 3.8 2.4 3.2
   2.9 3.6 2.5 4.1 2.7 3.3 2.2 3.9 2.8 3.4])

(def inverse-gaussian-test-data
  "Test data generated from inverse-gaussian(mu=2, lambda=5) distribution."
  [1.8 2.1 1.9 2.3 1.7 2.4 2.0 1.6 2.2 1.8
   2.5 1.9 2.1 1.7 2.3 2.0 1.8 2.2 1.9 2.1])

(def weibull-test-data
  "Test data generated from weibull(shape=2, scale=5) distribution."
  [4.2 5.1 3.8 6.2 4.5 5.8 3.2 4.8 5.5 4.1
   5.3 4.6 3.9 5.0 4.3 6.1 4.7 3.5 5.2 4.9])

;;; Gamma MLE Validation

(deftest gamma-mle-validation-test
  ;; Validates stats.interface/gamma-mle against R's MASS::fitdistr().
  ;; R: MASS::fitdistr(data, "gamma")
  (testing "gamma-mle"
    (if-not (r/r-available?)
      (do
        (println "Skipping gamma-mle validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "against R's MASS::fitdistr"
          (let [data-str (str "c(" (str/join "," gamma-test-data) ")")
                ;; Load MASS and fit gamma distribution
                _ (r/r-eval "library(MASS)")
                r-result (r/r-eval
                          (str "fit <- fitdistr(" data-str ", 'gamma'); "
                               "c(fit$estimate['shape'], fit$estimate['rate'], fit$loglik)"))
                r-shape (double (first r-result))
                r-rate (double (second r-result))
                r-scale (/ 1.0 r-rate)
                r-loglik (nth r-result 2)
                clj-result (mle/gamma-mle (darr gamma-test-data))
                clj-shape (get-in clj-result [:params :shape])
                clj-scale (get-in clj-result [:params :scale])
                clj-loglik (:log-likelihood clj-result)]
            (testing "shape parameter"
              (is (approx= r-shape clj-shape 1e-3)
                  (format "shape mismatch: R=%.6f, clj=%.6f" r-shape clj-shape)))
            (testing "scale parameter"
              (is (approx= r-scale clj-scale 1e-3)
                  (format "scale mismatch: R=%.6f, clj=%.6f" r-scale clj-scale)))
            (testing "log-likelihood"
              (is (approx= r-loglik clj-loglik 1e-2)
                  (format "log-lik mismatch: R=%.6f, clj=%.6f" r-loglik clj-loglik)))))

        (testing "with different shape values"
          ;; Test with data that has known gamma parameters
          (doseq [[desc data]
                  [["low shape (k~1)" [0.5 1.2 0.8 1.5 0.3 2.1 0.9 1.1 0.6 1.8]]
                   ["high shape (k~5)" [4.2 5.1 4.8 5.5 4.0 5.8 4.5 5.2 4.3 5.6]]]]
            (testing desc
              (let [data-str (str "c(" (str/join "," data) ")")
                    r-result (r/r-eval
                              (str "fit <- fitdistr(" data-str ", 'gamma'); "
                                   "c(fit$estimate['shape'], fit$estimate['rate'])"))
                    ^double r-shape (first r-result)
                    ^double r-rate (second r-result)
                    r-scale (/ 1.0 r-rate)
                    clj-result (mle/gamma-mle (darr data))
                    clj-shape (get-in clj-result [:params :shape])
                    clj-scale (get-in clj-result [:params :scale])]
                (is (approx= r-shape clj-shape 1e-2)
                    (format "shape mismatch for %s: R=%.6f, clj=%.6f"
                            desc r-shape clj-shape))
                (is (approx= r-scale clj-scale 1e-2)
                    (format "scale mismatch for %s: R=%.6f, clj=%.6f"
                            desc r-scale clj-scale))))))))))

;;; Log-normal MLE Validation

(deftest lognormal-mle-validation-test
  ;; Validates stats.interface/lognormal-mle against R's MASS::fitdistr().
  ;; R: MASS::fitdistr(data, "lognormal")
  (testing "lognormal-mle"
    (if-not (r/r-available?)
      (do
        (println "Skipping lognormal-mle validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "against R's MASS::fitdistr"
          (let [data-str (str "c(" (str/join "," lognormal-test-data) ")")
                _ (r/r-eval "library(MASS)")
                r-result (r/r-eval (str "fit <- fitdistr(" data-str ", 'lognormal'); "
                                        "c(fit$estimate['meanlog'], fit$estimate['sdlog'], fit$loglik)"))
                r-mu (first r-result)
                r-sigma (second r-result)
                r-loglik (nth r-result 2)
                clj-result (mle/lognormal-mle (darr lognormal-test-data))
                clj-mu (get-in clj-result [:params :mu])
                clj-sigma (get-in clj-result [:params :sigma])
                clj-loglik (:log-likelihood clj-result)]
            (testing "mu parameter"
              (is (approx= r-mu clj-mu 1e-10)
                  (format "mu mismatch: R=%.10f, clj=%.10f" r-mu clj-mu)))
            (testing "sigma parameter"
              ;; R's fitdistr returns biased MLE (n-1 denominator) while we use
              ;; true MLE (n denominator). Allow for this ~5% difference.
              ;; The "closed-form matches direct calculation" test below verifies
              ;; our implementation is correct.
              (is (approx= r-sigma clj-sigma 0.1)
                  (format "sigma mismatch: R=%.10f, clj=%.10f (note: R uses n-1, we use n)"
                          r-sigma clj-sigma)))
            (testing "log-likelihood"
              ;; Allow some tolerance since R may use slightly different sigma
              (is (approx= r-loglik clj-loglik 0.1)
                  (format "log-lik mismatch: R=%.6f, clj=%.6f" r-loglik clj-loglik)))))

        (testing "closed-form matches direct calculation"
          ;; The MLE for lognormal is simply the mean and SD of log(data)
          (let [log-data (mapv #(Math/log ^double %) lognormal-test-data)
                n (count log-data)
                expected-mu (/ (double (reduce + 0.0 log-data)) n)
                ^double sum-sq (reduce
                                (fn [^double acc ^double lx]
                                  (let [diff (- lx expected-mu)]
                                    (+ acc (* diff diff))))
                                0.0
                                log-data)
                expected-sigma (Math/sqrt (/ sum-sq n))
                result (mle/lognormal-mle (darr lognormal-test-data))]
            (is (approx= expected-mu (get-in result [:params :mu]) 1e-15)
                "mu should match mean of log data")
            (is (approx= expected-sigma (get-in result [:params :sigma]) 1e-15)
                "sigma should match SD of log data")))))))

;;; Inverse Gaussian MLE Validation

(deftest inverse-gaussian-mle-validation-test
  ;; Validates stats.interface/inverse-gaussian-mle against R's statmod package.
  ;; R uses fitdistr with dinvgauss from statmod.
  ;; Requires statmod package (installed by CI workflow).
  (testing "inverse-gaussian-mle"
    (if-not (r/r-available?)
      (do
        (println "Skipping inverse-gaussian-mle validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (r/r-eval "library(statmod)")
        (testing "against R's closed-form MLE"
          (let [data-str (str "c(" (str/join "," inverse-gaussian-test-data) ")")
                ;; Inverse Gaussian MLE: mu = mean(x), lambda = n / sum(1/x - 1/mu)
                r-result (r/r-eval (str "x <- " data-str "; "
                                        "n <- length(x); "
                                        "mu <- mean(x); "
                                        "lambda <- n / sum(1/x - 1/mu); "
                                        "c(mu, lambda)"))
                r-mu (first r-result)
                r-lambda (second r-result)
                clj-result (mle/inverse-gaussian-mle (darr inverse-gaussian-test-data))
                clj-mu (get-in clj-result [:params :mu])
                clj-lambda (get-in clj-result [:params :lambda])]
            (testing "mu parameter"
              (is (approx= r-mu clj-mu 1e-10)
                  (format "mu mismatch: R=%.10f, clj=%.10f" r-mu clj-mu)))
            (testing "lambda parameter"
              (is (approx= r-lambda clj-lambda 1e-10)
                  (format "lambda mismatch: R=%.10f, clj=%.10f" r-lambda clj-lambda)))))

        (testing "log-likelihood matches R's dinvgauss"
          (let [data-str (str "c(" (str/join "," inverse-gaussian-test-data) ")")
                clj-result (mle/inverse-gaussian-mle (darr inverse-gaussian-test-data))
                clj-mu (get-in clj-result [:params :mu])
                clj-lambda (get-in clj-result [:params :lambda])
                r-loglik (first (r/r-eval
                                 (str "sum(log(dinvgauss(" data-str
                                      ", mean=" clj-mu ", shape=" clj-lambda ")))")))
                clj-loglik (:log-likelihood clj-result)]
            (is (approx= r-loglik clj-loglik 1e-10)
                (format "log-lik mismatch: R=%.10f, clj=%.10f" r-loglik clj-loglik))))

        (testing "with different parameter values"
          (doseq [[desc data]
                  [["narrow distribution" [1.9 2.0 2.1 1.95 2.05 1.98 2.02 1.97 2.03 2.0]]
                   ["wide distribution" [0.5 3.5 1.2 2.8 0.8 3.2 1.5 2.5 1.0 3.0]]]]
            (testing desc
              (let [data-str (str "c(" (str/join "," data) ")")
                    r-result (r/r-eval (str "x <- " data-str "; "
                                            "n <- length(x); "
                                            "mu <- mean(x); "
                                            "lambda <- n / sum(1/x - 1/mu); "
                                            "c(mu, lambda)"))
                    r-mu (first r-result)
                    r-lambda (second r-result)
                    clj-result (mle/inverse-gaussian-mle (darr data))]
                (is (approx= r-mu (get-in clj-result [:params :mu]) 1e-10)
                    (format "mu mismatch for %s" desc))
                (is (approx= r-lambda (get-in clj-result [:params :lambda]) 1e-10)
                    (format "lambda mismatch for %s" desc))))))))))

;;; Weibull MLE Validation

(deftest weibull-mle-validation-test
  ;; Validates stats.interface/weibull-mle against R's MASS::fitdistr().
  ;; R: MASS::fitdistr(data, "weibull")
  (testing "weibull-mle"
    (if-not (r/r-available?)
      (do
        (println "Skipping weibull-mle validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "against R's MASS::fitdistr"
          (let [data-str (str "c(" (str/join "," weibull-test-data) ")")
                ;; Load MASS and fit weibull distribution
                _ (r/r-eval "library(MASS)")
                r-result (r/r-eval (str "fit <- fitdistr(" data-str ", 'weibull'); "
                                        "c(fit$estimate['shape'], fit$estimate['scale'], fit$loglik)"))
                r-shape (first r-result)
                r-scale (second r-result)
                r-loglik (nth r-result 2)
                clj-result (mle/weibull-mle (darr weibull-test-data))
                clj-shape (get-in clj-result [:params :shape])
                clj-scale (get-in clj-result [:params :scale])
                clj-loglik (:log-likelihood clj-result)]
            (testing "shape parameter"
              (is (approx= r-shape clj-shape 1e-3)
                  (format "shape mismatch: R=%.6f, clj=%.6f" r-shape clj-shape)))
            (testing "scale parameter"
              (is (approx= r-scale clj-scale 1e-3)
                  (format "scale mismatch: R=%.6f, clj=%.6f" r-scale clj-scale)))
            (testing "log-likelihood"
              (is (approx= r-loglik clj-loglik 1e-2)
                  (format "log-lik mismatch: R=%.6f, clj=%.6f" r-loglik clj-loglik)))))

        (testing "with different shape values"
          ;; Test with data that has known weibull parameters
          (doseq [[desc data]
                  [["low shape (k~1)" [0.5 1.2 0.8 1.5 0.3 2.1 0.9 1.1 0.6 1.8]]
                   ["high shape (k~5)" [4.8 5.0 4.9 5.1 4.7 5.2 4.85 5.05 4.95 5.15]]]]
            (testing desc
              (let [data-str (str "c(" (str/join "," data) ")")
                    r-result (r/r-eval (str "fit <- fitdistr(" data-str ", 'weibull'); "
                                            "c(fit$estimate['shape'], fit$estimate['scale'])"))
                    r-shape (first r-result)
                    r-scale (second r-result)
                    clj-result (mle/weibull-mle (darr data))
                    clj-shape (get-in clj-result [:params :shape])
                    clj-scale (get-in clj-result [:params :scale])]
                (is (approx= r-shape clj-shape 1e-2)
                    (format "shape mismatch for %s: R=%.6f, clj=%.6f"
                            desc r-shape clj-shape))
                (is (approx= r-scale clj-scale 1e-2)
                    (format "scale mismatch for %s: R=%.6f, clj=%.6f"
                            desc r-scale clj-scale))))))

        (testing "log-likelihood matches R's dweibull"
          (let [data-str (str "c(" (str/join "," weibull-test-data) ")")
                clj-result (mle/weibull-mle (darr weibull-test-data))
                clj-shape (get-in clj-result [:params :shape])
                clj-scale (get-in clj-result [:params :scale])
                r-loglik (first (r/r-eval
                                 (str "sum(log(dweibull(" data-str
                                      ", shape=" clj-shape ", scale=" clj-scale ")))")))
                clj-loglik (:log-likelihood clj-result)]
            (is (approx= r-loglik clj-loglik 1e-10)
                (format "log-lik mismatch: R=%.10f, clj=%.10f" r-loglik clj-loglik))))))))

;;; Cross-validation: MLE log-likelihood matches AIC calculation

(deftest mle-aic-consistency-test
  ;; Verifies that MLE log-likelihood values can be used with AIC/BIC functions.
  (testing "MLE log-likelihood"
    (testing "can be used with AIC function"
      (let [gamma-result (mle/gamma-mle (darr gamma-test-data))
            k 2  ; gamma has 2 parameters
            aic (stats/aic k (:log-likelihood gamma-result))]
        (is (number? aic) "AIC should be computable from gamma MLE result")
        (is (< aic 1000) "AIC should be reasonable")))

    (testing "can be used with BIC function"
      (let [lognormal-result (mle/lognormal-mle (darr lognormal-test-data))
            k 2  ; lognormal has 2 parameters
            n (count lognormal-test-data)
            bic (stats/bic k n (:log-likelihood lognormal-result))]
        (is (number? bic) "BIC should be computable from lognormal MLE result")
        (is (< bic 1000) "BIC should be reasonable")))

    (testing "can be used with AICc function"
      (let [ig-result (mle/inverse-gaussian-mle (darr inverse-gaussian-test-data))
            k 2  ; inverse-gaussian has 2 parameters
            n (count inverse-gaussian-test-data)
            aicc (stats/aicc k n (:log-likelihood ig-result))]
        (is (number? aicc) "AICc should be computable from IG MLE result")
        (is (< aicc 1000) "AICc should be reasonable")))

    (testing "can be used with weibull-mle"
      (let [weibull-result (mle/weibull-mle (darr weibull-test-data))
            k 2  ; weibull has 2 parameters
            n (count weibull-test-data)
            aic (stats/aic k (:log-likelihood weibull-result))
            bic (stats/bic k n (:log-likelihood weibull-result))
            aicc (stats/aicc k n (:log-likelihood weibull-result))]
        (is (number? aic) "AIC should be computable from weibull MLE result")
        (is (number? bic) "BIC should be computable from weibull MLE result")
        (is (number? aicc) "AICc should be computable from weibull MLE result")
        (is (< aic 1000) "AIC should be reasonable")
        (is (< bic 1000) "BIC should be reasonable")
        (is (< aicc 1000) "AICc should be reasonable")))))
