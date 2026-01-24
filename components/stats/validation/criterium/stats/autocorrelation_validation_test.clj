(ns criterium.stats.autocorrelation-validation-test
  "Validation tests for ACF computation against R's acf() function.

  Tests compare criterium's autocorrelation implementation against R
  for various signal types: white noise, AR(1), and periodic signals.

  Also validates Ljung-Box Q statistic against R's Box.test().

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.r-validation.r :as r :refer [vec->r-str]]
   [criterium.stats.autocorrelation :as acf]
   [criterium.stats.chi-squared :as chi-squared]
   [criterium.test.assert :refer [approx=]]))

;;; Test data generation

(defn- generate-white-noise
  "Generate white noise samples with fixed seed for reproducibility."
  [^long n ^long seed]
  (let [rng (java.util.Random. seed)]
    (double-array (repeatedly n #(.nextGaussian rng)))))

(defn- generate-ar1
  "Generate AR(1) process: x_t = phi * x_{t-1} + e_t
  where e_t ~ N(0, 1-phi^2) to maintain unit variance."
  [^long n ^double phi ^long seed]
  (let [rng (java.util.Random. seed)
        samples (double-array n)
        noise-sd (Math/sqrt (- 1.0 (* phi phi)))]
    (aset samples 0 (.nextGaussian rng))
    (dotimes [i (dec n)]
      (aset samples (inc i)
            (+ (* phi (aget samples i))
               (* noise-sd (.nextGaussian rng)))))
    samples))

(defn- generate-periodic
  "Generate periodic signal: sin(2*pi*t/period) + small noise."
  [^long n ^long period ^double noise-sd ^long seed]
  (let [rng (java.util.Random. seed)
        samples (double-array n)]
    (dotimes [i n]
      (aset samples i
            (+ (Math/sin (* 2.0 Math/PI (/ (double i) period)))
               (* noise-sd (.nextGaussian rng)))))
    samples))

;;; Helper functions

(defn- r-acf
  "Compute ACF in R and return as vector.
  R's acf() returns lag 0 (=1) through lag.max.
  Returns ACF for lags 1 to lag-max (excluding lag 0)."
  [samples lag-max]
  (let [r-code (str "x <- " (vec->r-str (vec samples)) ";"
                    "result <- acf(x, lag.max=" lag-max ", plot=FALSE);"
                    "as.vector(result$acf)[-1]")]  ; Remove lag 0
    (r/r-eval r-code)))

(defn- r-ljung-box
  "Compute Ljung-Box test in R.
  Returns [Q-statistic p-value]."
  [samples lag]
  (let [r-code (str "x <- " (vec->r-str (vec samples)) ";"
                    "result <- Box.test(x, lag=" lag ", type='Ljung-Box');"
                    "c(result$statistic, result$p.value)")]
    (r/r-eval r-code)))

;;; ACF Validation Tests

(deftest acf-white-noise-validation-test
  ;; Validates ACF computation for white noise (iid samples).
  ;; For white noise, all ACF values should be near zero.
  ;; We validate that criterium's ACF matches R's acf() exactly.
  (testing "acf"
    (if-not (r/r-available?)
      (do
        (println "Skipping ACF white noise validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with white noise"
        (let [n          100
              samples    (generate-white-noise n 42)
              lag-max    (quot n 2)
              clj-acf    (acf/acf samples)
              r-acf-vals (r-acf samples lag-max)]
          (doseq [^long lag (range 1 (inc (min lag-max (count r-acf-vals))))]
            (let [clj-val (get clj-acf lag)
                  r-val   (nth r-acf-vals (dec lag))]
              (is (approx= r-val clj-val 1e-6)
                  (format "ACF mismatch at lag %d: R=%.10f, clj=%.10f"
                          lag r-val clj-val)))))))))

(deftest acf-ar1-validation-test
  ;; Validates ACF computation for AR(1) process.
  ;; Theoretical ACF for AR(1) is phi^k at lag k.
  ;; We validate that criterium's ACF matches R's acf().
  (testing "acf"
    (if-not (r/r-available?)
      (do
        (println "Skipping ACF AR(1) validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with AR(1) process"
        (doseq [phi [0.3 0.7 0.9]]
          (testing (str "phi=" phi)
            (let [n          200
                  samples    (generate-ar1 n phi 123)
                  lag-max    (quot n 2)
                  clj-acf    (acf/acf samples)
                  r-acf-vals (r-acf samples lag-max)
                  ;; Test first 20 lags (most relevant for AR(1))
                  test-lags  (min 20 (count r-acf-vals))]
              (doseq [^long lag (range 1 (inc test-lags))]
                (let [clj-val (get clj-acf lag)
                      r-val   (nth r-acf-vals (dec lag))]
                  (is (approx= r-val clj-val 1e-6)
                      (format "ACF mismatch at lag %d (phi=%.1f): R=%.10f, clj=%.10f"
                              lag phi r-val clj-val)))))))))))

(deftest acf-periodic-validation-test
  ;; Validates ACF computation for periodic signals.
  ;; Periodic signal should show ACF peak at the period.
  (testing "acf"
    (if-not (r/r-available?)
      (do
        (println "Skipping ACF periodic validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with periodic signal"
        (let [n          100
              period     10
              samples    (generate-periodic n period 0.1 456)
              lag-max    (quot n 2)
              clj-acf    (acf/acf samples)
              r-acf-vals (r-acf samples lag-max)
              ;; Test lags around and including the period
              test-lags  (min 25 (count r-acf-vals))]
          (doseq [^long lag (range 1 (inc test-lags))]
            (let [clj-val (get clj-acf lag)
                  r-val   (nth r-acf-vals (dec lag))]
              (is (approx= r-val clj-val 1e-6)
                  (format "ACF mismatch at lag %d: R=%.10f, clj=%.10f"
                          lag r-val clj-val)))))))))

(deftest acf-larger-sample-validation-test
  ;; Validates ACF with larger sample size for more precision.
  (testing "acf"
    (if-not (r/r-available?)
      (do
        (println "Skipping ACF larger sample validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with larger sample (n=500)"
        (let [n          500
              samples    (generate-white-noise n 789)
              lag-max    50  ; Test first 50 lags
              clj-acf    (acf/acf samples)
              r-acf-vals (r-acf samples lag-max)]
          (doseq [^long lag (range 1 (inc (min lag-max (count r-acf-vals))))]
            (let [clj-val (get clj-acf lag)
                  r-val   (nth r-acf-vals (dec lag))]
              (is (approx= r-val clj-val 1e-6)
                  (format "ACF mismatch at lag %d: R=%.10f, clj=%.10f"
                          lag r-val clj-val)))))))))

;;; Ljung-Box Validation Tests

(deftest ljung-box-white-noise-validation-test
  ;; Validates Ljung-Box Q statistic against R's Box.test() for white noise.
  ;; White noise should have high p-value (no significant autocorrelation).
  (testing "ljung-box"
    (if-not (r/r-available?)
      (do
        (println "Skipping Ljung-Box white noise validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with white noise"
        (let [n 100
              samples (generate-white-noise n 42)
              clj-acf (acf/acf samples)
              clj-lb (acf/ljung-box clj-acf n)
              ;; R's Box.test uses h lags, matching our min(20, n/4)
              h (:df clj-lb)
              [r-q r-p] (r-ljung-box samples h)]
          (is (approx= r-q (:q-statistic clj-lb) 1e-6)
              (format "Q-statistic mismatch: R=%.10f, clj=%.10f"
                      r-q (:q-statistic clj-lb)))
          (is (approx= r-p (:p-value clj-lb) 1e-6)
              (format "p-value mismatch: R=%.10f, clj=%.10f"
                      r-p (:p-value clj-lb))))))))

(deftest ljung-box-ar1-validation-test
  ;; Validates Ljung-Box Q statistic against R's Box.test() for AR(1).
  ;; AR(1) should have low p-value (significant autocorrelation).
  (testing "ljung-box"
    (if-not (r/r-available?)
      (do
        (println "Skipping Ljung-Box AR(1) validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with AR(1) process"
        (doseq [phi [0.5 0.7]]
          (testing (str "phi=" phi)
            (let [n 200
                  samples (generate-ar1 n phi 123)
                  clj-acf (acf/acf samples)
                  clj-lb (acf/ljung-box clj-acf n)
                  h (:df clj-lb)
                  [r-q r-p] (r-ljung-box samples h)]
              (is (approx= r-q (:q-statistic clj-lb) 1e-6)
                  (format "Q-statistic mismatch (phi=%.1f): R=%.10f, clj=%.10f"
                          phi r-q (:q-statistic clj-lb)))
              (is (approx= r-p (:p-value clj-lb) 1e-6)
                  (format "p-value mismatch (phi=%.1f): R=%.10f, clj=%.10f"
                          phi r-p (:p-value clj-lb))))))))))

(deftest ljung-box-periodic-validation-test
  ;; Validates Ljung-Box Q statistic for periodic signal.
  (testing "ljung-box"
    (if-not (r/r-available?)
      (do
        (println "Skipping Ljung-Box periodic validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with periodic signal"
        (let [n 100
              period 10
              samples (generate-periodic n period 0.1 456)
              clj-acf (acf/acf samples)
              clj-lb (acf/ljung-box clj-acf n)
              h (:df clj-lb)
              [r-q r-p] (r-ljung-box samples h)]
          (is (approx= r-q (:q-statistic clj-lb) 1e-6)
              (format "Q-statistic mismatch: R=%.10f, clj=%.10f"
                      r-q (:q-statistic clj-lb)))
          (is (approx= r-p (:p-value clj-lb) 1e-6)
              (format "p-value mismatch: R=%.10f, clj=%.10f"
                      r-p (:p-value clj-lb))))))))

(deftest ljung-box-various-lags-validation-test
  ;; Validates Ljung-Box with various lag values.
  (testing "ljung-box"
    (if-not (r/r-available?)
      (do
        (println "Skipping Ljung-Box various lags validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (testing "with various lag values"
        (let [n       200
              samples (generate-ar1 n 0.5 999)
              clj-acf (acf/acf samples)]
          (doseq [^long h [5 10 15 20]]
            (testing (str "h=" h)
              (let [;; Manually compute Ljung-Box with specific h
                    n+2       (+ n 2)
                    sum       (double
                               (loop [k 1, acc 0.0]
                                 (if (> k h)
                                   acc
                                   (let [rk (double (get clj-acf k 0.0))]
                                     (recur (inc k)
                                            (+ acc
                                               (/ (* rk rk)
                                                  (- n k))))))))
                    clj-q     (* (double n) (double n+2) sum)
                    clj-p     (- 1.0 (chi-squared/cdf clj-q h))
                    [r-q r-p] (r-ljung-box samples h)]
                (is (approx= r-q clj-q 1e-6)
                    (format "Q-statistic mismatch (h=%d): R=%.10f, clj=%.10f"
                            h r-q clj-q))
                (is (approx= r-p clj-p 1e-6)
                    (format "p-value mismatch (h=%d): R=%.10f, clj=%.10f"
                            h r-p clj-p))))))))))
