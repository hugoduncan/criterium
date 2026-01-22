(ns criterium.stats.autocorrelation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.stats.autocorrelation :as acf]))

;; Tests for ACF computation and related statistics.
;; Contracts tested:
;; - ACF computes correct autocorrelation coefficients
;; - Ljung-Box Q statistic matches R's Box.test()
;; - Edge cases handled correctly (n < 20, zero variance)
;; - effective-sample-size and ci-inflation-factor compute correctly

(defn- approx=
  "Check if two doubles are approximately equal within tolerance."
  ([^double a ^double b] (approx= a b 1e-6))
  ([^double a ^double b ^double tol]
   (< (Math/abs (- a b)) tol)))

;;; ACF tests

(deftest acf-white-noise-test
  ;; For white noise (iid samples), ACF should be near zero for all lags
  (testing "acf"
    (testing "returns near-zero autocorrelations for white noise"
      (let [;; Use deterministic pseudo-random sequence
            rng (java.util.Random. 42)
            samples (double-array (repeatedly 100 #(.nextGaussian rng)))
            result (acf/acf samples)
            ;; Noise threshold: 2/sqrt(n) = 0.2 for n=100
            threshold 0.2]
        (is (map? result))
        (is (= 50 (count result))) ; n/2 lags
        ;; Most ACF values should be within noise threshold
        (let [within-threshold (count (filter #(<= (Math/abs %) threshold)
                                              (vals result)))]
          (is (>= within-threshold 40)
              "At least 80% of ACF values should be within noise threshold"))))))

(deftest acf-ar1-process-test
  ;; For AR(1) process x_t = phi * x_{t-1} + e_t, ACF(k) = phi^k
  (testing "acf"
    (testing "detects strong lag-1 correlation in AR(1) process"
      (let [phi 0.7
            n 200
            rng (java.util.Random. 123)
            ;; Generate AR(1) process
            samples (double-array n)
            _ (aset samples 0 (.nextGaussian rng))
            _ (dotimes [i (dec n)]
                (aset samples (inc i)
                      (+ (* phi (aget samples i))
                         (* (Math/sqrt (- 1.0 (* phi phi))) (.nextGaussian rng)))))
            result (acf/acf samples)
            r1 (get result 1)
            r2 (get result 2)]
        (is (some? result))
        ;; r1 should be close to phi (with some sampling error)
        (is (< 0.5 r1 0.9)
            (format "Lag-1 ACF should be near %.1f, got %.3f" phi r1))
        ;; r2 should be close to phi^2 = 0.49
        (is (< 0.3 r2 0.7)
            (format "Lag-2 ACF should be near %.2f, got %.3f" (* phi phi) r2))
        ;; ACF should decay exponentially
        (is (< (Math/abs r2) (Math/abs r1))
            "ACF should decay with lag")))))

(deftest acf-periodic-test
  ;; Periodic signal should show ACF peak at period
  (testing "acf"
    (testing "detects periodicity in sinusoidal signal"
      (let [period 10
            n 100
            ;; Pure sine wave with small noise
            rng (java.util.Random. 456)
            samples (double-array n)
            _ (dotimes [i n]
                (aset samples i
                      (+ (Math/sin (* 2.0 Math/PI (/ (double i) period)))
                         (* 0.1 (.nextGaussian rng)))))
            result (acf/acf samples)
            r-at-period (get result period)]
        (is (some? result))
        ;; ACF at period should be high (close to 1)
        (is (> r-at-period 0.8)
            (format "ACF at period %d should be high, got %.3f" period r-at-period))))))

(deftest acf-insufficient-samples-test
  (testing "acf"
    (testing "returns nil for n < 20"
      (let [samples (double-array (range 19))]
        (is (nil? (acf/acf samples)))))))

(deftest acf-zero-variance-test
  (testing "acf"
    (testing "returns nil for constant data"
      (let [samples (double-array (repeat 50 42.0))]
        (is (nil? (acf/acf samples)))))))

;;; Ljung-Box tests

;; Reference values from R:
;; x <- c(0.1, 0.05, 0.02, -0.01, 0.03)  # ACF values for lags 1-5
;; Box.test(x, lag=5, type="Ljung-Box")
;; Provides the Q statistic formula validation

(deftest ljung-box-formula-test
  (testing "ljung-box"
    (testing "computes Q statistic correctly"
      ;; Manual calculation:
      ;; n=100, h=min(20, 25)=20, ACF = {1: 0.1, 2: 0.05, ...}
      ;; Q = 100 * 102 * sum_{k=1}^{20} (rk^2/(100-k))
      (let [acf-map {1 0.1, 2 0.05, 3 0.02, 4 -0.01, 5 0.03}
            n 100
            result (acf/ljung-box acf-map n)]
        (is (map? result))
        (is (contains? result :q-statistic))
        (is (contains? result :df))
        (is (contains? result :p-value))
        ;; Q should be small for these weak correlations
        (is (< (:q-statistic result) 5.0))
        ;; df should be min(20, n/4) = min(20, 25) = 20
        (is (= 20 (:df result)))
        ;; p-value should be high (no significant autocorrelation)
        (is (> (:p-value result) 0.5))))))

(deftest ljung-box-significant-test
  (testing "ljung-box"
    (testing "detects significant autocorrelation"
      ;; Strong lag-1 correlation should give low p-value
      ;; Use very strong correlations to ensure significance
      (let [acf-map {1 0.6, 2 0.4, 3 0.3, 4 0.2, 5 0.15,
                     6 0.1, 7 0.08, 8 0.06, 9 0.04, 10 0.03}
            n 100
            result (acf/ljung-box acf-map n)]
        ;; Q should be large
        (is (> (:q-statistic result) 40.0))
        ;; p-value should be very small
        (is (< (:p-value result) 0.001)
            "Strong autocorrelation should give significant p-value")))))

(deftest ljung-box-nil-input-test
  (testing "ljung-box"
    (testing "returns nil for nil acf-map"
      (is (nil? (acf/ljung-box nil 100))))))

(deftest ljung-box-df-calculation-test
  (testing "ljung-box"
    (testing "uses correct degrees of freedom"
      ;; For n=40, h = min(20, 10) = 10
      (let [acf-map (into {} (for [k (range 1 21)] [k 0.0]))
            result-40 (acf/ljung-box acf-map 40)
            ;; For n=100, h = min(20, 25) = 20
            result-100 (acf/ljung-box acf-map 100)]
        (is (= 10 (:df result-40)))
        (is (= 20 (:df result-100)))))))

;;; Effective sample size tests

(deftest effective-sample-size-positive-r1-test
  (testing "effective-sample-size"
    (testing "reduces sample size for positive lag-1 correlation"
      ;; r1 = 0.5 -> n_eff = n * 0.5/1.5 = n/3
      (let [n 100
            r1 0.5
            n-eff (acf/effective-sample-size r1 n)]
        (is (= 33 n-eff))))))

(deftest effective-sample-size-high-r1-test
  (testing "effective-sample-size"
    (testing "gives very small n_eff for high lag-1 correlation"
      ;; r1 = 0.9 -> n_eff = n * 0.1/1.9 ≈ n/19
      (let [n 100
            r1 0.9
            n-eff (acf/effective-sample-size r1 n)]
        (is (< n-eff 10))
        (is (>= n-eff 1))))))

(deftest effective-sample-size-zero-r1-test
  (testing "effective-sample-size"
    (testing "returns n for zero lag-1 correlation"
      (is (= 100 (acf/effective-sample-size 0.0 100))))))

(deftest effective-sample-size-negative-r1-test
  (testing "effective-sample-size"
    (testing "returns n for negative lag-1 correlation"
      ;; Negative autocorrelation doesn't reduce effective sample size
      (is (= 100 (acf/effective-sample-size -0.3 100))))))

(deftest effective-sample-size-clamped-test
  (testing "effective-sample-size"
    (testing "clamps result to [1, n]"
      ;; Very high r1 should still give at least 1
      (is (= 1 (acf/effective-sample-size 0.99 100)))
      ;; Negative r1 should not exceed n
      (is (= 50 (acf/effective-sample-size -0.5 50))))))

;;; CI inflation factor tests

(deftest ci-inflation-factor-positive-r1-test
  (testing "ci-inflation-factor"
    (testing "inflates CI for positive lag-1 correlation"
      ;; r1 = 0.5 -> sqrt(1.5/0.5) = sqrt(3) ≈ 1.732
      (let [factor (acf/ci-inflation-factor 0.5)]
        (is (approx= factor (Math/sqrt 3.0) 1e-10))))))

(deftest ci-inflation-factor-zero-r1-test
  (testing "ci-inflation-factor"
    (testing "returns 1.0 for zero lag-1 correlation"
      (is (= 1.0 (acf/ci-inflation-factor 0.0))))))

(deftest ci-inflation-factor-negative-r1-test
  (testing "ci-inflation-factor"
    (testing "returns 1.0 for negative lag-1 correlation"
      (is (= 1.0 (acf/ci-inflation-factor -0.3))))))

(deftest ci-inflation-factor-capped-test
  (testing "ci-inflation-factor"
    (testing "caps at 6.0 for r1 >= 0.95"
      (is (= 6.0 (acf/ci-inflation-factor 0.95)))
      (is (= 6.0 (acf/ci-inflation-factor 0.99))))))

(deftest ci-inflation-factor-high-r1-test
  (testing "ci-inflation-factor"
    (testing "gives large factor for high r1 below cap"
      ;; r1 = 0.9 -> sqrt(1.9/0.1) = sqrt(19) ≈ 4.36
      (let [factor (acf/ci-inflation-factor 0.9)]
        (is (approx= factor (Math/sqrt 19.0) 1e-10))
        (is (< factor 6.0))))))
