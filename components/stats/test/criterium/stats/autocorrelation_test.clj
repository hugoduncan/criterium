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
        (let [within-threshold (count (filter #(<= (Math/abs ^double %) threshold)
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
        (is (< (Math/abs ^double r2) (Math/abs ^double r1))
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

;;; Noise floor tests

(deftest noise-floor-test
  ;; Tests noise floor calculation: 2/√n
  ;; Contracts: returns correct threshold for various sample sizes
  (testing "noise-floor"
    (testing "computes 2/sqrt(n) correctly"
      (is (approx= (acf/noise-floor 100) 0.2))
      (is (approx= (acf/noise-floor 400) 0.1))
      (is (approx= (acf/noise-floor 25) 0.4)))))

;;; Severity classification tests

(deftest lag-1-severity-test
  ;; Tests lag-1 severity classification
  ;; Contracts: thresholds at 0.10, 0.20, 0.35 (above noise floor)
  ;; Use n=400 where floor=0.1, so thresholds are at their nominal values
  (testing "lag-1-severity"
    (testing "classifies positive values"
      (is (= :none (acf/lag-1-severity 0.05 400)))
      (is (= :minor (acf/lag-1-severity 0.15 400)))
      (is (= :moderate (acf/lag-1-severity 0.25 400)))
      (is (= :severe (acf/lag-1-severity 0.40 400))))

    (testing "classifies negative values with alternating prefix"
      (is (= :alternating-none (acf/lag-1-severity -0.05 400)))
      (is (= :alternating-minor (acf/lag-1-severity -0.15 400)))
      (is (= :alternating-moderate (acf/lag-1-severity -0.30 400)))
      (is (= :alternating-severe (acf/lag-1-severity -0.50 400))))

    (testing "uses noise floor for small samples"
      ;; For n=25, noise floor = 0.4, so 0.35 is below floor -> :none
      (is (= :none (acf/lag-1-severity 0.35 25))))

    (testing "noise floor dominates for n=100"
      ;; For n=100, floor=0.2, so 0.15 < 0.2 -> :none
      (is (= :none (acf/lag-1-severity 0.15 100))))))

(deftest lag-severity-test
  ;; Tests other lag severity classification
  ;; Contracts: thresholds at 0.15, 0.25, 0.40 (above noise floor)
  ;; Use n=400 where floor=0.1, so thresholds are at their nominal values
  (testing "lag-severity"
    (testing "classifies as :none below threshold"
      (is (= :none (acf/lag-severity 0.10 400))))

    (testing "classifies as :minor between 0.15 and 0.25"
      (is (= :minor (acf/lag-severity 0.20 400))))

    (testing "classifies as :moderate between 0.25 and 0.40"
      (is (= :moderate (acf/lag-severity 0.30 400))))

    (testing "classifies as :severe above 0.40"
      (is (= :severe (acf/lag-severity 0.45 400))))))

(deftest classify-lag-severities-test
  ;; Tests classification of all lags
  ;; Contract: lag 1 uses lag-1-severity, others use lag-severity
  ;; Use n=400 where floor=0.1, so thresholds are at their nominal values
  (testing "classify-lag-severities"
    (testing "applies correct threshold for each lag"
      (let [acf-map {1 0.15, 2 0.20, 3 0.30}
            result (acf/classify-lag-severities acf-map 400)]
        ;; Lag 1 at 0.15 -> :minor (lag-1 threshold: 0.10 <= 0.15 < 0.20)
        (is (= :minor (get result 1)))
        ;; Lag 2 at 0.20 -> :minor (other lag threshold: 0.15 <= 0.20 < 0.25)
        (is (= :minor (get result 2)))
        ;; Lag 3 at 0.30 -> :moderate (0.25 <= 0.30 < 0.40)
        (is (= :moderate (get result 3)))))))

;;; Pattern detection tests

(deftest detect-pattern-clean-test
  ;; Tests clean pattern detection
  ;; Contract: all lags below noise floor -> :clean
  (testing "detect-pattern"
    (testing "returns :clean when all lags below noise floor"
      (let [;; For n=100, noise floor = 0.2
            acf-map {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.00}]
        (is (= :clean (acf/detect-pattern acf-map 100)))))))

(deftest detect-pattern-alternating-test
  ;; Tests alternating pattern detection with severity levels
  ;; Contract: negative r1 returns severity-qualified alternating pattern
  ;; For n=400, noise floor = 0.1, so thresholds are at nominal values
  (testing "detect-pattern"
    (testing "returns :alternating-none for small negative r1"
      (let [acf-map {1 -0.05, 2 0.02, 3 -0.01}]
        (is (= :alternating-none (acf/detect-pattern acf-map 400)))))

    (testing "returns :alternating-minor for r1 between -0.20 and -0.10"
      (let [acf-map {1 -0.15, 2 0.05, 3 -0.02}]
        (is (= :alternating-minor (acf/detect-pattern acf-map 400)))))

    (testing "returns :alternating-moderate for r1 between -0.35 and -0.20"
      (let [acf-map {1 -0.30, 2 0.10, 3 -0.05}]
        (is (= :alternating-moderate (acf/detect-pattern acf-map 400)))))

    (testing "returns :alternating-severe for r1 below -0.35"
      (let [acf-map {1 -0.50, 2 0.15, 3 -0.08}]
        (is (= :alternating-severe (acf/detect-pattern acf-map 400)))))))

(deftest detect-pattern-severe-test
  ;; Tests severe pattern detection
  ;; Contract: lag-1 at severe level -> :severe
  (testing "detect-pattern"
    (testing "returns :severe when lag-1 is severe"
      (let [acf-map {1 0.5, 2 0.3, 3 0.2}]
        (is (= :severe (acf/detect-pattern acf-map 100)))))))

(deftest detect-pattern-transient-effects-test
  ;; Tests transient-effects pattern detection
  ;; Contract: lag-1 elevated with exponential decay -> :transient-effects
  (testing "detect-pattern"
    (testing "returns :transient-effects for exponential decay r1 > r2 > r3"
      (let [acf-map {1 0.25, 2 0.15, 3 0.08, 4 0.04}]
        (is (= :transient-effects (acf/detect-pattern acf-map 100)))))))

(deftest detect-pattern-drift-test
  ;; Tests drift pattern detection
  ;; Contract: slow decay with lag at n/10 still elevated -> :drift
  (testing "detect-pattern"
    (testing "returns :drift when decay is slow"
      ;; n=100, so drift-lag = 10, floor = 0.2
      ;; drift-threshold = max(0.15, 0.2) = 0.2
      ;; Need r1 elevated and r10 > 0.2
      (let [acf-map (merge
                     {1 0.30, 2 0.29, 3 0.28}
                     (into {} (for [^long k (range 4 51)]
                                [k (- 0.30 (* 0.005 k))])))]
        ;; r10 = 0.30 - 0.05 = 0.25, above 0.2 threshold
        (is (= :drift (acf/detect-pattern acf-map 100)))))))

(deftest detect-pattern-periodic-test
  ;; Tests periodic pattern detection
  ;; Contract: lag-1 clean but peak at k > 5 -> :periodic
  (testing "detect-pattern"
    (testing "returns :periodic when lag-1 clean but peak at k > 5"
      (let [acf-map {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.01,
                     6 0.02, 7 0.03, 8 0.02, 9 0.01, 10 0.30}]
        (is (= :periodic (acf/detect-pattern acf-map 100)))))))

(deftest detect-period-test
  ;; Tests period detection
  ;; Contract: finds peak lag > 5 exceeding threshold
  (testing "detect-period"
    (testing "returns peak lag when above threshold"
      (let [acf-map {1 0.05, 6 0.10, 10 0.30, 15 0.05}]
        (is (= 10 (acf/detect-period acf-map 100)))))

    (testing "returns nil when no peak above threshold"
      (let [acf-map {1 0.05, 6 0.10, 10 0.10}]
        (is (nil? (acf/detect-period acf-map 100)))))))

;;; Overall classification tests

(deftest classify-overall-pass-test
  ;; Tests :pass classification
  ;; Contract: all lags :none AND Ljung-Box p > 0.10
  (testing "classify-overall"
    (testing "returns :pass when all none and high p-value"
      (let [lag-sevs {1 :none, 2 :none, 3 :none}
            lb {:p-value 0.50}]
        (is (= :pass (acf/classify-overall lag-sevs lb 100 100)))))))

(deftest classify-overall-acceptable-test
  ;; Tests :acceptable classification
  ;; Contract: lag-1 none/minor AND no severe
  (testing "classify-overall"
    (testing "returns :acceptable when lag-1 minor and no severe"
      (let [lag-sevs {1 :minor, 2 :none, 3 :none}
            lb {:p-value 0.50}]
        (is (= :acceptable (acf/classify-overall lag-sevs lb 80 100)))))))

(deftest classify-overall-warning-test
  ;; Tests :warning classification
  ;; Contract: any moderate OR Ljung-Box p <= 0.01
  (testing "classify-overall"
    (testing "returns :warning when any lag is moderate"
      (let [lag-sevs {1 :minor, 2 :moderate, 3 :none}
            lb {:p-value 0.50}]
        (is (= :warning (acf/classify-overall lag-sevs lb 80 100)))))

    (testing "returns :warning when Ljung-Box p <= 0.01"
      (let [lag-sevs {1 :minor, 2 :none}
            lb {:p-value 0.005}]
        (is (= :warning (acf/classify-overall lag-sevs lb 80 100)))))))

(deftest classify-overall-fail-test
  ;; Tests :fail classification
  ;; Contract: any severe OR n_eff < n/3
  (testing "classify-overall"
    (testing "returns :fail when any lag is severe"
      (let [lag-sevs {1 :severe, 2 :none}
            lb {:p-value 0.50}]
        (is (= :fail (acf/classify-overall lag-sevs lb 80 100)))))

    (testing "returns :fail when n_eff < n/3"
      (let [lag-sevs {1 :minor, 2 :none}
            lb {:p-value 0.50}]
        (is (= :fail (acf/classify-overall lag-sevs lb 20 100)))))))

;;; Full analysis tests

(deftest analyse-autocorrelation-white-noise-test
  ;; Tests analyse-autocorrelation on white noise
  ;; Contract: returns ACF, lag-1, and n-original for downstream analyses
  (testing "analyse-autocorrelation"
    (testing "returns core ACF data for white noise"
      (let [rng (java.util.Random. 42)
            samples (double-array (repeatedly 100 #(.nextGaussian rng)))
            result (acf/analyse-autocorrelation samples)]
        (is (map? result))
        (is (contains? result :acf))
        (is (contains? result :lag-1))
        (is (contains? result :effective-sample-size))
        ;; effective-sample-size only contains :n-original now
        (is (= #{:n-original} (set (keys (:effective-sample-size result)))))
        (is (= 100 (get-in result [:effective-sample-size :n-original])))
        ;; Classification and CI inflation are now in separate functions
        (let [class-result (acf/autocorrelation-classification (:acf result) 100)]
          (is (#{:clean :pass :acceptable} (:classification class-result))))))))

(deftest analyse-autocorrelation-ar1-test
  ;; Tests analyse-autocorrelation on AR(1) process
  ;; Contract: returns ACF data that classification functions can detect
  (testing "analyse-autocorrelation"
    (testing "returns ACF data for AR(1) process"
      (let [phi 0.7
            n 200
            rng (java.util.Random. 123)
            samples (double-array n)
            _ (aset samples 0 (.nextGaussian rng))
            _ (dotimes [i (dec n)]
                (aset samples (inc i)
                      (+ (* phi (aget samples i))
                         (* (Math/sqrt (- 1.0 (* phi phi))) (.nextGaussian rng)))))
            result (acf/analyse-autocorrelation samples)
            acf-map (:acf result)
            class-result (acf/autocorrelation-classification acf-map n)
            ess-result (acf/effective-sample-size-analysis acf-map n)]
        (is (map? result))
        ;; Should detect transient-effects or severe pattern
        (is (#{:transient-effects :severe} (:pattern class-result)))
        ;; Classification should be warning or fail
        (is (#{:warning :fail} (:classification class-result)))
        ;; Effective sample size should be reduced
        (is (< (get-in ess-result [:effective-sample-size :ratio]) 0.5))))))

(deftest analyse-autocorrelation-insufficient-samples-test
  ;; Tests full analysis returns nil for insufficient samples
  (testing "analyse-autocorrelation"
    (testing "returns nil for n < 20"
      (let [samples (double-array (range 19))]
        (is (nil? (acf/analyse-autocorrelation samples)))))))

;;; anomalous-lags tests

(deftest anomalous-lags-test
  ;; Tests anomalous-lags helper function
  ;; Contracts: filters out :none and :alternating-none, sorts by lag
  (testing "anomalous-lags"
    (testing "returns empty vector when all lags are :none"
      (let [lag-sevs {1 :none, 2 :none, 3 :none}]
        (is (= [] (acf/anomalous-lags lag-sevs)))))

    (testing "returns empty vector when all lags are :alternating-none"
      (let [lag-sevs {1 :alternating-none, 2 :alternating-none}]
        (is (= [] (acf/anomalous-lags lag-sevs)))))

    (testing "filters out :none and :alternating-none severities"
      (let [lag-sevs {1 :minor, 2 :none, 3 :moderate, 4 :alternating-none, 5 :severe}]
        (is (= [1 3 5] (acf/anomalous-lags lag-sevs)))))

    (testing "sorts results by lag number"
      (let [lag-sevs {5 :minor, 2 :moderate, 10 :severe, 1 :minor}]
        (is (= [1 2 5 10] (acf/anomalous-lags lag-sevs)))))

    (testing "includes alternating severities that are not :alternating-none"
      (let [lag-sevs {1 :alternating-minor, 2 :alternating-moderate, 3 :alternating-severe}]
        (is (= [1 2 3] (acf/anomalous-lags lag-sevs)))))))

;;; analyse-autocorrelation with new fields tests

(deftest analyse-autocorrelation-lag-severities-test
  ;; Tests analyse-autocorrelation includes :lag-severities field
  ;; Contract: lag-severities map contains severity for all lags in ACF
  (testing "analyse-autocorrelation"
    (testing "includes :lag-severities map"
      (let [rng (java.util.Random. 42)
            samples (double-array (repeatedly 100 #(.nextGaussian rng)))
            result (acf/analyse-autocorrelation samples)]
        (is (contains? result :lag-severities))
        (is (map? (:lag-severities result)))
        ;; Should have same keys as ACF map
        (is (= (set (keys (:acf result)))
               (set (keys (:lag-severities result)))))
        ;; All values should be severity keywords
        (is (every? #{:none :minor :moderate :severe
                      :alternating-none :alternating-minor
                      :alternating-moderate :alternating-severe}
                    (vals (:lag-severities result))))))))

(deftest analyse-autocorrelation-anomalous-lags-test
  ;; Tests analyse-autocorrelation includes :anomalous-lags field
  ;; Contract: anomalous-lags is vector of lag numbers with non-trivial severities
  (testing "analyse-autocorrelation"
    (testing "includes :anomalous-lags vector"
      (let [rng (java.util.Random. 42)
            samples (double-array (repeatedly 100 #(.nextGaussian rng)))
            result (acf/analyse-autocorrelation samples)]
        (is (contains? result :anomalous-lags))
        (is (vector? (:anomalous-lags result)))))

    (testing "anomalous-lags matches filtered lag-severities"
      (let [;; Generate AR(1) process with some autocorrelation
            phi 0.5
            n 100
            rng (java.util.Random. 789)
            samples (double-array n)
            _ (aset samples 0 (.nextGaussian rng))
            _ (dotimes [i (dec n)]
                (aset samples (inc i)
                      (+ (* phi (aget samples i))
                         (* (Math/sqrt (- 1.0 (* phi phi))) (.nextGaussian rng)))))
            result (acf/analyse-autocorrelation samples)
            lag-sevs (:lag-severities result)
            anomalous (:anomalous-lags result)]
        ;; anomalous-lags should match what anomalous-lags fn produces
        (is (= anomalous (acf/anomalous-lags lag-sevs)))
        ;; All entries should be integers
        (is (every? integer? anomalous))
        ;; Should be sorted ascending
        (is (= anomalous (sort anomalous)))
        ;; Severity can be looked up in lag-severities
        (is (every? #(contains? lag-sevs %) anomalous))))))

(deftest analyse-autocorrelation-clean-data-test
  ;; Tests analyse-autocorrelation with clean data has empty anomalous-lags
  ;; Contract: white noise should have no anomalous lags
  (testing "analyse-autocorrelation"
    (testing "clean white noise has empty or near-empty anomalous-lags"
      (let [rng (java.util.Random. 42)
            ;; Use larger sample for lower noise floor
            samples (double-array (repeatedly 400 #(.nextGaussian rng)))
            result (acf/analyse-autocorrelation samples)
            anomalous (:anomalous-lags result)]
        ;; Most ACF values should be below noise floor for white noise
        ;; Allow up to 5% of lags to be spuriously significant
        (is (<= (count anomalous) 10)
            (format "Expected few anomalous lags for white noise, got %d"
                    (count anomalous)))))))

;;; effective-sample-size-analysis tests

(deftest effective-sample-size-analysis-test
  ;; Tests effective-sample-size-analysis function
  ;; Contract: computes effective sample size and CI inflation from ACF map
  (testing "effective-sample-size-analysis"
    (testing "returns correct structure"
      (let [acf-map {1 0.5, 2 0.25, 3 0.125}
            result (acf/effective-sample-size-analysis acf-map 100)]
        (is (map? result))
        (is (contains? result :effective-sample-size))
        (is (contains? result :ci-inflation-factor))
        (is (contains? (:effective-sample-size result) :n-original))
        (is (contains? (:effective-sample-size result) :n-effective))
        (is (contains? (:effective-sample-size result) :ratio))))

    (testing "computes values correctly for positive r1"
      ;; r1 = 0.5 -> n_eff = 100 * 0.5/1.5 = 33
      ;; CI inflation = sqrt(1.5/0.5) = sqrt(3) ≈ 1.732
      (let [acf-map {1 0.5, 2 0.25}
            result (acf/effective-sample-size-analysis acf-map 100)]
        (is (= 100 (get-in result [:effective-sample-size :n-original])))
        (is (= 33 (get-in result [:effective-sample-size :n-effective])))
        (is (approx= (get-in result [:effective-sample-size :ratio]) 0.33 0.01))
        (is (approx= (:ci-inflation-factor result) (Math/sqrt 3.0) 0.001))))

    (testing "returns nil for nil acf-map"
      (is (nil? (acf/effective-sample-size-analysis nil 100))))

    (testing "returns full n-effective for zero r1"
      (let [acf-map {1 0.0, 2 0.0}
            result (acf/effective-sample-size-analysis acf-map 100)]
        (is (= 100 (get-in result [:effective-sample-size :n-effective])))
        (is (= 1.0 (:ci-inflation-factor result)))))))

;;; autocorrelation-classification tests

(deftest autocorrelation-classification-test
  ;; Tests autocorrelation-classification function
  ;; Contract: computes pattern, classification, Ljung-Box from ACF map
  (testing "autocorrelation-classification"
    (testing "returns correct structure"
      (let [acf-map {1 0.05, 2 0.03, 3 0.02}
            result (acf/autocorrelation-classification acf-map 100)]
        (is (map? result))
        (is (contains? result :ljung-box))
        (is (contains? result :pattern))
        (is (contains? result :classification))
        (is (contains? result :detected-period))
        (is (contains? result :noise-floor))
        (is (contains? result :thresholds))))

    (testing "includes noise-floor value"
      (let [result (acf/autocorrelation-classification {1 0.05} 100)]
        (is (= 0.2 (:noise-floor result)))))

    (testing "includes threshold values"
      (let [result (acf/autocorrelation-classification {1 0.05} 100)
            thresholds (:thresholds result)]
        (is (= {:minor 0.10 :moderate 0.20 :severe 0.35} (:lag-1 thresholds)))
        (is (= {:minor 0.15 :moderate 0.25 :severe 0.40} (:other thresholds)))))

    (testing "detects clean pattern for white noise ACF"
      (let [acf-map {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.00}
            result (acf/autocorrelation-classification acf-map 100)]
        (is (= :clean (:pattern result)))
        (is (= :pass (:classification result)))))

    (testing "detects severe pattern for high r1"
      (let [acf-map {1 0.5, 2 0.3, 3 0.2}
            result (acf/autocorrelation-classification acf-map 100)]
        (is (= :severe (:pattern result)))
        (is (= :fail (:classification result)))))

    (testing "detects periodic pattern and period"
      (let [acf-map {1 0.05, 2 0.03, 3 0.02, 4 0.01, 5 0.01,
                     6 0.02, 7 0.03, 8 0.02, 9 0.01, 10 0.30}
            result (acf/autocorrelation-classification acf-map 100)]
        (is (= :periodic (:pattern result)))
        (is (= 10 (:detected-period result)))))

    (testing "returns nil for nil acf-map"
      (is (nil? (acf/autocorrelation-classification nil 100))))))

;;; analyse-autocorrelation uses composable functions

(deftest analyse-autocorrelation-uses-composable-functions-test
  ;; Tests that analyse-autocorrelation provides data for composable functions
  ;; Contract: analyse-autocorrelation returns core data that ESS and classification
  ;;           functions can use independently
  (testing "analyse-autocorrelation"
    (testing "provides data for composable functions"
      (let [rng (java.util.Random. 42)
            samples (double-array (repeatedly 100 #(.nextGaussian rng)))
            result (acf/analyse-autocorrelation samples)
            acf-map (:acf result)
            n (get-in result [:effective-sample-size :n-original])
            ess-result (acf/effective-sample-size-analysis acf-map n)
            class-result (acf/autocorrelation-classification acf-map n)]
        ;; analyse-autocorrelation only contains :n-original
        (is (= {:n-original n} (:effective-sample-size result)))
        ;; ESS analysis has full effective-sample-size data
        (is (contains? (:effective-sample-size ess-result) :n-effective))
        (is (contains? (:effective-sample-size ess-result) :ratio))
        ;; ESS analysis has ci-inflation-factor
        (is (contains? ess-result :ci-inflation-factor))
        ;; Classification has pattern and classification
        (is (contains? class-result :ljung-box))
        (is (contains? class-result :pattern))
        (is (contains? class-result :classification))))))
