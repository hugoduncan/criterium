(ns criterium.stats.tail-test
  ;; Tests for tail statistics functions.
  ;; Tests verify contracts: functions compute expected statistical values
  ;; for tail analysis including Hill estimator, GPD, MRL, and tail ratios.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.stats.interface :as stats]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

(defn- abs-error
  "Compute absolute error between expected and actual values."
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

;;; exceedances-over-threshold tests

(deftest exceedances-over-threshold-test
  (testing "exceedances-over-threshold"
    (testing "returns excesses (value - threshold) for values > threshold"
      (let [samples (darr [1.0 2.0 3.0 4.0 5.0])
            exceeds (stats/exceedances-over-threshold samples 3.0)]
        (is (= 2 (arr/length exceeds)))
        ;; 4.0 - 3.0 = 1.0, 5.0 - 3.0 = 2.0
        (is (= 1.0 (arr/get-double exceeds 0)))
        (is (= 2.0 (arr/get-double exceeds 1)))))
    (testing "returns empty array when no values exceed threshold"
      (let [samples (darr [1.0 2.0 3.0])
            exceeds (stats/exceedances-over-threshold samples 5.0)]
        (is (= 0 (arr/length exceeds)))))
    (testing "returns all excesses when threshold is below minimum"
      (let [samples (darr [1.0 2.0 3.0])
            exceeds (stats/exceedances-over-threshold samples 0.0)]
        (is (= 3 (arr/length exceeds)))
        ;; 1.0 - 0.0 = 1.0, 2.0 - 0.0 = 2.0, 3.0 - 0.0 = 3.0
        (is (= 1.0 (arr/get-double exceeds 0)))
        (is (= 2.0 (arr/get-double exceeds 1)))
        (is (= 3.0 (arr/get-double exceeds 2)))))
    (testing "handles threshold equal to a value (excludes equal values)"
      (let [samples (darr [1.0 2.0 3.0 3.0 4.0])
            exceeds (stats/exceedances-over-threshold samples 3.0)]
        (is (= 1 (arr/length exceeds)))
        ;; 4.0 - 3.0 = 1.0
        (is (= 1.0 (arr/get-double exceeds 0)))))))

;;; Hill estimator tests

(deftest hill-estimator-test
  (testing "hill-estimator"
    (testing "returns empty vector for insufficient samples"
      (let [samples (darr [1.0])]
        (is (= [] (stats/hill-estimator samples [1 2 3])))))
    (testing "computes estimates for valid k values"
      (let [;; Pareto-like data: sorted ascending
            samples (darr (sort [10 20 30 40 50 60 70 80 90 100]))
            results (stats/hill-estimator samples [1 2 3])]
        (is (= 3 (count results)))
        (is (every? #(contains? % :k) results))
        (is (every? #(contains? % :estimate) results))
        (is (every? #(contains? % :tail-index) results))
        (is (= [1 2 3] (mapv :k results)))))
    (testing "filters invalid k values"
      (let [samples (darr (sort [1 2 3 4 5]))
            ;; k=0 invalid, k=5 invalid (k must be < n)
            results (stats/hill-estimator samples [0 1 2 5 10])]
        (is (= 2 (count results)))
        (is (= [1 2] (mapv :k results)))))
    (testing "tail-index is 1/estimate when estimate is positive"
      (let [samples (darr (sort (range 1 21)))
            results (stats/hill-estimator samples [5])]
        (is (= 1 (count results)))
        (let [{:keys [estimate tail-index]} (first results)
              estimate (double estimate)]
          (when (pos? estimate)
            (is (< (abs-error (/ 1.0 estimate) tail-index) 1e-10))))))))

(deftest hill-estimator-default-k-range-test
  (testing "hill-estimator-default-k-range"
    (testing "returns nil for small n"
      (is (nil? (stats/hill-estimator-default-k-range 5)))
      (is (nil? (stats/hill-estimator-default-k-range 10))))
    (testing "returns sequence starting at 10 for valid n"
      (let [k-range (stats/hill-estimator-default-k-range 100)]
        (is (seq k-range))
        (is (= 10 (first k-range)))
        (is (<= (last k-range) 50))))
    (testing "caps at 500 for large n"
      (let [k-range (stats/hill-estimator-default-k-range 10000)]
        (is (<= (last k-range) 500))))))

;;; GPD function tests

(deftest gpd-pdf-test
  (testing "gpd-pdf"
    (testing "exponential case (xi ≈ 0)"
      (let [sigma 2.0
            pdf (stats/gpd-pdf 0.0 sigma)]
        ;; f(0) = 1/σ
        (is (< (abs-error (/ 1.0 sigma) (pdf 0.0)) 1e-10))
        ;; f(y) = (1/σ) * exp(-y/σ)
        (is (< (abs-error (/ (Math/exp -1.0) sigma) (pdf 2.0)) 1e-10))))
    (testing "positive xi (heavy tail)"
      (let [xi 0.5
            sigma 1.0
            pdf (stats/gpd-pdf xi sigma)]
        ;; f(0) = 1/σ
        (is (< (abs-error 1.0 (pdf 0.0)) 1e-10))
        ;; Values should be positive for y > 0
        (is (pos? (pdf 1.0)))
        (is (pos? (pdf 5.0)))))
    (testing "negative xi (bounded support)"
      (let [xi -0.5
            sigma 2.0
            pdf (stats/gpd-pdf xi sigma)
            ;; Upper bound is -σ/ξ = 4.0
            upper-bound (- (/ sigma xi))]
        (is (= 4.0 upper-bound))
        ;; Within support
        (is (pos? (pdf 1.0)))
        (is (pos? (pdf 3.0)))
        ;; At or beyond upper bound
        (is (= 0.0 (pdf 4.0)))
        (is (= 0.0 (pdf 5.0)))))
    (testing "returns 0 for negative y"
      (let [pdf (stats/gpd-pdf 0.5 1.0)]
        (is (= 0.0 (pdf -1.0)))))))

(deftest gpd-cdf-test
  (testing "gpd-cdf"
    (testing "exponential case (xi ≈ 0)"
      (let [sigma 2.0
            cdf (stats/gpd-cdf 0.0 sigma)]
        ;; F(0) = 0
        (is (= 0.0 (cdf 0.0)))
        ;; F(y) = 1 - exp(-y/σ)
        (is (< (abs-error (- 1.0 (Math/exp -1.0)) (cdf 2.0)) 1e-10))))
    (testing "positive xi"
      (let [xi 0.5
            sigma 1.0
            cdf (stats/gpd-cdf xi sigma)]
        (is (= 0.0 (cdf 0.0)))
        (is (< (cdf 1.0) 1.0))
        (is (< (cdf 1.0) (cdf 2.0)))))
    (testing "negative xi (bounded support)"
      (let [xi -0.5
            sigma 2.0
            cdf (stats/gpd-cdf xi sigma)]
        (is (= 0.0 (cdf 0.0)))
        ;; At upper bound, CDF = 1
        (is (= 1.0 (cdf 4.0)))
        (is (= 1.0 (cdf 5.0)))))
    (testing "returns 0 for negative y"
      (let [cdf (stats/gpd-cdf 0.5 1.0)]
        (is (= 0.0 (cdf -1.0)))))))

(deftest gpd-quantile-test
  (testing "gpd-quantile"
    (testing "exponential case (xi ≈ 0)"
      (let [sigma 2.0
            quantile (stats/gpd-quantile 0.0 sigma)]
        ;; Q(0) = 0
        (is (= 0.0 (quantile 0.0)))
        ;; Q(0.5) = -σ*log(0.5) = σ*log(2)
        (is (< (abs-error (* sigma (Math/log 2.0)) (quantile 0.5)) 1e-10))))
    (testing "positive xi"
      (let [quantile (stats/gpd-quantile 0.5 1.0)]
        (is (= 0.0 (quantile 0.0)))
        (is (pos? (quantile 0.5)))
        (is (< (quantile 0.5) (quantile 0.9)))))
    (testing "negative xi (finite upper bound)"
      (let [xi -0.5
            sigma 2.0
            quantile (stats/gpd-quantile xi sigma)
            upper-bound (- (/ sigma xi))]
        (is (= 0.0 (quantile 0.0)))
        ;; Q(1) = upper bound
        (is (= upper-bound (quantile 1.0)))))
    (testing "CDF and quantile are inverses"
      (let [xi 0.3
            sigma 1.5
            cdf (stats/gpd-cdf xi sigma)
            quantile (stats/gpd-quantile xi sigma)]
        (doseq [p [0.1 0.25 0.5 0.75 0.9]]
          (is (< (abs-error p (cdf (quantile p))) 1e-8)))))))

;;; GPD MLE tests

(deftest gpd-mle-test
  (testing "gpd-mle"
    (testing "throws on empty input"
      (is (thrown? IllegalArgumentException
                   (stats/gpd-mle (darr [])))))
    (testing "throws on non-positive values"
      (is (thrown? IllegalArgumentException
                   (stats/gpd-mle (darr [1.0 -0.5 2.0])))))
    (testing "fits exponential data (xi ≈ 0)"
      (let [;; Exponential samples with mean 1.0
            samples (darr [0.1 0.3 0.5 0.7 1.0 1.2 1.5 2.0 2.5 3.0])
            result (stats/gpd-mle samples)]
        (is (contains? result :xi))
        (is (contains? result :sigma))
        (is (contains? result :log-likelihood))
        (is (contains? result :converged?))
        (is (:converged? result))
        ;; For exponential-like data, xi should be close to 0
        (is (< (Math/abs (double (:xi result))) 1.0))))
    (testing "returns positive sigma"
      (let [samples (darr [1.0 2.0 3.0 4.0 5.0])
            result (stats/gpd-mle samples)]
        (is (pos? (:sigma result)))))
    (testing "log-likelihood is finite"
      (let [samples (darr (range 0.1 5.0 0.5))
            result (stats/gpd-mle samples)]
        (is (Double/isFinite (:log-likelihood result)))))))

;;; Mean residual life tests

(deftest mean-residual-life-test
  (testing "mean-residual-life"
    (testing "returns empty vector for empty samples"
      (let [samples (darr [])]
        (is (= [] (stats/mean-residual-life samples [1 2 3])))))
    (testing "computes MRL for each threshold"
      (let [samples (darr (sort [1 2 3 4 5 6 7 8 9 10]))
            results (stats/mean-residual-life samples [3.0 5.0])]
        (is (= 2 (count results)))
        (is (every? #(contains? % :threshold) results))
        (is (every? #(contains? % :mrl) results))
        (is (every? #(contains? % :n-exceed) results))))
    (testing "MRL is mean of exceedances minus threshold"
      (let [samples (darr (sort [1 2 3 4 5]))
            ;; Threshold 2.0: exceedances are 3, 4, 5
            ;; Mean excess = ((3-2) + (4-2) + (5-2))/3 = 6/3 = 2.0
            results (stats/mean-residual-life samples [2.0])
            {:keys [mrl n-exceed]} (first results)]
        (is (= 3 n-exceed))
        (is (< (abs-error 2.0 mrl) 1e-10))))
    (testing "returns NaN MRL when no values exceed threshold"
      (let [samples (darr [1 2 3])
            results (stats/mean-residual-life samples [5.0])
            {:keys [mrl n-exceed]} (first results)]
        (is (= 0 n-exceed))
        (is (Double/isNaN mrl))))))

(deftest mean-residual-life-default-thresholds-test
  (testing "mean-residual-life-default-thresholds"
    (testing "returns nil for empty samples"
      ;; Empty array has length 0
      (is (nil? (seq (stats/mean-residual-life-default-thresholds (darr []))))))
    (testing "returns sequence of thresholds from 50th to 95th percentile"
      (let [samples (darr (sort (range 1 101)))
            thresholds (stats/mean-residual-life-default-thresholds samples)]
        (is (seq thresholds))
        ;; First threshold should be around 50th percentile (≈50)
        (is (>= (first thresholds) 45))
        (is (<= (first thresholds) 55))
        ;; Last threshold should be around 95th percentile (≈95)
        (is (>= (last thresholds) 90))
        (is (<= (last thresholds) 99))))
    (testing "respects n-points parameter"
      (let [samples (darr (sort (range 1 101)))
            thresholds (stats/mean-residual-life-default-thresholds samples 5)]
        (is (= 5 (count thresholds)))))))

;;; Tail ratios tests

(deftest tail-ratios-test
  (testing "tail-ratios"
    (testing "computes p99/p95 ratio with keyword keys"
      (let [percentiles {:p95 100.0 :p99 150.0}
            ratios (stats/tail-ratios percentiles)]
        (is (= 1.5 (:p99-p95 ratios)))))
    (testing "computes p999/p99 ratio"
      (let [percentiles {:p99 100.0 :p999 200.0}
            ratios (stats/tail-ratios percentiles)]
        (is (= 2.0 (:p999-p99 ratios)))))
    (testing "computes all ratios when all percentiles present"
      (let [percentiles {:p95 100.0 :p99 150.0 :p999 300.0}
            ratios (stats/tail-ratios percentiles)]
        (is (= 1.5 (:p99-p95 ratios)))
        (is (= 2.0 (:p999-p99 ratios)))
        (is (= 3.0 (:p999-p95 ratios)))))
    (testing "returns empty map when no valid ratios can be computed"
      (let [percentiles {:p50 100.0}
            ratios (stats/tail-ratios percentiles)]
        (is (empty? ratios))))
    (testing "handles numeric keys"
      (let [percentiles {0.95 100.0 0.99 150.0}
            ratios (stats/tail-ratios percentiles)]
        (is (= 1.5 (:p99-p95 ratios)))))
    (testing "handles zero percentile values gracefully"
      (let [percentiles {:p95 0.0 :p99 150.0}
            ratios (stats/tail-ratios percentiles)]
        ;; Should not include ratio with zero denominator
        (is (not (contains? ratios :p99-p95)))))))

;;; Edge case tests

(deftest tail-edge-cases-test
  (testing "edge cases"
    (testing "single element array"
      (let [samples (darr [5.0])]
        (is (= [] (stats/hill-estimator samples [1])))
        (is (= 0 (arr/length (stats/exceedances-over-threshold samples 5.0))))
        (is (= 1 (arr/length (stats/exceedances-over-threshold samples 4.0))))))
    (testing "all equal values"
      (let [samples (darr (repeat 10 5.0))]
        ;; Hill estimator with equal values will have log(1) = 0
        (let [results (stats/hill-estimator samples [1 2 3])]
          (is (every? #(= 0.0 (:estimate %)) results)))
        ;; No exceedances when all equal to threshold
        (is (= 0 (arr/length (stats/exceedances-over-threshold samples 5.0))))))))
