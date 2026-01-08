(ns stats.moment-match-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [stats.moment-match :as mm]))

;; Tests for moment-based parameter estimation and distribution suitability
;; screening. These functions provide initial estimates for MLE fitting and
;; filter out distributions unsuitable for a given dataset.

(deftest gamma-moment-estimate-test
  (testing "gamma-moment-estimate"
    (testing "computes shape and scale from mean and variance"
      (let [result (mm/gamma-moment-estimate 100.0 400.0)]
        (is (some? result))
        (is (= 25.0 (:shape result)))
        (is (= 4.0 (:scale result)))))

    (testing "returns nil for non-positive mean"
      (is (nil? (mm/gamma-moment-estimate 0.0 100.0)))
      (is (nil? (mm/gamma-moment-estimate -5.0 100.0))))

    (testing "returns nil for non-positive variance"
      (is (nil? (mm/gamma-moment-estimate 100.0 0.0)))
      (is (nil? (mm/gamma-moment-estimate 100.0 -10.0))))))

(deftest lognormal-moment-estimate-test
  (testing "lognormal-moment-estimate"
    (testing "computes mu and sigma from mean and variance"
      (let [result (mm/lognormal-moment-estimate 100.0 400.0)]
        (is (some? result))
        (is (number? (:mu result)))
        (is (number? (:sigma result)))
        (is (pos? (:sigma result)))))

    (testing "returns nil for non-positive mean"
      (is (nil? (mm/lognormal-moment-estimate 0.0 100.0)))
      (is (nil? (mm/lognormal-moment-estimate -5.0 100.0))))

    (testing "returns nil for negative variance"
      (is (nil? (mm/lognormal-moment-estimate 100.0 -10.0))))

    (testing "accepts zero variance"
      (let [result (mm/lognormal-moment-estimate 100.0 0.0)]
        (is (some? result))
        (is (= (Math/log 100.0) (:mu result)))
        (is (zero? (:sigma result)))))))

(deftest inverse-gaussian-moment-estimate-test
  (testing "inverse-gaussian-moment-estimate"
    (testing "computes mu and lambda from mean and variance"
      (let [result (mm/inverse-gaussian-moment-estimate 100.0 400.0)]
        (is (some? result))
        (is (= 100.0 (:mu result)))
        (is (= 2500.0 (:lambda result)))))

    (testing "returns nil for non-positive mean"
      (is (nil? (mm/inverse-gaussian-moment-estimate 0.0 100.0)))
      (is (nil? (mm/inverse-gaussian-moment-estimate -5.0 100.0))))

    (testing "returns nil for non-positive variance"
      (is (nil? (mm/inverse-gaussian-moment-estimate 100.0 0.0)))
      (is (nil? (mm/inverse-gaussian-moment-estimate 100.0 -10.0))))))

(deftest weibull-moment-estimate-test
  (testing "weibull-moment-estimate"
    (testing "computes shape and scale from mean and variance"
      (let [result (mm/weibull-moment-estimate 100.0 400.0)]
        (is (some? result))
        (is (pos? (:shape result)))
        (is (pos? (:scale result)))))

    (testing "returns nil for non-positive mean"
      (is (nil? (mm/weibull-moment-estimate 0.0 100.0)))
      (is (nil? (mm/weibull-moment-estimate -5.0 100.0))))

    (testing "returns nil for non-positive variance"
      (is (nil? (mm/weibull-moment-estimate 100.0 0.0)))
      (is (nil? (mm/weibull-moment-estimate 100.0 -10.0))))

    (testing "returns nil for very high CV (> 2)"
      ;; CV = sqrt(variance)/mean = sqrt(50000)/100 = 2.236 > 2
      (is (nil? (mm/weibull-moment-estimate 100.0 50000.0))))

    (testing "handles low CV (concentrated data)"
      ;; CV = sqrt(100)/100 = 0.1, should give high shape
      (let [result (mm/weibull-moment-estimate 100.0 100.0)]
        (is (some? result))
        (is (pos? (:shape result)))))

    (testing "handles moderate CV"
      ;; CV = sqrt(2500)/100 = 0.5
      (let [result (mm/weibull-moment-estimate 100.0 2500.0)]
        (is (some? result))
        (is (pos? (:shape result)))))))

(deftest moment-match-prefilter-test
  (testing "moment-match-prefilter"
    (testing "returns results for all distributions with valid data"
      (let [result (mm/moment-match-prefilter 100.0 400.0)]
        (is (= #{:gamma :lognormal :inverse-gaussian :weibull}
               (set (keys result))))
        (doseq [[_ v] result]
          (is (:suitable? v))
          (is (some? (:params v))))))

    (testing "marks all as unsuitable with negative mean"
      (let [result (mm/moment-match-prefilter -5.0 100.0)]
        (doseq [[_ v] result]
          (is (not (:suitable? v)))
          (is (nil? (:params v))))))

    (testing "accepts subset of distributions"
      (let [result (mm/moment-match-prefilter 100.0 400.0 #{:gamma :lognormal})]
        (is (= #{:gamma :lognormal} (set (keys result))))))

    (testing "handles unknown distribution"
      (let [result (mm/moment-match-prefilter 100.0 400.0 #{:unknown})]
        (is (= #{:unknown} (set (keys result))))
        (is (not (:suitable? (:unknown result))))
        (is (= :unknown-distribution (:error (:unknown result))))))))

(deftest suitable-distributions-test
  (testing "suitable-distributions"
    (testing "returns all suitable distributions for valid data"
      (let [result (mm/suitable-distributions 100.0 400.0)]
        (is (set? result))
        (is (= #{:gamma :lognormal :inverse-gaussian :weibull} result))))

    (testing "returns empty set for invalid data"
      (is (empty? (mm/suitable-distributions -5.0 100.0))))

    (testing "filters by distribution subset"
      (let [result (mm/suitable-distributions 100.0 400.0 #{:gamma})]
        (is (= #{:gamma} result))))))

(deftest unsuitable-distributions-test
  (testing "unsuitable-distributions"
    (testing "returns empty set for valid data"
      (is (empty? (mm/unsuitable-distributions 100.0 400.0))))

    (testing "returns all for negative mean"
      (is (= #{:gamma :lognormal :inverse-gaussian :weibull}
             (mm/unsuitable-distributions -5.0 100.0))))

    (testing "returns weibull for high CV data"
      ;; High CV makes Weibull unsuitable
      (let [result (mm/unsuitable-distributions 100.0 50000.0)]
        (is (contains? result :weibull))
        (is (not (contains? result :gamma)))))))
