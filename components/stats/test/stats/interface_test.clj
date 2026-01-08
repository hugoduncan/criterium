(ns stats.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [random.interface :as random]
   [stats.interface :as stats]))

;; Tests for stats component functions.
;; Tests verify contract: each function computes expected statistical values.

(deftest mean-test
  (testing "mean"
    (testing "returns arithmetic mean of data"
      (is (= 1.0 (stats/mean (repeat 20 1))))
      (is (= 3.0 (stats/mean (range 0 7))))
      (is (= 50.0 (stats/mean (range 0 101)))))))

(deftest sum-test
  (testing "sum"
    (testing "returns sum of data points"
      (is (= 20 (stats/sum (take 20 (repeatedly (constantly 1))))))
      (is (= 21 (stats/sum (range 0 7)))))))

(deftest sum-of-squares-test
  (testing "sum-of-squares"
    (testing "returns sum of squared data points"
      (is (= 20.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 1))))))
      (is (= 80.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 2))))))
      (is (= 91.0 (stats/sum-of-squares (range 0 7)))))))

(deftest variance-test
  (testing "variance"
    (testing "returns zero for constant data"
      (is (= 0.0 (stats/variance (take 20 (repeatedly (constantly 1)))))))
    (testing "with df=0 returns population variance"
      (is (= 4.0 (stats/variance (range 0 7) 0)))
      (is (= 850.0 (stats/variance (range 0 101) 0))))
    (testing "with df=1 returns sample variance"
      (is (= 858.5 (stats/variance (range 0 101) 1))))))

(deftest median-test
  (testing "median"
    (testing "returns median and partitions for odd count"
      (is (= [5 [1 2] [7 8]]
             (stats/median [1 2 5 7 8]))))
    (testing "returns median and partitions for even count"
      (is (= [3.5 [1 2 2] [5 7 8]]
             (stats/median [1 2 2 5 7 8]))))))

(deftest quartiles-test
  (testing "quartiles"
    (testing "returns [q1 median q3] for sorted data"
      (is (= [1.5 5 7.5]
             (stats/quartiles [1 2 5 7 8])))
      (is (= [2 3.5 7]
             (stats/quartiles [1 2 2 5 7 8]))))))

(deftest quantile-test
  (testing "quantile"
    (testing "returns exact data points at quantile boundaries"
      (is (= 2 (stats/quantile 0.25 [1 2 5 7 8])))
      (is (= 5 (stats/quantile 0.5 [1 2 5 7 8])))
      (is (= 7 (stats/quantile 0.75 [1 2 5 7 8]))))
    (testing "interpolates between data points"
      (is (= 2.0 (stats/quantile 0.25 [1 2 2 5 7 8])))
      (is (= 3.5 (stats/quantile 0.5 [1 2 2 5 7 8])))
      (is (= 6.5 (stats/quantile 0.75 [1 2 2 5 7 8]))))
    (testing "handles edge quantiles"
      (is (= 5 (stats/quantile 0.05 (range 0 101))))
      (is (= 95 (stats/quantile 0.95 (range 0 101)))))))

(deftest skewness-test
  ;; Tests the skewness function returns 0.0 for constant data (zero variance).
  ;; This prevents NaN values that would break bootstrap sorting.
  (testing "skewness"
    (testing "returns 0.0 for constant data"
      (is (= 0.0 (stats/skewness (repeat 50 1.0))))
      (is (= 0.0 (stats/skewness (repeat 50 1.0) 1)))
      (is (= 0.0 (stats/skewness (repeat 50 1.0) 2)))
      (is (= 0.0 (stats/skewness (repeat 50 1.0) 3))))
    (testing "returns finite value for non-constant data"
      (is (Double/isFinite (stats/skewness [1 2 3 4 5 6 7 8 9 10]))))))

(deftest kurtosis-test
  ;; Tests the kurtosis function returns 0.0 for constant data (zero variance).
  ;; This prevents NaN values that would break bootstrap sorting.
  (testing "kurtosis"
    (testing "returns 0.0 for constant data"
      (is (= 0.0 (stats/kurtosis (repeat 50 1.0))))
      (is (= 0.0 (stats/kurtosis (repeat 50 1.0) 1)))
      (is (= 0.0 (stats/kurtosis (repeat 50 1.0) 2)))
      (is (= 0.0 (stats/kurtosis (repeat 50 1.0) 3))))
    (testing "returns finite value for non-constant data"
      (is (Double/isFinite (stats/kurtosis [1 2 3 4 5 6 7 8 9 10]))))))

(deftest cv-test
  (testing "cv"
    (testing "returns coefficient of variation (std dev / mean)"
      ;; For data [2 4 6 8]: mean=5, var=20/3, sd=sqrt(20/3)≈2.582
      ;; CV = 2.582/5 ≈ 0.5164
      (is (< (Math/abs (- (stats/cv [2 4 6 8])
                          (/ (Math/sqrt (/ 20.0 3)) 5.0)))
             1e-10)))
    (testing "returns NaN for single element"
      (is (Double/isNaN (stats/cv [5.0]))))
    (testing "returns NaN for empty collection"
      (is (Double/isNaN (stats/cv []))))
    (testing "returns NaN when mean is zero"
      (is (Double/isNaN (stats/cv [-1.0 1.0]))))))

(deftest boxplot-outlier-thresholds-test
  (testing "boxplot-outlier-thresholds"
    (testing "returns [severe-low mild-low mild-high severe-high]"
      (is (= [-4.0 -1.0 7.0 10.0] (stats/boxplot-outlier-thresholds 2.0 4.0))))))

(defspec sample-uniform-range-test 50
  (testing "sample-uniform"
    (testing "returns values in [0..n)]"
      (prop/for-all
       [t gen/nat]
       (every?
        #(<= 0 % t)
        (stats/sample-uniform 100 t (random/well-rng-1024a)))))))

(defspec sample-uniform-count-test 50
  (testing "sample-uniform"
    (testing "returns the correct number of samples"
      (prop/for-all
       [t gen/nat]
       (= t
          (count (stats/sample-uniform t 1 (random/well-rng-1024a))))))))

(deftest confidence-interval-test
  (testing "confidence-interval"
    (testing "returns 95% confidence interval around mean"
      (let [[low high] (stats/confidence-interval 100.0 25.0)]
        (is (< low 100.0))
        (is (> high 100.0))
        (is (< (- 100.0 low) 10.0))
        (is (< (- high 100.0) 10.0))))))

;;; Probability tests
;; Tests verify contract: probability functions compute expected values
;; within documented error bounds.

(defn- abs-error
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

;; Values from R, qnorm (with options(digits=15))
(deftest normal-quantile-test
  (testing "normal-quantile"
    (testing "returns 0 at median"
      (is (= 0.0 (stats/normal-quantile 0.5))))
    (testing "returns positive for p > 0.5"
      (is (pos? (stats/normal-quantile 0.5001))))
    (testing "returns negative for p < 0.5"
      (is (neg? (stats/normal-quantile 0.4999))))
    (testing "is monotonic"
      (is (< 2e-8 (- (stats/normal-quantile 0.999)
                     (stats/normal-quantile 0.001)))))
    (let [max-error 1.0e-7]
      (testing "matches R qnorm at 0.9"
        (is (< (abs-error 1.2815515655446 (stats/normal-quantile 0.9))
               max-error)))
      (testing "matches R qnorm at 0.75"
        (is (< (abs-error 0.674489750196082 (stats/normal-quantile 0.75))
               max-error)))
      (testing "matches R qnorm at 0.15"
        (is (< (abs-error -1.03643338949379 (stats/normal-quantile 0.15))
               max-error)))
      (testing "matches R qnorm at 0.01"
        (is (< (abs-error -2.32634787404084 (stats/normal-quantile 0.01))
               max-error))))))

;; Values from R, erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
(deftest erf-test
  (testing "erf"
    (let [max-error 1.5e-7]
      (testing "matches R at x=4.0"
        (is (< (abs-error 0.999999984582742 (stats/erf 4.0)) max-error)))
      (testing "matches R at x=2.0"
        (is (< (abs-error 0.995322265018953 (stats/erf 2.0)) max-error)))
      (testing "matches R at x=1.0"
        (is (< (abs-error 0.842700792949715 (stats/erf 1.0)) max-error)))
      (testing "matches R at x=0.1"
        (is (< (abs-error 0.112462916018285 (stats/erf 0.1)) max-error)))
      (testing "matches R at x=0.01"
        (is (< (abs-error 0.0112834155558497 (stats/erf 0.01)) max-error))))))

;; Values from R, pnorm
(deftest normal-cdf-test
  (testing "normal-cdf"
    (let [max-error 1.5e-7]
      (testing "returns 0.5 at x=0"
        (is (< (abs-error 0.5 (stats/normal-cdf 0.0)) max-error)))
      (testing "matches R pnorm at x=3.0"
        (is (< (abs-error 0.99865010196837 (stats/normal-cdf 3.0)) max-error)))
      (testing "matches R pnorm at x=2.0"
        (is (< (abs-error 0.977249868051821 (stats/normal-cdf 2.0)) max-error)))
      (testing "matches R pnorm at x=1.0"
        (is (< (abs-error 0.841344746068543 (stats/normal-cdf 1.0)) max-error)))
      (testing "matches R pnorm at x=0.5"
        (is (< (abs-error 0.691462461274013 (stats/normal-cdf 0.5)) max-error)))
      (testing "matches R pnorm at x=-1.0"
        (is (< (abs-error 0.158655253931457 (stats/normal-cdf -1.0)) max-error)))
      (testing "matches R pnorm at x=-3.0"
        (is (< (abs-error 0.00134989803163009 (stats/normal-cdf -3.0))
               max-error))))))

(deftest normal-pdf-test
  (testing "normal-pdf"
    (testing "returns function for given mu and sigma"
      (let [pdf (stats/normal-pdf 0.0 1.0)]
        (is (fn? pdf))))
    (testing "standard normal pdf at mean equals 1/sqrt(2*pi)"
      (let [pdf (stats/normal-pdf 0.0 1.0)
            expected (/ 1.0 (Math/sqrt (* 2.0 Math/PI)))]
        (is (< (abs-error expected (pdf 0.0)) 1e-10))))
    (testing "pdf at mu equals 1/(sigma*sqrt(2*pi))"
      (let [mu 5.0
            sigma 2.0
            pdf (stats/normal-pdf mu sigma)
            expected (/ 1.0 (* sigma (Math/sqrt (* 2.0 Math/PI))))]
        (is (< (abs-error expected (pdf mu)) 1e-10))))))

;;; Histogram tests
;; Tests verify contract: histogram computes correct bin structure and counts.

(deftest histogram-test
  (testing "histogram"
    (testing "computes valid histogram structure"
      (let [data (range 1 101)
            h (stats/histogram data)]
        (is (= :criterium/histogram-fixed-width (:type h)))
        (is (= 100 (:n h)))
        (is (= 1 (:min h)))
        (is (= 100 (:max h)))
        (is (vector? (:counts h)))
        (is (vector? (:centers h)))
        (is (vector? (:density h)))
        (is (number? (:width h)))
        (is (pos? (:num-bins h)))))
    (testing "bin counts sum to sample count"
      (let [data (range 1 101)
            h (stats/histogram data)]
        (is (= (:n h) (reduce + (:counts h))))))
    (testing "density sums to approximately 1"
      (let [data (range 1 101)
            h (stats/histogram data)
            density-sum (reduce + (:density h))]
        (is (< (abs-error 1.0 density-sum) 0.01))))
    (testing "throws on empty input"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"empty"
           (stats/histogram []))))
    (testing "throws on constant values"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"same"
           (stats/histogram (repeat 100 5.0)))))
    (testing "accepts precomputed IQR"
      (let [data (range 1 101)
            iqr 25.0
            h (stats/histogram data iqr)]
        (is (= :criterium/histogram-fixed-width (:type h)))))))
