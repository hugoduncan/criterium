(ns criterium.util.stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.data.r-validation.adjbox :as adjbox-data]
   [criterium.data.r-validation.medcouple :as mc-data]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well])
  (:import
   [java.lang Math]))

(deftest mean-test
  (is (= 1.0 (stats/mean (repeat 20 1))))
  (is (= 3.0 (stats/mean (range 0 7))))
  (is (= 50.0 (stats/mean (range 0 101)))))

(deftest sum-test
  (is (= 20 (stats/sum (take 20 (repeatedly (constantly 1))))))
  (is (= 21 (stats/sum (range 0 7)))))

(deftest sum-of-squares-test
  (is (= 20.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 1))))))
  (is (= 80.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 2))))))
  (is (= 91.0 (stats/sum-of-squares (range 0 7)))))

(deftest variance-test
  (is (= 0.0 (stats/variance (take 20 (repeatedly (constantly 1))))))
  (is (= 4.0 (stats/variance (range 0 7) 0)))
  (is (= 850.0 (stats/variance (range 0 101) 0))) ; R: mean((y-mean(y))^2)
  (is (= 858.5 (stats/variance (range 0 101) 1)))) ; R: var(y)

(deftest median-test
  ;; R: median(vs)
  (is (= [5 [1 2] [7 8]]
         (stats/median [1 2 5 7 8])))
  (is (= [3.5 [1 2 2] [5 7 8]]
         (stats/median [1 2 2 5 7 8]))))

(deftest quartiles-test
  ;; R: quantile(vs, prob=c(.25,0.5,.75))
  (is (= [1.5 5 7.5]
         (stats/quartiles [1 2 5 7 8])))
  (is (= [2 3.5 7]
         (stats/quartiles [1 2 2 5 7 8]))))

(deftest quantile-test
  (testing "exact data points"
    ;; R: quantile(c(1,2,5,7,8), prob=c(.25,0.5,.75))
    (is (= 2 (stats/quantile 0.25 [1 2 5 7 8])))
    (is (= 5 (stats/quantile 0.5 [1 2 5 7 8])))
    (is (= 7 (stats/quantile 0.75 [1 2 5 7 8]))))
  (testing "interpolated data points"
    ;; R: quantile(c(1,2,2,5,7,8), prob=c(.25,0.5,.75))
    (is (= 2.0 (stats/quantile 0.25 [1 2 2 5 7 8])))
    (is (= 3.5 (stats/quantile 0.5 [1 2 2 5 7 8])))
    (is (= 6.5 (stats/quantile 0.75 [1 2 2 5 7 8])))))

(defspec sample-uniform-test
  (testing "sample-uniform"
    (testing "returns values in [0..n)]"
      (prop/for-all
       [t gen/nat]
       (every?
        #(<= 0 % t)
        (stats/sample-uniform 100 t (well/well-rng-1024a)))))))

(defspec sample-uniform-count-test
  (testing "sample-uniform"
    (testing "returns the correct number of samples"
      (prop/for-all
       [t gen/nat]
       (= t
          (count (stats/sample-uniform t 1 (well/well-rng-1024a))))))))

(deftest boxplot-outlier-thresholds-test
  (is (= [-4.0 -1.0 7.0 10.0] (stats/boxplot-outlier-thresholds 2.0 4.0))))

(deftest quantiles-test
  (let [max-error 1.5e-7]
    (test-max-error 1.0 (stats/quantile 0.5 [0 1 2]) max-error)
    (test-max-error 1.5 (stats/quantile 0.5 [0 1 2 3]) max-error)
    (test-max-error 1.0 (stats/quantile 0.25 [0 1 1.5 2 3]) max-error)
    (test-max-error 2.0 (stats/quantile 0.75 [0 1 1.5 2 3]) max-error))
  (is (= 5 (stats/quantile 0.05 (range 0 101))))
  (is (= 95 (stats/quantile 0.95 (range 0 101)))))

;;; Medcouple tests
;; Tests the medcouple function, a robust measure of skewness.
;; Validates against reference values computed from the algorithm definition
;; in Brys, Hubert, and Struyf (2004).

(deftest medcouple-test
  (testing "medcouple"
    (testing "returns correct values for reference test cases"
      (doseq [{:keys [data expected description]} mc-data/test-cases]
        (testing description
          (let [result   (stats/medcouple data)
                max-diff 1e-10]
            (is (< (Math/abs (- ^double result ^double expected)) max-diff)
                (format "Expected %s, got %s for %s"
                        expected result description))))))

    (testing "returns correct value for ozone data"
      (let [{:keys [data expected]} mc-data/ozone-data
            result                  (stats/medcouple data)
            max-diff                1e-10]
        (is (< (Math/abs (- ^double result ^double expected)) max-diff)
            (format "Expected %s, got %s" expected result))))

    (testing "returns value in range [-1, 1]"
      (doseq [{:keys [data]} mc-data/test-cases]
        (let [result (stats/medcouple data)]
          (is (<= -1.0 result 1.0)
              (format "Medcouple %s out of range [-1, 1]" result)))))

    (testing "is symmetric under reflection"
      (let [data     [1 2 3 4 5 10 15 20]
            reflected (mapv (fn [^long x] (- x)) (reverse data))
            mc-orig (double (stats/medcouple (vec (sort data))))
            mc-ref (double (stats/medcouple (vec (sort reflected))))]
        (is (< (Math/abs (+ mc-orig mc-ref)) 1e-10)
            "mc(-x) should equal -mc(x)")))

    (testing "is location and scale invariant"
      (let [data       [1 2 3 4 5 10 15 20]
            shifted    (mapv (fn [^long x] (+ x 100)) data)
            scaled     (mapv (fn [^long x] (* x 10)) data)
            mc-orig (double (stats/medcouple (vec (sort data))))
            mc-shifted (double (stats/medcouple (vec (sort shifted))))
            mc-scaled (double (stats/medcouple (vec (sort scaled))))]
        (is (< (Math/abs (- mc-orig mc-shifted)) 1e-10)
            "Medcouple should be location invariant")
        (is (< (Math/abs (- mc-orig mc-scaled)) 1e-10)
            "Medcouple should be scale invariant")))))

(defspec medcouple-range-test 50
  (testing "medcouple"
    (testing "always returns value in [-1, 1]"
      (prop/for-all
       [data (gen/vector gen/small-integer 3 100)]
       (let [sorted (vec (sort data))
             mc     (double (stats/medcouple sorted))]
         (and (<= -1.0 mc) (<= mc 1.0)))))))

;;; Adjusted boxplot outlier threshold tests
;; Tests the adjusted-boxplot-outlier-thresholds function which computes
;; asymmetric outlier fences based on the medcouple skewness measure.
;; Validates against values computed from the Hubert & Vandervieren (2008)
;; formula with coefficients a=-4, b=3.

(deftest adjusted-boxplot-outlier-thresholds-test
  (testing "adjusted-boxplot-outlier-thresholds"
    (testing "returns correct values for reference test cases"
      (doseq [{:keys [q1 q3 mc expected description]} adjbox-data/test-cases]
        (testing description
          (let [result   (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)
                max-diff 1e-10]
            (is (= 4 (count result)) "should return 4 threshold values")
            (doseq [[i exp act] (map vector (range) expected result)]
              (is (< (Math/abs (- ^double exp ^double act)) max-diff)
                  (format "Threshold %d: expected %s, got %s for %s"
                          i exp act description)))))))

    (testing "returns correct values for ozone data"
      (let [{:keys [q1 q3 mc expected]} adjbox-data/ozone-data
            result                      (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)
            max-diff                    1e-10]
        (doseq [[i exp act] (map vector (range) expected result)]
          (is (< (Math/abs (- ^double exp ^double act)) max-diff)
              (format "Ozone threshold %d: expected %s, got %s" i exp act)))))

    (testing "equals standard boxplot when mc = 0"
      (let [q1       2.0
            q3       8.0
            standard (stats/boxplot-outlier-thresholds q1 q3)
            adjusted (stats/adjusted-boxplot-outlier-thresholds q1 q3 0.0)]
        (is (= standard adjusted)
            "Adjusted thresholds should equal standard when mc=0")))

    (testing "widens upper fence for right-skewed data (mc > 0)"
      (let [q1                2.0
            q3                8.0
            mc                0.3
            [_ _ std-high _]  (stats/boxplot-outlier-thresholds q1 q3)
            [_ _ adj-high _]  (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)]
        (is (> adj-high std-high)
            "Upper fence should be wider for right-skewed data")))

    (testing "narrows lower fence for right-skewed data (mc > 0)"
      (let [q1               2.0
            q3               8.0
            mc               0.3
            [_ std-low _ _]  (stats/boxplot-outlier-thresholds q1 q3)
            [_ adj-low _ _]  (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)]
        (is (> adj-low std-low)
            "Lower fence should be narrower (higher) for right-skewed data")))

    (testing "widens lower fence for left-skewed data (mc < 0)"
      (let [q1               2.0
            q3               8.0
            mc               -0.3
            [_ std-low _ _]  (stats/boxplot-outlier-thresholds q1 q3)
            [_ adj-low _ _]  (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)]
        (is (< adj-low std-low)
            "Lower fence should be wider (lower) for left-skewed data")))

    (testing "narrows upper fence for left-skewed data (mc < 0)"
      (let [q1                2.0
            q3                8.0
            mc                -0.3
            [_ _ std-high _]  (stats/boxplot-outlier-thresholds q1 q3)
            [_ _ adj-high _]  (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)]
        (is (< adj-high std-high)
            "Upper fence should be narrower for left-skewed data")))))
