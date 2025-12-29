(ns optimisation.regression-test
  ;; Tests for linear regression algorithm.
  ;; Validates coefficient calculation, variance, and R-squared.
  (:require
   [clojure.test :refer [deftest is testing]]
   [optimisation.interface :as opt]))

(deftest linear-regression-test
  (testing "linear-regression"
    (testing "with perfect positive identity fit (y = x)"
      (let [result (opt/linear-regression (range 10) (range 10))]
        (is (= [0.0 1.0] (:coeffs result))
            "coefficients should be [0, 1]")
        (is (= 1.0 (:r-sqr result))
            "R-squared should be 1.0 for perfect fit")))

    (testing "with perfect positive offset fit (y = x + 1)"
      (let [result (opt/linear-regression (range 10) (range 1 11))]
        (is (= [1.0 1.0] (:coeffs result))
            "coefficients should be [1, 1]")
        (is (= 1.0 (:r-sqr result))
            "R-squared should be 1.0 for perfect fit")))

    (testing "with doubled slope (y = 2x)"
      (let [xs     (range 1 11)
            ys     (map #(* 2 %) xs)
            result (opt/linear-regression xs ys)]
        (is (< (Math/abs (- 0.0 (first (:coeffs result)))) 1e-10)
            "intercept should be ~0")
        (is (< (Math/abs (- 2.0 (second (:coeffs result)))) 1e-10)
            "slope should be ~2")
        (is (> (:r-sqr result) 0.999)
            "R-squared should be ~1.0")))

    (testing "with noisy data"
      (let [xs     [1 2 3 4 5]
            ys     [2.1 3.9 6.2 7.8 10.1]
            result (opt/linear-regression xs ys)]
        (is (< (Math/abs (- 2.0 (second (:coeffs result)))) 0.2)
            "slope should be approximately 2")
        (is (> (:r-sqr result) 0.99)
            "R-squared should be high for near-linear data")
        (is (pos? (:variance result))
            "variance should be positive for noisy data")))))

(deftest sum-square-delta-test
  (testing "sum-square-delta"
    (testing "with values centered on mean"
      (is (= 0.0 (opt/sum-square-delta [5.0 5.0 5.0] 5.0))
          "should be 0 when all values equal mean"))

    (testing "with symmetric deviation from mean"
      (is (= 2.0 (opt/sum-square-delta [4.0 6.0] 5.0))
          "should sum squared deviations: (4-5)^2 + (6-5)^2 = 2"))

    (testing "with asymmetric values"
      (is (= 14.0 (opt/sum-square-delta [1.0 2.0 3.0] 0.0))
          "should be 1 + 4 + 9 = 14"))))
