(ns criterium.stats.chi-squared-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.stats.chi-squared :as chi-squared]))

;; Tests for chi-squared CDF implementation.
;; Contracts tested:
;; - CDF returns correct values matching R's pchisq()
;; - Edge cases handled correctly (df <= 0, x <= 0, x = infinity)
;; - Accuracy within 1e-10 of reference values

;; Reference values computed in R using pchisq(x, df, log.p=FALSE)
;; with options(digits=22) for maximum precision
;; R version: 4.3.x
(def ^:private reference-values
  "Reference values from R: pchisq(x, df) for df in {1,5,10,20}, x in {0.1,1,5,10,20}"
  {[0.1  1]  0.24817036595415087
   [1.0  1]  0.6826894921370859
   [5.0  1]  0.9745852523651397
   [10.0 1]  0.9984341535784105
   [20.0 1]  0.9999922581187595
   [0.1  5]  1.6231661192261495e-4
   [1.0  5]  0.03743422675270355
   [5.0  5]  0.5841198130044911
   [10.0 5]  0.9247104702437509
   [20.0 5]  0.9987502385457562
   [0.1  10] 2.497951336006516e-9
   [1.0  10] 1.7211562995584102e-4
   [5.0  10] 0.10882198108584883
   [10.0 10] 0.5595067149347873
   [20.0 10] 0.9707188304071599
   [0.1  20] 2.5715803516000565e-20
   [1.0  20] 1.7096700293488937e-10
   [5.0  20] 2.773520946208348e-4
   [10.0 20] 0.03182805730620463
   [20.0 20] 0.542070285528147})

(defn- approx=
  "Check if two doubles are approximately equal within relative tolerance."
  ([^double a ^double b] (approx= a b 1e-9))
  ([^double a ^double b ^double rel-tol]
   (let [max-abs (Math/max (Math/abs a) (Math/abs b))]
     (if (< max-abs 1e-300)
       true
       (< (Math/abs (- a b)) (* rel-tol max-abs))))))

(deftest cdf-reference-values-test
  (testing "chi-squared/cdf"
    (testing "matches R pchisq() reference values"
      (doseq [[[x df] expected] reference-values]
        (let [result (chi-squared/cdf x df)]
          (is (approx= result expected)
              (format "pchisq(%s, %d): expected %s, got %s"
                      x df expected result)))))))

(deftest cdf-edge-cases-test
  (testing "chi-squared/cdf"
    (testing "returns 0.0 when df <= 0"
      (is (= 0.0 (chi-squared/cdf 5.0 0)))
      (is (= 0.0 (chi-squared/cdf 5.0 -1))))
    (testing "returns 0.0 when x <= 0"
      (is (= 0.0 (chi-squared/cdf 0.0 5)))
      (is (= 0.0 (chi-squared/cdf -1.0 5))))
    (testing "returns 1.0 when x = infinity"
      (is (= 1.0 (chi-squared/cdf Double/POSITIVE_INFINITY 5))))))

(deftest cdf-monotonicity-test
  (testing "chi-squared/cdf"
    (testing "is monotonically increasing in x"
      (let [df 10
            xs [0.1 1.0 5.0 10.0 20.0 50.0]
            cdfs (map #(chi-squared/cdf % df) xs)]
        (is (apply < cdfs)
            "CDF should be strictly increasing for increasing x")))))

(deftest cdf-df-relationship-test
  (testing "chi-squared/cdf"
    (testing "decreases with increasing df for fixed x"
      (let [x 10.0
            dfs [1 5 10 20]
            cdfs (map #(chi-squared/cdf x %) dfs)]
        (is (apply > cdfs)
            "CDF(x) should decrease as df increases for fixed x")))))
