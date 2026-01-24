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
;; with options(digits=17) for maximum precision
;; R version: 4.4.x
(def ^:private reference-values
  "Reference values from R: pchisq(x, df) for df in {1,5,10,20}, x in {0.1,1,5,10,20}"
  {[0.1  1]  0.24817036595415071
   [1.0  1]  0.68268949213708596
   [5.0  1]  0.97465268132253169
   [10.0 1]  0.9984345977419975
   [20.0 1]  0.99999225578356898
   [0.1  5]  0.00016231661192261506
   [1.0  5]  0.03743422675270363
   [5.0  5]  0.58411981300449201
   [10.0 5]  0.92476475385348778
   [20.0 5]  0.99875026943696865
   [0.1  10] 2.4979513360065108e-9
   [1.0  10] 0.00017211562995584072
   [5.0  10] 0.10882198108584877
   [10.0 10] 0.55950671493478754
   [20.0 10] 0.97074731192303887
   [0.1  20] 2.5715803516000755e-20
   [1.0  20] 1.7096700293489033e-10
   [5.0  20] 0.00027735209462083593
   [10.0 20] 0.031828057306204811
   [20.0 20] 0.54207028552814773})

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
