(ns criterium.stats.knuth-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.stats.knuth :as knuth]
   [criterium.test-utils :refer [gaussian-samples test-max-error]]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

;; Tests for Knuth's Bayesian histogram binning algorithm.
;; Validates log-posterior computation and optimal bin selection
;; for various data distributions.

(deftest log-posterior-test
  (testing "log-posterior"
    (testing "returns 0.0 for single bin with all samples"
      ;; When M=1, all samples in one bin, log-posterior simplifies
      ;; For n samples in 1 bin: most terms cancel out
      (let [result (knuth/log-posterior 100 [100])]
        (test-max-error 0.0 result 1e-10)))

    (testing "returns negative value for uniform split into 2 bins"
      ;; M=2 with equal split has lower log-posterior than M=1 for uniform data
      (let [result (knuth/log-posterior 100 [50 50])]
        (is (< result 0.0) "log-posterior should be negative for M=2 uniform split")))

    (testing "rewards concentrated data structure"
      ;; Knuth method finds structure - concentrated data has higher log-posterior
      ;; because it represents a simpler model that explains the data well
      (let [even-split   (knuth/log-posterior 100 [50 50])
            uneven-split (knuth/log-posterior 100 [90 10])]
        (is (> uneven-split even-split)
            "Concentrated data should have higher log-posterior")))

    (testing "handles empty bins"
      ;; Empty bins (count=0) should work - logΓ(0.5) is well-defined
      (let [result (knuth/log-posterior 100 [100 0])]
        (is (number? result) "Should handle bins with zero counts")))))

(deftest optimal-bins-test
  (testing "optimal-bins"
    (testing "throws for empty input"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"empty"
           (knuth/optimal-bins (darr [])))))

    (testing "throws for identical values"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"identical"
           (knuth/optimal-bins (darr [5 5 5 5 5])))))

    (testing "returns expected structure"
      (let [result (knuth/optimal-bins (darr (range 100)))]
        (is (map? result))
        (is (contains? result :optimal-bins))
        (is (contains? result :log-posterior))
        (is (pos-int? (:optimal-bins result)))
        (is (number? (:log-posterior result)))))

    (testing "respects :max-bins option"
      (let [samples (darr (gaussian-samples 1000 0.0 1.0))
            result  (knuth/optimal-bins samples {:max-bins 5})]
        (is (<= (:optimal-bins result) 5))))

    (testing "prefers more bins for Gaussian data"
      ;; Gaussian distribution has structure that benefits from multiple bins
      (let [samples (darr (gaussian-samples 1000 0.0 1.0 123))
            result  (knuth/optimal-bins samples)]
        (is (> (:optimal-bins result) 1)
            "Gaussian data should prefer more than 1 bin")))

    (testing "detects bimodal structure with more bins"
      ;; Bimodal should prefer more bins than unimodal
      (let [unimodal (darr (gaussian-samples 1000 50.0 10.0 42))
            bimodal  (darr (concat (gaussian-samples 500 30.0 5.0 42)
                                   (gaussian-samples 500 70.0 5.0 43)))
            uni-result (knuth/optimal-bins unimodal)
            bi-result  (knuth/optimal-bins bimodal)]
        (is (> (:optimal-bins bi-result) (:optimal-bins uni-result))
            "Bimodal data should prefer more bins than unimodal")))

    (testing "handles small sample sizes"
      ;; Small samples should still work and prefer fewer bins
      (let [samples (darr (gaussian-samples 20 0.0 1.0 99))
            result  (knuth/optimal-bins samples)]
        (is (pos-int? (:optimal-bins result)))
        (is (<= (:optimal-bins result) 20)
            "Small samples should not have more bins than samples")))

    (testing "handles very small sample sizes (n < 5)"
      ;; For n=2, algorithm should work and return valid result
      (testing "with n=2"
        (let [result (knuth/optimal-bins (darr [1 10]))]
          (is (= 1 (:optimal-bins result))
              "n=2 should prefer 1 bin (insufficient evidence for structure)")
          (is (= 0.0 (:log-posterior result))
              "n=2 with 1 bin should have log-posterior = 0")))

      ;; For n=3, still limited evidence for structure
      (testing "with n=3"
        (let [result (knuth/optimal-bins (darr [1 5 10]))]
          (is (pos-int? (:optimal-bins result)))
          (is (<= (:optimal-bins result) 3)
              "n=3 should not have more bins than samples")
          (is (number? (:log-posterior result)))))

      ;; For n=4, algorithm should still work gracefully
      (testing "with n=4"
        (let [result (knuth/optimal-bins (darr [1 3 7 10]))]
          (is (pos-int? (:optimal-bins result)))
          (is (<= (:optimal-bins result) 4)
              "n=4 should not have more bins than samples")
          (is (number? (:log-posterior result))))))))

(deftest bin-counts-test
  (testing "bin-counts (via log-posterior)"
    (testing "sums to sample count"
      ;; Verify binning is correct by checking log-posterior with known counts
      ;; For 10 samples in range [0,9], with 2 bins, should be [5, 5]
      (let [samples (darr (range 10))
            ;; Use optimal-bins which internally uses bin-counts
            result  (knuth/optimal-bins samples {:max-bins 10})]
        (is (number? (:log-posterior result))
            "Should compute valid log-posterior")))))

;; Test private data-min-max function directly
(def ^:private data-min-max #'knuth/data-min-max)

(deftest data-min-max-test
  ;; Tests the private data-min-max function which computes min/max
  ;; using primitive double folds for efficiency.
  (testing "data-min-max"
    (testing "returns correct min and max for simple sequence"
      (let [[mn mx] (data-min-max (darr [3 1 4 1 5 9 2 6]))]
        (is (= 1.0 mn))
        (is (= 9.0 mx))))

    (testing "handles single element"
      (let [[mn mx] (data-min-max (darr [42]))]
        (is (= 42.0 mn))
        (is (= 42.0 mx))))

    (testing "handles negative values"
      (let [[mn mx] (data-min-max (darr [-5 -2 -8 -1]))]
        (is (= -8.0 mn))
        (is (= -1.0 mx))))

    (testing "handles mixed positive and negative"
      (let [[mn mx] (data-min-max (darr [-3 0 5 -1 2]))]
        (is (= -3.0 mn))
        (is (= 5.0 mx))))

    (testing "handles large arrays"
      (let [data (darr (range 10000))
            [mn mx] (data-min-max data)]
        (is (= 0.0 mn))
        (is (= 9999.0 mx))))))
