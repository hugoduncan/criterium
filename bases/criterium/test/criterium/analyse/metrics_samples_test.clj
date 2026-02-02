(ns criterium.analyse.metrics-samples-test
  ;; Tests for the metrics-samples analysis module.
  ;; Verifies modes-for-metric early stopping behavior and correct results.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse.metrics-samples :as ms]
   [criterium.array :as arr]
   [criterium.stats.kde :as kde]
   [criterium.test-utils :as tu]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

(deftest modes-for-metric-early-stopping-test
  ;; Tests that modes-for-metric stops testing k values once p-value >= alpha.
  ;; For unimodal data, it should stop after k=1 instead of testing all k.
  (testing "modes-for-metric"
    (testing "with unimodal data"
      (let [;; Generate clearly unimodal Gaussian data
            data
            (darr (tu/gaussian-samples 200 50.0 10.0 42))
            ;; Compute KDE
            kde-result (kde/kde data {:n-points 256 :n-bootstrap 10})
            ;; Create mock metric config
            metric-config
            {:path [:elapsed-time-ns]}
            ;; Run modes analysis with reduced bootstrap for speed
            options {:n-bootstrap 50 :n-points 256 :alpha 0.05 :max-modes 5}
            result (ms/modes-for-metric
                    kde-result
                    data
                    nil
                    metric-config
                    options)]
        (testing "stops early when k=1 fails to reject H0"
          ;; For unimodal data, k=1 should have p-value >= alpha
          ;; So we should only test k=1 (early stopping)
          (let [k-tested (get-in result [:test-results :k-tested])]
            (is (vector? k-tested))
            ;; Should only test k=1 since it should fail to reject H0
            (is (= [1] k-tested)
                (str "Expected only k=1 to be tested for unimodal data, "
                     "but got: " k-tested))))

        (testing "returns n-modes = 1 for unimodal data"
          (is (= 1 (:n-modes result))))

        (testing "has p-value >= alpha for k=1"
          (let [p-value (get-in result [:test-results :p-values 1])]
            (is (>= p-value 0.05)
                (str
                 "Expected p-value >= 0.05 for unimodal data, got: "
                 p-value))))))

    (testing "with bimodal data"
      (let [;; Generate clearly bimodal data (two separated Gaussians)
            data
            (darr (concat (tu/gaussian-samples 100 20.0 5.0 1)
                          (tu/gaussian-samples 100 80.0 5.0 2)))
            ;; Compute KDE
            kde-result (kde/kde data {:n-points 256 :n-bootstrap 10})
            metric-config {:path [:elapsed-time-ns]}
            options {:n-bootstrap 50 :n-points 256 :alpha 0.05 :max-modes 5}
            result (ms/modes-for-metric
                    kde-result
                    data
                    nil
                    metric-config
                    options)]
        (testing "tests k=1 and rejects it (p-value < alpha)"
          (let [p-value-k1 (get-in result [:test-results :p-values 1])]
            (is (< p-value-k1 0.05)
                (str "Expected p-value < 0.05 for bimodal data at k=1, "
                     "got: " p-value-k1))))

        (testing "stops at k=2 for bimodal data"
          ;; Should test k=1 (reject) then k=2 (fail to reject) then stop
          (let [k-tested (get-in result [:test-results :k-tested])]
            (is (vector? k-tested))
            (is (= [1 2] k-tested)
                (str "Expected k=[1,2] for bimodal data, got: " k-tested))))

        (testing "returns n-modes = 2 for bimodal data"
          (is (= 2 (:n-modes result))))))

    (testing "test results match exhaustive testing"
      ;; Verify that early stopping produces the same n-modes as testing all k
      (let [data (darr (tu/gaussian-samples 150 50.0 10.0 123))
            kde-result (kde/kde data {:n-points 256 :n-bootstrap 10})
            metric-config {:path [:elapsed-time-ns]}
            ;; Run with early stopping (default behavior)
            result-early (ms/modes-for-metric kde-result data nil metric-config
                                              {:n-bootstrap 30 :n-points 256
                                               :alpha 0.05 :max-modes 3})
            n-modes-early (:n-modes result-early)
            k-tested-early (get-in result-early [:test-results :k-tested])]
        ;; The validated k should be the first k where we fail to reject
        ;; This is what we expect from the early stopping algorithm
        (is (= 1 n-modes-early)
            "Unimodal data should validate 1 mode")
        (is (= [1] k-tested-early)
            "Should stop after testing k=1 for unimodal data")))))
