(ns criterium.domain-plans-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.test-util :refer [mock-bench-result
                                       mock-bench-result-with-defs]]))

;; Tests for pre-defined domain plans.
;; Validates that domain-plans namespace provides valid plan structures.

(deftest domain-plans-structure-test
  ;; Tests for domain plan structure validation.
  ;; Validates that pre-defined plans have required keys.
  ;; Note: :viewer is optional - uses default viewer when not specified.
  (testing "domain-plans structure"
    (testing "complexity-analysis has required keys"
      (is (vector? (:analyse domain-plans/complexity-analysis)))
      (is (vector? (:view domain-plans/complexity-analysis))))
    (testing "implementation-comparison has required keys"
      (is (vector? (:analyse domain-plans/implementation-comparison)))
      (is (vector? (:view domain-plans/implementation-comparison))))
    (testing "extract-elapsed-time has required keys"
      (is (vector? (:analyse domain-plans/extract-elapsed-time)))
      (is (vector? (:view domain-plans/extract-elapsed-time))))))

(deftest domain-plans-integration-test
  ;; Tests for executing pre-defined domain plans.
  ;; Validates end-to-end plan execution with actual domains.
  (testing "domain-plans integration"
    (testing "complexity-analysis executes successfully"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 100.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 200.0}})}
               {:coord {:n 300}
                :data (mock-bench-result {:elapsed-time {:mean 300.0}})})
            plan (analysis/options->domain-plan
                  domain-plans/complexity-analysis
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (contains? result :extract))
        (is (contains? result :regression))
        (is (= :criterium/domain-extract (:type (:extract result))))
        (is (= :criterium/domain-regression (:type (:regression result))))))
    (testing "implementation-comparison executes successfully"
      (let [d (domain/domain
               {:coord {:impl :foo}
                :data (mock-bench-result-with-defs {:elapsed-time {:mean 1.0}})}
               {:coord {:impl :bar}
                :data (mock-bench-result-with-defs {:elapsed-time {:mean 2.0}})}
               {:implementations [:foo :bar]})
            plan (analysis/options->domain-plan
                  domain-plans/implementation-comparison
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (contains? result :comparison))
        (is (= :criterium/domain-comparison (:type (:comparison result))))
        (is (contains? (:comparison result) :metrics))
        (is (= [:foo :bar] (:implementations (:comparison result))))))
    (testing "extract-elapsed-time executes successfully"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            plan (analysis/options->domain-plan
                  domain-plans/extract-elapsed-time
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (contains? result :extract))
        (is (= :criterium/domain-extract (:type (:extract result))))))))
