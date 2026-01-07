(ns criterium.call-graph.plans-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.call-graph.plans :as plans]))

;; Tests for pre-defined call graph plans.
;; Validates that plans have required structure.

(deftest plans-structure-test
  (testing "call-graph plans structure"
    (testing "default has required keys"
      (is (vector? (:analyse plans/default)))
      (is (vector? (:view plans/default)))
      (is (= [:call-tree :call-flame :most-called] (:view plans/default))))
    (testing "tree-only has required keys"
      (is (vector? (:analyse plans/tree-only)))
      (is (vector? (:view plans/tree-only)))
      (is (= [:call-tree] (:view plans/tree-only))))
    (testing "flame-only has required keys"
      (is (vector? (:analyse plans/flame-only)))
      (is (vector? (:view plans/flame-only)))
      (is (= [:call-flame] (:view plans/flame-only))))))

(deftest plans-analyse-test
  ;; Tests that analyse vectors have expected contents.
  (testing "call-graph plans analyse vectors"
    (testing "default includes most-called analysis"
      (is (= [:most-called] (:analyse plans/default))))
    (testing "tree-only and flame-only have empty analyse"
      (is (empty? (:analyse plans/tree-only)))
      (is (empty? (:analyse plans/flame-only))))))
