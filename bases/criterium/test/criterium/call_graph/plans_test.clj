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
      (is (= [:call-tree :call-flame] (:view plans/default))))
    (testing "tree-only has required keys"
      (is (vector? (:analyse plans/tree-only)))
      (is (vector? (:view plans/tree-only)))
      (is (= [:call-tree] (:view plans/tree-only))))
    (testing "flame-only has required keys"
      (is (vector? (:analyse plans/flame-only)))
      (is (vector? (:view plans/flame-only)))
      (is (= [:call-flame] (:view plans/flame-only))))))

(deftest plans-analyse-empty-test
  ;; Call graph plans currently have empty :analyse vectors.
  ;; This test documents the current behavior.
  (testing "call-graph plans analyse vectors"
    (testing "are empty (reserved for future use)"
      (is (empty? (:analyse plans/default)))
      (is (empty? (:analyse plans/tree-only)))
      (is (empty? (:analyse plans/flame-only))))))
