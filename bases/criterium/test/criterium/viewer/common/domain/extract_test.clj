(ns criterium.viewer.common.domain.extract-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common.domain.extract :as extract]))

;;; Tests for prepare-domain-extract-table helper.
;;; Verifies table generation with correct column key/header matching.

(deftest prepare-domain-extract-table-test
  ;; Tests prepare-domain-extract-table for various scenarios
  (testing "prepare-domain-extract-table"
    (testing "single-impl multi-point uses string key matching column-name"
      ;; This test verifies the fix for issue where axis values were blank
      ;; because coord-header was used as keyword key but column-names are strings
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:default]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 10 :impl :default} 1.0e-6]
                                              [{:n 50 :impl :default} 2.0e-6]
                                              [{:n 100 :impl :default} 3.0e-6]]}}}
            result (extract/prepare-domain-extract-table domain-extract {})]
        (is (= "Domain Extract" (:heading result)))
        (is (= "n" (:coord-header result)))
        ;; Key point: row map keys must match column-names (strings, not keywords)
        (is (= 3 (count (:rows result))))
        (let [first-row (first (:rows result))]
          ;; The coord column key should be the string "n", not :n
          (is (contains? first-row "n"))
          (is (= 10 (get first-row "n"))))))

    (testing "multi-impl uses string key for coord column"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 10 :impl :foo} 1.0e-6]
                                              [{:n 10 :impl :bar} 2.0e-6]
                                              [{:n 50 :impl :foo} 1.5e-6]
                                              [{:n 50 :impl :bar} 2.5e-6]]}}}
            result (extract/prepare-domain-extract-table domain-extract {})]
        (is (= "n" (:coord-header result)))
        (is (= 2 (count (:rows result))))
        (let [first-row (first (:rows result))]
          (is (contains? first-row "n"))
          (is (= 10 (get first-row "n"))))))

    (testing "returns nil for nil extract"
      (is (nil? (extract/prepare-domain-extract-table nil {}))))))

;;; Tests for prepare-domain-extract-table-transposed helper.
;;; Verifies transposed table generation for single-point multi-impl scenarios
;;; where each row is an implementation with value and factor columns.

(deftest prepare-domain-extract-table-transposed-test
  (testing "prepare-domain-extract-table-transposed"
    (testing "returns transposed table with implementation rows"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar :baz]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]
                                              [{:n 100 :impl :baz} 1.5e-6]]}}}
            result (extract/prepare-domain-extract-table-transposed domain-extract)]
        (is (= "Domain Extract" (:heading result)))
        (is (vector? (:col-headers result)))
        (is (= "Implementation" (first (:col-headers result))))
        (is (= 3 (count (:rows result))))
        (is (= "foo" (get (first (:rows result)) "Implementation")))
        (is (= "bar" (get (second (:rows result)) "Implementation")))
        (is (= "baz" (get (nth (:rows result) 2) "Implementation")))))

    (testing "includes factor columns relative to baseline"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]]}}}
            result (extract/prepare-domain-extract-table-transposed domain-extract)
            foo-row (first (:rows result))
            bar-row (second (:rows result))]
        ;; Baseline (foo) should have factor 1.00
        (is (= "1.00" (get foo-row "elapsed-time ×")))
        ;; Bar is 2x baseline
        (is (= "2.00" (get bar-row "elapsed-time ×")))))

    (testing "handles multiple metrics"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo :bar]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]
                                              [{:n 100 :impl :bar} 2.0e-6]]}
                                      :thread-allocation
                                      {:metric [:stats :thread-allocation :mean]
                                       :data [[{:n 100 :impl :foo} 1000]
                                              [{:n 100 :impl :bar} 500]]}}}
            result (extract/prepare-domain-extract-table-transposed domain-extract)
            col-headers (:col-headers result)]
        ;; Should have Implementation + 2 metrics * 2 columns each = 5 headers
        (is (= 5 (count col-headers)))
        (is (= "Implementation" (first col-headers)))
        ;; Should have value and factor columns for each metric
        (is (some #(str/includes? % "elapsed-time") col-headers))
        (is (some #(str/includes? % "thread-allocation") col-headers))
        (is (some #(str/ends-with? % "×") col-headers))))

    (testing "applies SI scaling to values"
      (let [domain-extract {:type :criterium/domain-extract
                            :impl-axis :impl
                            :implementations [:foo]
                            :metrics {:elapsed-time
                                      {:metric [:stats :elapsed-time :mean]
                                       :data [[{:n 100 :impl :foo} 1.0e-6]]}}}
            result (extract/prepare-domain-extract-table-transposed domain-extract)
            col-headers (:col-headers result)]
        ;; Should have SI unit in header (μs or similar for microseconds)
        (is (some #(or (str/includes? % "(")
                       (str/includes? % "μ")
                       (str/includes? % "m"))
                  col-headers))))

    (testing "returns nil for nil extract"
      (is (nil? (extract/prepare-domain-extract-table-transposed nil))))))
