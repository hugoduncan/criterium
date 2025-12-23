(ns criterium.schema.instrument-test
  "Tests for malli instrumentation of criterium public API.
  Verifies that instrument!/unstrument! work correctly and that
  invalid inputs produce useful validation errors."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.bench :as bench]
   [criterium.schema.instrument :as inst]))

;;; Helper to run tests with clean instrumentation state

(defmacro with-clean-instrumentation
  "Ensures instrumentation is disabled before and after body."
  [& body]
  `(do
     (inst/unstrument!)
     (try
       ~@body
       (finally
         (inst/unstrument!)))))

;;; instrument! tests

(deftest instrument!-test
  ;; Tests that instrument! enables malli validation on criterium.bench
  ;; functions and returns the instrumented vars.
  (testing "instrument!"
    (testing "returns collection of instrumented vars"
      (with-clean-instrumentation
        (let [result (inst/instrument!)]
          (is (seq result) "Should return non-empty collection")
          (is (every? var? result) "Should contain vars"))))
    (testing "includes criterium.bench vars"
      (with-clean-instrumentation
        (let [result (set (inst/instrument!))]
          (is (contains? result #'bench/set-default-viewer!))
          (is (contains? result #'bench/default-viewer)))))
    (testing "is idempotent"
      (with-clean-instrumentation
        (let [first-result (set (inst/instrument!))
              second-result (set (inst/instrument!))]
          (is (= first-result second-result)))))))

;;; unstrument! tests

(deftest unstrument!-test
  ;; Tests that unstrument! disables malli validation and returns
  ;; the unstrumented vars.
  (testing "unstrument!"
    (testing "returns collection of unstrumented vars"
      (with-clean-instrumentation
        (inst/instrument!)
        (let [result (inst/unstrument!)]
          (is (seq result) "Should return non-empty collection")
          (is (every? var? result) "Should contain vars"))))
    (testing "can be called when not instrumented"
      (with-clean-instrumentation
        (let [result (inst/unstrument!)]
          (is (coll? result) "Should return collection"))))))

;;; Validation error tests

(deftest validation-error-test
  ;; Tests that instrumented functions throw validation errors with
  ;; useful messages when given invalid inputs.
  (testing "validation errors"
    (with-clean-instrumentation
      (inst/instrument!)
      (testing "set-default-viewer! rejects non-keyword"
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #":malli.core/invalid-input"
             (bench/set-default-viewer! "not-a-keyword"))))
      (testing "set-default-viewer! rejects nil"
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #":malli.core/invalid-input"
             (bench/set-default-viewer! nil))))
      (testing "error contains schema information in :data"
        (try
          (bench/set-default-viewer! 123)
          (is false "Expected exception")
          (catch clojure.lang.ExceptionInfo e
            (let [ex-data (ex-data e)
                  inner-data (:data ex-data)]
              (is (contains? inner-data :schema)
                  (str "Expected :schema in :data, got: " (keys inner-data)))
              (is (contains? inner-data :args)
                  (str "Expected :args in :data, got: " (keys inner-data))))))))))

;;; Valid input tests

(deftest valid-input-test
  ;; Tests that instrumented functions work correctly with valid inputs.
  (testing "valid inputs"
    (with-clean-instrumentation
      (inst/instrument!)
      (testing "set-default-viewer! accepts keyword"
        (let [original (bench/default-viewer)]
          (bench/set-default-viewer! :print)
          (is (= :print (bench/default-viewer)))
          (bench/set-default-viewer! original)))
      (testing "default-viewer returns keyword"
        (is (keyword? (bench/default-viewer))))
      (testing "last-bench returns nil or map"
        (let [result (bench/last-bench)]
          (is (or (nil? result) (map? result))))))))
