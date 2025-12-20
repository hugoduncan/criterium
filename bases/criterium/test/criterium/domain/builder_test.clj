(ns criterium.domain.builder-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.domain.builder :as builder]
   [criterium.measured :as measured]))

;; Tests for input sequence generators.
;; Validates generation of sequences useful for scaling analysis,
;; including powers of 2, arbitrary powers, logarithmic ranges,
;; and linear ranges.

(deftest powers-of-2-test
  (testing "powers-of-2"
    (testing "generates powers from 0"
      (is (= [1 2 4 8 16] (builder/powers-of-2 0 4))))
    (testing "generates powers from non-zero exponent"
      (is (= [16 32 64 128 256] (builder/powers-of-2 4 8))))
    (testing "handles single value range"
      (is (= [8] (builder/powers-of-2 3 3))))
    (testing "generates large powers"
      (is (= [1024 2048 4096] (builder/powers-of-2 10 12))))))

(deftest powers-of-test
  (testing "powers-of"
    (testing "generates powers of 10"
      (is (= [10 100 1000 10000] (builder/powers-of 10 1 4))))
    (testing "generates powers of 3"
      (is (= [1 3 9 27 81] (builder/powers-of 3 0 4))))
    (testing "generates powers of 2 (same as powers-of-2)"
      (is (= [1 2 4 8] (builder/powers-of 2 0 3))))
    (testing "handles single value range"
      (is (= [100] (builder/powers-of 10 2 2))))))

(deftest log-range-test
  (testing "log-range"
    (testing "generates 4 points from 1 to 1000"
      (let [result (builder/log-range 1 1000 4)]
        (is (= 4 (count result)))
        (is (= 1 (first result)))
        (is (= 1000 (last result)))))
    (testing "generates 5 points from 10 to 10000"
      (let [result (builder/log-range 10 10000 5)]
        (is (= 5 (count result)))
        (is (= 10 (first result)))
        (is (= 10000 (last result)))))
    (testing "produces increasing values"
      (let [result (builder/log-range 1 1000 5)]
        (is (apply < result))))
    (testing "handles 2-point range"
      (is (= [10 1000] (builder/log-range 10 1000 2))))))

(deftest linear-range-test
  (testing "linear-range"
    (testing "generates evenly spaced values"
      (is (= [100 200 300 400 500] (builder/linear-range 100 500 5))))
    (testing "handles range starting at 0"
      (is (= [0 250 500 750 1000] (builder/linear-range 0 1000 5))))
    (testing "handles 2-point range"
      (is (= [100 1000] (builder/linear-range 100 1000 2))))
    (testing "produces increasing values"
      (let [result (builder/linear-range 10 1000 10)]
        (is (apply < result))))
    (testing "handles single point"
      (is (= [500] (builder/linear-range 500 500 1))))))

;; Tests n-log-n-range generates values spaced along an n*log(n) curve.
;; Contracts: correct count, endpoints match, values strictly increasing,
;; edge cases (single point, 2 points), start validation, and even spacing
;; in the n*log(n) domain.
(deftest n-log-n-range-test
  (testing "n-log-n-range"
    (testing "generates correct count of values"
      (is (= 5 (count (builder/n-log-n-range 10 10000 5)))))
    (testing "first value equals start"
      (is (= 10 (first (builder/n-log-n-range 10 10000 5)))))
    (testing "last value equals end"
      (is (= 10000 (last (builder/n-log-n-range 10 10000 5)))))
    (testing "produces strictly increasing values"
      (let [result (builder/n-log-n-range 10 10000 5)]
        (is (apply < result))))
    (testing "produces evenly spaced values in n*log(n) domain"
      (let [result (builder/n-log-n-range 10 10000 7)
            f (fn [x] (* x (Math/log x)))
            y-values (map f result)
            diffs (map - (rest y-values) y-values)
            mean-diff (/ (reduce + diffs) (count diffs))
            ;; Allow 1% tolerance for rounding errors
            tolerance (* 0.01 mean-diff)]
        (is (every? #(< (Math/abs (- % mean-diff)) tolerance) diffs))))
    (testing "handles 2-point range"
      (is (= [10 1000] (builder/n-log-n-range 10 1000 2))))
    (testing "handles single point"
      (is (= [500] (builder/n-log-n-range 500 500 1))))
    (testing "throws for start below e^-1"
      (is (thrown? AssertionError (builder/n-log-n-range 0.1 100 5))))))

;; Tests for domain-builder and related utilities.
;; Validates automated benchmark running across parameter spaces
;; with adaptive time estimation and progress reporting.

(deftest cartesian-product-test
  ;; Tests for internal cartesian product helper.
  ;; Validates generation of all axis value combinations.
  (testing "cartesian-product"
    (testing "returns single empty map for empty axes"
      (is (= [{}] (#'builder/cartesian-product {}))))
    (testing "generates all combinations for single axis"
      (is (= [{:n 1} {:n 2} {:n 3}]
             (#'builder/cartesian-product {:n [1 2 3]}))))
    (testing "generates cartesian product for two axes"
      (let [result (#'builder/cartesian-product {:n [1 2] :m [10 20]})]
        (is (= 4 (count result)))
        (is (= #{:n :m} (set (keys (first result)))))
        (is (= #{[1 10] [1 20] [2 10] [2 20]}
               (set (map (juxt :n :m) result))))))
    (testing "generates cartesian product for three axes"
      (let [result (#'builder/cartesian-product {:a [1] :b [2 3] :c [4 5]})]
        (is (= 4 (count result)))
        (is (every? #(= #{:a :b :c} (set (keys %))) result))))))

(deftest dot-reporter-test
  ;; Tests for dot reporter protocol implementation.
  ;; Validates progress reporting interface.
  (testing "dot-reporter"
    (testing "creates DotReporter instance"
      (let [reporter (builder/dot-reporter)]
        (is (satisfies? builder/DomainBuilderReporter reporter))))
    (testing "report-start prints impl name and run count"
      (let [reporter (builder/dot-reporter)
            output (with-out-str (builder/report-start reporter :test 5))]
        (is (= "test (5 runs): " output))))
    (testing "report-run prints a dot"
      (let [reporter (builder/dot-reporter)
            output (with-out-str (builder/report-run reporter :test {:n 100} 0))]
        (is (= "." output))))
    (testing "report-end prints newline"
      (let [reporter (builder/dot-reporter)
            output (with-out-str (builder/report-end reporter :test))]
        (is (= "\n" output))))))

(deftest measured-impl-map?-test
  ;; Tests for simplified implementation map detection.
  ;; Validates recognition of {impl-key Measured} form vs function form.
  (testing "measured-impl-map?"
    (testing "returns true for map with Measured values"
      (let [m (measured/expr (+ 1 2))]
        (is (true? (#'builder/measured-impl-map? {:impl-a m})))))
    (testing "returns true for map with multiple Measured values"
      (let [m1 (measured/expr (+ 1 2))
            m2 (measured/expr (* 3 4))]
        (is (true? (#'builder/measured-impl-map? {:impl-a m1 :impl-b m2})))))
    (testing "returns false for function form"
      (is (not (#'builder/measured-impl-map?
                {:impl-a (fn [_] (measured/expr (+ 1 2)))}))))
    (testing "returns falsy for empty map"
      (is (not (#'builder/measured-impl-map? {}))))
    (testing "returns falsy for non-map"
      (is (not (#'builder/measured-impl-map? [(measured/expr (+ 1 2))]))))
    (testing "returns falsy for nil"
      (is (not (#'builder/measured-impl-map? nil))))))

(deftest normalize-implementations-test
  ;; Tests for implementation map normalization.
  ;; Validates conversion of Measured values to functions returning Measured.
  (testing "normalize-implementations"
    (testing "converts Measured to function returning that Measured"
      (let [m (measured/expr (+ 1 2))
            result (#'builder/normalize-implementations {:impl-a m})
            impl-fn (:impl-a result)]
        (is (= #{:impl-a} (set (keys result))))
        (is (fn? impl-fn))
        (is (= m (impl-fn {})))
        (is (= m (impl-fn {:n 100})))))
    (testing "preserves multiple implementations"
      (let [m1 (measured/expr (+ 1 2))
            m2 (measured/expr (* 3 4))
            result (#'builder/normalize-implementations {:impl-a m1 :impl-b m2})]
        (is (= #{:impl-a :impl-b} (set (keys result))))
        (is (= m1 ((:impl-a result) {})))
        (is (= m2 ((:impl-b result) {})))))
    (testing "passes through function values unchanged"
      (let [impl-fn (fn [{:keys [n]}] (measured/expr (+ n 1)))
            result (#'builder/normalize-implementations {:impl-a impl-fn})]
        (is (= impl-fn (:impl-a result)))))))
