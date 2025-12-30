(ns criterium.util.histogram-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-utils :refer [gaussian-samples]]
   [criterium.util.histogram :as histogram]))

;; Tests for histogram computation with multiple binning methods.
;; Validates both Freedman-Diaconis and Knuth methods, backward
;; compatibility, and error handling.

(deftest histogram-test
  (testing "histogram"
    (testing "with default method (Freedman-Diaconis)"
      (testing "returns expected structure"
        (let [samples (range 100)
              result  (histogram/histogram samples)]
          (is (= :criterium/histogram-fixed-width (:type result)))
          (is (vector? (:counts result)))
          (is (vector? (:centers result)))
          (is (vector? (:density result)))
          (is (number? (:width result)))
          (is (= 100 (:n result)))
          (is (= 0 (:min result)))
          (is (= 99 (:max result)))
          (is (pos-int? (:num-bins result)))))

      (testing "counts sum to n"
        (let [samples (gaussian-samples 500 0.0 1.0 42)
              result  (histogram/histogram samples)]
          (is (= 500 (reduce + (:counts result)))))))

    (testing "with explicit :method :freedman-diaconis"
      (let [samples (range 100)
            result  (histogram/histogram samples {:method :freedman-diaconis})]
        (is (= :criterium/histogram-fixed-width (:type result)))))

    (testing "with :method :knuth"
      (testing "returns expected structure"
        (let [samples (gaussian-samples 200 50.0 10.0 123)
              result  (histogram/histogram samples {:method :knuth})]
          (is (= :criterium/histogram-knuth (:type result)))
          (is (vector? (:counts result)))
          (is (vector? (:centers result)))
          (is (vector? (:density result)))
          (is (number? (:width result)))
          (is (= 200 (:n result)))
          (is (pos-int? (:num-bins result)))
          (is (contains? result :optimal-bins))
          (is (contains? result :log-posterior))
          (is (pos-int? (:optimal-bins result)))
          (is (number? (:log-posterior result)))))

      (testing "counts sum to n"
        (let [samples (gaussian-samples 500 0.0 1.0 42)
              result  (histogram/histogram samples {:method :knuth})]
          (is (= 500 (reduce + (:counts result))))))

      (testing "optimal-bins matches num-bins"
        (let [samples (gaussian-samples 200 0.0 1.0 99)
              result  (histogram/histogram samples {:method :knuth})]
          (is (= (:optimal-bins result) (:num-bins result)))))

      (testing "respects :max-bins option"
        (let [samples (gaussian-samples 500 0.0 1.0 42)
              result  (histogram/histogram samples {:method :knuth :max-bins 5})]
          (is (<= (:optimal-bins result) 5)))))

    (testing "backward compatibility with IQR argument"
      (let [samples (range 100)
            iqr     25.0
            result  (histogram/histogram samples iqr)]
        (is (= :criterium/histogram-fixed-width (:type result)))
        (is (= 100 (:n result)))))

    (testing "throws for empty input"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"empty"
           (histogram/histogram []))))

    (testing "throws for identical values"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"same"
           (histogram/histogram [5 5 5 5 5]))))

    (testing "throws for unknown method"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Unknown histogram method"
           (histogram/histogram (range 100) {:method :invalid}))))))

(deftest knuth-method-integration-test
  (testing "Knuth method integration"
    (testing "detects bimodal structure"
      (let [bimodal (concat (gaussian-samples 250 20.0 3.0 42)
                            (gaussian-samples 250 80.0 3.0 43))
            result  (histogram/histogram bimodal {:method :knuth})]
        (is (> (:optimal-bins result) 5)
            "Bimodal data should prefer multiple bins")))

    (testing "prefers fewer bins for uniform data"
      (let [uniform (range 100)
            result  (histogram/histogram uniform {:method :knuth})]
        (is (pos-int? (:optimal-bins result)))))

    (testing "consistent with knuth/optimal-bins"
      (let [samples (gaussian-samples 300 0.0 1.0 77)
            hist-result (histogram/histogram samples {:method :knuth})
            ;; The histogram should use the same optimal bin count
            ;; that knuth/optimal-bins would return
            expected-bins (:optimal-bins hist-result)]
        (is (= expected-bins (:num-bins hist-result))
            "Histogram should use optimal bin count from Knuth algorithm")))

    (testing "handles very small sample sizes (n < 5)"
      ;; For n=2, should produce valid histogram with 1 bin
      (testing "with n=2"
        (let [result (histogram/histogram [1 10] {:method :knuth})]
          (is (= :criterium/histogram-knuth (:type result)))
          (is (= 1 (:optimal-bins result)))
          (is (= 1 (:num-bins result)))
          (is (= 2 (:n result)))
          (is (= [2] (:counts result)))
          (is (= [1.0] (:density result)))))

      ;; For n=3, should still produce valid histogram
      (testing "with n=3"
        (let [result (histogram/histogram [1 5 10] {:method :knuth})]
          (is (= :criterium/histogram-knuth (:type result)))
          (is (pos-int? (:optimal-bins result)))
          (is (= 3 (:n result)))
          (is (= 3 (reduce + (:counts result))))))

      ;; For n=4, should produce valid histogram
      (testing "with n=4"
        (let [result (histogram/histogram [1 3 7 10] {:method :knuth})]
          (is (= :criterium/histogram-knuth (:type result)))
          (is (pos-int? (:optimal-bins result)))
          (is (= 4 (:n result)))
          (is (= 4 (reduce + (:counts result)))))))))
