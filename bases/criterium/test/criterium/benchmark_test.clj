(ns criterium.benchmark-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.benchmark :as benchmark]
   [criterium.test-data :as test-data]))

;; Tests for wrap-with-timing
;; Contract: wrap-with-timing takes a spec and a function, returning a wrapper
;; that times execution and optionally prints results when *time-analyse* is true.

(deftest wrap-with-timing-test
  (testing "wrap-with-timing"
    (testing "when *time-analyse* is false"
      (testing "does not produce output"
        (let [f (fn [x] (assoc x :result 42))
              wrapped (benchmark/wrap-with-timing :test-spec f)
              output (with-out-str (wrapped {:input 1}))]
          (is (= "" output))))

      (testing "returns the result unchanged"
        (let [f (fn [x] (assoc x :result 42))
              wrapped (benchmark/wrap-with-timing :test-spec f)
              result (wrapped {:input 1})]
          (is (= {:input 1 :result 42} result)))))

    (testing "when *time-analyse* is true"
      (testing "produces timing output"
        (let [f (fn [x] (assoc x :result 42))
              wrapped (benchmark/wrap-with-timing :test-spec f)
              output (binding [benchmark/*time-analyse* true]
                       (with-out-str (wrapped {:input 1})))]
          (is (str/includes? output ":test-spec"))
          (is (str/includes? output "ms"))))

      (testing "returns the result unchanged"
        (let [f (fn [x] (assoc x :result 42))
              wrapped (benchmark/wrap-with-timing :test-spec f)
              result (binding [benchmark/*time-analyse* true]
                       (wrapped {:input 1}))]
          (is (= {:input 1 :result 42} result))))

      (testing "prints spec using pr-str format"
        (let [spec [:quantiles {:quantiles [0.025 0.975]}]
              f identity
              wrapped (benchmark/wrap-with-timing spec f)
              output (binding [benchmark/*time-analyse* true]
                       (with-out-str (wrapped {})))]
          (is (str/includes? output "[:quantiles {:quantiles [0.025 0.975]}]"))))

      (testing "prints elapsed time in milliseconds"
        (let [f (fn [x]
                  (Thread/sleep 10)
                  x)
              wrapped (benchmark/wrap-with-timing :slow-fn f)
              output (binding [benchmark/*time-analyse* true]
                       (with-out-str (wrapped {})))]
          (is (re-find #"\d+\.\d+ ms" output)
              "output should include numeric elapsed time"))))))

(deftest resolve-analyse-fn-timing-integration-test
  ;; Tests that resolve-analyse-fn integrates timing wrapper correctly.
  ;; Contract: resolved analysis functions print timing when *time-analyse* is true.
  (testing "resolve-analyse-fn timing integration"
    (testing "when *time-analyse* is true"
      (testing "prints timing output for keyword specs"
        (let [analyse-fn (benchmark/->analyse [:stats])
              data-map (:data (test-data/samples-with-variance-12-map))
              output (binding [benchmark/*time-analyse* true]
                       (with-out-str (analyse-fn data-map)))]
          (is (str/includes? output ":stats"))
          (is (str/includes? output "ms"))))

      (testing "prints timing output for vector specs"
        (let [analyse-fn (benchmark/->analyse [[:quantiles {:quantiles [0.5]}]])
              data-map (:data (test-data/samples-with-variance-12-map))
              output (binding [benchmark/*time-analyse* true]
                       (with-out-str (analyse-fn data-map)))]
          (is (str/includes? output "[:quantiles"))
          (is (str/includes? output "ms"))))

      (testing "prints timing for each step in multi-step analysis"
        (let [analyse-fn (benchmark/->analyse [[:quantiles {:quantiles [0.5]}]
                                               :stats])
              data-map (:data (test-data/samples-with-variance-12-map))
              output (binding [benchmark/*time-analyse* true]
                       (with-out-str (analyse-fn data-map)))]
          (is (str/includes? output ":stats"))
          (is (str/includes? output "[:quantiles"))
          (is (= 2 (count (re-seq #"ms" output)))
              "should print timing for both analysis steps"))))

    (testing "when *time-analyse* is false (default)"
      (testing "produces no timing output"
        (let [analyse-fn (benchmark/->analyse [:stats])
              data-map (:data (test-data/samples-with-variance-12-map))
              output (with-out-str (analyse-fn data-map))]
          (is (= "" output)))))))

(deftest format-elapsed-ms-test
  (testing "format-elapsed-ms"
    (testing "formats to 4 significant figures"
      ;; 1234567890 ns = 1234.56789 ms -> "1234.6"
      (is (= "1234.6"
             (#'benchmark/format-elapsed-ms 1234567890.0)))
      ;; 123456789 ns = 123.456789 ms -> "123.46"
      (is (= "123.46"
             (#'benchmark/format-elapsed-ms 123456789.0)))
      ;; 12345678 ns = 12.345678 ms -> "12.346"
      (is (= "12.346"
             (#'benchmark/format-elapsed-ms 12345678.0)))
      ;; 1234567 ns = 1.234567 ms -> "1.2346"
      (is (= "1.2346"
             (#'benchmark/format-elapsed-ms 1234567.0)))
      ;; 123456 ns = 0.123456 ms -> "0.1235"
      (is (= "0.1235"
             (#'benchmark/format-elapsed-ms 123456.0)))
      ;; 12345 ns = 0.012345 ms -> "0.01235"
      (is (= "0.01235"
             (#'benchmark/format-elapsed-ms 12345.0)))
      ;; 1234.5 ns = 0.0012345 ms -> "0.001235"
      (is (= "0.001235"
             (#'benchmark/format-elapsed-ms 1234.5))))))
