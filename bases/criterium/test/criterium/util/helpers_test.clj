(ns criterium.util.helpers-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.util.helpers :as util]))

(defn- current-thread-priority
  []
  (.getPriority (Thread/currentThread)))

(deftest with-thread-priority-test
  (testing "with-thread-priority"
    (testing "when given a valid priority,"
      (let [original-priority (current-thread-priority)
            priority          (first (set/difference
                                      #{Thread/MAX_PRIORITY Thread/MIN_PRIORITY}
                                      #{original-priority}))]
        (assert (not= original-priority priority))
        (testing "sets the priority for the body,"
          (util/with-thread-priority priority
            (is (= priority (current-thread-priority))))
          (testing "and resets it on exit"
            (is (= original-priority (current-thread-priority)))))
        (testing "sets the priority for the body,"
          (try
            (util/with-thread-priority priority
              (is (= priority (current-thread-priority)))
              (throw (ex-info "Intentional" {})))
            (catch clojure.lang.ExceptionInfo _))
          (testing "and resets it on exit when the body throws"
            (is (= original-priority (current-thread-priority)))))))
    (testing "when given :max-priority,"
      (testing "sets Threaad/MAX_PRIORITY priority for the body,"
        (util/with-thread-priority :max-priority
          (is (= Thread/MAX_PRIORITY (current-thread-priority))))))
    (testing "when given :min-priority,"
      (testing "sets Threaad/MIN_PRIORITY priority for the body,"
        (util/with-thread-priority :min-priority
          (is (= Thread/MIN_PRIORITY (current-thread-priority))))))
    (testing "when given a nil priority,"
      (let [p (current-thread-priority)]
        (util/with-thread-priority nil
          (testing "the thread priority is unchanged"
            (is (= p (current-thread-priority)))))))
    (testing "when given a non-integer priority, throws"
      (is (thrown? clojure.lang.ExceptionInfo
                   (util/with-thread-priority 0.1))))
    (testing "when given an out of range priority, throws"
      (is (thrown? clojure.lang.ExceptionInfo
                   (util/with-thread-priority (dec Thread/MIN_PRIORITY))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (util/with-thread-priority (inc Thread/MAX_PRIORITY)))))))

;;; Bootstrap stats extraction tests
;;;
;;; Tests that bootstrap stats extraction correctly reads from the
;;; :bootstrap-stats -> :bootstrap -> metric-id path structure.

(deftest bootstrap-quantile-value-test
  ;; Tests that bootstrap-quantile-value correctly extracts quantile values
  ;; from the :bootstrap-stats -> :bootstrap -> metric-id -> :quantiles path.
  (testing "bootstrap-quantile-value"
    (let [data-map (test-data/histogram-with-bootstrap-data-map)]
      (testing "extracts p10 quantile from bootstrap stats"
        (is (= 9.2 (util/bootstrap-quantile-value data-map :elapsed-time 0.1))))
      (testing "extracts p50 (median) quantile from bootstrap stats"
        (is
         (= 9.75 (util/bootstrap-quantile-value data-map :elapsed-time 0.5))))
      (testing "extracts p90 quantile from bootstrap stats"
        (is
         (= 10.5 (util/bootstrap-quantile-value data-map :elapsed-time 0.9))))
      (testing "returns nil when bootstrap-stats is missing"
        (is (nil? (util/bootstrap-quantile-value {} :elapsed-time 0.5))))
      (testing "returns nil for non-existent metric"
        (is (nil? (util/bootstrap-quantile-value data-map :nonexistent 0.5))))
      (testing "returns nil for non-existent quantile"
        (is
         (nil? (util/bootstrap-quantile-value data-map :elapsed-time 0.99)))))))

(deftest bootstrap-quantile-ci-test
  ;; Tests that bootstrap-quantile-ci correctly extracts CI bounds
  ;; from the :bootstrap-stats -> :bootstrap -> metric-id path.
  (testing "bootstrap-quantile-ci"
    (let [data-map (test-data/histogram-with-bootstrap-data-map)]
      (testing "extracts CI bounds for median quantile"
        (let [ci (util/bootstrap-quantile-ci data-map :elapsed-time 0.5)]
          (is (= 9.3 (:ci-lower ci)))
          (is (= 10.2 (:ci-upper ci)))))
      (testing "returns nil when CI bounds are empty"
        (is (nil? (util/bootstrap-quantile-ci data-map :elapsed-time 0.1))))
      (testing "returns nil when bootstrap-stats is missing"
        (is (nil? (util/bootstrap-quantile-ci {} :elapsed-time 0.5)))))))

(deftest bootstrap-box-plot-stats-test
  ;; Tests that bootstrap-box-plot-stats correctly assembles box plot data
  ;; from the bootstrap stats structure.
  (testing "bootstrap-box-plot-stats"
    (let [data-map (test-data/histogram-with-bootstrap-data-map)]
      (testing "extracts all required box plot stats"
        (let [stats (util/bootstrap-box-plot-stats data-map :elapsed-time)]
          (is (= 9.2 (:p10 stats)) "p10 should match 0.1 quantile")
          (is (= 9.75 (:median stats)) "median should match 0.5 quantile")
          (is (= 10.5 (:p90 stats)) "p90 should match 0.9 quantile")
          (is (= 9.3 (:ci-lower stats)) "ci-lower from median CI")
          (is (= 10.2 (:ci-upper stats)) "ci-upper from median CI")))
      (testing "returns nil when required quantiles are missing"
        (let [data-without-p10 (update-in data-map
                                          [:bootstrap-stats
                                           :bootstrap
                                           :elapsed-time
                                           :quantiles]
                                          dissoc 0.1)]
          (is (nil? (util/bootstrap-box-plot-stats
                     data-without-p10 :elapsed-time)))))
      (testing "returns nil when bootstrap-stats is missing"
        (is (nil? (util/bootstrap-box-plot-stats {} :elapsed-time)))))))

;;; Single-sample extraction tests
;;;
;;; Tests that samples-single-value correctly extracts raw values from
;;; single-sample data produced by :one-shot benchmarks.

(deftest samples-single-value-test
  ;; Tests that samples-single-value extracts values from single-sample data,
  ;; applies transforms, and returns nil for multi-sample or missing data.
  (testing "samples-single-value"
    (testing "extracts value from single-sample data"
      (let [data-map (:data (test-data/samples-with-1-value-map))]
        (is (= 42.5 (util/samples-single-value data-map :elapsed-time)))))
    (testing "returns nil for multi-sample data"
      (let [data-map (:data (test-data/samples-with-2-values-map))]
        (is (nil? (util/samples-single-value data-map :elapsed-time)))))
    (testing "returns nil when samples is missing"
      (is (nil? (util/samples-single-value {} :elapsed-time))))
    (testing "returns nil for non-existent metric"
      (let [data-map (:data (test-data/samples-with-1-value-map))]
        (is (nil? (util/samples-single-value data-map :nonexistent)))))))
