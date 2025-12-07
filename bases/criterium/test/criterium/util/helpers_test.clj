(ns criterium.util.helpers-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
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
