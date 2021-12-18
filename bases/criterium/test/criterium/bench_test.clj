(ns criterium.bench-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.bench :as bench]))

(deftest bench-test
  (testing "bench"
    (vreset! bench/last-bench* nil)
    (is (nil? (bench/last-bench)))
    (let [out (with-out-str (bench/bench 1))]
      (testing "outputs the estimated time on stdout"
        (is (re-find
             #"Elapsed Time: [0-9.]+ [mn]s  3σ \[[0-9.-]+ [0-9.]+]  min [0-9.]+"
             out)))))
  (testing "time with stats"
    (let [out (with-out-str (bench/bench 1 :limit-time-s 0.1))]
      (testing "outputs statistics on stdout"
        (is (re-find #"3σ" out)))))
  (testing "time with one-shot"
    (let [out (with-out-str (bench/bench 1 :collect-plan :one-shot))]
      (testing "outputs statistics on stdout"
        (is (not (re-find #"±" out))))))
  (testing "time returns expression-value"
    (with-out-str
      (let [v (bench/bench 1)]
        (is (= 1 v)))))

  (testing "all pipelines"
    (with-out-str
      (let [v (bench/bench
               1
               :limit-time-s 0.1
               :metric-ids [:elapsed-time
                            :memory
                            :thread-allocation
                            :garbage-collector
                            :finalization
                            :compilation
                            :measured-args
                            :class-loader])]
        (is (= 1 v))))))
