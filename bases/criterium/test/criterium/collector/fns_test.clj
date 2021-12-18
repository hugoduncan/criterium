(ns criterium.collector.fns-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.collector :as collector]
   [criterium.measured :as measured]))

;; (defn prim-f ^long [^long l]
;;   l)

#_(deftest expression-garbage-test
    (let [[allocations res]     (agent/with-allocation-tracing
                                  {:a "a"})
          {:keys [freed-bytes]} (agent/allocations-summary
                                 (filterv (agent/allocation-on-thread?) allocations))]
      (is (some? res))
      (is (zero? freed-bytes)
          (filterv agent/allocation-freed?
                   (filterv (agent/allocation-on-thread?) allocations))))
    (let [[allocations res]     (agent/with-allocation-tracing
                                  {:a "a"
                                   :b "b"})
          {:keys [freed-bytes]} (agent/allocations-summary
                                 (filterv (agent/allocation-on-thread?) allocations))]
      (is (some? res))
      (is (zero? freed-bytes)
          (filterv agent/allocation-freed?
                   (filterv (agent/allocation-on-thread?) allocations))))
    (let [[allocations res]     (agent/with-allocation-tracing
                                  [:a :b])
          {:keys [freed-bytes]} (agent/allocations-summary
                                 (filterv (agent/allocation-on-thread?) allocations))]
      (is (some? res))
      (is (zero? freed-bytes)
          (filterv agent/allocation-freed?
                   (filterv (agent/allocation-on-thread?) allocations))))
    (let [[allocations res]     (agent/with-allocation-tracing
                                  (criterium.jvm/class-loader-counts))
          {:keys [freed-bytes]} (agent/allocations-summary
                                 (filterv (agent/allocation-on-thread?) allocations))]
      (is (some? res))
      (is (zero? freed-bytes)
          (filterv agent/allocation-freed?
                   (filterv (agent/allocation-on-thread?) allocations))))
    (let [[allocations res]     (agent/with-allocation-tracing
                                  [(criterium.jvm/class-loader-counts)
                                   (criterium.jvm/class-loader-counts)])
          {:keys [freed-bytes]} (agent/allocations-summary
                                 (filterv (agent/allocation-on-thread?) allocations))]
      (is (some? res))
      (is (zero? freed-bytes)
          (filterv agent/allocation-freed?
                   (filterv (agent/allocation-on-thread?) allocations))))
    (let [f                     prim-f
          [allocations res]     (agent/with-allocation-tracing
                                  (* (+ (f 1)
                                        (f 2))
                                     (f 3)))
          {:keys [freed-bytes]} (agent/allocations-summary
                                 (filterv (agent/allocation-on-thread?) allocations))]
      (tap> (filterv agent/allocation-freed?
                     (filterv (agent/allocation-on-thread?) allocations)))
      (is (zero? freed-bytes)
          (filterv agent/allocation-freed?
                   (filterv (agent/allocation-on-thread?) allocations)))
      (is (= 9 res))))

(deftest zero-garbage-test
  (testing "Sampling is zero garbage"
    (let [measured (measured/measured
                    (fn [] [])
                    (fn [_ _] [1 1]))
          measured measured
          state    []]
      (testing "for elapsed-time-metric"
        (let [sample                (make-array Object 1)
              p                     (collector/collector
                                     {:stages [] :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (= [1 1] res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for measured-args"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:measured-args]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for class-loader"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:class-loader]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for compilation"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:compilation]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for memory"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:memory]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for finalization"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:finalization]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for garbage-collector"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:garbage-collector]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for thread-allocation"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:thread-allocation]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      ((:f p) sample measured state 1 0))
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              {:keys [freed-bytes]} (->> allocations
                                         (filterv (agent/allocation-on-thread?))
                                         agent/allocations-summary)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (->> allocations
                   (filterv (agent/allocation-on-thread?))
                   (filterv agent/allocation-freed?)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check"))))))
