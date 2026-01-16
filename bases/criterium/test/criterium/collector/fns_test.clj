(ns criterium.collector.fns-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.agent.core :as agent-core]
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
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (= [1 1] res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for elapsed-time-only metric"
        (let [sample                (make-array Object 1)
              p                     (collector/collector
                                     {:stages [] :terminator :elapsed-time-only})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (= [1 1] res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for measured-args"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:measured-args]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for class-loader"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:class-loader]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for compilation"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:compilation]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for memory"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:memory]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for finalization"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:finalization]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for garbage-collector"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:garbage-collector]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check")))
      (testing "for thread-allocation"
        (let [sample                (make-array Object 2)
              p                     (collector/collector
                                     {:stages     [:thread-allocation]
                                      :terminator :elapsed-time})
              _                     (dotimes [_ 10]
                                      (agent/with-allocation-tracing
                                        ((:f p) sample measured state 1 0)))
              ;; clear any allocations from warmup
              _                     (reset! agent-core/records [])
              [allocations res]     (agent/with-allocation-tracing
                                      ((:f p) sample measured state 1 0))
              relevant-allocations  (filterv (agent/allocation-on-thread?) allocations)
              {:keys [freed-bytes]} (agent/allocations-summary relevant-allocations)]
          (is (nil? res))
          (is (zero? freed-bytes)
              (pr-str (filterv agent/allocation-freed? relevant-allocations)))
          (is (some? sample)
              "Make sure sample isn't collected until after garbage check"))))))

;; Tests elapsed-time-only output structure:
;; - contains :elapsed-time key
;; - contains :expr-value set to :criterium/not-collected (to distinguish from
;;   expressions that genuinely return nil)
;; - produces :elapsed-time metric ID (not :elapsed-time-only)
(deftest elapsed-time-only-output-test
  (testing "elapsed-time-only collector"
    (let [measured (measured/measured
                    (fn [] [])
                    (fn [_ _] [100 :large-value]))
          state    []]
      (testing "produces output with :elapsed-time and :expr-value :criterium/not-collected"
        (let [^objects sample (make-array Object 1)
              p      (collector/collector
                      {:stages [] :terminator :elapsed-time-only})]
          ((:f p) sample measured state 1 0)
          ((:x p) sample 0)
          (let [result (aget sample 0)]
            (is (contains? result :elapsed-time)
                "Output should contain :elapsed-time")
            (is (= :criterium/not-collected (:expr-value result))
                "Output should have :expr-value :criterium/not-collected"))))
      (testing "has metrics-defs with :elapsed-time key, not :elapsed-time-only"
        (let [p (collector/collector
                 {:stages [] :terminator :elapsed-time-only})]
          (is (contains? (:metrics-defs p) :elapsed-time)
              "metrics-defs should contain :elapsed-time")
          (is (not (contains? (:metrics-defs p) :elapsed-time-only))
              "metrics-defs should NOT contain :elapsed-time-only")))
      (testing "compared to elapsed-time which retains :expr-value"
        (let [^objects sample (make-array Object 1)
              p      (collector/collector
                      {:stages [] :terminator :elapsed-time})]
          ((:f p) sample measured state 1 0)
          ((:x p) sample 0)
          (let [result (aget sample 0)]
            (is (contains? result :elapsed-time)
                "elapsed-time output should contain :elapsed-time")
            (is (contains? result :expr-value)
                "elapsed-time output should contain :expr-value")))))))
