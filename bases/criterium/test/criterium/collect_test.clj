(ns criterium.collect-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.array :as arr]
   [criterium.collect :as collect]
   [criterium.collector :as collector]
   [criterium.measured :as measured])
  (:import
   [criterium.array DoubleArray LongArray ObjectArray]))

(deftest full-zero-garbage-test
  (testing "full sampling"
    (let [measured              (measured/measured
                                 (fn [] [:b])
                                 (fn [_ _] [10000000 1]))
          collector             (collector/collector
                                 {:stages     [:garbage-collector :compilation]
                                  :terminator :elapsed-time})
          ;; run collector for JIT
          _                     (dotimes [_ 10000]
                                  (collect/collect-arrays
                                   collector
                                   measured
                                   1
                                   10))
          [allocations sampled] (agent/with-allocation-tracing
                                  (collect/collect-arrays
                                   collector
                                   measured
                                   1
                                   10))
          {:keys [freed-bytes]} (->> allocations
                                     (filterv (agent/allocation-on-thread?))
                                     (filterv agent/allocation-freed?)
                                     agent/allocations-summary)]
      (is (= 10 (alength ^objects (:collections sampled)))
          (pr-str sampled))
      (is (zero? freed-bytes)
          (->> allocations
               (filterv (agent/allocation-on-thread?))
               (filterv agent/allocation-freed?)))
      (is (some? sampled) "hold onto samples reference until this point"))))

;; Tests that sample-maps->map-of-samples produces correctly typed
;; arrays (DoubleArray, LongArray, ObjectArray) based on metric type.
(deftest sample-maps->map-of-samples-test
  (testing "sample-maps->map-of-samples"
    (let [metrics-defs {:elapsed-time
                        {:type   :quantitative
                         :values [{:path [:elapsed-time]
                                   :type :quantitative
                                   :dimension :time
                                   :scale 1e-9
                                   :label "Elapsed Time"}
                                  {:path [:expr-value]
                                   :type :nominal
                                   :dimension :fn-value
                                   :scale 1
                                   :label "Expr value"}]}
                        :class-loader
                        {:type   :event
                         :values [{:path [:class-loader :loaded-count]
                                   :type :event
                                   :dimension :count
                                   :scale 1
                                   :label "Loaded classes"}]}}
          samples [{:elapsed-time 1000000
                    :expr-value :a
                    :class-loader {:loaded-count 5}}
                   {:elapsed-time 2000000
                    :expr-value :b
                    :class-loader {:loaded-count 3}}
                   {:elapsed-time 1500000
                    :expr-value :c
                    :class-loader {:loaded-count 0}}]
          result (collect/sample-maps->map-of-samples samples metrics-defs)]
      (testing "returns correct keys for each metric path"
        (is (= #{[:elapsed-time] [:expr-value] [:class-loader :loaded-count]}
               (set (keys result)))))
      (testing "produces DoubleArray for :quantitative metrics"
        (is (instance? DoubleArray (result [:elapsed-time])))
        (is (= :double (arr/elem-type (result [:elapsed-time]))))
        (is (= 3 (arr/length (result [:elapsed-time])))))
      (testing "produces LongArray for :event metrics"
        (is (instance? LongArray (result [:class-loader :loaded-count])))
        (is (= :long (arr/elem-type (result [:class-loader :loaded-count]))))
        (is (= 3 (arr/length (result [:class-loader :loaded-count])))))
      (testing "produces ObjectArray for :nominal metrics"
        (is (instance? ObjectArray (result [:expr-value])))
        (is (= :object (arr/elem-type (result [:expr-value]))))
        (is (= 3 (arr/length (result [:expr-value])))))
      (testing "preserves values correctly via fold"
        (is (= 4500000.0
               (arr/fold-double
                (result [:elapsed-time])
                (fn ^double [^double acc ^double v] (+ acc v))
                0.0)))
        (is (= 8
               (arr/fold-long
                (result [:class-loader :loaded-count])
                (fn ^long [^long acc ^long v] (+ acc v))
                0)))
        (is (= [:a :b :c]
               (arr/fold
                (result [:expr-value])
                conj
                [])))))))

;;; warmup tests

;; Tests that warmup uses warmup-args-fn (not args-fn) when it's present
;; on a Measured. Validates that the collection pipeline respects the
;; warmup-args-fn contract.
(deftest warmup-uses-warmup-args-fn-test
  (testing "warmup"
    (testing "uses warmup-args-fn when present"
      (let [measurement-args-calls (atom 0)
            warmup-args-calls      (atom 0)
            m                      (measured/measured
                                    (fn []
                                      (swap! measurement-args-calls inc)
                                      [:measurement])
                                    (fn [_ _] [1 1])
                                    nil
                                    (fn []
                                      (swap! warmup-args-calls inc)
                                      [:warmup]))
            collector              (collector/collector
                                    {:stages     []
                                     :terminator :elapsed-time})
            _                      (collect/warmup collector m 3 1)]
        (is (pos? @warmup-args-calls)
            "warmup-args-fn should be called during warmup")
        (is (zero? @measurement-args-calls)
            "measurement args-fn should NOT be called during warmup")))
    (testing "falls back to args-fn when warmup-args-fn is nil"
      (let [measurement-args-calls (atom 0)
            m                      (measured/measured
                                    (fn []
                                      (swap! measurement-args-calls inc)
                                      [:measurement])
                                    (fn [_ _] [1 1]))
            collector              (collector/collector
                                    {:stages     []
                                     :terminator :elapsed-time})
            _                      (collect/warmup collector m 3 1)]
        (is (pos? @measurement-args-calls)
            "args-fn should be called when warmup-args-fn is nil")))))
