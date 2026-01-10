(ns criterium.collect-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.collect :as collect]
   [criterium.collector :as collector]
   [criterium.measured :as measured]
   [criterium.typed-samples :as typed-samples])
  (:import
   [criterium.typed_samples DoubleSamples LongSamples ObjectSamples]))

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
;; primitive arrays based on metric type configuration.
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
      (testing "produces DoubleSamples for :quantitative metrics"
        (is (instance? DoubleSamples (result [:elapsed-time])))
        (is (= :double (typed-samples/elem-type (result [:elapsed-time]))))
        (is (= 3 (typed-samples/sample-count (result [:elapsed-time])))))
      (testing "produces LongSamples for :event metrics"
        (is (instance? LongSamples (result [:class-loader :loaded-count])))
        (is (= :long (typed-samples/elem-type (result [:class-loader :loaded-count]))))
        (is (= 3 (typed-samples/sample-count (result [:class-loader :loaded-count])))))
      (testing "produces ObjectSamples for :nominal metrics"
        (is (instance? ObjectSamples (result [:expr-value])))
        (is (= :object (typed-samples/elem-type (result [:expr-value]))))
        (is (= 3 (typed-samples/sample-count (result [:expr-value])))))
      (testing "preserves values correctly via fold"
        (is (= 4500000.0
               (typed-samples/fold-double
                (result [:elapsed-time])
                (fn ^double [^double acc ^double v] (+ acc v))
                0.0)))
        (is (= 8
               (typed-samples/fold-long
                (result [:class-loader :loaded-count])
                (fn ^long [^long acc ^long v] (+ acc v))
                0)))
        (is (= [:a :b :c]
               (typed-samples/fold
                (result [:expr-value])
                conj
                [])))))))
