(ns criterium.instrument-fn-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collector :as collector]
   [criterium.instrument-fn :as instrument-fn]
   [criterium.jvm :as jvm]
   [criterium.sampler :as sampler]))

(def ^:private seen (volatile! 0))

(defn- busy-wait
  ([] (busy-wait 10))
  ([t-ns]
   (let [t-ns (long t-ns)
         t0   (jvm/timestamp)]
     (vswap! seen (fn [i] (inc (long i))))
     (loop []
       (when (< (unchecked-subtract (jvm/timestamp) t0) t-ns)
         (recur))))))

(deftest instrumented-fn-test
  (testing "basic instrumentation"
    (vreset! seen 0)
    (let [conf   {:stages       []
                  :terminator   (collector/maybe-var-get-stage
                                 :elapsed-time)
                  :sample-count 1}
          inst-f (instrument-fn/instrument-fn busy-wait conf)]

      (is (satisfies? sampler/Sampler inst-f)
          "implements Sampler")
      (is (instance? clojure.lang.IFn inst-f)
          "implements IFn")
      (is (instance? Runnable inst-f)
          "implements Runnable")
      (is (instance? java.util.concurrent.Callable inst-f)
          "implements Callable")

      (inst-f 1)
      (is (= 1 @seen) "original function called")
      (is (= 1 (count (-> (sampler/samples-map inst-f)
                          :metric->values
                          (get [:elapsed-time]))))
          "sample collected")

      (sampler/reset-samples! inst-f)
      (is (empty? (-> (sampler/samples-map inst-f)
                      :metric->values
                      (get [:elapsed-time])))
          "samples can be reset")))

  (testing "function invocation"
    (vreset! seen 0)
    (let [conf   {:stages       []
                  :terminator   (collector/maybe-var-get-stage
                                 :elapsed-time)
                  :sample-count 1}
          inst-f (instrument-fn/instrument-fn busy-wait conf)]

      (.run ^Runnable inst-f)
      (is (= 1 @seen) "Runnable.run works")

      (.call ^Callable inst-f)
      (is (= 2 @seen) "Callable.call works")

      (inst-f 1)
      (is (= 3 @seen) "direct invoke works")

      (apply inst-f [1])
      (is (= 4 @seen) "apply works")

      (is (= 4 (count (-> (sampler/samples-map inst-f)
                          :metric->values
                          (get [:elapsed-time]))))
          "all invocations collected samples")))

  (testing "samples can be analyzed"
    (let [conf     {:stages     [:compilation
                                 :garbage-collector]
                    :terminator :elapsed-time}
          inst-f   (instrument-fn/instrument-fn busy-wait conf)
          start    (jvm/timestamp)
          _        (inst-f 1)
          _        (inst-f 2)
          finish   (jvm/timestamp)
          elapsed  (unchecked-subtract finish start)
          sample-m (sampler/samples-map inst-f)]
      (is (= :criterium/metrics-samples (:type sample-m)))
      (is (= 2 (count ((:metric->values sample-m) [:elapsed-time])))
          "samples returned")
      (is (= (count ((:metric->values sample-m) [:elapsed-time]))
             (:eval-count sample-m))
          "eval-count correct")
      (is (= 1  (:batch-size sample-m)) "batch-size is correct")
      (is (>= elapsed
              (reduce + ((:metric->values sample-m) [:elapsed-time])))
          "elapsed time is sane"))))
