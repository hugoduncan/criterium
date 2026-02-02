(ns criterium.instrument-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.array :as arr]
   [criterium.collector :as collector]
   [criterium.instrument :as instrument]
   [criterium.jvm :as jvm]
   [criterium.sampler :as sampler]
   [criterium.util.helpers :as util]))

;; instrument's measured never have their `args-fn` called.
(def seen (volatile! 0))

(def original-f @#'instrument/original-f)

(defn busy-wait
  [t-ns]
  (let [t-ns (long t-ns)
        t0   (jvm/timestamp)]
    (vswap! seen (fn [i] (inc (long i))))
    (loop []
      (when (< (unchecked-subtract (jvm/timestamp) t0) t-ns)
        (recur)))))

(deftest instrument!-test
  (testing "basic instrumentation"
    (let [v      #'busy-wait
          orig-f @v
          conf   {:stages       []
                  :terminator   (collector/maybe-var-get-stage
                                 :elapsed-time)
                  :sample-count 1}]
      (vreset! seen 0)
      (instrument/instrument! v conf)
      (is (not= busy-wait v) "function is wrapped")
      (is (= orig-f (#'instrument/original-f (meta v)))
          "original function stored")
      (is (= :criterium/metrics-samples (:type (sampler/samples-map @v)))
          "samples atom added")

      ;; Test idempotency
      (instrument/instrument! v conf)
      (is (= @v @v) "second instrumentation is idempotent")

      (busy-wait 1)
      (is (= 1 @seen) "original function called")
      (is (= 1 (arr/length
                (-> @v
                    (sampler/samples-map)
                    :metric->values
                    (get [:elapsed-time]))))
          "sample collected")

      (instrument/uninstrument! v)))

  (testing "instrumentation preserves metadata"
    (let [v    #'busy-wait
          conf {:stages       []
                :terminator   (collector/maybe-var-get-stage
                               :elapsed-time)
                :sample-count 1}]
      (alter-meta! v assoc :test-key :test-value)
      (instrument/instrument! v conf)
      (is (= :test-value (:test-key (meta v))) "preserves existing metadata")
      (instrument/uninstrument! v))))

(deftest uninstrument!-test
  (testing "basic uninstrumentation"
    (let [v    #'busy-wait
          conf {:stages       []
                :terminator   :elapsed-time
                :sample-count 1}]
      (vreset! seen 0)
      (let [original @v]
        (instrument/instrument! v conf)
        (instrument/uninstrument! v)
        (is (= original @v) "original function restored")
        (is (not (#'instrument/original-f (meta v)))
            "tracking metadata removed"))))

  (testing "uninstrumentation idempotency"
    (let [v        #'busy-wait
          original @v]
      (instrument/uninstrument! v)
      (is (= original @v) "safe to call on non-instrumented var"))))

(deftest basic-instrumentation-test
  (vreset! seen 0)
  (let [original         @#'busy-wait
        collector-config {:stages     [:compilation
                                       :garbage-collector]
                          :terminator :elapsed-time}
        start            (jvm/timestamp)]
    (instrument/uninstrument! #'busy-wait)
    (instrument/instrument! #'busy-wait collector-config)
    (is (original-f (meta #'busy-wait)) "function wrapped")
    (is (= :criterium/metrics-samples (:type (sampler/samples-map busy-wait)))
        "sample atom added")
    (is (not= original-f @#'busy-wait) "wrapper is installed")
    (busy-wait 1)
    (is (= 1
           (-> busy-wait
               sampler/samples-map
               :metric->values
               (get [:elapsed-time])
               arr/length))
        "one sample added")
    (busy-wait 2)
    (is (= 2
           (-> busy-wait
               sampler/samples-map
               :metric->values
               (get [:elapsed-time])
               arr/length))
        "two samples added")
    (let [finish     (jvm/timestamp)
          elapsed    (unchecked-subtract finish start)
          sample-map (sampler/samples-map busy-wait)]
      (is (= 2 @seen) "original function called twice")

      ;; (is result "result returned")
      (is
       (= :criterium/metrics-samples (:type sample-map))
       "sample map returned")
      (is (= 2 (arr/length ((:metric->values sample-map) [:elapsed-time])))
          "samples returned")
      (is (= (arr/length ((:metric->values sample-map) [:elapsed-time]))
             (:eval-count sample-map))
          "eval-count correct")
      (is (= 1  (:batch-size sample-map)) "batch-size is correct")
      (is (>= elapsed (arr/fold-double
                       ((:metric->values sample-map) [:elapsed-time])
                       (fn ^double [^double a ^double b] (+ a b))
                       0.0))
          "elapsed time is sane")
      (let [data-map  ((analyse/stats) {:samples sample-map})
            mean-time (-> data-map
                          :stats util/stats
                          :elapsed-time
                          :mean)]
        (is (>= (/ elapsed 2) mean-time) "can be analysed")))
    (instrument/uninstrument! #'busy-wait)
    (is (= original @#'busy-wait) "function restored")
    (is (empty? (set/intersection
                 (set (keys (meta #'busy-wait)))
                 #{original-f}))
        "metadata removed")))
