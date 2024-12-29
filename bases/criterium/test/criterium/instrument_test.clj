(ns criterium.instrument-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collector :as collector]
   [criterium.instrument :as instrument]
   [criterium.jvm :as jvm]
   [criterium.util.helpers :as util]))

;; instrument's measured never have their `args-fn` called.
(def seen (volatile! 0))

(def original-f @#'instrument/original-f)
(def samples @#'instrument/samples)

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
                  :sample-count 1}
          pipe   (collector/collector conf)]
      (vreset! seen 0)
      (instrument/instrument! v pipe)
      (is (not= busy-wait v) "function is wrapped")
      (is (#'instrument/original-f (meta v)) "original function stored")
      (is (instance? clojure.lang.Atom (#'instrument/samples (meta v)))
          "samples atom added")

      ;; Test idempotency
      (instrument/instrument! v pipe)
      (is (= @v @v) "second instrumentation is idempotent")

      (busy-wait 1)
      (is (= 1 @seen) "original function called")
      (is (= 1 (count @(#'instrument/samples (meta v)))) "sample collected")

      (instrument/uninstrument! v)))

  (testing "instrumentation preserves metadata"
    (let [v    #'busy-wait
          conf {:stages       []
                :terminator   (collector/maybe-var-get-stage
                               :elapsed-time)
                :sample-count 1}
          pipe (collector/collector conf)]
      (alter-meta! v assoc :test-key :test-value)
      (instrument/instrument! v pipe)
      (is (= :test-value (:test-key (meta v))) "preserves existing metadata")
      (instrument/uninstrument! v))))

(deftest uninstrument!-test
  (testing "basic uninstrumentation"
    (let [v    #'busy-wait
          conf {:stages       [:elapsed-time]
                :terminator   :sample-count
                :sample-count 1}
          pipe (collector/collector conf)]
      (vreset! seen 0)
      (let [original @v]
        (instrument/instrument! v pipe)
        (instrument/uninstrument! v)
        (is (= original @v) "original function restored")
        (is (not (#'instrument/original-f (meta v)))
            "tracking metadata removed")
        (is (not (#'instrument/samples (meta v)))
            "samples metadata removed"))))

  (testing "uninstrumentation idempotency"
    (let [v        #'busy-wait
          original @v]
      (instrument/uninstrument! v)
      (is (= original @v) "safe to call on non-instrumented var"))))

(deftest basic-instrumentation-test
  (vreset! seen 0)
  (let [orignal-f        busy-wait
        collector-config {:stages     [:compilation
                                       :garbage-collector]
                          :terminator :elapsed-time}
        start            (jvm/timestamp)
        [sampled result]
        (instrument/with-instrumentation [busy-wait collector-config]
          (let [m (meta #'busy-wait)]
            (is (original-f m) "function wrapped")
            (is (instance? clojure.lang.Atom (samples m)) "sample atom added")
            (is (not= orignal-f @#'busy-wait) "wrapper is installed"))
          (busy-wait 1)
          (is (= 1 (-> #'busy-wait meta samples deref count))
              "one sample added")
          (busy-wait 2)
          (is (= 2 (-> #'busy-wait meta samples deref count))
              "two samples added"))
        finish           (jvm/timestamp)
        elapsed          (unchecked-subtract finish start)]
    (is (= 2 @seen) "original function called twice")
    (is (= orignal-f @#'busy-wait) "function restored")
    (is (empty? (set/intersection
                 (set (keys (meta #'busy-wait)))
                 #{original-f samples}))
        "metadata removed")
    (is result "result returned")
    (is (map? sampled) "sample map returned")
    (is (util/metrics-samples-map? sampled))
    (is (= 2 (count ((:metric->values sampled) [:elapsed-time])))
        "samples returned")
    (is (= (count ((:metric->values sampled) [:elapsed-time]))
           (:eval-count sampled))
        "eval-count correct")
    (is (= 1  (:batch-size sampled)) "batch-size is correct")
    (is (>= elapsed (reduce + ((:metric->values sampled) [:elapsed-time])))
        "elapsed time is sane")
    (let [bench-map ((analyse/stats) {:data {:samples sampled}})
          mean-time (-> bench-map :data :stats util/stats :elapsed-time :mean)]
      (is (>= (/ elapsed 2) mean-time) "can be analysed"))))
