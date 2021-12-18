(ns criterium.instrument-test
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]
   [criterium.analyse :as analyse]
   [criterium.instrument :as instrument]
   [criterium.jvm :as jvm]))

;; instrument's measured never have their `args-fn` called.
(def seen (volatile! false))

(def original-f @#'instrument/original-f)
(def samples @#'instrument/samples)

(defn busy-wait [t-ns]
  (let [t0 (jvm/timestamp)]
    (vswap! seen inc)
    (loop []
      (when (< (unchecked-subtract (jvm/timestamp) t0) t-ns)
        (recur)))))

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
    (is (= 2 (count ((:samples sampled) [:elapsed-time]))) "samples returned")
    (is (= (count ((:samples sampled) [:elapsed-time]))
           (:eval-count sampled))
        "eval-count correct")
    (is (= 1  (:batch-size sampled)) "batch-size is correct")
    (is (>= elapsed (reduce + ((:samples sampled) [:elapsed-time])))
        "elapsed time is sane")
    (let [sampled   ((analyse/stats) sampled)
          mean-time (-> sampled :stats :elapsed-time :mean)]
      (is (>= (/ elapsed 2) mean-time) "can be analysed"))))
