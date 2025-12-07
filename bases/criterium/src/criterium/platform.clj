(ns criterium.platform
  "Platform characterisation"
  (:require
   [clojure.pprint :as pp]
   [criterium.analyse]
   [criterium.bench :as bench]
   [criterium.collect-plan.config :as collect-plan-config]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.util.helpers :as util])
  (:gen-class))

(def ^:private benchmark
  {:analyse [:transform-log
             [:quantiles {:quantiles [0.9 0.99 0.99]}]
             :outliers
             [:stats {:samples-id :log-samples}]
             :event-stats]
   :view    []})

(def ^:private collection
  {:collect-plan
   (collect-plan-config/collect-plan-config
    :with-jit-warmup
    {:batch-time-ns       100000
     :num-measure-samples 500
     :limit-time-ns       20000000000})
   :collector-config
   {:stages     [:measured-args :compilation :garbage-collector],
    :terminator :elapsed-time}
   :return-value [::nil]
   :viewer       :none})

;;; nanoTime latency
(def ^:private timestamp-measured
  (measured/expr (jvm/timestamp)))

(defn nanotime-latency
  ;; this takes a while for the timestamp capture to synch with the change in
  ;; the timestamp.  Ideally we would throw away the first half of the samples.
  ([] (nanotime-latency {}))
  ([options]
   (bench/bench-measured
    (merge
     benchmark
     {:collect-plan
      (collect-plan-config/collect-plan-config
       :with-jit-warmup
       {:batch-time-ns       100000
        :num-measure-samples 1000
        :limit-time-ns       20000000000})
      :collector-config
      {:stages     [:measured-args :compilation :garbage-collector],
       :terminator :elapsed-time}
      :return-value [::nil]
      :viewer       :none}
     options)
    timestamp-measured)))

;;; nanoTime granularity

(defn- nanotime-granularity-fn
  [_ ^long eval-count]
  ;; this takes a while for the timestamp capture to synch with the change in
  ;; the timestamp.  Ideally we would throw away the first half of the samples.
  (let [start        (jvm/timestamp)
        ^long finish (loop [n eval-count
                            t start]
                       (let [t1 (jvm/timestamp)]
                         (if (= t t1)
                           (recur n t1)
                           (if (pos? n)
                             (recur (unchecked-dec n) t1)
                             t1))))
        delta        (unchecked-subtract finish start)]
    [delta (long (/ delta eval-count))]))

(def nanotime-granularity-measured
  (let [args-fn (fn granularity-args-fn [] nil)]
    (measured/measured
     args-fn
     nanotime-granularity-fn)))

(defn nanotime-granularity
  ([] (nanotime-granularity {}))
  ([options]
   (bench/bench-measured
    (merge
     benchmark
     {:collect-plan
      (collect-plan-config/collect-plan-config
       :with-jit-warmup
       {:batch-time-ns       100000
        :num-measure-samples 500
        :limit-time-ns       20000000000})
      :collector-config
      {:stages     [:measured-args :compilation :garbage-collector],
       :terminator :elapsed-time}
      :return-value [::nil]
      :viewer       :none}
     options)
    nanotime-granularity-measured)))

;;; Minimum measured time

(defn constant-long
  ([] (constant-long {}))
  ([options]
   (bench/bench-measured
    (merge
     {:viewer :none}
     benchmark
     collection
     options)
    (measured/expr 1))))

(defn constant-double
  ([] (constant-double {}))
  ([options]
   (bench/bench-measured
    (merge
     {:viewer :none}
     benchmark
     collection
     options)
    (measured/expr 1.0))))

(defn constant-object
  ([] (constant-double {}))
  ([options]
   (bench/bench-measured
    (merge
     {:viewer :none}
     benchmark
     collection
     options)
    (measured/expr {}))))

(defn constant-nil
  ([] (constant-nil {}))
  ([options]
   (bench/bench-measured
    (merge
     {:viewer :none}
     benchmark
     collection
     options)
    (measured/expr nil))))

(defn- find-jit-threasholds [measured collector]
  (loop [i    0
         res  []
         comp (jvm/compilation-sample)
         t    0]
    (if (< i 4000000)
      (let [sample     (collector/collect
                        collector measured (measured/args measured) 1)
            comp2      (jvm/compilation-sample)
            comp-delta (jvm/compilation-change comp comp2)]
        (when (= 0 (mod i 10000))
          ;; pause to let compilation "catchup"/complete
          (Thread/sleep 50))
        (recur
         (unchecked-inc i)
         (if (pos? (long (:time-ms comp-delta)))
           (conj res [i (:time-ms comp-delta)])
           res)
         comp2
         (unchecked-add t (long (:elapsed-time sample)))))
      [res t])))

;;; JIT compilation thresholds

(defn jit-threasholds
  "Estimate how many iterations are required for JIT compilation.
  This is not very accurate, as JIT runs in the background, and there
  are several compilation targets.

  The highest value returned should be less than TARGET-WARMUP-SAMPLES for
  our collection plans to be realistic."
  ([] (jit-threasholds {}))
  ([_options]
   (let [collector (collector/collector {:stages [] :terminator :elapsed-time})
         measured  (measured/expr 1)
         [res _t]  (find-jit-threasholds measured collector)]
     res)))

;; (jit-threasholds)

;;; platform description

(defn- mean-elapsed-time [result]
  (util/stats-value result :stats :elapsed-time :mean))

(defn- min-elapsed-time [result]
  (util/stats-value result :stats :elapsed-time :min-val))

(defn platform-stats
  "Return a sequence of estimates for times that describe accuracy of timing.

  Each estimate has a :name value."
  ([] (platform-stats {}))
  ([options]
   (let [options (merge
                  options
                  {:return-value []})]
     [(assoc (nanotime-latency options) :name "latency")
      (assoc (nanotime-granularity options) :name "granularity")
      (assoc (constant-long options) :name "constant-long")
      (assoc (constant-double options) :name "constant-double")
      (assoc (constant-object options) :name "constant-object")
      (assoc (constant-nil options) :name "constant-nil")])))

(defn platform-point-estimates
  "Return estimates for times that describe the accuracy of timing.

  The latency and granularity are min estimate, and the the rest are
  mean estimates."
  ([] (platform-point-estimates {}))
  ([options]
   (reduce
    (fn [res stat]
      (let [view {:name    (:name stat)
                  :min-ns  (min-elapsed-time (dissoc stat :name))
                  :mean-ns (mean-elapsed-time (dissoc stat :name))}]
        (conj res view)))
    []
    (platform-stats options))))

(defn exec-main
  "Output a table of the platform min and mean point estimates.
  Compatible with deps.edn :exec-fn."
  [_opts]
  (pp/pprint (jvm/os-details))
  (pp/pprint (select-keys (jvm/runtime-details)
                          [:vm-version :vm-name :vm-vendor
                           :clojure-version-string]))
  (let [stats (platform-point-estimates)]
    (pp/print-table stats)
    (println)
    (println "JIT compilation threasholds: " (jit-threasholds))
    (println)))

(defn -main
  "Output a table of the platform min and mean point estimates.
  Compatible with deps.edn :main."
  []
  (exec-main {}))
