(ns criterium.platform
  "Platform characterisation"
  (:require
   [clojure.pprint :as pp]
   [criterium.bench :as bench]
   [criterium.benchmark :as benchmark]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured])
  (:gen-class))

(def benchmark
  (benchmark/->benchmark
   {:analyse [:transform-log
              [:quantiles {:quantiles [0.9 0.99 0.99]}]
              :outliers
              [:stats {:samples-id :log-samples}]
              :event-stats]}))

;;; nanoTime latency
(def timestamp-measured
  (measured/expr (jvm/timestamp)))

(defn nanotime-latency
  ;; this takes a while for the timestamp capture to synch with the change in
  ;; the timestamp.  Ideally we would throw away the first half of the samples.
  ([] (nanotime-latency {}))
  ([options]
   (bench/bench-measured
    timestamp-measured
    (merge
     {:collect-plan {:scheme-type         :with-jit-warmup
                     :batch-time-ns       100000
                     :num-measure-samples 1000}
      :limit-time-s 20
      :benchmark    benchmark
      :return-value [::nil]}
     options))))

;;; nanoTime granularity

(defn- nanotime-granularity-fn
  [_ ^long eval-count]
  ;; this takes a while for the timestamp capture to synch with the change in
  ;; the timestamp.  Ideally we would throw away the first half of the samples.
  (let [start  (jvm/timestamp)
        finish (loop [n eval-count
                      t start]
                 (let [t1 (jvm/timestamp)]
                   (if (= t t1)
                     (recur n t1)
                     (if (pos? n)
                       (recur (unchecked-dec n) t1)
                       t1))))
        delta  (unchecked-subtract finish start)]
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
    nanotime-granularity-measured
    (merge
     {:collect-plan {:scheme-type         :with-jit-warmup
                     :batch-time-ns       100000
                     :num-measure-samples 500}
      :limit-time-s 25
      :benchmark    benchmark
      :return-value [::nil]}
     options))))

;;; Minimum measured time

(defn constant-long
  ([] (constant-long {}))
  ([options]
   (bench/bench-measured
    (measured/expr 1)
    (merge
     {:benchmark benchmark}
     options))))

(defn constant-double
  ([] (constant-double {}))
  ([options]
   (bench/bench-measured
    (measured/expr 1.0)
    (merge
     {:benchmark benchmark}
     options))))

(defn constant-object
  ([] (constant-double {}))
  ([options]
   (bench/bench-measured
    (measured/expr {})
    (merge
     {:benchmark benchmark}
     options))))

(defn constant-nil
  ([] (constant-nil {}))
  ([options]
   (bench/bench-measured
    (measured/expr nil)
    (merge
     {:benchmark benchmark}
     options))))

(defn- find-jit-threasholds [measured pipeline]
  (loop [i    0
         res  []
         comp (jvm/compilation-sample)
         t    0]
    (if (< i 400000)
      (let [sample     (collector/collect
                        pipeline measured (measured/args measured) 1)
            comp2      (jvm/compilation-sample)
            comp-delta (jvm/compilation-change comp comp2)]
        (recur
         (unchecked-inc i)
         (if (pos? (:time-ms comp-delta))
           (conj res i)
           res)
         comp2
         (unchecked-add t (long (:elapsed-time sample)))))
      [res t])))

;;; JIT compilation thresholds

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn jit-threasholds
  "Estimate how many iterations are required for JIT compilation.
  This is not very accurate, as JIT runs in the background, and there
  are several compilation targets."
  ([] (jit-threasholds {}))
  ([_options]
   (let [pipeline (collector/collector {:stages [] :terminator :elapsed-time})
         measured (measured/expr nil)
         [res _t] (find-jit-threasholds measured pipeline)]
     res)))

;; (jit-threasholds)

;;; platform description

(defn- mean-elapsed-time [result]
  (-> result :elapsed-time :mean))

(defn- min-elapsed-time [result]
  (-> result :elapsed-time :min-val))

(defn platform-stats
  "Return a sequence of estimates for times that describe accuracy of timing.

  Each estimate has a :name value."
  ([] (platform-stats {}))
  ([options]
   (let [options (merge
                  options
                  {:return-value [:stats]})]
     [(assoc (nanotime-latency options) :name "latency")
      (assoc (nanotime-granularity options) :name "granularity")
      (assoc (constant-long options) :name "constant-long")
      (assoc (constant-double options) :name "constant-double")
      (assoc (constant-object options) :name "constant-object")
      (assoc (constant-nil options) :name "constant-nil")])))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn platform-point-estimates
  "Return estimates for times that describe the accuracy of timing.

  The latency and granularity are min estimate, and the the rest are
  mean estimates."
  ([] (platform-point-estimates {}))
  ([options]
   (let [stats          (platform-stats options)
         point-estimate {:latency         min-elapsed-time
                         :granularity     min-elapsed-time
                         :constant-long   mean-elapsed-time
                         :constant-double mean-elapsed-time
                         :constant-object mean-elapsed-time
                         :constant-nil    mean-elapsed-time}]
     (reduce
      (fn [res stat]
        (let [kw (keyword (:name stat))]
          (assoc res kw ((point-estimate kw) stat))))
      {}
      stats))))

(defn -main
  "Output a table of the platform min and mean point estimates."
  []
  (pp/pprint (jvm/os-details))
  (pp/pprint (select-keys (jvm/runtime-details)
                          [:vm-version :vm-name :vm-vendor
                           :clojure-version-string]))
  (let [stats
        (reduce
         (fn [res stat]
           (let [view {:name    (:name stat)
                       :min-ns  (min-elapsed-time stat)
                       :mean-ns (mean-elapsed-time stat)}]
             (conj res view)))
         []
         (platform-stats))]
    (pp/print-table stats)
    (println)
    (println "JIT compilation threasholds: " (jit-threasholds))
    (println)))
