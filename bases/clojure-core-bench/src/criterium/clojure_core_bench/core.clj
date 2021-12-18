(ns criterium.clojure-core-bench.core
  (:require
   [clojure.test.check.generators :as gen]
   [criterium.time :as time]
   [criterium.arg-gen :as arg-gen]
   [criterium.stats :as stats]
   [criterium.metric :as metric]
   [criterium.sample-scheme.config :as sample-scheme-config]
   [criterium.benchmarks :as benchmarks]))

;;; arg-gen a constant

(defn constant-measured
  []
  (arg-gen/measured
      {}
      [i gen/small-integer]
    i))

(defn measure-constant
  ([] (measure-constant {}))
  ([options]
   (time/time-measured
    (constant-measured)
    (merge
     {:limit-time-s 1
      :return-value ::nil}
     options))))

;;; Identity

(defmacro recursively
  "Expand to call f recursively n times with the initial argument, init."
  [n f init]
  (reduce
   (fn [expr _n]
     `(~f ~expr))
   init
   (range n)))

(def gen-for-identity (gen/vector gen/small-integer))

(defn identity-measured0 [mx]
  (arg-gen/measured {:size mx} [i gen-for-identity]
    i))

(defn identity-measured [mx]
  (arg-gen/measured {:size mx} [i gen-for-identity]
    (identity i)))

(defn measure-identity
  ([] (measure-identity {}))
  ([options]
   (time/time-measured
    (identity-measured 10)
    (merge
     {:limit-time-s 1
      :return-value ::nil}
     options))))

(defn identity-measured2 [mx]
  (arg-gen/measured {:size mx} [i gen-for-identity]
    (recursively 2 identity i)))

(defn identity-measured3 [mx]
  (arg-gen/measured {:size mx} [i gen-for-identity]
    (recursively 3 identity i)))

(defn identity-measured4 [mx]
  (arg-gen/measured {:size mx} [i gen-for-identity]
    (recursively 4 identity i)))

(defn identity-regression
  ([] (identity-regression {}))
  ([options]
   (let [ms [;; (identity-measured0 100)
             (identity-measured 10)
             (identity-measured2 10)
             (identity-measured3 10)
             (identity-measured4 10)]
         n  (count ms)
         xs (range 1 (inc n))
         ys (reduce
             (fn [res m]
               (conj res (-> (time/measure
                              (metric/metrics)
                              {:stages [] :terminator :elapsed-time}
                              (sample-scheme-config/full-sample-scheme
                               {:limit-time-s 10})
                              benchmarks/log-histogram
                              {:viewer :portal}
                              m)
                             :stats :elapsed-time :mean first)))
             []
             ms)
         lr (criterium.stats/linear-regression xs ys)
         ]
     (println "Regression" lr))))

(comment
  (identity-regression))

;;; inc

(defn inc-measured [mx]
  (arg-gen/measured {:size mx} [i (gen/choose 0 1000)]
    (inc i)))

(defn measure-inc
  ([] (measure-inc {}))
  ([options]
   (time/time-measured
    (inc-measured 10000)
    (merge
     {:limit-time-s 1
      :return-value ::nil}
     options))))

;;; instance?

(defn instance?-measured []
  (arg-gen/measured
      {:size 5}
      [i (gen/elements [Integer String Long Short BigInteger])]
    (instance? Long i)))

(defn measure-instance?
  ([] (measure-instance? {}))
  ([options]
   (time/time-measured
    (instance?-measured)
    (merge
     {:limit-time-s 1
      :return-value ::nil}
     options))))

;;; nth

(defn nth-measured [mx]
  ;; Uses standard vector generator, with explicit type hints,
  ;; to allow use of mx in the gen args.
  ;; This is just an alternative approach to using a custom generator.
  (arg-gen/measured
      {:size      mx
       :arg-metas []}
      [v (gen/vector gen/int mx)
       i (gen/choose 0 (dec (count v)))]
    (nth v i)))

(defn vec-nth-measured [mx]
  ;; Uses standard vector generator, with explicit type hints,
  ;; to allow use of mx in the gen args.
  ;; This is just an alternative approach to using a custom generator.
  (arg-gen/measured
      {:size      mx
       :arg-metas [{:tag clojure.lang.PersistentVector}
                   {:tag 'long}]}
      [v (gen/vector gen/int mx)
       i (gen/choose 0 (dec (count v)))]
    (.nth v i)))

(defn measure-nth
  "Measure nth.
  Note that the timing is multi-modal, depending on the depth of the
  tree backing the vector.

  Compare;
     (measure-nth 32 {:histogram true})
     (measure-nth (* 32 32) {:histogram true})"
  [vec-size options]
  (time/time-measured
   (nth-measured vec-size)
   (merge
    {:limit-time-s 1
     :return-value ::nil}
    options)))

(defn measure-vec-nth
  "Measure .nth on a vector.
  Note that the timing is multi-modal, depending on the depth of the
  tree backing the vector.

  Compare;
     (measure-vec-nth 32 {:histogram true})
     (measure-vec-nth (* 32 32) {:histogram true})"
  [vec-size options]
  (time/time-measured
   (vec-nth-measured vec-size)
   (merge
    {:limit-time-s 1
     :return-value ::nil}
    options)))

;;; destructuring

(defn vector-destructure-measured
  []
  (arg-gen/measured
      {:size 3}
      [v (gen/vector gen/int 3)]
    (let [[a b c] v]
      (+ a b c))))

(defn vector-explicit-destructure-measured
  []
  (arg-gen/measured
      {:size 3}
      [v (gen/vector gen/int 3)]
    (let [w v]
      (+ (.nth w 0)
         (.nth w 1)
         (.nth w 2)))))

(defn measure-vector-destructure
  [options]
  (time/time-measured
   (vector-destructure-measured)
   (merge
    {:limit-time-s 1
     :return-value ::nil}
    options)))

(defn measure-explicit-destructure
  [options]
  (time/time-measured
   (vector-explicit-destructure-measured)
   (merge
    {:limit-time-s 1
     :return-value ::nil}
    options)))
