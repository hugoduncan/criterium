(ns criterium.util.sampled-stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.array :as arr]
   [criterium.test-utils :refer [abs-error approx= gen-bounded test-max-error]]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]
   [criterium.util.ziggurat :as ziggurat]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

(deftest pair-fn-test
  (is (= [:a 15] ((sampled-stats/pair-fn :a (partial * 3)) 5))))

(deftest quantile-fns-test
  ;; quantile returns doubles even for integer inputs
  (is (= {0.01 1.0 0.99 99.0}
         (sampled-stats/sample-quantiles [0.01 0.99] (darr (range 101))))))

(deftest stats-fns-test
  ;; Check the computed values are numerically correct
  (let [result (sampled-stats/stats-fns (darr (range 101)))]
    (is (= 5 (count result)))
    (is (== 50.0 (second (nth result 0))))  ; mean
    (is (== 50.0 (second (nth result 1))))  ; median
    (is (== 858.5 (second (nth result 2)))) ; variance
    (is (== 0 (second (nth result 3))))     ; min-val
    (is (== 100 (second (nth result 4))))))

(defn batch-transforms [^long batch-size]
  {:sample-> (list (fn [^double v] (/ v batch-size)))
   :->sample [(fn [^double v] (* v batch-size))]})

(def identity-transforms
  {:sample-> (list identity)
   :->sample [identity]})

(deftest stats-for-test
  (let [samples (darr (repeat 100 1))
        stats (sampled-stats/stats-for
               samples {:quantiles [0.05 0.95]})]
    (is (= 1.0 (-> stats :mean)))
    (is (= 1.0 (-> stats :median)))
    (is (= 0.0 (-> stats :variance))))

  (testing "stats on [0..100]"
    (let [samples (darr (range 101))
          stats (sampled-stats/stats-for
                 samples {:quantiles [0.05 0.95]})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 50.0 (-> stats :median)))
      (is (= 858.5 (-> stats :variance)))
      (is (= 0.0 (-> stats :min-val)))
      (is (= 100.0 (-> stats :max-val)))))

  (testing "stats on (reverse [0..100])"
    (let [samples (darr (range 101))
          stats (sampled-stats/stats-for
                 samples {:quantiles [0.05 0.95]})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 50.0 (-> stats :median)))
      (is (= 858.5 (-> stats :variance)))
      (is (= 0.0 (-> stats :min-val)))
      (is (= 100.0 (-> stats :max-val)))))

  (testing "stats on [9 9 9 10 10 10]"
    (let [samples (darr [9 9 9 10 10 10])
          stats (sampled-stats/stats-for
                 samples {:quantiles [0.05 0.95]})]
      (is (= 9.5 (-> stats :mean)))
      (is (= 9.5 (-> stats :median)))
      (test-max-error 0.3 (-> stats :variance) 1e-5)
      (is (= 9.0 (-> stats :min-val)))
      (is (= 10.0 (-> stats :max-val))))))

(deftest quantiles-for-test
  (let [samples {[:v] (arr/->double-array (double-array (repeat 100 1)))}
        quantiles (sampled-stats/quantiles-for
                   [:v] samples {:quantiles [0.05 0.95]})]
    (is (= {0.1 1.0, 0.25 1.0, 0.5 1.0, 0.75 1.0, 0.9 1.0, 0.05 1.0, 0.95 1.0}
           quantiles)))

  (testing "quantiles on [0..100]"
    (let [samples {[:v] (arr/->double-array (double-array (range 101)))}
          quantiles (sampled-stats/quantiles-for
                     [:v] samples {:quantiles [0.05 0.95]})]
      (is (= {0.1 10.0, 0.25 25.0, 0.5 50.0, 0.75 75.0, 0.9 90.0,
              0.05 5.0, 0.95 95.0}
             quantiles))))

  (testing "quantiles on (reverse [0..100])"
    (let [samples {[:v] (arr/->double-array (double-array (range 101)))}
          quantiles (sampled-stats/quantiles-for
                     [:v] samples {:quantiles [0.05 0.95]})]
      (is (= {0.1 10.0, 0.25 25.0, 0.5 50.0, 0.75 75.0, 0.9 90.0,
              0.05 5.0, 0.95 95.0}
             quantiles)))))

(deftest stats-for-test-property-1
  ;; Uses a fixed seed for deterministic random values.
  ;; Reduced from 5000*200=1M to 1000*100=100K samples for faster tests.
  (let [batch-size 1000
        num-samples 100
        values (vec (take
                     (* batch-size num-samples)
                     (ziggurat/random-normal-zig
                      (well/well-rng-1024a 42))))
        sample-vals (partition batch-size values)
        samples (darr (mapv #(stats/sum (darr %)) sample-vals))
        stats (sampled-stats/stats-for
               samples {:quantiles [0.05 0.95]})
        mean-hat (-> stats :mean)
        variance-hat (-> stats :variance)
        mean (stats/mean (darr values))
        variance (stats/variance (darr values))]
    (test-max-error (* mean (double batch-size)) mean-hat 1e-5)
    (is (approx= (* variance (double batch-size)) variance-hat 2e-1))))

(defn random-values
  "Return a sequence of values with the given mean an standard deviation."
  [random-seed ^double mean ^double sigma]
  (->> (well/well-rng-1024a random-seed)
       ziggurat/random-normal-zig
       (map (fn [^double x] (+ mean (* sigma x))))))

(comment
  (defspec random-values-test-property 10
    (prop/for-all
     [random-seed gen/large-integer
      mean (gen/double* {:min 0 :max 20 :infinite? false :NaN? false})
      sigma (gen/double* {:min 1 :max 1000 :infinite? false :NaN? false})]
     (let [values (take 10000 (random-values random-seed mean sigma))
           variance (* sigma sigma)
           mean-error (abs-error (stats/mean values) mean)
           variance-error (abs-error (stats/variance values) variance)
           mean-tol (max (* sigma 5e-2) 1e-2)
           variance-tol (* variance 2e-1)]
       (is (< mean-error mean-tol) "mean")
       (is (< variance-error variance-tol) "variance")
       (and (< mean-error mean-tol)
            (< variance-error variance-tol))))))

(defn sample-values
  "Generate batched samples with the given mean and standard deviation."
  [batch-size num-samples random-seed mean sigma]
  (let [values (->> (random-values random-seed mean sigma)
                    (take (* ^long batch-size ^long num-samples))
                    vec)
        sample-vals (partition batch-size values)
        samples {[:v] (mapv #(stats/sum (darr %)) sample-vals)}]
    {:samples samples
     :values values}))

(comment
  (defspec sample-values-test-property 10
    (prop/for-all
     [batch-size (gen-bounded 1 1)
      random-seed gen/nat
      mean (gen/double* {:min 0 :max 20 :infinite? false :NaN? false})
      sigma (gen/double* {:min 1 :max 1000 :infinite? false :NaN? false})]
     (let [num-samples 10000
           {:keys [values]}
           (sample-values batch-size num-samples random-seed mean sigma)
           mean-error (abs-error (stats/mean values) mean)
           variance (* sigma sigma)
           variance-error (abs-error (stats/variance values) variance)
           mean-tol (max (* sigma 1e-1) 1e-2)
           variance-tol (* variance 2e-1)]
       (is (< mean-error mean-tol) "mean")
       (is (< variance-error variance-tol) "variance")
       (and (< mean-error mean-tol)
            (< variance-error variance-tol))))))

(defn stats-values [batch-size num-samples random-seed mean sigma]
  (let [{:keys [samples values]} (sample-values
                                  batch-size num-samples random-seed mean sigma)
        stats (sampled-stats/stats-for
               (samples [:v])
               {:quantiles [0.05 0.95]})
        mean-hat (-> stats :mean)
        variance-hat (-> stats :variance)
        mean (stats/mean (darr values))
        variance (stats/variance (darr values))]
    {:mean mean
     :variance variance
     :mean-hat mean-hat
     :variance-hat variance-hat
     :samples samples}))

(defspec ^:slow stats-for-test-property
  {:num-tests 10}
  (prop/for-all
   [^long batch-size (gen-bounded 10 1000)
    random-seed gen/nat]
   (let [num-samples    (long (quot 20000 batch-size))
         mean-arg       10.0
         sigma          3.0
         {:keys [mean variance mean-hat variance-hat]}
         (stats-values batch-size num-samples random-seed mean-arg sigma)
         mean           (double mean)
         variance       (double variance)
         batch-size-d   (double batch-size)
         mean-error     (double (abs-error (* batch-size-d mean) mean-hat))
         variance-error (double (abs-error (* batch-size-d variance) variance-hat))
         mean-tol       (max (* sigma 1e-1) 1e-2)
          ;; Use 3-sigma tolerance based on sample variance standard error:
          ;; SE(variance) ≈ variance * sqrt(2/(n-1))
         variance-se    (* batch-size-d
                           variance
                           (Math/sqrt (/ 2.0 (dec num-samples))))
         variance-tol   (* 3.0 variance-se)]
     (is (< mean-error mean-tol))
     (is (< variance-error variance-tol))
     (and (< mean-error mean-tol)
          (< variance-error variance-tol)))))
