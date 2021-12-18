(ns criterium.util.sampled-stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.test-utils :refer [abs-error gen-bounded test-max-error]]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]
   [criterium.util.ziggurat :as ziggurat]))

(deftest pair-fn-test
  (is (= [:a 15] ((sampled-stats/pair-fn :a (partial * 3)) 5))))

(deftest quantile-fns-test
  (is (= {0.01 1 0.99 99}
         (sampled-stats/sample-quantiles [0.01 0.99] (range 101)))))

(deftest stats-fns-test
  (is (= [[:mean 50.0] [:variance 858.5] [:min-val 0] [:max-val 100]]
         (sampled-stats/stats-fns (range 101)))))

(defn batch-transforms [^long batch-size]
  {:sample-> (list (fn [v] (/ v batch-size)))
   :->sample [(fn [v] (* v batch-size))]})

(def identity-transforms
  {:sample-> (list identity)
   :->sample [identity]})

(deftest stats-for-test
  (let [samples (mapv double (repeat 100 1))
        stats   (sampled-stats/stats-for
                 samples {:quantiles [0.05 0.95]} identity-transforms)]
    (is (= 1.0 (-> stats :mean)))
    (is (= 0.0 (-> stats :variance))))

  (testing "stats on [0..100]"
    (let [samples (mapv double (range 101))
          stats   (sampled-stats/stats-for
                   samples {:quantiles [0.05 0.95]} identity-transforms)]
      (is (= 50.0 (-> stats :mean)))
      (is (= 858.5 (-> stats :variance)))
      (is (= 0.0 (-> stats :min-val)))
      (is (= 100.0 (-> stats :max-val)))))

  (testing "stats on (reverse [0..100])"
    (let [samples (mapv double (range 101))
          stats   (sampled-stats/stats-for
                   samples {:quantiles [0.05 0.95]} identity-transforms)]
      (is (= 50.0 (-> stats :mean)))
      (is (= 858.5 (-> stats :variance)))
      (is (= 0.0 (-> stats :min-val)))
      (is (= 100.0 (-> stats :max-val)))))

  (testing "stats on [0..100]*2 (ie batch-size 2)"
    (let [samples    (mapv double (range 0 202 2))
          transforms (batch-transforms 2)
          stats      (sampled-stats/stats-for
                      samples {:quantiles [0.05 0.95]} transforms)]
      (is (= 50.0 (-> stats :mean)))
      (is (= 1717.0 (-> stats :variance)))
      (is (= 0.0 (-> stats :min-val)))
      (is (= 100.0 (-> stats :max-val))))))

(deftest quantiles-for-test
  (let [samples {[:v] (repeat 100 1)}
        stats   (sampled-stats/quantiles-for
                 [:v] samples {:quantiles [0.05 0.95]} identity-transforms)]
    (is (= {0.25 1.0, 0.5 1.0, 0.75 1.0, 0.05 1.0, 0.95 1.0} stats)))

  (testing "stats on [0..100]"
    (let [samples {[:v] (range 101)}
          stats   (sampled-stats/quantiles-for
                   [:v] samples {:quantiles [0.05 0.95]} identity-transforms)]
      (is (= {0.25 25.0, 0.5 50.0, 0.75 75.0, 0.05 5.0, 0.95 95.0} stats))))

  (testing "stats on (reverse [0..100])"
    (let [samples {[:v] (range 101)}
          stats   (sampled-stats/quantiles-for
                   [:v] samples {:quantiles [0.05 0.95]} identity-transforms)]
      (is (= {0.25 25.0, 0.5 50.0, 0.75 75.0, 0.05 5.0, 0.95 95.0} stats))))

  (testing "stats on [0..100]*2 (ie batch-size 2)"
    (let [samples    {[:v] (range 0 202 2)}
          transforms (batch-transforms 2)
          stats      (sampled-stats/quantiles-for
                      [:v] samples {:quantiles [0.05 0.95]} transforms)]
      (is (= {0.25 25.0, 0.5 50.0, 0.75 75.0, 0.05 5.0, 0.95 95.0} stats)))))

(deftest stats-for-test-property-1
  (let [batch-size   10000
        num-samples  100
        values       (take
                      (* batch-size num-samples)
                      (ziggurat/random-normal-zig
                       (well/well-rng-1024a)))
        sample-vals  (partition batch-size values)
        samples      (mapv #(stats/sum (mapv double %)) sample-vals)
        transforms   (batch-transforms batch-size)
        stats        (sampled-stats/stats-for
                      samples {:quantiles [0.05 0.95]} transforms)
        mean-hat     (-> stats :mean)
        variance-hat (-> stats :variance)
        mean         (stats/mean values)
        variance     (stats/variance values)]
    (test-max-error mean mean-hat 1e-5)
    (test-max-error variance variance-hat 0.35)))

(defn random-values
  "Return a sequence of values with the given mean an standard deviation."
  [random-seed ^double mean ^double sigma]
  (let [random-source (java.util.Random. random-seed)]
    (->> #(.nextDouble random-source)
         repeatedly
         ziggurat/random-normal-zig
         (map (fn [^double x] (+ mean (* sigma x)))))))

(comment
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (defspec random-values-test-property 10
    (prop/for-all
     [random-seed gen/large-integer
      mean (gen/double* {:min 0 :max 20 :infinite? false :NaN? false})
      sigma (gen/double* {:min 1 :max 1000 :infinite? false :NaN? false})]
     (let [values         (take 10000 (random-values random-seed mean sigma))
           variance       (* sigma sigma)
           mean-error     (abs-error (stats/mean values) mean)
           variance-error (abs-error (stats/variance values) variance)
           mean-tol       (max (* sigma 5e-2) 1e-2)
           variance-tol   (* variance 2e-1)]
       (is (< mean-error mean-tol) "mean")
       (is (< variance-error variance-tol) "variance")
       (and (< mean-error mean-tol)
            (< variance-error variance-tol))))))

(defn sample-values
  "Generate batched samples with the given mean and standard deviation."
  [batch-size num-samples random-seed mean sigma]
  (let [values      (->> (random-values random-seed mean sigma)
                         (take (* ^long batch-size ^long num-samples))
                         vec)
        sample-vals (partition batch-size values)
        samples     {[:v] (mapv #(stats/sum %) sample-vals)}]
    {:samples samples
     :values  values}))

(comment
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (defspec sample-values-test-property 10
    (prop/for-all
     [batch-size (gen-bounded 1 1)
      random-seed gen/nat
      mean (gen/double* {:min 0 :max 20 :infinite? false :NaN? false})
      sigma (gen/double* {:min 1 :max 1000 :infinite? false :NaN? false})]
     (let [num-samples    10000
           {:keys [values]}
           (sample-values batch-size num-samples random-seed mean sigma)
           mean-error     (abs-error (stats/mean values) mean)
           variance       (* sigma sigma)
           variance-error (abs-error (stats/variance values) variance)
           mean-tol       (max (* sigma 1e-1) 1e-2)
           variance-tol   (* variance 2e-1)]
       (is (< mean-error mean-tol) "mean")
       (is (< variance-error variance-tol) "variance")
       (and (< mean-error mean-tol)
            (< variance-error variance-tol))))))

(defn stats-values [batch-size num-samples random-seed mean sigma]
  (let [{:keys [samples values]} (sample-values
                                  batch-size num-samples random-seed mean sigma)
        transforms               (batch-transforms batch-size)
        stats                    (sampled-stats/stats-for
                                  (samples [:v])
                                  {:quantiles [0.05 0.95]} transforms)
        mean-hat                 (-> stats :mean)
        variance-hat             (-> stats :variance)
        mean                     (stats/mean values)
        variance                 (* (stats/variance values) 1)]
    {:mean         mean
     :variance     variance
     :mean-hat     mean-hat
     :variance-hat variance-hat
     :samples      samples}))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec stats-for-test-property 10
  (prop/for-all
   [^long batch-size (gen-bounded 1 1000)
    random-seed gen/nat]
   (let [num-samples    (long (quot 10000 batch-size))
         mean           10
         sigma          3
         {:keys [mean variance mean-hat variance-hat]}
         (stats-values batch-size num-samples random-seed mean sigma)
         mean-error     (abs-error mean mean-hat)
         variance-error (abs-error variance variance-hat)
         mean-tol       (max (* sigma 1e-1) 1e-2)
         variance-tol   (* ^double variance 5e-1)]
     (is (< mean-error mean-tol))
     (is (< variance-error variance-tol))
     (and (< mean-error mean-tol)
          (< variance-error variance-tol)))))
