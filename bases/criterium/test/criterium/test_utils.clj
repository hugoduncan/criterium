(ns criterium.test-utils
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.generators :as gen]
   [criterium.array :as arr]
   [criterium.random.interface :as random]
   [criterium.stats.interface :as stats]
   [criterium.test.assert :as assert]))

(defn darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

;;; Re-exports from criterium.test.assert
;; Requiring the assert namespace registers the approx= assert-expr

(def abs-error assert/abs-error)
(def rel-error assert/rel-error)
(def ulp assert/ulp)
(def compare-doubles assert/compare-doubles)
(def element-diffs assert/element-diffs)
(def approx= assert/approx=)

(defmacro test-max-error
  "Assert that absolute error is less than max-error."
  ([expected actual max-error]
   `(is (< (abs-error ~expected ~actual) ~max-error)))
  ([expected actual max-error msg]
   `(is (< (abs-error ~expected ~actual) ~max-error) ~msg)))

(defn gen-bounded
  "Generates a long in the range from min-val to max-val inclusive.
  Unlike gen/choose, this is bounded by the generator's `size` parameter,
  starting at min-val."
  [^long min-val ^long max-val]
  (let [r (- max-val min-val)]
    (gen/fmap
     (fn [^long x] (+ min-val x))
     (gen/sized
      (fn [^long size]
        (let [s (min size r)]
          (gen/choose 0 s)))))))

(defn gen-double [options]
  (gen/double*
   (merge {:infinite? false :NaN? false} options)))

(defn trimmed-lines
  [s]
  (->> s
       str/split-lines
       (mapv str/trim)))

(defn gaussian-samples
  "Generate n samples from a Gaussian distribution.
  Uses WELL RNG and Ziggurat algorithm for high-quality random numbers.
  Returns a vector of doubles."
  ([n] (gaussian-samples n 0.0 1.0))
  ([n mean std-dev] (gaussian-samples n mean std-dev 42))
  ([n mean std-dev seed]
   (let [rng (random/make-normal-rng (random/make-well-rng-1024a seed))
         mean (double mean)
         std-dev (double std-dev)]
     (loop [i (long 0)
            result (transient [])]
       (if (< i (long n))
         (let [x (random/next-gaussian! rng)]
           (recur (inc i) (conj! result (+ mean (* std-dev x)))))
         (persistent! result))))))

(defn plus-frac ^double [^double x ^double f]
  (+ x (* x f)))

(deftest approximately=-test
  (testing "exact equality"
    (is (nil? (compare-doubles 1.0 1.0)))
    (is (nil? (compare-doubles 0.0 0.0)))
    (is (nil? (compare-doubles -1.0 -1.0))))

  (testing "exact equality, integer args"
    (is (nil? (compare-doubles 1 1)))
    (is (nil? (compare-doubles 0 0)))
    (is (nil? (compare-doubles -1 -1))))

  (testing "ulp-based comparison"
    (let [x (+ 0.1 0.2)]
      (is (nil? (compare-doubles 0.3 x)))))

  (testing "ulp-based failure"
    (let [x (+ 0.1 (* 3.0 (double (ulp 0.1))))]
      (is (compare-doubles 0.3 x))))

  (testing "relative tolerance"
    (is (nil? (compare-doubles 100.0 100.1 0.01 2))) ; within 1% tolerance
    (is (some? (compare-doubles 100.0 101.0 0.001 2)))) ; exceeds 0.1% tolerance

  (testing "near zero values"
    (is (nil? (compare-doubles 0.0 1e-12))) ; within default tolerance
    (is (some? (compare-doubles 0.0 1e-6)))) ; exceeds tolerance

  (testing "large numbers"
    (is (nil? (compare-doubles 1e6 (plus-frac 1e6 1e-9)))) ; small rel diff
    (is (some? (compare-doubles 1e6 (plus-frac 1e6 1e-7))))) ; large rel diff

  (testing "small numbers"
    (is (nil? (compare-doubles 1e-6 (plus-frac 1e-6 1e-9)))) ; small rel diff
    (is (some? (compare-doubles 1e-6 (plus-frac 1e-6 1e-7)))))) ;

(comment
  (deftest approx=-test
    (is (approx= 1 2))
    (is (approx= 1 1))
    (is (approx= 1.0 (plus-frac 1.0 0.1)))
    (is (approx= [1.0] [(plus-frac 1.0 0.1)]))
    (is (approx= [1.0] [(plus-frac 1.0 1e-9)]))))

;;; Statistical test helpers

(defn autocorrelation
  "Compute sample autocorrelation at a given lag.

  Returns the correlation coefficient between x[i] and x[i+lag] for
  i = 0 to n-lag-1. Result is in [-1, 1] where 0 indicates no
  correlation.

  Requires lag < (count samples)."
  ^double [samples ^long lag]
  (assert (< lag (count samples)) "lag must be less than sample count")
  (let [samples  (double-array samples)
        n        (long (alength samples))
        sum-all  (double (areduce samples i sum 0.0 (+ sum (aget samples i))))
        mean     (/ sum-all (double n))
        ;; Compute variance (denominator)
        var     (double
                 (areduce samples i sum 0.0
                          (let [d (- (aget samples i) mean)]
                            (+ sum (* d d)))))
        ;; Compute covariance at lag (numerator)
        limit   (- n lag)
        cov     (double
                 (loop [i   (long 0)
                        sum 0.0]
                   (if (< i limit)
                     (recur (inc i)
                            (+ sum (* (- (aget samples i) mean)
                                      (- (aget samples (+ i lag)) mean))))
                     sum)))]
    (if (zero? var)
      0.0
      (/ cov var))))

(defn variance-ratio
  "Compute the ratio of observed batch-sum variance to expected variance.

  For independent samples, batch sums have variance n*σ² where σ² is
  the individual sample variance. Returns observed/expected ratio.
  A ratio of 1.0 indicates independent samples; higher values suggest
  positive autocorrelation.

  Parameters:
    samples - vector of samples
    batch-size - number of samples per batch
    expected-individual-variance - expected variance of individual samples (default 1.0)

  Expects samples to be a vector."
  (^double [samples ^long batch-size]
   (variance-ratio samples batch-size 1.0))
  (^double [samples ^long batch-size ^double expected-individual-variance]
   (let [n            (count samples)
         num-batches  (quot n batch-size)
         batches      (mapv (fn [^long i]
                              (subvec samples (* i batch-size) (* (inc i) batch-size)))
                            (range num-batches))
         batch-sums   (mapv #(reduce + %) batches)
         ;; Expected variance for sum of batch-size independent samples
         ;; Var(sum) = batch-size * expected-individual-variance
         expected-var (* (double batch-size) expected-individual-variance)
         observed-var (stats/variance (darr batch-sums))]
     (/ observed-var expected-var))))

(defn variance-ratio-uniform
  "Compute variance ratio for uniform [0,1) samples.

  Convenience function that calls variance-ratio with
  expected-individual-variance of 1/12 (the variance of U(0,1))."
  ^double [samples ^long batch-size]
  (variance-ratio samples batch-size (/ 1.0 12)))

;;; Xoshiro RNG helpers (JDK 17+)

(defn xoshiro-available?
  "Check if Xoshiro256PlusPlus is available (JDK 17+)."
  []
  (try
    (Class/forName "java.util.random.RandomGeneratorFactory")
    true
    (catch ClassNotFoundException _ false)))

(defn make-xoshiro-rng
  "Create a Xoshiro256PlusPlus RNG instance using reflection.

  Returns nil if not available (JDK < 17)."
  [^long seed]
  (try
    (let [factory-class (Class/forName "java.util.random.RandomGeneratorFactory")
          of-method     (.getMethod factory-class "of" (into-array Class [String]))
          factory       (.invoke of-method nil (object-array ["Xoshiro256PlusPlus"]))
          create-method (.getMethod (class factory) "create" (into-array Class [Long/TYPE]))]
      (.invoke create-method factory (object-array [seed])))
    (catch Exception _ nil)))
