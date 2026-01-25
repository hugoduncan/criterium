(ns criterium.random.ziggurat-test
  (:require
   [clojure.pprint :as pprint]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.array :as arr]
   [criterium.random.well :as well]
   [criterium.random.ziggurat :as ziggurat]
   [criterium.stats.core :as stats]
   [criterium.test-utils :refer [abs-error
                                 autocorrelation
                                 make-xoshiro-rng
                                 variance-ratio
                                 xoshiro-available?]]))

(defn- darr
  "Create a DoubleArray from a sequence."
  [coll]
  (arr/->double-array (double-array coll)))

;; Tests for ziggurat algorithm for generating normal random variates.
;; Verifies correct distribution and independence of samples when using WELL RNG.

(defspec ^:slow random-normal-zig-test-property 10
  (prop/for-all
   [random-seed gen/small-integer]
   (let [rng            (ziggurat/make-normal-rng (well/make-well-rng-1024a random-seed))
         values         (darr (repeatedly 10000 #(ziggurat/next-gaussian! rng)))
         mean           (stats/mean values)
         variance       (stats/variance values)
         mean-error     (double (abs-error mean 0.0))
         variance-error (double (abs-error variance 1.0))
         mean-tol       1e-1
         variance-tol   1e-1]
     (is (< mean-error mean-tol))
     (is (< variance-error variance-tol))
     (and (< mean-error mean-tol)
          (< variance-error variance-tol)))))

;;; Autocorrelation tests
;; Verify that ziggurat with WELL RNG produces normal samples with negligible
;; autocorrelation and correct marginal distribution. The ziggurat algorithm
;; consumes 2-3+ uniform values per output, which can amplify RNG correlations.

(deftest ^:slow ziggurat-well-rng-autocorrelation-test
  ;; Verify ziggurat produces independent normal samples when using WELL RNG.
  ;; With n=100,000, SE approx 1/sqrt(n) approx 0.003, so threshold of 0.02 is conservative.
  (testing "NormalRng with WellRng1024a"
    (let [seed    42
          n       100000
          rng     (ziggurat/make-normal-rng (well/make-well-rng-1024a seed))
          samples (vec (repeatedly n #(ziggurat/next-gaussian! rng)))]
      (testing "has negligible autocorrelation at multiple lags"
        (let [lags [1 2 5 10]]
          (doseq [lag lags]
            (let [ac (autocorrelation samples lag)]
              (is (< (Math/abs ac) 0.02)
                  (format "lag %d: autocorrelation %.4f exceeds threshold"
                          lag ac))))))

      (testing "has batch-sum variance ratio near 1.0"
        (let [batch-size 500
              ;; Normal distribution has variance 1.0
              ratio      (variance-ratio samples batch-size 1.0)]
          (is (< 0.9 ratio 1.1)
              (format "variance ratio %.3f outside [0.9, 1.1]" ratio))))

      (testing "has correct marginal distribution"
        (let [mean     (stats/mean (darr samples))
              variance (stats/variance (darr samples))]
          (is (< (Math/abs mean) 0.02)
              (format "mean %.4f exceeds tolerance 0.02" mean))
          (is (< (Math/abs (- variance 1.0)) 0.1)
              (format "variance %.4f not within 0.1 of 1.0" variance)))))))

;;; Normal sample comparison table

(defn print-ziggurat-comparison-table
  "Print a table comparing normal sample sources.

  Computes and displays mean, variance, autocorrelation at various lags,
  and variance ratio for each available normal sample source:
  - WELL-1024a + NormalRng
  - Xoshiro256++ nextGaussian (JDK 17+, native normal generation)

  This function is for manual exploration and is not called in tests."
  ([]
   (print-ziggurat-comparison-table {}))
  ([{:keys [seed n batch-size lags]
     :or   {seed 42, n 100000, batch-size 500, lags [1 2 5 10]}}]
   (let [;; Format to 4 significant figures
         fmt-4sf (fn [x] (format "%.4g" (double x)))

         ;; Generate normal samples from each source
         well-rng      (ziggurat/make-normal-rng (well/make-well-rng-1024a seed))
         well-samples  (vec (repeatedly n #(ziggurat/next-gaussian! well-rng)))

         ;; Xoshiro256++ nextGaussian (native normal) if available
         xoshiro-gaussian-samples
         (when (xoshiro-available?)
           (when-let [rng (make-xoshiro-rng seed)]
             (let [next-gaussian (.getMethod (class rng) "nextGaussian"
                                             (into-array Class []))]
               (vec (repeatedly n #(.invoke next-gaussian rng (object-array [])))))))

         ;; Compute metrics for each source
         compute-metrics
         (fn [name samples]
           (let [samples-arr (darr samples)
                 mean      (stats/mean samples-arr)
                 variance  (stats/variance samples-arr)
                 ac-vals   (mapv #(autocorrelation samples %) lags)
                 ;; Normal samples have variance 1.0
                 vr        (variance-ratio samples batch-size 1.0)]
             (into {:source         name
                    :mean           (fmt-4sf mean)
                    :variance       (fmt-4sf variance)
                    :variance-ratio (fmt-4sf vr)}
                   (map vector
                        (map #(keyword (str "ac-lag-" %)) lags)
                        (map fmt-4sf ac-vals)))))

         results
         (cond-> [(compute-metrics "WELL + NormalRng" well-samples)]
           xoshiro-gaussian-samples
           (conj (compute-metrics "Xoshiro++ nextGaussian" xoshiro-gaussian-samples)))]

     (println "\nNormal Sample Source Comparison")
     (println (str "Samples: " n ", Batch size: " batch-size ", Lags: " lags))
     (println)
     (pprint/print-table
      (into [:source :mean :variance]
            (concat (map #(keyword (str "ac-lag-" %)) lags)
                    [:variance-ratio]))
      results)
     (println)
     (println "Expected values for standard normal:")
     (println "  - Mean approx 0, Variance approx 1")
     (println "  - Autocorrelation approx 0 (threshold: |ac| < 0.02)")
     (println "  - Variance ratio approx 1.0 (threshold: 0.9 to 1.1)"))))

(comment
  (print-ziggurat-comparison-table)
  (print-ziggurat-comparison-table {:n 50000}))

;;; NormalRng deftype tests

(deftest normal-rng-deftype-test
  ;; Tests for the mutable NormalRng deftype.
  ;; Verifies deterministic seeding, distribution properties, and state mutation.
  (testing "NormalRng deftype"
    (testing "produces values with correct distribution"
      (let [rng    (ziggurat/make-normal-rng (well/make-well-rng-1024a 42))
            values (darr (repeatedly 10000 #(ziggurat/next-gaussian! rng)))
            mean   (stats/mean values)
            var    (stats/variance values)]
        (is (< (Math/abs mean) 0.1)
            (format "mean %.4f exceeds tolerance 0.1" mean))
        (is (< (Math/abs (- var 1.0)) 0.1)
            (format "variance %.4f not within 0.1 of 1.0" var))))

    (testing "with same seed produces same sequence"
      (let [rng1  (ziggurat/make-normal-rng (well/make-well-rng-1024a 123))
            rng2  (ziggurat/make-normal-rng (well/make-well-rng-1024a 123))
            vals1 (vec (repeatedly 100 #(ziggurat/next-gaussian! rng1)))
            vals2 (vec (repeatedly 100 #(ziggurat/next-gaussian! rng2)))]
        (is (= vals1 vals2))))))

(defspec normal-rng-deftype-statistics-property 50
  (prop/for-all
   [seed (gen/large-integer* {:min 1 :max Long/MAX_VALUE})]
   (let [rng    (ziggurat/make-normal-rng (well/make-well-rng-1024a seed))
         values (darr (repeatedly 10000 #(ziggurat/next-gaussian! rng)))
         mean   (stats/mean values)
         var    (stats/variance values)]
     (and (< (Math/abs mean) 0.1)
          (< (Math/abs (- var 1.0)) 0.1)))))

(defspec normal-rng-deftype-determinism-property 50
  (prop/for-all
   [seed (gen/large-integer* {:min 0 :max Long/MAX_VALUE})]
   (let [rng1  (ziggurat/make-normal-rng (well/make-well-rng-1024a seed))
         rng2  (ziggurat/make-normal-rng (well/make-well-rng-1024a seed))
         vals1 (vec (repeatedly 100 #(ziggurat/next-gaussian! rng1)))
         vals2 (vec (repeatedly 100 #(ziggurat/next-gaussian! rng2)))]
     (= vals1 vals2))))
