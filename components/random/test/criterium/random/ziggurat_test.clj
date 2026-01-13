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
   [criterium.stats.interface :as stats]
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
   (let [rng            (well/well-rng-1024a random-seed)
         values         (->> rng
                             ziggurat/random-normal-zig
                             (take 10000)
                             darr)
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
  (testing "random-normal-zig with WELL RNG"
    (let [seed    42
          n       100000
          samples (vec (take n (ziggurat/random-normal-zig
                                (well/well-rng-1024a seed))))]
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
  - LCG (java.util.Random) + ziggurat
  - WELL-1024a + ziggurat
  - Xoshiro256++ + ziggurat (JDK 17+)
  - Xoshiro256++ nextGaussian (JDK 17+, native normal generation)

  This function is for manual exploration and is not called in tests."
  ([]
   (print-ziggurat-comparison-table {}))
  ([{:keys [seed n batch-size lags]
     :or   {seed 42, n 100000, batch-size 500, lags [1 2 5 10]}}]
   (let [;; Format to 4 significant figures
         fmt-4sf (fn [x] (format "%.4g" (double x)))

         ;; Generate normal samples from each source
         lcg           (java.util.Random. seed)
         lcg-rng-seq   (repeatedly #(.nextDouble lcg))
         lcg-samples   (vec (take n (ziggurat/random-normal-zig lcg-rng-seq)))
         well-samples  (vec (take n (ziggurat/random-normal-zig
                                     (well/well-rng-1024a seed))))

         ;; Xoshiro256++ + ziggurat if available
         xoshiro-zig-samples
         (when (xoshiro-available?)
           (when-let [rng (make-xoshiro-rng seed)]
             (let [next-double (.getMethod (class rng) "nextDouble"
                                           (into-array Class []))
                   rng-seq     (repeatedly #(.invoke next-double rng (object-array [])))]
               (vec (take n (ziggurat/random-normal-zig rng-seq))))))

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
         (cond-> [(compute-metrics "LCG + ziggurat" lcg-samples)
                  (compute-metrics "WELL + ziggurat" well-samples)]
           xoshiro-zig-samples
           (conj (compute-metrics "Xoshiro++ + ziggurat" xoshiro-zig-samples))
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
