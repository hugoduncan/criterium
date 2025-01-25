(ns criterium.util.t-digest.merging-digest-test
  "Tests for t-digest implementation"
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.test-utils :refer [approx= gen-double]]
   [criterium.util.probability :as probability]
   [criterium.util.t-digest.merging-digest :as md]
   [criterium.util.t-digest.scale :as scale]
   [criterium.util.well :as well]
   [criterium.util.ziggurat :as ziggurat]))

;; (deftest k1-scale-function-test
;;   (let [compression 100.0]
;;     (testing "k1 monotonicity and symmetry"
;;       (let [qs           [0.0 0.1 0.2 0.3 0.4]
;;             k-first-half (mapv #(#'md/integrated-location % compression) qs)]
;;         ;; Values should increase monotonically
;;         (is (approx= k-first-half (sort k-first-half)))
;;         ;; Check symmetry around q=0.5
;;         (is (approx= k-first-half
;;                      (mapv #(- (#'md/integrated-location (- 1.0 %) compression))
;;                            qs)))))))


;; (defspec k1-scale-function-properties-test
;;   (prop/for-all [q      (gen/double* {:min 0.0 :max 0.5 :NaN? false})
;;                  comp   (gen/double* {:min 1.0 :max 1000.0 :NaN? false})]
;;     (let [k1 (#'md/integrated-location q comp)]
;;       (prn :q q :comp comp)
;;       (and
;;        ;; k1 should increase monotonically with q
;;        (> (#'md/integrated-location (+ q 0.1) comp) k1)
;;        ;; k1 should be symmetric around q=0.5
;;        (approx= k1 (- (#'md/integrated-location (- 1.0 q) comp)) 1e-7 5)))))

;; (defspec k1-scale-function-properties-test
;;   (prop/for-all [q      (gen/double* {:min 0.0 :max 0.5 :NaN? false})
;;                  comp   (gen/double* {:min 1.0 :max 1000.0 :NaN? false})]
;;     (let [scale scale/k1
;;           k1    (scale/k scale q comp)]
;;       (prn :q q :comp comp)
;;       (and
;;        ;; k1 should increase monotonically with q
;;        (> (scale/k scale (+ q 0.1) comp) k1)
;;        ;; k1 should be symmetric around q=0.5
;;        (approx= k1 (- (scale/k scale (- 1.0 q) comp)) 1e-7 5)))))

#_(deftest merge-centroids-invariants
    (let [compression 100.0]
      (testing "merge-centroids maintains t-digest invariants"
        (let [centroids    [(td/->Centroid 1.0 1.0)
                            (td/->Centroid 1.1 1.0)
                            (td/->Centroid 2.0 1.0)
                            (td/->Centroid 3.0 1.0)]
              total-weight 4.0
              result       (#'td/merge-centroids compression centroids total-weight)
              _            (println "\nResult centroids:" result)
              weights      (reductions + (map :weight result))
              _            (println "Cumulative weights:" weights)
              qs           (map #(/ % total-weight) weights)
              _            (println "Quantiles:" qs)
              k-values     (map #(#'td/k1 % compression) qs)
              _            (println "K values:" k-values)
              k-gaps       (map - (rest k-values) k-values)
              _            (println "K gaps:" k-gaps)]

          ;; Basic invariants
          (is (< (count result) (count centroids)))
          (is (= total-weight (reduce + (map :weight result))))
          (is (= (map :mean result) (sort (map :mean result))))

          ;; k1 separation invariant
          (is (every? #(>= % 1.0) k-gaps))))))

(def gen-centroid
  (gen/fmap (fn [m] (md/->Centroid m 1.0))
            (gen-double {:min -1000 :max 1000}) ))

(defspec merge-centroids-centroids-test
  (prop/for-all [centroids (gen/vector gen-centroid 1 100)
                 compression (gen-double {:min 1.0 :max 1000.0})]
    (let [sorted       (sort-by :mean centroids)
          total-weight (reduce + (map :weight sorted))
          merged       (#'md/merge-centroids
                        compression
                        sorted
                        total-weight
                        scale/k0)]
      (testing "Number of centroids should not increase"
        (>= (count sorted) (count merged))))))

(defspec merge-centroids-total-weight-test
  (prop/for-all [centroids (gen/vector gen-centroid 1 100)
                 compression (gen-double {:min 1.0 :max 1000.0})]
    (let [sorted       (sort-by :mean centroids)
          total-weight (reduce + (map :weight sorted))
          merged       (#'md/merge-centroids
                        compression
                        sorted
                        total-weight
                        scale/k0)]
      (testing "Total weight should be preserved"
        (approx= total-weight (reduce + (map :weight merged)))))))

(defspec merge-centroids-means-ordered-test
  (prop/for-all [centroids (gen/vector gen-centroid 1 100)
                 compression (gen-double {:min 1.0 :max 1000.0})]
    (let [sorted       (sort-by :mean centroids)
          total-weight (reduce + (map :weight sorted))
          merged       (#'md/merge-centroids
                        compression
                        sorted
                        total-weight
                        scale/k0)]
      (testing "Means should remain ordered"
        (when (not= (map :mean merged) (sort (map :mean merged)))
          (prn (map :mean merged) (sort (map :mean merged))))
        (= (map :mean merged) (sort (map :mean merged)))))))

#_(defspec merge-centroids-separation-test
    (prop/for-all [centroids (gen/vector gen-centroid 5 100)
                   compression (gen-double {:min 1.0 :max 1000.0})]
      (let [sorted       (sort-by :mean centroids)
            total-weight (reduce + (map :weight sorted))
            merged       (#'md/merge-centroids
                          compression
                          sorted
                          total-weight
                          scale/k2)]
        (testing "Check separation between centroids meets k1 requirements"
          (let [qs         (reductions + (map #(/ (:weight %) total-weight) merged))
                normalizer (scale/normalizer scale/k0 compression total-weight)
                k-values   (map #(scale/k scale/k0 % normalizer total-weight) qs)
                k-gaps     (map - (rest k-values) k-values)]
            (when (not (every? #(>= % 1.0) k-gaps))
              (prn :k-values k-values :k-gaps k-gaps))
            (every? #(>= % 1.0) k-gaps))))))

(deftest quantile-edge-cases
  (testing "quantile edge cases"
    (let [d (reduce md/add-point (md/new-digest)
                    [1.0 2.0 3.0])]
      (is (= 1.0 (md/quantile d 0.0)))
      (is (= 3.0 (md/quantile d 1.0)))
      (is (some? (md/quantile d 0.5))))))

(deftest buffering-operations
  (testing "buffer behavior"
    (let [d (md/new-digest 100.0 3)]
      (testing "points are buffered"
        (let [d (-> d
                    (md/add-point 1.0)
                    (md/add-point 2.0))]
          (is (= 2 (count (:temp-centroids d))))
          (is (= 0 (count (:centroids d))))
          (is (= 2.0 (:unmerged-weight d)))
          (is (= 0.0 (:total-weight d)))))

      (testing "buffer merges when full"
        (let [d (-> d
                    (md/add-point 1.0)
                    (md/add-point 2.0)
                    (md/add-point 3.0)
                    (md/add-point 4.0))]
          (is (= 1 (count (:temp-centroids d))))
          (is (= 3 (count (:centroids d))))
          (is (= 1.0 (:unmerged-weight d)))
          (is (= 3.0 (:total-weight d)))))

      (testing "compress merges partial buffer"
        (let [d (-> d
                    (md/add-point 1.0)
                    (md/add-point 2.0)
                    md/compress)]
          (is (empty? (:temp-centroids d)))
          (is (= 2 (count (:centroids d))))
          (is (= 0.0 (:unmerged-weight d)))
          (is (= 2.0 (:total-weight d)))))))

  (testing "NaN handling"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cannot add NaN to t-digest"
         (md/add-point (md/new-digest) Double/NaN)))))

(defn box-muller
  "Generate normal random numbers using Box-Muller transform."
  [^double mean ^double std-dev]
  (let [u1 (double (rand))
        u2 (double (rand))
        r  (Math/sqrt (* -2 (Math/log u1)))
        t  (* 2 Math/PI u2)]
    (+ mean (* std-dev r (Math/cos t)))))

(deftest normal-distribution-quantile-test
  (testing "accuracy with normal distribution"
    (let [n          20000
          mean       0.0
          std-dev    1.0
          samples    (take n (ziggurat/random-normal-zig
                              (well/well-rng-1024a)))
          digest     (reduce md/add-point (md/new-digest 100.0) samples)
          expected-q {0.01 (probability/normal-quantile 0.01)
                      0.1  (probability/normal-quantile 0.1)
                      0.25 (probability/normal-quantile 0.25)
                      0.5  mean
                      0.75 (probability/normal-quantile 0.75)
                      0.9  (probability/normal-quantile 0.9)
                      0.99 (probability/normal-quantile 0.99)}
          digest     (md/compress digest)]
      (doseq [[q expected-val] expected-q]
        (let [actual (md/quantile digest q)
              error  (Math/abs (/ (- actual (double expected-val)) std-dev))]
          (testing (format "quantile %.2f" q)
            (is (< error 0.2)
                (format "error %.3f std-dev at q=%.2f (expected=%.2f, got=%.2f)"
                        error q expected-val actual))))))))

(deftest normal-distribution-cdf-test
  (testing "accuracy with normal distribution"
    (let [n            20000
          mean         0.0
          std-dev      1.0
          samples      (take n (ziggurat/random-normal-zig
                                (well/well-rng-1024a)))
          digest       (reduce md/add-point (md/new-digest 100.0) samples)
          expected-cdf {-2.0  (probability/normal-cdf -2.0)
                        -1.0  (probability/normal-cdf -1.0)
                        -0.25 (probability/normal-cdf -0.25)
                        0.0   (probability/normal-cdf 0.0)
                        0.25  (probability/normal-cdf 0.25)
                        1.0   (probability/normal-cdf 1.0)
                        2.0   (probability/normal-cdf 2.0)}
          digest       (md/compress digest)]
      (doseq [[z expected-val] expected-cdf]
        (let [actual (md/cdf digest z)
              error  (Math/abs (/ (- actual (double expected-val)) std-dev))]
          (testing (format "cdf %.2f" z)
            (is (< error 0.01)
                (format "error %.3f std-dev at q=%.2f (expected=%.2f, got=%.2f)"
                        error z expected-val actual))))))))

(deftest basic-operations
  (testing "quantile"
    (testing "empty digest"
      (let [d (md/new-digest)]
        (is (= 0.0 (:total-weight d)))
        (is (empty? (:centroids d)))
        (is (NaN? (md/quantile d 0.5)))))

    (testing "single point"
      (let [d (-> (md/new-digest)
                  (md/add-point 1.0)
                  (md/compress))]
        (is (= 1.0 (:total-weight d)))
        (is (= 1.0 (md/quantile d 0.0)))
        (is (= 1.0 (md/quantile d 0.5)))
        (is (= 1.0 (md/quantile d 1.0)))))

    (testing "two points"
      (let [d (-> (md/new-digest)
                  (md/add-point 1.0)
                  (md/add-point 2.0)
                  (md/compress))]
        (is (= 2.0 (:total-weight d)))
        (is (= 1.0 (md/quantile d 0.0)))
        (is (= 2.0 (md/quantile d 1.0)))))

    (testing "uniform distribution"
      (let [points (range 0 100 10)
            d      (reduce md/add-point (md/new-digest) points)
            _      (println "Centroids:" (:centroids d))
            _      (println "Total weight:" (:total-weight d))
            q50    (md/quantile d 0.5)
            _      (println "q50:" q50)
            q100   (md/quantile d 1.0)
            _      (println "q100:" q100)]
        (is (= 0.0 (md/quantile d 0.0)))
        (is (< 44.0 q50 46.0))
        (is (= 90.0 q100)))))  )


(deftest cdf-edge-cases
  (testing "invalid inputs"
    (let [d (md/new-digest)]
      (is (thrown? Exception (md/cdf d Double/NaN)))
      (is (thrown? Exception (md/cdf d Double/POSITIVE_INFINITY)))
      (is (thrown? Exception (md/cdf d Double/NEGATIVE_INFINITY))))))

(deftest cdf-empty-test
  (testing "empty digest"
    (let [d (md/new-digest)]
      (is (Double/isNaN (md/cdf d 0.0))))))

(deftest cdf-single-centroid-test
  (testing "single centroid"
    (let [d (-> (md/new-digest)
                (md/add-point 1.0))]
      (is (= 0.0 (md/cdf d 0.0)))
      (is (= 1.0 (md/cdf d 2.0)))
      (is (= 0.5 (md/cdf d 1.0))))))

(deftest cdf-two-point-test
  (testing "two singletom points"
    (let [d (-> (md/new-digest)
                (md/add-point 1.0)
                (md/add-point 3.0)
                (md/compress))]
      (is (= 0.0 (md/cdf d 0.0)) "below")
      (is (= 0.5 (md/cdf d 1.0)) "at first")
      (is (= 1.0 (md/cdf d 3.0)) "at last")
      (is (= 1.0 (md/cdf d 4.0)) "above")
      (is (approx= 0.5 (md/cdf d 2.0)) "midpoint"))))

(deftest cdf-uniform-test
  (testing "uniform distribution"
    (let [points (range 0 100 10)
          d      (reduce md/add-point (md/new-digest) points)]
      (is (= 0.0 (md/cdf d -1.0)) "below")
      (is (= 1.0 (md/cdf d 100.0)) "above")
      (is (approx= 0.5 (md/cdf d 45.0) 0.1) "midpoint"))))

;; Generators

(def gen-finite-double
  (gen/double* {:infinite? false :NaN? false}))

(def gen-digest
  (gen/fmap (fn [points]
              (reduce md/add-point (md/new-digest) points))
            (gen/vector gen-finite-double 1 100)))

;; Property-based tests

(defspec cdf-bounds-property
  100
  (prop/for-all [d gen-digest
                 x gen-finite-double]
    (let [cdf-x (md/cdf d x)]
      (and (<= 0.0 cdf-x 1.0)
           (= 0.0 (md/cdf d (- Double/MAX_VALUE)))
           (= 1.0 (md/cdf d Double/MAX_VALUE))))))

(defspec cdf-monotonic-property
  100
  (prop/for-all [d gen-digest
                 ^double x gen-finite-double
                 ^double dx (gen/double*
                             {:min       0.0   :max  100.0
                              :infinite? false :NaN? false})]
    (let [cdf-x    (md/cdf d x)
          cdf-x+dx (md/cdf d (+ x dx))]
      (when-not (>= cdf-x+dx cdf-x)
        (prn :x x :dx dx :cdf-x+dx cdf-x+dx :cdf-x cdf-x ))
      (>= cdf-x+dx cdf-x))))

(defspec cdf-continuous-property
  100
  (prop/for-all [d gen-digest
                 ^double x gen-finite-double]
    (let [epsilon 1e-10
          left    (md/cdf d (- x epsilon))
          right   (md/cdf d (+ x epsilon))]
      (approx= left right 1e-5))))

(defspec cdf-quantile-inverse-property
  100
  (prop/for-all [d gen-digest
                 p (gen/double* {:min       0.0   :max  1.0
                                 :infinite? false :NaN? false})]
    (let [q  (md/quantile d p)
          p' (md/cdf d q)]
      (or (Double/isNaN q)  ; empty digest case
          (approx= p p' 1e-5)))))
