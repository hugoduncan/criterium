(ns criterium.util.t-digest.scale-test
  (:require
   [clojure.math :refer [ulp]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.test-utils :refer [approx=]]
   [criterium.util.t-digest.scale :as scale]))

(defn gen-double [options]
  (gen/double*
   (merge {:infinite? false :NaN? false} options)))

(defn inverse-property
  [scale]
  ;; we lose accuracy near the q limits
  (prop/for-all [q      (gen-double {:min 1e-12 :max 0.5})
                 norm   (gen-double {:min 1.0 :max 1000.0})]
    (let [k  (scale/k scale q norm)
          q' (scale/q scale k norm)]
      (approx=
       q
       q'
       1e-4))))

(defn k-normalizer-arity-property
  [scale]
  (prop/for-all [q      (gen-double {:min 0.0 :max 0.5})
                 comp   (gen-double {:min 1.0 :max 1000.0})
                 n      (gen-double {:min 1.0 :max 1000.0})]
    (let [norm (scale/normalizer scale comp n)
          k    (scale/k scale q norm)
          k'   (scale/k scale q comp n)]
      (approx= k k'))))

(defn q-normalizer-arity-property
  [scale ^double k-min]
  ;; we lose accuracy near the q limits
  (prop/for-all [k      (gen-double {:min k-min :max 100.0})
                 comp   (gen-double {:min 1.0 :max 1000.0})
                 n      (gen-double {:min 1.0 :max 1000.0})]
    (let [norm (scale/normalizer scale comp n)
          q    (scale/q scale k norm)
          q'   (scale/q scale k comp n)]
      (approx= q q' 1e-4))))

(defn max-size-normalizer-arity-property
  [scale]
  (prop/for-all [q      (gen-double {:min 1e-15 :max (- 1.0 1e-15)})
                 comp   (gen-double {:min 1.0 :max 1000.0})
                 n      (gen-double {:min 1.0 :max 1000.0})]
    (let [norm (scale/normalizer scale comp n)
          s    (scale/max-size scale q norm)
          s'   (scale/max-size scale q comp n)]
      (approx= s s'))))

(defn k-reflection-property
  [scale]
  (prop/for-all [^double q      (gen-double {:min 1e-15 :max (- 0.5 (ulp 0.5))})
                 norm   (gen-double {:min 1.0 :max 1000.0})]
    (let [k  (scale/k scale q norm)
          k' (- (scale/k scale (- 1.0 q) norm))]
      ;; k1 should be symmetric around q=0.5
      (approx= k k' 1e-4 5))))

(defn k-monotonic-property
  [scale]
  (prop/for-all [^double q      (gen-double {:min 0.0 :max 0.5})
                 norm   (gen-double {:min 1.0 :max 1000.0})]
    (let [k  (scale/k scale q norm)
          k' (scale/k scale (+ q 0.1) norm)]
      ;; k1 should increase monotonically with q
      (> (scale/k scale (+ q 0.1) norm) k))))

(defspec k0-inverse-property-test
  (inverse-property scale/k0))

(defspec k0-k-normalizer-arity-property
  (k-normalizer-arity-property scale/k0))

(defspec k0-q-normalizer-arity-property
  (q-normalizer-arity-property scale/k0 0.0))

(defspec k0-max-size-normalizer-arity-property
  (max-size-normalizer-arity-property scale/k0))

(comment
  ;; This doesn't hold for k0
  (defspec k0-k-reflection-property
    (k-reflection-property scale/k0)))

(defspec k0-k-monotonic-property
  (k-monotonic-property scale/k0))



(defspec k1-inverse-property-test
  (inverse-property scale/k1))

(defspec k1-k-normalizer-arity-property
  (k-normalizer-arity-property scale/k1))

#_(defspec k1-q-normalizer-arity-property
    (q-normalizer-arity-property scale/k1 0.0))

(defspec k1-max-size-normalizer-arity-property
  (max-size-normalizer-arity-property scale/k1))

(defspec k1-k-reflection-property
  (k-reflection-property scale/k1))

(defspec k1-k-monotonic-property
  (k-monotonic-property scale/k1))


(defspec k2-inverse-property-test
  (inverse-property scale/k2))

#_(defspec k2-k-normalizer-arity-property
    (k-normalizer-arity-property scale/k2))

(defspec k2-q-normalizer-arity-property
  (q-normalizer-arity-property scale/k2 (+ 1.0 1e-5)))

(defspec k2-max-size-normalizer-arity-property
  (max-size-normalizer-arity-property scale/k2))

#_(defspec k2-k-reflection-property
    (k-reflection-property scale/k2 ))

(defspec k2-k-monotonic-property
  (k-monotonic-property scale/k2))


#_(defspec k1-scale-function-properties-test
    (prop/for-all [q      (gen/double* {:min 0.0 :max 0.5 :NaN? false})
                   comp   (gen/double* {:min 1.0 :max 1000.0 :NaN? false})]
      (let [scale scale/k1
            k1    (scale/k scale q comp)]
        (and
         ;; k1 should increase monotonically with q
         (> (scale/k scale (+ q 0.1) comp) k1)
         ;; k1 should be symmetric around q=0.5
         (approx= k1 (- (scale/k scale (- 1.0 q) comp)) 1e-7 5)))))
