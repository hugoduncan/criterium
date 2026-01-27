(ns criterium.stats-core-benchmark
  "Benchmark notebook for stats.core functions to identify allocation hotspots.

  Uses `:with-allocation-trace true` to capture both timing and allocation
  data for criterium.stats.core functions."
  (:require
   [criterium.array :as arr]
   [criterium.bench :as bench]
   [criterium.stats.core :as stats]))

(bench/set-default-viewer! :kindly)

;; # Stats Core Function Benchmarks
;;
;; This notebook benchmarks stats.core functions to identify allocation
;; hotspots and verify which functions are garbage-free.

;; ## Test Data Helpers

(defn unimodal-data
  "Create unimodal test data from sequential values."
  [n]
  (arr/->double-array (double-array (range n))))

;; Sample data for benchmarks (30 samples)
(def ^:private data-30 (unimodal-data 30))

;; Pre-sorted data for order statistics
(def ^:private sorted-30
  (arr/->double-array
   (double-array (sort (seq (.array ^criterium.array.DoubleArray data-30))))))

;; ## Basic Aggregation Functions

;; ### min

(bench/bench (stats/min data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### max

(bench/bench (stats/max data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### mean

(bench/bench (stats/mean data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### sum

(bench/bench (stats/sum data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### sum-of-squares

(bench/bench (stats/sum-of-squares data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Variance

;; ### variance (sample, default df=1)

(bench/bench (stats/variance data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### variance (population, df=0)

(bench/bench (stats/variance data-30 0)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Order Statistics (pre-sorted data)

;; ### median-value

(bench/bench (stats/median-value sorted-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### median

(bench/bench (stats/median sorted-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### quartiles

(bench/bench (stats/quartiles sorted-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### quantile (0.5 - median)

(bench/bench (stats/quantile 0.5 sorted-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### quantile (0.95)

(bench/bench (stats/quantile 0.95 sorted-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Moments and Shape Statistics

;; ### central-moment (r=2)

(let [mu (stats/mean data-30)]
  (bench/bench (stats/central-moment data-30 mu 2)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### central-moment (r=3)

(let [mu (stats/mean data-30)]
  (bench/bench (stats/central-moment data-30 mu 3)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### central-moment (r=4)

(let [mu (stats/mean data-30)]
  (bench/bench (stats/central-moment data-30 mu 4)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### skewness (type 2, default)

(bench/bench (stats/skewness data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### kurtosis (type 2, default)

(bench/bench (stats/kurtosis data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### cv (coefficient of variation)

(bench/bench (stats/cv data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Summary
;;
;; Expected allocation behavior:
;; - min, max: garbage-free (fold-double with primitives)
;; - mean, sum, sum-of-squares: garbage-free (fold-based)
;; - variance: allocates small arrays for single-pass algorithm
;; - median-value: garbage-free (direct array access)
;; - median: allocates result vector [median nil nil]
;; - quartiles: allocates result vector [q1 median q3]
;; - quantile: garbage-free (direct array access)
;; - central-moment: garbage-free (fold-based)
;; - skewness, kurtosis: multiple central-moment calls
;; - cv: combines mean and variance calls
