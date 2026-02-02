(ns criterium.kde-benchmark
  "Benchmark notebook for KDE functions to identify allocation hotspots.

  Uses `:with-allocation-trace true` to capture both timing and allocation
  data for criterium.stats.kde functions."
  (:require
   [criterium.array :as arr]
   [criterium.bench :as bench]
   [criterium.stats.kde :as kde]))

(bench/set-default-viewer! :kindly)

;; # KDE Function Benchmarks
;;
;; This notebook benchmarks KDE functions to identify allocation hotspots
;; and verify which functions are garbage-free.

;; ## Test Data Helpers

(defn unimodal-data
  "Create unimodal test data (uniform distribution)."
  [n]
  (arr/->double-array (double-array (range n))))

(defn bimodal-data
  "Create bimodal test data with two clusters."
  [^long n]
  (let [half (quot n 2)]
    (arr/->double-array
     (double-array
      (concat (range half)
              (range (* 3 half) (* 4 half)))))))

(defn normal-like-data
  "Create roughly normal-distributed test data."
  [^long n]
  (let [center (/ (double n) 2.0)
        scale  (/ (double n) 4.0)]
    (arr/->double-array
     (double-array
      (for [_ (range n)]
        (+ center (* scale (- (double (rand)) 0.5))))))))

;; Sample data for benchmarks
(def ^:private data-20 (unimodal-data 20))
(def ^:private data-30 (unimodal-data 30))
(def ^:private bimodal-30 (bimodal-data 30))

;; Standard grid for KDE evaluation
(def ^:private grid-256
  (double-array (for [i (range 256)]
                  (/ (double i) 255.0))))

(def ^:private grid-512
  (double-array (for [i (range 512)]
                  (* 100.0 (/ (double i) 511.0)))))

;; ## Bandwidth Selection Functions

;; ### silverman-bandwidth

(bench/bench (kde/silverman-bandwidth data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### isj-bandwidth

(bench/bench (kde/isj-bandwidth data-30)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Core KDE Functions

;; ### linear-bin

(bench/bench (kde/linear-bin data-30 grid-256)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### dct-ii

(let [binned (kde/linear-bin data-30 grid-256)]
  (bench/bench (kde/dct-ii binned)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### gaussian-kde (uses gaussian-kde-fft internally)

(bench/bench (kde/gaussian-kde data-30 1.0 grid-256)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Mode Detection Functions

;; ### find-modes

(let [density (kde/gaussian-kde data-30 5.0 grid-512)]
  (bench/bench (kde/find-modes grid-512 density)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### count-modes

(bench/bench (kde/count-modes data-30 5.0 256)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Critical Bandwidth Functions

;; ### critical-bandwidth (single k)

(bench/bench (kde/critical-bandwidth data-30 1 {:n-points 256})
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### critical-bandwidths (multiple k values)

(bench/bench (kde/critical-bandwidths data-30 3 {:n-points 256})
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### locate-modes

(bench/bench (kde/locate-modes data-30 2 {:n-points 256})
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Excess Mass and Multimodality Tests

;; ### excess-mass

(bench/bench (kde/excess-mass data-30 1)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; excess-mass with k=2

(bench/bench (kde/excess-mass bimodal-30 2)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### silverman-test
;;
;; Note: This is slow due to bootstrap iterations. Using reduced n-bootstrap.

(bench/bench (kde/silverman-test data-20 1 {:n-bootstrap 20 :n-points 256})
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ### acr-test
;;
;; Note: This is slow due to bootstrap iterations. Using reduced n-bootstrap.

(bench/bench (kde/acr-test data-20 1 {:n-bootstrap 20 :n-points 256})
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## High-Level API

;; ### kde (full analysis)
;;
;; Note: Uses reduced bootstrap for speed.

(bench/bench (kde/kde data-30 {:n-points 256 :n-bootstrap 20})
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Internal Function Benchmarks
;;
;; These access private functions to benchmark performance-critical internals.

;; ### build-distance-matrix

(let [sorted-data (double-array
                   (sort (seq (.array ^criterium.array.DoubleArray data-30))))]
  (bench/bench (#'kde/build-distance-matrix sorted-data)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### compute-all-interval-distances

(let [sorted-data (double-array
                   (sort (seq (.array ^criterium.array.DoubleArray data-30))))
      dist-arr (#'kde/build-distance-matrix sorted-data)
      n (alength sorted-data)]
  (bench/bench (#'kde/compute-all-interval-distances dist-arr n 3)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### count-modes-with-grid

(let [[x-min x-max] (#'kde/data-min-max data-30)
      grid (#'kde/make-mode-counting-grid x-min x-max 256)]
  (bench/bench (#'kde/count-modes-with-grid data-30 5.0 grid)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### dct-via-fft

(let [binned (kde/linear-bin data-30 grid-256)]
  (bench/bench (#'kde/dct-via-fft binned)
               :with-allocation-trace true
               :collect-plan :one-shot))

;; ### gaussian-kde-fft

(bench/bench (#'kde/gaussian-kde-fft data-30 1.0 grid-256)
             :with-allocation-trace true
             :collect-plan :one-shot)

;; ## Summary
;;
;; Functions to watch for allocations:
;; - build-distance-matrix: O(n^2) array allocation
;; - compute-all-interval-distances: Multiple array allocations per k
;; - gaussian-kde-fft: FFT arrays
;; - critical-bandwidth: Grid allocation per binary search iteration
;; - bootstrap tests: Many iterations each allocating samples
