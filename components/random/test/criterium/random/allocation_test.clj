(ns criterium.random.allocation-test
  ;; Tests that RNG operations avoid allocation during generation.
  ;; These tests verify the performance characteristics of the mutable
  ;; RNG implementations, separate from statistical correctness tests.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.random.well :as well]
   [criterium.random.ziggurat :as ziggurat]
   [criterium.util.blackhole :as blackhole]))

;;; Test Helpers

(defn- count-allocations
  "Count allocations on current thread."
  ^long [allocs]
  (let [on-thread? (agent/allocation-on-thread?)]
    (reduce (fn ^long [^long acc alloc]
              (if (on-thread? alloc)
                (unchecked-inc acc)
                acc))
            0
            allocs)))

(defmacro assert-zero-allocation
  "Verifies the expression allocates nothing.
  Includes seed in failure message for reproducibility."
  [seed expr]
  `(if (agent/attached?)
     (let [seed#           ~seed
           ;; let JVM allocate function stat tracing objects
           jvm-once#       (agent/with-allocation-tracing ~expr)
           ;; verify zero allocations
           [allocs# result#] (agent/with-allocation-tracing ~expr)
           allocated#      (count-allocations allocs#)]
       (is (zero? allocated#)
           (str "Expected zero allocations, got " allocated#
                " (seed: " seed# ") " allocs#))
       (blackhole/consume jvm-once#)
       (blackhole/consume result#)
       nil)
     (is true)))

;;; WellRng1024a zero-allocation tests

(deftest well-rng-next-double-zero-allocation-test
  (testing "next-double!"
    (testing "allocates nothing after warmup"
      (let [seed (System/nanoTime)
            rng  (well/make-well-rng-1024a seed)]
        ;; warmup - generate one value to ensure JIT compilation
        (well/next-double! rng)
        (assert-zero-allocation seed
                                (well/next-double! rng))))))

;;; NormalRng zero-allocation tests

(deftest normal-rng-next-gaussian-zero-allocation-test
  (testing "next-gaussian!"
    (testing "allocates nothing after warmup"
      (let [seed        (System/nanoTime)
            uniform-rng (well/make-well-rng-1024a seed)
            rng         (ziggurat/make-normal-rng uniform-rng)]
        ;; warmup - generate one value to ensure JIT compilation
        (ziggurat/next-gaussian! rng)
        (assert-zero-allocation seed
                                (ziggurat/next-gaussian! rng))))))
