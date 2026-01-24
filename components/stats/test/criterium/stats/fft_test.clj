(ns criterium.stats.fft-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.stats.fft :as fft]
   [criterium.util.blackhole :as blackhole]))

;; Tests for radix-2 FFT implementation.
;; Contracts tested:
;; - next-power-of-2 returns correct powers of 2
;; - FFT produces correct frequency domain representation
;; - IFFT inverts FFT exactly (round-trip)
;; - FFT/IFFT are O(n log n) and zero-garbage during execution

(defn- approx=
  "Check if two doubles are approximately equal within tolerance."
  ([^double a ^double b] (approx= a b 1e-10))
  ([^double a ^double b ^double tol]
   (< (Math/abs (- a b)) tol)))

(defn- complex-approx=
  "Check if two interleaved complex arrays are approximately equal."
  ([^doubles a ^doubles b] (complex-approx= a b 1e-10))
  ([^doubles a ^doubles b ^double tol]
   (and (== (alength a) (alength b))
        (every? true?
                (map #(approx= %1 %2 tol)
                     (seq a) (seq b))))))

(deftest next-power-of-2-test
  (testing "next-power-of-2"
    (testing "returns 1 for n <= 1"
      (is (= 1 (fft/next-power-of-2 0)))
      (is (= 1 (fft/next-power-of-2 1)))
      (is (= 1 (fft/next-power-of-2 -5))))
    (testing "returns n when n is already a power of 2"
      (is (= 2 (fft/next-power-of-2 2)))
      (is (= 4 (fft/next-power-of-2 4)))
      (is (= 8 (fft/next-power-of-2 8)))
      (is (= 1024 (fft/next-power-of-2 1024))))
    (testing "returns next power of 2 for non-powers"
      (is (= 4 (fft/next-power-of-2 3)))
      (is (= 8 (fft/next-power-of-2 5)))
      (is (= 8 (fft/next-power-of-2 7)))
      (is (= 16 (fft/next-power-of-2 9)))
      (is (= 1024 (fft/next-power-of-2 513))))))

(deftest power-of-2-test
  (testing "power-of-2?"
    (testing "returns true for powers of 2"
      (is (fft/power-of-2? 1))
      (is (fft/power-of-2? 2))
      (is (fft/power-of-2? 4))
      (is (fft/power-of-2? 1024)))
    (testing "returns false for non-powers of 2"
      (is (not (fft/power-of-2? 0)))
      (is (not (fft/power-of-2? 3)))
      (is (not (fft/power-of-2? 5)))
      (is (not (fft/power-of-2? -4))))))

(deftest fft-impulse-test
  ;; DFT of unit impulse [1,0,0,0] is constant [1,1,1,1]
  (testing "fft!"
    (testing "transforms unit impulse to constant spectrum"
      (let [input (double-array [1.0 0.0  ; sample 0: 1+0i
                                 0.0 0.0  ; sample 1: 0+0i
                                 0.0 0.0  ; sample 2: 0+0i
                                 0.0 0.0]) ; sample 3: 0+0i
            result (fft/fft! input)
            expected (double-array [1.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0])]
        (is (complex-approx= result expected))))))

(deftest fft-constant-test
  ;; DFT of constant signal [1,1,1,1] has energy only at DC
  (testing "fft!"
    (testing "transforms constant signal to impulse at DC"
      (let [input (double-array [1.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0])
            result (fft/fft! input)
            ;; DC bin should be 4, all others 0
            expected (double-array [4.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])]
        (is (complex-approx= result expected))))))

(deftest fft-nyquist-test
  ;; DFT of alternating [1,-1,1,-1] has energy only at Nyquist
  (testing "fft!"
    (testing "transforms alternating signal to Nyquist impulse"
      (let [input (double-array [1.0 0.0 -1.0 0.0 1.0 0.0 -1.0 0.0])
            result (fft/fft! input)
            ;; Nyquist bin (n/2) should be 4, all others 0
            expected (double-array [0.0 0.0 0.0 0.0 4.0 0.0 0.0 0.0])]
        (is (complex-approx= result expected))))))

(deftest fft-sinusoid-test
  ;; DFT of cos(2πk/n) for k=1 should have peaks at bins 1 and n-1
  (testing "fft!"
    (testing "transforms cosine to symmetric frequency peaks"
      (let [n 8
            ;; cos(2π·1·i/8) for i=0..7
            input (double-array (mapcat (fn [^long i]
                                          [(Math/cos (* 2.0 Math/PI (double i) (/ 1.0 n)))
                                           0.0])
                                        (range n)))
            result (fft/fft! input)
            ;; Should have n/2=4 at bins 1 and 7 (conjugate pair)
            expected-mag-1 4.0
            expected-mag-7 4.0
            mag-1 (Math/sqrt (+ (* (aget result 2) (aget result 2))
                                (* (aget result 3) (aget result 3))))
            mag-7 (Math/sqrt (+ (* (aget result 14) (aget result 14))
                                (* (aget result 15) (aget result 15))))]
        (is (approx= mag-1 expected-mag-1 1e-9))
        (is (approx= mag-7 expected-mag-7 1e-9))))))

(deftest fft-ifft-roundtrip-test
  (testing "fft and ifft"
    (testing "are inverses (round-trip preserves signal)"
      (let [original (double-array [1.0 0.0 2.0 0.0 3.0 0.0 4.0 0.0
                                    5.0 0.0 6.0 0.0 7.0 0.0 8.0 0.0])
            orig-copy (java.util.Arrays/copyOf original (alength original))
            result (fft/ifft! (fft/fft! original))]
        (is (complex-approx= result orig-copy 1e-10))))))

(deftest fft-ifft-complex-roundtrip-test
  (testing "fft and ifft"
    (testing "preserve complex signals in round-trip"
      (let [original (double-array [1.0 0.5 2.0 -0.5 3.0 1.0 4.0 -1.0])
            orig-copy (java.util.Arrays/copyOf original (alength original))
            result (fft/ifft! (fft/fft! original))]
        (is (complex-approx= result orig-copy 1e-10))))))

(deftest fft-non-mutating-test
  (testing "fft (non-mutating)"
    (testing "does not modify input array"
      (let [original (double-array [1.0 0.0 2.0 0.0 3.0 0.0 4.0 0.0])
            orig-copy (java.util.Arrays/copyOf original (alength original))
            _ (fft/fft original)]
        (is (complex-approx= original orig-copy))))))

(deftest ifft-non-mutating-test
  (testing "ifft (non-mutating)"
    (testing "does not modify input array"
      (let [original (double-array [1.0 0.0 2.0 0.0 3.0 0.0 4.0 0.0])
            orig-copy (java.util.Arrays/copyOf original (alength original))
            _ (fft/ifft original)]
        (is (complex-approx= original orig-copy))))))

(deftest real->complex-test
  (testing "real->complex"
    (testing "converts real signal to interleaved complex"
      (let [real (double-array [1.0 2.0 3.0 4.0])
            complex (fft/real->complex real)
            expected (double-array [1.0 0.0 2.0 0.0 3.0 0.0 4.0 0.0])]
        (is (complex-approx= complex expected))))))

(deftest complex->real-test
  (testing "complex->real"
    (testing "extracts real parts from interleaved complex"
      (let [complex (double-array [1.0 0.5 2.0 -0.5 3.0 1.0 4.0 -1.0])
            real (fft/complex->real complex)
            expected (double-array [1.0 2.0 3.0 4.0])]
        (is (complex-approx= real expected))))))

(deftest complex-magnitude-test
  (testing "complex-magnitude"
    (testing "computes correct magnitudes"
      (let [complex (double-array [3.0 4.0  ; |3+4i| = 5
                                   1.0 0.0  ; |1+0i| = 1
                                   0.0 2.0  ; |0+2i| = 2
                                   1.0 1.0]) ; |1+i| = sqrt(2)
            mag (fft/complex-magnitude complex)
            expected (double-array [5.0 1.0 2.0 (Math/sqrt 2.0)])]
        (is (complex-approx= mag expected 1e-10))))))

(deftest zero-pad-real-test
  (testing "zero-pad-real"
    (testing "pads to next power of 2 by default"
      (let [input (double-array [1.0 2.0 3.0])
            padded (fft/zero-pad-real input)
            ;; Should pad to 4, giving 8 doubles (interleaved)
            expected (double-array [1.0 0.0 2.0 0.0 3.0 0.0 0.0 0.0])]
        (is (complex-approx= padded expected))))
    (testing "pads to specified power of 2"
      (let [input (double-array [1.0 2.0])
            padded (fft/zero-pad-real input 8)
            ;; 8 complex samples = 16 doubles
            expected (double-array [1.0 0.0 2.0 0.0
                                    0.0 0.0 0.0 0.0
                                    0.0 0.0 0.0 0.0
                                    0.0 0.0 0.0 0.0])]
        (is (complex-approx= padded expected))))))

(deftest zero-pad-for-autocorrelation-test
  (testing "zero-pad-for-autocorrelation"
    (testing "pads to 2*next-power-of-2"
      (let [input (double-array [1.0 2.0 3.0])
            padded (fft/zero-pad-for-autocorrelation input)]
        ;; n=3 -> next-pow-2=4 -> 2*4=8 complex samples = 16 doubles
        (is (= 16 (alength padded)))
        ;; First 3 values present
        (is (approx= 1.0 (aget padded 0)))
        (is (approx= 2.0 (aget padded 2)))
        (is (approx= 3.0 (aget padded 4)))
        ;; Rest are zeros
        (is (every? #(approx= 0.0 %) (drop 6 (seq padded))))))))

(deftest fft-larger-size-test
  ;; Test with larger FFT to ensure algorithm scales
  (testing "fft!"
    (testing "works correctly for larger sizes (256 samples)"
      (let [n 256
            ;; Create a signal: sum of two cosines
            input (double-array
                   (mapcat (fn [^long i]
                             [(+ (Math/cos (* 2.0 Math/PI 3.0 (double i) (/ 1.0 n)))
                                 (* 0.5 (Math/cos (* 2.0 Math/PI 10.0 (double i) (/ 1.0 n)))))
                              0.0])
                           (range n)))
            result (fft/fft! input)
            ;; Check peaks at bins 3, 10, 246 (n-10), 253 (n-3)
            mag-3 (Math/sqrt (+ (* (aget result 6) (aget result 6))
                                (* (aget result 7) (aget result 7))))
            mag-10 (Math/sqrt (+ (* (aget result 20) (aget result 20))
                                 (* (aget result 21) (aget result 21))))]
        ;; Magnitude at bin 3 should be n/2 = 128
        (is (approx= mag-3 128.0 1e-6))
        ;; Magnitude at bin 10 should be n/4 = 64 (amplitude 0.5)
        (is (approx= mag-10 64.0 1e-6))))))

;;; Allocation tracking helpers

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

(defn- count-freed
  "Count freed objects on current thread."
  ^long [allocs]
  (let [on-thread? (agent/allocation-on-thread?)]
    (reduce (fn ^long [^long acc alloc]
              (if (and (on-thread? alloc) (:freed alloc))
                (unchecked-inc acc)
                acc))
            0
            allocs)))

(defmacro assert-zero-allocation
  "Verifies the expression allocates nothing.
  For primitive operations that should never box."
  [expr]
  `(if (agent/attached?)
     (let [;; let JVM allocate function stat tracing objects
           jvm-once#         (agent/with-allocation-tracing ~expr)
           ;; verify zero allocations
           [allocs# result#] (agent/with-allocation-tracing ~expr)
           allocated#        (count-allocations allocs#)]
       (is (zero? allocated#)
           (str "Expected zero allocations, got " allocated# " " allocs#))
       (blackhole/consume jvm-once#)
       (blackhole/consume result#)
       nil)
     (is true)))

(defmacro assert-zero-garbage
  "Verifies the expression produces zero garbage (freed objects).
  For operations that allocate a result but no temporary objects."
  [expr]
  `(if (agent/attached?)
     (let [;; let JVM allocate function stat tracing objects
           jvm-once#         (agent/with-allocation-tracing ~expr)
           ;; verify zero freed
           [allocs# result#] (agent/with-allocation-tracing ~expr)
           freed#            (count-freed allocs#)]
       (is (zero? freed#)
           (str "Expected zero garbage, got "
                freed# " freed objects" " " allocs#))
       (blackhole/consume jvm-once#)
       (blackhole/consume result#)
       nil)
     (is true)))

;;; Zero-allocation tests for FFT

(deftest fft-zero-allocation-test
  ;; Verifies that fft! produces zero allocations during execution
  (testing "fft!"
    (testing "allocates nothing during in-place transform"
      (let [arr (double-array [1.0 0.0 2.0 0.0 3.0 0.0 4.0 0.0])]
        (assert-zero-allocation (fft/fft! arr))))))

(deftest ifft-zero-allocation-test
  ;; Verifies that ifft! produces zero allocations during execution
  (testing "ifft!"
    (testing "allocates nothing during in-place transform"
      (let [arr (double-array [1.0 0.0 2.0 0.0 3.0 0.0 4.0 0.0])]
        (assert-zero-allocation (fft/ifft! arr))))))
