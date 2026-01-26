(ns criterium.stats.fft
  "Pure Clojure radix-2 Cooley-Tukey FFT implementation.

  Provides O(n log n) Fast Fourier Transform for autocorrelation computation.
  Uses interleaved complex representation [re0 im0 re1 im1 ...] for cache
  efficiency. All operations use primitive double arrays with zero garbage
  allocation during transform execution.

  Main functions:
  - `fft!` / `fft` - Forward FFT (in-place / copying)
  - `ifft!` / `ifft` - Inverse FFT (in-place / copying)
  - `next-power-of-2` - Find smallest power of 2 >= n
  - `zero-pad-real` - Zero-pad real signal to power-of-2 length

  Complex arrays use interleaved format: [re0 im0 re1 im1 ...]
  Array length is 2*n where n is the number of complex samples.")

;;; Utility functions

(defn next-power-of-2
  "Returns the smallest power of 2 greater than or equal to n.
  For n <= 0, returns 1."
  ^long [^long n]
  (if (<= n 1)
    1
    (Long/highestOneBit (dec (bit-shift-left n 1)))))

(defn power-of-2?
  "Returns true if n is a positive power of 2."
  [^long n]
  (and (pos? n)
       (zero? (bit-and n (dec n)))))

;;; Bit-reversal permutation

(defn- bit-reverse
  "Reverses the lower `bits` bits of n."
  ^long [^long n ^long bits]
  (loop [n n
         result 0
         i bits]
    (if (zero? i)
      result
      (recur (bit-shift-right n 1)
             (bit-or (bit-shift-left result 1) (bit-and n 1))
             (dec i)))))

(defn- bit-reversal-permute!
  "In-place bit-reversal permutation of interleaved complex array.
  The array has n complex samples (2*n doubles)."
  [^doubles arr ^long n]
  (let [bits (Long/numberOfTrailingZeros n)]
    (dotimes [i n]
      (let [j (bit-reverse i bits)]
        (when (< i j)
          (let [i2 (bit-shift-left i 1)
                j2 (bit-shift-left j 1)
                ;; Swap complex values
                tmp-re (aget arr i2)
                tmp-im (aget arr (unchecked-inc i2))]
            (aset arr i2 (aget arr j2))
            (aset arr (unchecked-inc i2) (aget arr (unchecked-inc j2)))
            (aset arr j2 tmp-re)
            (aset arr (unchecked-inc j2) tmp-im))))))
  arr)

;;; Core FFT implementation

(defn- fft-butterfly!
  "Perform in-place Cooley-Tukey radix-2 DIT FFT.
  direction: 1.0 for forward FFT, -1.0 for inverse FFT.
  The array contains n complex samples in interleaved format."
  [^doubles arr ^long n ^double direction]
  ;; Bit-reversal permutation
  (bit-reversal-permute! arr n)
  ;; Cooley-Tukey iterative FFT
  (let [log-n (Long/numberOfTrailingZeros n)]
    (loop [s 1]
      (when (<= s log-n)
        (let [m (bit-shift-left 1 s)
              m2 (bit-shift-right m 1)
              ;; Twiddle factor angle increment
              theta (* direction (/ (* 2.0 Math/PI) (double m)))
              ;; Principal root of unity
              wm-re (Math/cos theta)
              wm-im (Math/sin theta)]
          ;; Process all groups of size m
          (loop [k 0]
            (when (< k n)
              ;; Process one butterfly group
              (loop [j 0
                     w-re 1.0
                     w-im 0.0]
                (when (< j m2)
                  (let [;; Indices into interleaved array
                        idx1 (bit-shift-left (+ k j) 1)
                        idx2 (bit-shift-left (+ k j m2) 1)
                        ;; Load values
                        t-re (aget arr idx1)
                        t-im (aget arr (unchecked-inc idx1))
                        u-re (aget arr idx2)
                        u-im (aget arr (unchecked-inc idx2))
                        ;; Complex multiply: w * u
                        wu-re (- (* w-re u-re) (* w-im u-im))
                        wu-im (+ (* w-re u-im) (* w-im u-re))]
                    ;; Butterfly
                    (aset arr idx1 (+ t-re wu-re))
                    (aset arr (unchecked-inc idx1) (+ t-im wu-im))
                    (aset arr idx2 (- t-re wu-re))
                    (aset arr (unchecked-inc idx2) (- t-im wu-im))
                    ;; Update twiddle factor: w *= wm
                    (recur (unchecked-inc j)
                           (- (* w-re wm-re) (* w-im wm-im))
                           (+ (* w-re wm-im) (* w-im wm-re))))))
              (recur (+ k m)))))
        (recur (unchecked-inc s)))))
  arr)

;;; Public API

(defn fft!
  "In-place forward FFT on interleaved complex array.
  The array must have length 2*n where n is a power of 2.
  Modifies arr in place and returns it.

  Input/output format: [re0 im0 re1 im1 ... re_{n-1} im_{n-1}]

  Uses standard DFT sign convention: X[k] = Σ x[n] * e^{-i 2π k n / N}"
  ^doubles [^doubles arr]
  (let [len (alength arr)
        n (bit-shift-right len 1)]
    (assert (power-of-2? n) "FFT length must be a power of 2")
    (fft-butterfly! arr n -1.0)))

(defn ifft!
  "In-place inverse FFT on interleaved complex array.
  The array must have length 2*n where n is a power of 2.
  Modifies arr in place and returns it.
  Result is scaled by 1/n.

  Input/output format: [re0 im0 re1 im1 ... re_{n-1} im_{n-1}]

  Uses standard IDFT sign convention: x[n] = (1/N) Σ X[k] * e^{+i 2π k n / N}"
  ^doubles [^doubles arr]
  (let [len (alength arr)
        n (bit-shift-right len 1)
        scale (/ 1.0 (double n))]
    (assert (power-of-2? n) "FFT length must be a power of 2")
    (fft-butterfly! arr n 1.0)
    ;; Scale by 1/n
    (dotimes [i len]
      (aset arr i (* scale (aget arr i))))
    arr))

(defn fft
  "Forward FFT on interleaved complex array.
  Returns a new array with the FFT result; input is not modified.
  The array must have length 2*n where n is a power of 2.

  Input/output format: [re0 im0 re1 im1 ... re_{n-1} im_{n-1}]"
  ^doubles [^doubles arr]
  (let [copy (java.util.Arrays/copyOf arr (alength arr))]
    (fft! copy)))

(defn ifft
  "Inverse FFT on interleaved complex array.
  Returns a new array with the IFFT result; input is not modified.
  The array must have length 2*n where n is a power of 2.
  Result is scaled by 1/n.

  Input/output format: [re0 im0 re1 im1 ... re_{n-1} im_{n-1}]"
  ^doubles [^doubles arr]
  (let [copy (java.util.Arrays/copyOf arr (alength arr))]
    (ifft! copy)))

;;; Convenience functions for real signals

(defn real->complex
  "Converts a real signal to interleaved complex format.
  Imaginary parts are set to zero.
  Output length is 2 * (length of input)."
  ^doubles [^doubles real-arr]
  (let [n (alength real-arr)
        ^doubles complex (double-array (* 2 n))]
    (dotimes [i n]
      (aset complex (bit-shift-left i 1) (aget real-arr i)))
    complex))

(defn complex->real
  "Extracts real parts from interleaved complex array.
  Output length is half the input length."
  ^doubles [^doubles complex-arr]
  (let [n (bit-shift-right (alength complex-arr) 1)
        ^doubles real (double-array n)]
    (dotimes [i n]
      (aset real i (aget complex-arr (bit-shift-left i 1))))
    real))

(defn complex-magnitude
  "Computes magnitude of each complex sample.
  Output length is half the input length."
  ^doubles [^doubles complex-arr]
  (let [n (bit-shift-right (alength complex-arr) 1)
        ^doubles mag (double-array n)]
    (dotimes [i n]
      (let [idx (bit-shift-left i 1)
            re (aget complex-arr idx)
            im (aget complex-arr (unchecked-inc idx))]
        (aset mag i (Math/sqrt (+ (* re re) (* im im))))))
    mag))

(defn zero-pad-real
  "Zero-pads a real signal to the specified power-of-2 length.
  Returns interleaved complex array with zero imaginary parts.
  If target-n is not specified, pads to next power of 2 >= (length input).
  If target-n is specified, it must be a power of 2 >= (length input)."
  (^doubles [^doubles real-arr]
   (let [n (alength real-arr)
         target-n (next-power-of-2 n)]
     (zero-pad-real real-arr target-n)))
  (^doubles [^doubles real-arr ^long target-n]
   (let [n (alength real-arr)]
     (assert (power-of-2? target-n) "target-n must be a power of 2")
     (assert (>= target-n n) "target-n must be >= input length")
     (let [^doubles complex (double-array (* 2 target-n))]
       (dotimes [i n]
         (aset complex (bit-shift-left i 1) (aget real-arr i)))
       complex))))

(defn zero-pad-for-autocorrelation
  "Zero-pads a real signal to 2*next-power-of-2(n) for autocorrelation.
  This provides sufficient padding to compute circular correlation
  equivalent to linear correlation."
  ^doubles [^doubles real-arr]
  (let [n (alength real-arr)
        padded-n (* 2 (next-power-of-2 n))]
    (zero-pad-real real-arr padded-n)))
