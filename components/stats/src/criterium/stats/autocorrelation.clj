(ns criterium.stats.autocorrelation
  "Autocorrelation function (ACF) computation and related statistics.

  Provides FFT-based ACF computation for detecting sample non-independence
  in benchmark results, along with derived statistics for quantifying the
  impact on statistical reliability.

  Main functions:
  - `acf` - Compute autocorrelation coefficients for all lags
  - `ljung-box` - Ljung-Box Q statistic and p-value for independence testing
  - `effective-sample-size` - Adjusted sample size accounting for autocorrelation
  - `ci-inflation-factor` - Factor to widen confidence intervals"
  (:require
   [criterium.stats.chi-squared :as chi-squared]
   [criterium.stats.fft :as fft]))

(defn- mean
  "Compute arithmetic mean of double array."
  ^double [^doubles arr]
  (let [n (alength arr)]
    (loop [i 0
           sum 0.0]
      (if (< i n)
        (recur (unchecked-inc i) (+ sum (aget arr i)))
        (/ sum (double n))))))

(defn- variance
  "Compute population variance of double array given mean."
  ^double [^doubles arr ^double m]
  (let [n (alength arr)]
    (loop [i 0
           sum-sq 0.0]
      (if (< i n)
        (let [diff (- (aget arr i) m)]
          (recur (unchecked-inc i) (+ sum-sq (* diff diff))))
        (/ sum-sq (double n))))))

(defn acf
  "Compute autocorrelation function using FFT.

  Algorithm:
  1. Center samples (subtract mean)
  2. Zero-pad to 2*next-power-of-2(n) for circular -> linear correlation
  3. FFT, compute power spectrum (multiply by conjugate)
  4. IFFT to get autocorrelation
  5. Normalize by r₀ (variance * n)

  Returns map with ACF values for lags 1 to floor(n/2): {1 r1, 2 r2, ...}

  Edge cases:
  - n < 20: logs warning, returns nil
  - Zero variance: logs warning, returns nil"
  [^doubles samples]
  (let [n (alength samples)]
    (cond
      (< n 20)
      (do
        (println "WARN: ACF requires at least 20 samples, got" n)
        nil)

      :else
      (let [m (mean samples)
            v (variance samples m)]
        (if (< v 1e-15)
          (do
            (println "WARN: ACF undefined for zero variance data")
            nil)
          (let [;; Center the samples
                ^doubles centered (double-array n)
                _ (dotimes [i n]
                    (aset centered i (- (aget samples i) m)))
                ;; Zero-pad for linear correlation via circular
                ^doubles padded (fft/zero-pad-for-autocorrelation centered)
                padded-n (quot (alength padded) 2)
                ;; Forward FFT
                _ (fft/fft! padded)
                ;; Compute power spectrum |X(f)|^2 in-place
                ;; For complex z = a + bi, |z|^2 = a^2 + b^2
                _ (dotimes [i padded-n]
                    (let [idx (* 2 i)
                          re (aget padded idx)
                          im (aget padded (unchecked-inc idx))
                          power (+ (* re re) (* im im))]
                      (aset padded idx power)
                      (aset padded (unchecked-inc idx) 0.0)))
                ;; Inverse FFT to get autocorrelation
                _ (fft/ifft! padded)
                ;; r0 is the variance * n (at lag 0)
                r0 (aget padded 0)
                ;; Build result map for lags 1 to n/2
                max-lag (quot n 2)]
            (loop [lag 1
                   result (transient {})]
              (if (> lag max-lag)
                (persistent! result)
                ;; ACF at lag k is real part at index k, normalized by r0
                (let [rk (/ (aget padded (* 2 lag)) r0)]
                  (recur (unchecked-inc lag)
                         (assoc! result lag rk)))))))))))

(defn ljung-box
  "Compute Ljung-Box Q statistic for testing autocorrelation.

  Q = n(n+2) * sum_k=1^h (rk^2/(n-k))

  where h = min(20, floor(n/4))

  Returns map with:
    :q-statistic - the Q value
    :df - degrees of freedom (h)
    :p-value - 1 - chi-squared-cdf(Q, h)

  Returns nil if acf-map is nil.

  Parameters:
    acf-map - map of lag -> autocorrelation from `acf` function
    n - original sample size"
  [acf-map ^long n]
  (when acf-map
    (let [h (min 20 (quot n 4))
          ;; Q = n(n+2) * sum(rk^2/(n-k))
          n+2 (long (+ n 2))
          sum (loop [k 1
                     acc 0.0]
                (if (> k h)
                  acc
                  (let [rk (double (get acf-map k 0.0))]
                    (recur (unchecked-inc k)
                           (+ acc (/ (* rk rk) (double (- n k))))))))
          coef (* (double n) (double n+2))
          q (* coef (double sum))
          p-value (- 1.0 (chi-squared/cdf q h))]
      {:q-statistic q
       :df h
       :p-value p-value})))

(defn effective-sample-size
  "Compute effective sample size accounting for lag-1 autocorrelation.

  n_eff = n * (1 - r1) / (1 + r1)

  Clamped to [1, n]. If r1 <= 0, returns n (negative autocorrelation
  doesn't reduce effective sample size in the same way).

  Parameters:
    r1 - lag-1 autocorrelation coefficient
    n - original sample size

  Returns effective sample size as a long."
  ^long [^double r1 ^long n]
  (if (<= r1 0.0)
    n
    (let [n-eff (* (double n) (/ (- 1.0 r1) (+ 1.0 r1)))]
      (max 1 (min n (long (Math/round n-eff)))))))

(defn ci-inflation-factor
  "Compute confidence interval inflation factor due to autocorrelation.

  CI_inflation = sqrt((1 + r1) / (1 - r1))

  Minimum value is 1.0. Capped at 6.0 if r1 >= 0.95.

  Parameters:
    r1 - lag-1 autocorrelation coefficient

  Returns the inflation factor as a double."
  ^double [^double r1]
  (cond
    (<= r1 0.0) 1.0
    (>= r1 0.95) 6.0
    :else (Math/sqrt (/ (+ 1.0 r1) (- 1.0 r1)))))
