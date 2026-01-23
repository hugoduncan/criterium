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
   [criterium.stats.fft :as fft]
   [criterium.utils.interface :as util]))

;;; Local helpers for double arrays
;;
;; These helpers work directly with primitive double arrays rather than using
;; criterium.stats.core, which requires ITypedArray. The ACF algorithm uses
;; raw double arrays throughout for FFT compatibility and to avoid typed array
;; overhead. Also, variance here is population variance (÷n) not sample (÷n-1).

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
        (util/report "WARN: ACF requires at least 20 samples, got %d\n" n)
        nil)

      :else
      (let [m (mean samples)
            v (variance samples m)]
        (if (< v 1e-15)
          (do
            (util/report "WARN: ACF undefined for zero variance data\n")
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

;;; Severity Classification

(defn noise-floor
  "Compute the noise floor threshold for ACF significance.
  For a sample of size n, values below 2/√n are indistinguishable from noise."
  ^double [^long n]
  (/ 2.0 (Math/sqrt (double n))))

(defn lag-1-severity
  "Classify lag-1 autocorrelation severity.

  For positive autocorrelation (consecutive samples correlated):
  - :none - |r₁| < max(0.10, 2/√n)
  - :minor - 0.10 ≤ |r₁| < 0.20
  - :moderate - 0.20 ≤ |r₁| < 0.35
  - :severe - |r₁| ≥ 0.35

  For negative autocorrelation (alternating pattern), uses :alternating-*
  variants. Negative autocorrelation doesn't reduce effective sample size
  the same way positive does, so these are informational rather than warnings.

  Returns keyword :none, :minor, :moderate, :severe, or :alternating-* variant."
  [^double r1 ^long n]
  (let [abs-r1 (Math/abs r1)
        floor (noise-floor n)
        base-severity (cond
                        (< abs-r1 (max 0.10 floor)) :none
                        (< abs-r1 0.20) :minor
                        (< abs-r1 0.35) :moderate
                        :else :severe)]
    (if (neg? r1)
      (case base-severity
        :none :alternating-none
        :minor :alternating-minor
        :moderate :alternating-moderate
        :severe :alternating-severe)
      base-severity)))

(defn lag-severity
  "Classify severity for lags other than lag-1.

  Thresholds (above noise floor 2/√n):
  - :none - |rₖ| < max(0.15, 2/√n)
  - :minor - 0.15 ≤ |rₖ| < 0.25
  - :moderate - 0.25 ≤ |rₖ| < 0.40
  - :severe - |rₖ| ≥ 0.40

  Returns keyword :none, :minor, :moderate, or :severe."
  [^double rk ^long n]
  (let [abs-rk (Math/abs rk)
        floor (noise-floor n)]
    (cond
      (< abs-rk (max 0.15 floor)) :none
      (< abs-rk 0.25) :minor
      (< abs-rk 0.40) :moderate
      :else :severe)))

(defn classify-lag-severities
  "Classify severity for all lags in an ACF map.

  Returns map of lag -> severity keyword."
  [acf-map ^long n]
  (reduce-kv
   (fn [result lag rk]
     (assoc result lag
            (if (= lag 1)
              (lag-1-severity rk n)
              (lag-severity rk n))))
   {}
   acf-map))

(defn detect-period
  "Detect periodic pattern by finding peak lag > 5 with max |rₖ|.

  Returns the lag of the peak if it exceeds threshold, nil otherwise."
  [acf-map ^long n]
  (let [floor (noise-floor n)
        threshold (max 0.15 floor)
        ;; Find lags > 5
        candidates (filter (fn [[lag _]] (> (long lag) 5)) acf-map)]
    (when (seq candidates)
      (let [[peak-lag peak-r] (apply max-key (fn [[_ r]] (Math/abs (double r))) candidates)]
        (when (> (Math/abs (double peak-r)) threshold)
          peak-lag)))))

(defn detect-pattern
  "Detect autocorrelation pattern from ACF values.

  Patterns:
  - :clean - all lags below 2/√n threshold
  - :alternating-pattern - r₁ < 0 (negative lag-1)
  - :severe - lag-1 at severe level
  - :drift - slow decay; lag-⌊n/10⌋ still above threshold
  - :warmup - lag-1 elevated AND r₁ > r₂ > r₃ (exponential decay)
  - :periodic - lag-1 clean but peak at k > 5 exceeds threshold

  Returns pattern keyword."
  [acf-map ^long n]
  (let [floor (noise-floor n)
        r1 (double (get acf-map 1 0.0))
        r2 (double (get acf-map 2 0.0))
        r3 (double (get acf-map 3 0.0))
        lag-1-sev (lag-1-severity r1 n)
        ;; Check if all lags below noise floor
        all-clean? (every? (fn [[_ r]] (< (Math/abs (double r)) floor))
                           acf-map)
        ;; Check for drift: lag at n/10 still elevated
        drift-lag (max 1 (quot n 10))
        r-drift (double (get acf-map drift-lag 0.0))
        drift-threshold (max 0.15 floor)
        is-drift? (and (> r-drift drift-threshold)
                       (not= lag-1-sev :none))]
    (cond
      ;; All lags below noise - clean
      all-clean?
      :clean

      ;; Negative lag-1 - alternating pattern
      (neg? r1)
      :alternating-pattern

      ;; Severe lag-1
      (= lag-1-sev :severe)
      :severe

      ;; Drift: slow decay, lag at n/10 still elevated (check before warmup)
      is-drift?
      :drift

      ;; Warmup: lag-1 elevated with exponential decay r1 > r2 > r3
      (and (not= lag-1-sev :none)
           (> r1 r2)
           (> r2 r3)
           (> r1 0)
           (> r2 0))
      :warmup

      ;; Periodic: lag-1 clean but peak at k > 5
      (and (= lag-1-sev :none)
           (some? (detect-period acf-map n)))
      :periodic

      ;; Default: use lag-1 severity to determine pattern
      (not= lag-1-sev :none)
      :warmup

      :else
      :clean)))

(def ^:private none-equivalent
  "Severities treated as :none for classification purposes."
  #{:none :alternating-none})

(def ^:private minor-equivalent
  "Severities treated as :minor or better for classification purposes."
  #{:none :minor :alternating-none :alternating-minor})

(defn classify-overall
  "Classify overall autocorrelation assessment.

  Classification:
  - :pass - all lags at :none AND Ljung-Box p > 0.10
  - :acceptable - lag-1 at :none or :minor AND no lag at :severe
  - :warning - any lag at :moderate OR Ljung-Box p ≤ 0.01
  - :fail - any lag at :severe OR n_eff < n/3

  Alternating severities (negative autocorrelation) are treated more leniently
  since they don't reduce effective sample size.

  Parameters:
    lag-severities - map of lag -> severity from classify-lag-severities
    ljung-box-result - result from ljung-box function
    n-eff - effective sample size
    n - original sample size

  Returns classification keyword."
  [lag-severities ljung-box-result ^long n-eff ^long n]
  (let [severities (set (vals lag-severities))
        lag-1-sev (get lag-severities 1 :none)
        p-value (double (get ljung-box-result :p-value 1.0))
        ;; Only positive :severe triggers failure (not alternating-severe)
        has-severe? (contains? severities :severe)
        has-moderate? (contains? severities :moderate)
        ;; Allow alternating-none to count as "none" for pass check
        all-none-equivalent? (every? none-equivalent severities)
        n-eff-ratio (/ (double n-eff) (double n))]
    (cond
      ;; Fail: any severe (positive) OR n_eff < n/3
      (or has-severe? (< n-eff-ratio (/ 1.0 3.0)))
      :fail

      ;; Warning: any moderate (positive) OR Ljung-Box p ≤ 0.01
      (or has-moderate? (<= p-value 0.01))
      :warning

      ;; Pass: all none/alternating-none AND Ljung-Box p > 0.10
      (and all-none-equivalent? (> p-value 0.10))
      :pass

      ;; Acceptable: lag-1 none/minor/alternating equiv AND no positive severe
      (and (minor-equivalent lag-1-sev) (not has-severe?))
      :acceptable

      :else
      :warning)))

(defn effective-sample-size-analysis
  "Compute effective sample size and CI inflation factor from ACF results.

  Takes ACF results (the :acf map) and original sample count.

  Returns map with:
    :effective-sample-size - {:n-original n :n-effective n_eff :ratio ratio}
    :ci-inflation-factor - inflation factor for CIs

  Returns nil if acf-map is nil."
  [acf-map ^long n]
  (when acf-map
    (let [r1 (double (get acf-map 1 0.0))
          n-eff (effective-sample-size r1 n)
          ci-factor (ci-inflation-factor r1)]
      {:effective-sample-size {:n-original n
                               :n-effective n-eff
                               :ratio (/ (double n-eff) (double n))}
       :ci-inflation-factor ci-factor})))

(defn autocorrelation-classification
  "Compute pattern detection and classification from ACF results.

  Takes ACF results (the :acf map) and original sample count.

  Returns map with:
    :ljung-box - {:q-statistic Q :df h :p-value p}
    :pattern - :clean, :warmup, :drift, :periodic, :severe, or :alternating-pattern
    :classification - :pass, :acceptable, :warning, or :fail
    :detected-period - Integer period for :periodic pattern, nil otherwise

  Returns nil if acf-map is nil."
  [acf-map ^long n]
  (when acf-map
    (let [r1 (double (get acf-map 1 0.0))
          lb (ljung-box acf-map n)
          n-eff (effective-sample-size r1 n)
          lag-sevs (classify-lag-severities acf-map n)
          pattern (detect-pattern acf-map n)
          classification (classify-overall lag-sevs lb n-eff n)
          detected-period (when (= pattern :periodic)
                            (detect-period acf-map n))]
      {:ljung-box lb
       :pattern pattern
       :classification classification
       :detected-period detected-period})))

(defn analyse-autocorrelation
  "Perform full autocorrelation analysis on samples.

  Returns map with:
    :acf - map of lag -> autocorrelation coefficient
    :lag-1 - {:value r₁ :severity <keyword>}
    :effective-sample-size - {:n-original n :n-effective n_eff :ratio ratio}
    :ci-inflation-factor - inflation factor for CIs
    :ljung-box - {:q-statistic Q :df h :p-value p}
    :pattern - detected pattern keyword
    :classification - overall assessment keyword
    :detected-period - period if periodic pattern, nil otherwise

  Returns nil if samples are insufficient (n < 20) or have zero variance."
  [^doubles samples]
  (let [n (alength samples)]
    (when-let [acf-map (acf samples)]
      (let [r1 (double (get acf-map 1 0.0))
            lag-sevs (classify-lag-severities acf-map n)
            ess-analysis (effective-sample-size-analysis acf-map n)
            class-analysis (autocorrelation-classification acf-map n)]
        (merge
         {:acf acf-map
          :lag-1 {:value r1
                  :severity (get lag-sevs 1 :none)}}
         ess-analysis
         class-analysis)))))
