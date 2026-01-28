(ns benchmark-problem-detection
  "Detecting and diagnosing benchmark problems using criterium's default analysis."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.notebook.helpers :refer [bench-display]]))

;; # Benchmark Problem Detection
;;
;; Benchmarks can produce misleading results when underlying assumptions are
;; violated. Criterium's `default` bench plan automatically checks for common
;; problems and warns you when results may be unreliable.
;;
;; This notebook covers:
;;
;; - **Multimodal detection** - Multiple distinct execution time modes
;; - **Autocorrelation analysis** - Non-independent samples
;; - **Effective sample size** - How autocorrelation affects confidence intervals
;;
;; Understanding these diagnostics helps you identify and fix benchmark issues
;; before drawing conclusions from potentially flawed data.

;; ## Why Problem Detection Matters
;;
;; Standard statistical analysis assumes:
;;
;; 1. **Unimodal distribution** - One "typical" execution time
;; 2. **Independent samples** - Each measurement is unrelated to the previous
;;
;; When these assumptions fail, mean and confidence intervals can be misleading.
;; A benchmark might report 100 ns ± 5 ns when it actually alternates between
;; 80 ns and 150 ns execution paths.

;; ## A Clean Benchmark
;;
;; First, let's see what healthy benchmark output looks like:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))))

;; A clean benchmark shows:
;;
;; - No multimodal warning (single execution mode)
;; - Autocorrelation classification: **Pass** or **Acceptable**
;; - Effective sample size close to actual sample count
;;
;; When these diagnostics pass, you can trust the reported statistics.

;; ## Multimodal Detection
;;
;; A multimodal distribution has multiple distinct peaks - the code sometimes
;; executes fast and sometimes slow, with no middle ground.

;; ### What Multimodality Indicates
;;
;; Multiple modes typically signal:
;;
;; - **JIT compilation states** - Code switching between interpreted and compiled
;; - **Branch prediction** - Different paths taken based on input
;; - **CPU frequency scaling** - Processor throttling during measurement
;; - **GC interference** - Some samples include garbage collection
;; - **Cache effects** - Hot vs cold cache access patterns

;; ### Example: Triggering Multimodal Detection
;;
;; This benchmark deliberately creates two execution paths:

(defn variable-work
  "Simulate variable-time work with occasional slow paths."
  [^long n slow?]
  (if slow?
    (reduce + (range (* n 3)))  ; slow path - 3x more work
    (reduce + (range n))))      ; fast path

;; When benchmarked with random path selection, it produces a bimodal
;; distribution:

^:kindly/hide-code
(bench-display
 (bench/bench
  (variable-work 100 (zero? (mod (rand-int 100) 3)))
  :bench-plan bench-plans/histogram
  :viewer :kindly))

;; The histogram shows two distinct peaks, and criterium reports:
;;
;; - **Mode count > 1** - Statistical validation of multiple peaks
;; - **Mode locations** - Where each peak occurs with confidence intervals

;; ### Interpreting Mode Detection
;;
;; Criterium uses the ACR test (Ameijeiras-Alonso, Crujeiras, Rodríguez-Casal)
;; to validate whether apparent peaks are statistically significant or just
;; noise. The test works by computing "excess mass" and comparing against
;; bootstrap samples.
;;
;; When n-modes > 1 is reported:
;;
;; 1. The mean is **not representative** - it falls between modes
;; 2. Confidence intervals are **misleading** - they assume unimodality
;; 3. You should investigate **why** multiple modes exist

;; ### Fixing Multimodal Benchmarks
;;
;; | Cause | Solution |
;; |-------|----------|
;; | JIT warmup incomplete | Increase `:limit-time-s` for more warmup |
;; | Input-dependent paths | Use fixed inputs or separate benchmarks per path |
;; | GC interference | Increase heap, reduce allocation, or accept variability |
;; | Deoptimization cycles | Simplify code or investigate type instability |
;;
;; See [Warmup](./warmup.html) for JIT-related issues and
;; [Non-Parametric Analysis](./non_parametric_analysis.html) for deeper
;; mode detection coverage.

;; ## Autocorrelation Analysis
;;
;; Autocorrelation measures whether consecutive samples are related. In a
;; healthy benchmark, each sample is independent - knowing one sample tells
;; you nothing about the next.

;; ### What Autocorrelation Indicates
;;
;; Positive lag-1 autocorrelation (consecutive samples correlated) suggests:
;;
;; - **GC cycles** - Samples after GC are consistently faster
;; - **CPU throttling** - Sustained work triggers frequency changes
;; - **JIT recompilation** - Code quality changes during measurement
;; - **Cache warming** - Early samples consistently slower than late ones
;;
;; Negative lag-1 (alternating pattern) is less common and usually indicates
;; measurement artifacts.

;; ### Example: Triggering Autocorrelation
;;
;; This function switches between fast and slow modes every 80-200 calls,
;; creating runs of similar-timed samples:

(defn make-stateful-work
  "Returns a function that periodically switches between fast/slow modes.
  Creates temporal correlation by maintaining the same mode for many calls."
  [^long n]
  (let [state (atom {:mode :fast :counter 0 :switch-at (+ 80 (rand-int 120))})]
    (fn []
      (let [{:keys [mode]} @state]
        (swap! state (fn [{:keys [mode counter switch-at] :as s}]
                       (let [c (inc counter)]
                         (if (>= c switch-at)
                           {:mode (if (= mode :fast) :slow :fast)
                            :counter 0
                            :switch-at (+ 80 (rand-int 120))}
                           (assoc s :counter c)))))
        (if (= mode :fast)
          (reduce + (range n))
          (reduce + (range (* n 2))))))))

(def stateful-work (make-stateful-work 50))

^:kindly/hide-code
(bench-display
 (bench/bench (stateful-work)))

;; The autocorrelation classification may show:
;;
;; - **Pattern**: transient-effects, drift, or periodic
;; - **Classification**: warning or fail
;; - **ACF plot**: Lag coefficients exceeding the noise floor

;; ### Pattern Classification
;;
;; Criterium classifies autocorrelation patterns:
;;
;; | Pattern | Description | Typical Cause |
;; |---------|-------------|---------------|
;; | :clean | All lags below noise floor | Healthy benchmark |
;; | :transient-effects | Elevated lag-1 with decay | GC, JIT warmup residual |
;; | :drift | Slow decay, still elevated at lag n/10 | Progressive warming/cooling |
;; | :periodic | Peak at lag > 5 | Regular interference (GC cycles, timers) |
;; | :severe | Very high lag-1 (≥0.35) | Serious measurement problem |
;; | :alternating-* | Negative lag-1 | Measurement oscillation |

;; ### The Ljung-Box Test
;;
;; The Ljung-Box Q statistic tests whether the autocorrelation pattern differs
;; significantly from white noise. A low p-value (< 0.01) indicates the samples
;; are not independent.
;;
;; Criterium reports:
;;
;; - **Q-statistic** - Test statistic value
;; - **p-value** - Probability of seeing this pattern by chance
;; - **Classification** - pass, acceptable, warning, or fail

;; ### Reading ACF Plots
;;
;; The ACF (Autocorrelation Function) plot shows correlation at each lag:
;;
;; - **Lag 1** - Correlation between consecutive samples (most important)
;; - **Higher lags** - Correlation between samples k apart
;; - **Noise floor** - 2/√n threshold; values below are indistinguishable from noise
;;
;; Look for:
;;
;; - Bars exceeding the noise floor lines
;; - Patterns (decay, periodic spikes)
;; - High lag-1 value

;; ### Severity Levels
;;
;; | Severity | Lag-1 threshold | Impact |
;; |----------|-----------------|--------|
;; | :none | < max(0.10, 2/√n) | No concern |
;; | :minor | 0.10 - 0.20 | Slight CI widening |
;; | :moderate | 0.20 - 0.35 | CI may be underestimated |
;; | :severe | ≥ 0.35 | Results unreliable |

;; ### Fixing Autocorrelation Issues
;;
;; | Cause | Solution |
;; |-------|----------|
;; | GC interference | Increase heap size, reduce allocation rate |
;; | CPU throttling | Ensure consistent CPU governor, reduce thermal load |
;; | JIT activity | Increase warmup time |
;; | External processes | Run on quiet system, disable unnecessary services |

;; ## Effective Sample Size (ESS)
;;
;; When samples are autocorrelated, you have less information than the raw
;; sample count suggests. Effective sample size quantifies this reduction.

;; ### What ESS Means
;;
;; ESS answers: "How many *independent* samples would give equivalent
;; statistical power?"
;;
;; For lag-1 autocorrelation r₁:
;;
;; ```
;; n_eff = n × (1 - r₁) / (1 + r₁)
;; ```
;;
;; With r₁ = 0.3, 200 samples have an effective size of only ~108.

;; ### CI Inflation Factor
;;
;; Autocorrelation makes confidence intervals too narrow. The CI inflation
;; factor tells you how much wider they should be:
;;
;; ```
;; CI_inflation = √((1 + r₁) / (1 - r₁))
;; ```
;;
;; | Lag-1 (r₁) | ESS Ratio | CI Inflation |
;; |------------|-----------|--------------|
;; | 0.0 | 100% | 1.0× |
;; | 0.1 | 82% | 1.1× |
;; | 0.2 | 67% | 1.2× |
;; | 0.3 | 54% | 1.4× |
;; | 0.5 | 33% | 1.7× |

;; ### When ESS Matters
;;
;; - **ESS > n/2**: Minor concern, results usable with caution
;; - **n/3 < ESS < n/2**: Moderate concern, investigate cause
;; - **ESS < n/3**: Serious concern, results may be unreliable
;;
;; Criterium's bootstrap confidence intervals account for ESS automatically
;; when computing intervals for mean and other statistics.

;; ## The Default Bench Plan Diagnostics
;;
;; The `default` bench plan runs all these diagnostics automatically:
;;
;; | Analysis | View |
;; |----------|------|
;; | Mode detection (KDE + ACR) | Multimodal warning when n-modes > 1 |
;; | Autocorrelation (raw samples) | Pattern classification |
;; | Autocorrelation (filtered) | Effective sample size |
;; | ESS computation | CI inflation factor |
;;
;; For more detailed distribution analysis, use the `histogram` bench plan
;; which adds histogram visualization and mode location reporting.

;; ## Summary: Healthy Benchmark Checklist
;;
;; A benchmark with reliable results shows:
;;
;; - [ ] No multimodal warning (or understood and acceptable)
;; - [ ] Autocorrelation classification: **pass** or **acceptable**
;; - [ ] Ljung-Box p-value > 0.01
;; - [ ] Effective sample size > n/2
;; - [ ] ACF plot shows no bars exceeding noise floor
;;
;; If diagnostics fail:
;;
;; 1. **Identify the cause** - Use pattern classification and ACF plot
;; 2. **Address the root issue** - Don't just ignore warnings
;; 3. **Re-run and verify** - Confirm fixes improved the situation
;;
;; For deeper investigation, see:
;;
;; - [Warmup](./warmup.html) - JIT compilation and warmup controls
;; - [Non-Parametric Analysis](./non_parametric_analysis.html) - Histogram, KDE, mode detection
;; - [Repeatability](./repeatability.html) - Understanding and improving consistency
