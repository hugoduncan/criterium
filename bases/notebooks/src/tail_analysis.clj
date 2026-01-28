(ns tail-analysis
  "Extreme value analysis for understanding worst-case latency."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.notebook.helpers :refer [bench-display]]))

;; # Tail Analysis
;;
;; Tail analysis examines the extreme values (tail) of latency distributions
;; to understand worst-case performance. While typical statistics focus on
;; central tendency (mean, median), tail analysis targets the slowest
;; executions - the 99th, 99.9th, or even 99.99th percentiles.
;;
;; **When to use tail analysis:**
;;
;; - **SLA compliance** - Guaranteeing p99 or p999 latency bounds
;; - **Capacity planning** - Understanding how systems behave at extremes
;; - **Performance regression** - Detecting tail latency degradation
;; - **Latency-sensitive systems** - Real-time, trading, gaming applications
;;
;; Unlike mean or median statistics, tail analysis focuses on rare but
;; impactful events - the slowest 1% or 0.1% of executions that can
;; determine user experience and SLA violations.

;; ## The tail-analysis Bench Plan
;;
;; Use the `tail-analysis` bench plan for extreme value analysis:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan bench-plans/tail-analysis))

;; The output includes:
;;
;; - **Tail ratios** - How much worse extreme percentiles are vs typical
;; - **Hill estimator** - Tail index measuring how heavy the tail is
;; - **GPD fit** - Generalized Pareto Distribution for extrapolation
;; - **High quantile estimates** - p99, p999, p9999 via GPD
;; - **Visualizations** - Zipf plot, MRL plot, Q-Q plots

;; ## Extreme Value Theory Background
;;
;; Extreme Value Theory (EVT) provides the mathematical foundation for
;; analyzing distribution tails. Two main approaches exist:

;; ### Block Maxima vs Peaks Over Threshold
;;
;; **Block Maxima**: Divide data into blocks, take the maximum from each.
;; The distribution of these maxima follows a Generalized Extreme Value
;; (GEV) distribution. Requires many blocks of data.
;;
;; **Peaks Over Threshold (POT)**: Analyze all values exceeding a high
;; threshold. Exceedances follow a Generalized Pareto Distribution (GPD).
;; More data-efficient - criterium uses this approach.
;;
;; POT is preferred for benchmark analysis because:
;;
;; - Uses all extreme observations, not just block maxima
;; - Works well with the sample sizes from typical benchmarks
;; - Directly models the tail behavior we care about

;; ### The Generalized Pareto Distribution
;;
;; GPD models the distribution of exceedances over a threshold. Its key
;; parameters:
;;
;; | Parameter | Symbol | Interpretation |
;; |-----------|--------|----------------|
;; | Shape | xi (ξ) | Tail heaviness: ξ>0 heavy, ξ≈0 exponential, ξ<0 bounded |
;; | Scale | sigma (σ) | Spread of exceedances above threshold |
;;
;; The shape parameter ξ determines tail behavior:
;;
;; - **ξ > 0**: Heavy (Pareto-like) tail - extreme values possible
;; - **ξ ≈ 0**: Exponential tail - moderate extremes
;; - **ξ < 0**: Light tail with finite upper bound

;; ### Why Raw Samples (No Outlier Filtering)
;;
;; Tail analysis deliberately uses raw samples WITHOUT outlier filtering.
;; This differs from other bench plans (like `distribution-analysis`) because:
;;
;; 1. **Extremes ARE the data** - We're specifically studying outliers
;; 2. **Filtering removes signal** - Those "outliers" are exactly what
;;    determines p99 and p999 behavior
;; 3. **EVT methods expect extremes** - GPD and Hill estimator are designed
;;    for extreme value data
;;
;; If you're concerned about measurement artifacts (not real performance
;; variation), address them at the collection stage through warmup and
;; environment isolation, not by filtering after the fact.

;; ## Sample Size Requirements
;;
;; Tail analysis requires more samples than typical benchmarking:
;;
;; | Quantile | Min Samples | Recommended |
;; |----------|-------------|-------------|
;; | p95 | 50 | 200+ |
;; | p99 | 200 | 500+ |
;; | p999 | 1000 | 2000+ |
;; | p9999 | 5000 | 10000+ |
;;
;; **Rule of thumb**: To reliably estimate the p-th percentile, you need
;; at least 1/(1-p) observations in the tail region. For p999, that's
;; 1000 samples minimum.
;;
;; To increase sample count, extend benchmark time:

(comment
  ;; Collect more samples for better tail estimates
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis
               :limit-time-s 30))

;; ## Tail Ratios
;;
;; Tail ratios indicate how heavy the distribution tail is by comparing
;; extreme percentiles:
;;
;; | Ratio | Meaning |
;; |-------|---------|
;; | p99/p95 | How much worse is the worst 1% vs worst 5% |
;; | p999/p99 | How much worse is the worst 0.1% vs worst 1% |
;; | p999/p95 | Overall tail heaviness measure |
;;
;; **Interpretation:**
;;
;; | Ratio Value | Tail Behavior |
;; |-------------|---------------|
;; | 1.0 - 1.5 | Light tail, well-behaved latency |
;; | 1.5 - 2.5 | Moderate tail, some variance |
;; | 2.5 - 4.0 | Heavy tail, significant worst-case outliers |
;; | > 4.0 | Very heavy tail, investigate causes |
;;
;; High tail ratios often indicate:
;;
;; - GC pauses affecting some executions
;; - Occasional slow code paths (cache misses, lock contention)
;; - Background system activity
;; - JIT deoptimization events

;; ## Hill Estimator
;;
;; The Hill estimator measures the tail index α, characterizing how
;; quickly the tail probability decays:
;;
;; - **Higher α**: Lighter tail (faster decay, fewer extremes)
;; - **Lower α**: Heavier tail (slower decay, more extremes)
;; - **α → ∞**: Exponential-like tail (very light)
;; - **α ≈ 1-2**: Pareto-like heavy tail
;;
;; The tail index relates to moments: if α ≤ k, the k-th moment doesn't
;; exist (infinite variance when α ≤ 2, infinite mean when α ≤ 1).

;; ### Interpreting the Hill Plot
;;
;; The Hill plot shows the tail index estimate across different k values
;; (number of order statistics used). An example with variable execution
;; time:

(defn variable-work
  "Work with occasional slow executions."
  []
  (let [base (reduce + (range 500))]
    (if (< (rand) 0.05)
      (do (Thread/sleep 1) base)  ; 5% chance of slow path
      base)))

^:kindly/hide-code
(bench-display
 (bench/bench (variable-work)
              :bench-plan bench-plans/tail-analysis))

;; **What to look for in the Hill plot:**
;;
;; 1. **Stable region**: Where the curve flattens, showing consistent
;;    estimates across k values
;; 2. **Avoid small k**: High variance due to few observations
;; 3. **Avoid large k**: Bias from including non-tail observations
;;
;; The red dashed line shows the algorithmically selected stable estimate.
;; If no stable region exists, the tail structure may be complex or
;; sample size insufficient.

;; ## GPD Fitting
;;
;; Criterium fits a Generalized Pareto Distribution to exceedances over
;; the threshold using Maximum Likelihood Estimation (MLE).

;; ### Parameters
;;
;; The fit produces:
;;
;; - **ξ (xi)**: Shape parameter indicating tail heaviness
;; - **σ (sigma)**: Scale parameter for spread of exceedances
;; - **threshold**: The value above which GPD applies
;; - **n-exceedances**: Number of observations above threshold

;; ### High Quantile Extrapolation
;;
;; GPD's main value is extrapolating to quantiles beyond observed data.
;; Given n samples and threshold at the q-th quantile, the p-th quantile
;; estimate is:
;;
;; ```
;; Q(p) = threshold + (σ/ξ) * ((n*(1-p)/(n*(1-q)))^(-ξ) - 1)
;; ```
;;
;; This allows estimating p9999 even with "only" 2000 samples, though
;; uncertainty increases for more extreme quantiles.

;; ## Threshold Selection
;;
;; The POT method requires choosing a threshold. Too low includes non-tail
;; observations; too high leaves too few exceedances.

;; ### Mean Residual Life (MRL) Plot
;;
;; The MRL plot helps select an appropriate threshold. For a valid GPD
;; model, MRL should be approximately linear above the threshold:
;;
;; ```
;; MRL(u) = E[X - u | X > u] = (σ + ξ*u) / (1 - ξ)
;; ```
;;
;; **Interpreting the MRL plot:**
;;
;; - Look for where MRL becomes approximately linear
;; - Non-linearity throughout suggests complex tail structure
;; - The red dashed line shows the current threshold (default: 90th percentile)

;; ### Configuring Threshold
;;
;; By default, criterium uses the 90th percentile as threshold. Customize
;; via the `:threshold-quantile` option:

(def custom-threshold-plan
  (-> bench-plans/tail-analysis
      (assoc-in [:analyse 8]
                [:tail-analysis {:threshold-quantile 0.95}])))

(comment
  ;; Use 95th percentile as threshold
  (bench/bench (reduce + (range 1000))
               :bench-plan custom-threshold-plan
               :viewer :kindly))

;; Higher thresholds focus on more extreme behavior but reduce the number
;; of exceedances available for fitting.

;; ## Visualizations
;;
;; The `:kindly` viewer provides several diagnostic plots:

;; ### Zipf Plot
;;
;; The Zipf plot (log-log complementary CDF) shows:
;;
;; - x-axis: log₁₀(observation value)
;; - y-axis: log₁₀(P(X > x)) = log₁₀(survival probability)
;;
;; **Interpretation:**
;;
;; - Linear pattern suggests Pareto/power-law tail
;; - Slope relates to tail index (-α)
;; - Curvature indicates deviation from pure power-law
;; - Steeper slope = lighter tail, flatter = heavier

;; ### Exponential Q-Q Plot
;;
;; Compares exceedances to an exponential distribution (ξ = 0 case):
;;
;; - Points on diagonal: Exponential tail behavior
;; - Points curving upward: Heavy tail (ξ > 0)
;; - Points curving downward: Light/bounded tail (ξ < 0)
;;
;; Use this to assess whether the tail is exponential or requires the
;; more general GPD model.

;; ### GPD Q-Q Plot
;;
;; Compares exceedances to the fitted GPD:
;;
;; - Points on diagonal: Good GPD fit
;; - Systematic deviation: Model misspecification
;; - Scatter increases at extremes: Expected with limited samples
;;
;; If the GPD Q-Q shows poor fit, the high quantile extrapolations may
;; be unreliable. Consider whether the tail has complex structure
;; (multimodal, time-varying) that GPD cannot capture.

;; ## Practical SLA Guidance
;;
;; Tail analysis directly informs SLA and capacity planning decisions:

;; ### Setting SLA Targets
;;
;; | SLA Level | Typical Use | Sample Needs |
;; |-----------|-------------|--------------|
;; | p95 | User-facing web apps | 200+ |
;; | p99 | API contracts, dashboards | 500+ |
;; | p999 | Financial systems, real-time | 2000+ |
;; | p9999 | Ultra-low-latency, HFT | 10000+ |
;;
;; **Conservative approach**: Use the upper confidence bound of quantile
;; estimates for SLA commitments, not point estimates.

;; ### Interpreting Results for Capacity Planning
;;
;; Tail ratios help predict system behavior under load:
;;
;; - **p99/p95 > 2**: Occasional slow requests will spike significantly
;;   under load as queuing effects amplify tail latency
;; - **p999/p99 > 3**: Rare extreme events exist - ensure timeouts and
;;   circuit breakers are configured appropriately
;; - **Heavy tails (ξ > 0.2)**: System may experience unbounded latency
;;   spikes under adverse conditions
;;
;; For multi-service architectures, tail latencies compound. A chain of
;; 10 services each with p99 = 100ms can have aggregate p99 > 500ms due
;; to tail coupling.

;; ## Accessing Results Programmatically
;;
;; Extract tail analysis data from the benchmark result:

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        tail (get-in data [:tail-analysis :tail-analysis [:elapsed-time]])]
    {:threshold (:threshold tail)
     :n-exceedances (:n-exceedances tail)
     :tail-ratios (:tail-ratios tail)
     :hill-estimate (get-in tail [:hill :stable-estimate])
     :gpd-xi (get-in tail [:gpd :xi])
     :gpd-sigma (get-in tail [:gpd :sigma])}))

;; ### High Quantile Extraction

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        tail (get-in data [:tail-analysis :tail-analysis [:elapsed-time]])]
    (:high-quantiles tail)))

;; ### Custom High Quantiles
;;
;; Request specific quantiles via bench plan customization:

(def extended-quantiles-plan
  (-> bench-plans/tail-analysis
      (assoc-in [:analyse 8]
                [:tail-analysis {:threshold-quantile 0.9
                                 :high-quantiles [0.99 0.999 0.9999 0.99999]}])))

(comment
  (bench/bench (reduce + (range 1000))
               :bench-plan extended-quantiles-plan
               :viewer :kindly))

;; ## Summary
;;
;; - Use `tail-analysis` bench plan for worst-case latency analysis
;; - **Tail ratios** show how extreme percentiles compare to typical
;; - **Hill estimator** measures tail heaviness via the tail index
;; - **GPD fitting** enables extrapolation to rare quantiles (p999, p9999)
;; - **MRL plot** guides threshold selection for the POT method
;; - **Q-Q plots** verify GPD model fit quality
;; - Need 1000+ samples for reliable p999 estimates
;; - Raw samples are used (no outlier filtering) - extremes ARE the signal
;;
;; For distribution shape analysis without tail focus, see
;; [Parametric Analysis](./parametric_analysis.html) or
;; [Non-Parametric Analysis](./non_parametric_analysis.html).
