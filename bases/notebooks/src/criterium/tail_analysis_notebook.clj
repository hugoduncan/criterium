(ns criterium.tail-analysis-notebook
  "Tail analysis for understanding worst-case latency behavior."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]))

;; # Tail Analysis
;;
;; Tail analysis examines the extreme values (tail) of latency distributions
;; to understand worst-case performance. This is essential for:
;;
;; - **SLA compliance**: Guaranteeing p99 or p999 latency bounds
;; - **Capacity planning**: Understanding how systems behave under load
;; - **Performance regression**: Detecting tail latency degradation
;;
;; Unlike mean or median statistics, tail analysis focuses on rare but
;; impactful events - the slowest 1% or 0.1% of executions.

;; ## When to Use Tail Analysis
;;
;; Use the `:tail-analysis` bench plan when you care about worst-case latency:
;;
;; - **Latency-sensitive systems**: Real-time systems, trading platforms
;; - **Service level objectives**: When you have p99/p999 SLA targets
;; - **Performance investigation**: Diagnosing occasional slow responses
;;
;; **Note:** Tail analysis requires sufficient samples (1000+) for reliable
;; estimates. The extreme quantiles (p999, p9999) need many observations to
;; be meaningful.

;; ## Basic Usage
;;
;; Use the `tail-analysis` bench plan:

(bench/bench (reduce + (range 1000))
             :bench-plan bench-plans/tail-analysis
             :viewer :kindly)

;; ## Understanding the Output
;;
;; The tail analysis produces several key metrics:

;; ### Tail Ratios
;;
;; Tail ratios indicate how heavy the distribution tail is:
;;
;; | Ratio | Meaning |
;; |-------|---------|
;; | p99/p95 | How much worse is the 99th percentile vs 95th |
;; | p999/p99 | How much worse is the 99.9th percentile vs 99th |
;; | p999/p95 | Overall tail heaviness measure |
;;
;; **Interpretation:**
;; - Ratio ≈ 1.0-1.5: Light tail, well-behaved latency
;; - Ratio ≈ 2.0-3.0: Moderate tail, some variance
;; - Ratio > 3.0: Heavy tail, significant worst-case outliers

;; ### Hill Estimator
;;
;; The Hill estimator measures the tail index α (also called tail exponent):
;;
;; - **Higher α**: Lighter tail (faster decay)
;; - **Lower α**: Heavier tail (slower decay)
;; - **α → ∞**: Exponential-like tail (very light)
;; - **α ≈ 1-2**: Pareto-like heavy tail
;;
;; The Hill plot shows the estimate across different k values (order statistics).
;; Look for a **stable region** where the estimate doesn't vary much with k.

;; ### GPD Fit (Generalized Pareto Distribution)
;;
;; GPD models the exceedances over a high threshold. Key parameters:
;;
;; - **ξ (xi)**: Shape parameter
;;   - ξ > 0: Heavy (Pareto-like) tail, extreme values possible
;;   - ξ ≈ 0: Exponential tail
;;   - ξ < 0: Light tail with finite upper bound
;; - **σ (sigma)**: Scale parameter (spread of exceedances)
;;
;; **Why GPD?** The Peaks Over Threshold (POT) method uses GPD to model
;; extreme values, allowing extrapolation to rare quantiles.

;; ### High Quantile Estimates
;;
;; GPD extrapolation provides estimates for extreme quantiles:
;;
;; - p99 (1 in 100)
;; - p999 (1 in 1,000)
;; - p9999 (1 in 10,000)
;;
;; These extrapolated values are more reliable than empirical estimates
;; when sample size is limited.

;; ## Interpreting the Hill Plot
;;
;; The Hill plot is crucial for choosing a reliable tail index estimate.

(defn variable-work
  "Simulates work with occasional slow executions."
  []
  (reduce + (range (rand-int 10000))))

(bench/bench (variable-work)
             :bench-plan bench-plans/tail-analysis
             :viewer :kindly)

;; **What to look for:**
;;
;; 1. **Stable region**: Where the curve flattens, showing consistent estimates
;; 2. **Avoid small k**: High variance due to few observations
;; 3. **Avoid large k**: Bias from including non-tail observations
;;
;; The red dashed line shows the algorithmically selected stable estimate.

;; ## Using MRL Plot for Threshold Selection
;;
;; The Mean Residual Life (MRL) plot helps select the POT threshold.
;;
;; **MRL definition:** e(u) = E[X - u | X > u] is the expected excess over
;; threshold u, given that X exceeds u.
;;
;; **What to look for:**
;; - Where MRL becomes approximately **linear** indicates GPD behavior
;; - The red dashed line shows the currently selected threshold (90th percentile)
;;
;; If MRL is non-linear throughout, the data may not follow a simple tail model.

;; ## Zipf Plot
;;
;; The Zipf plot shows the complementary CDF on a log-log scale:
;; log₁₀(P(X > x)) vs log₁₀(x)
;;
;; **Interpretation:**
;; - **Linear pattern**: Suggests Pareto/power-law tail
;; - **Slope**: Related to tail index (-α)
;; - **Curvature**: Indicates deviation from pure power-law

;; ## Q-Q Plots
;;
;; Two Q-Q plots assess model fit:
;;
;; ### Exponential Q-Q
;; Compares exceedances to exponential distribution:
;; - Points on diagonal: Exponential tail (ξ ≈ 0)
;; - Upward curve: Heavy tail (ξ > 0)
;; - Downward curve: Light tail (ξ < 0)
;;
;; ### GPD Q-Q
;; Compares exceedances to fitted GPD:
;; - Points on diagonal: Good GPD fit
;; - Systematic deviation: Model misspecification

;; ## Accessing Results Programmatically

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        tail (get-in data [:tail-analysis :tail-analysis [:elapsed-time]])]
    {:threshold (:threshold tail)
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

;; ## Raw Samples and Outliers
;;
;; **Important:** Tail analysis uses raw samples WITHOUT outlier filtering.
;; This is intentional because:
;;
;; 1. Extreme values ARE the tail being analyzed
;; 2. Outlier filtering would remove the very data points we need
;; 3. Statistical methods (GPD, Hill) are designed for extreme values
;;
;; This differs from other bench plans (like `:distribution-analysis`)
;; that filter outliers before fitting distributions.

;; ## Customizing Tail Analysis
;;
;; Create a custom plan with different threshold:

(def custom-tail-plan
  (-> bench-plans/tail-analysis
      (assoc-in [:analyse 4]
                [:tail-analysis {:threshold-quantile 0.95
                                 :high-quantiles [0.99 0.999 0.9999 0.99999]}])))

(comment
  ;; Use 95th percentile as threshold
  (bench/bench (reduce + (range 1000))
               :bench-plan custom-tail-plan
               :viewer :kindly))

;; ## Best Practices
;;
;; 1. **Collect sufficient samples**: Tail analysis needs 1000+ samples.
;;    Use longer benchmark times if needed.
;;
;; 2. **Check for stability**: The Hill plot should show a stable region.
;;    Erratic estimates indicate insufficient data or complex tail structure.
;;
;; 3. **Validate with Q-Q plots**: If the GPD Q-Q plot shows poor fit,
;;    the high quantile extrapolations may be unreliable.
;;
;; 4. **Compare empirical vs extrapolated**: High quantile estimates should
;;    be reasonably consistent with empirical percentiles.
;;
;; 5. **Consider the application**: For SLA monitoring, conservative
;;    estimates (upper confidence bounds) may be more appropriate.

;; ## Running Examples

(comment
  ;; Basic tail analysis
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis)

  ;; With Portal visualization
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis
               :viewer :portal)

  ;; With kindly (notebook) visualization
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/tail-analysis
               :viewer :kindly)

  ;; Access last benchmark results
  (let [data (:data (bench/last-bench))]
    (get-in data [:tail-analysis :tail-analysis [:elapsed-time]])))
