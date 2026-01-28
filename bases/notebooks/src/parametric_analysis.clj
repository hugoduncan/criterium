(ns parametric-analysis
  "Parametric distribution fitting for benchmark samples."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.notebook.helpers :refer [bench-display]]))

;; # Parametric Analysis
;;
;; Parametric analysis fits statistical distributions to benchmark samples,
;; assuming the data follows a specific mathematical shape. This complements
;; [non-parametric analysis](./non_parametric_analysis.html) which makes no
;; such assumptions.
;;
;; **When to use parametric analysis:**
;;
;; - You want to understand the statistical nature of timing variability
;; - You need to extrapolate beyond observed quantiles
;; - You want to compare benchmark stability across implementations
;; - You're diagnosing JVM or system issues affecting benchmarks
;;
;; **When to prefer non-parametric:**
;;
;; - You suspect multimodality (multiple execution time modes)
;; - You only need visualization of the actual distribution shape
;; - Sample size is very small (< 30 samples)

;; ## The distribution-analysis Bench Plan
;;
;; The `distribution-analysis` bench plan includes distribution fitting along
;; with shape statistics (skewness, kurtosis, coefficient of variation):

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (vec (range 20)))
              :bench-plan bench-plans/distribution-analysis))

;; The output includes:
;;
;; - **Shape statistics** - Skewness, kurtosis, CV with interpretations
;; - **Model comparison** - AIC/BIC for each fitted distribution
;; - **Goodness-of-fit** - K-S and Cramér-von Mises test p-values
;; - **Parameter estimates** - MLE parameters with bootstrap confidence intervals
;; - **Visualizations** - PDF/CDF overlays, Q-Q plots (with `:kindly` viewer)

;; ## Shape Statistics
;;
;; Shape statistics describe the distribution without assuming a specific form.
;; They help identify whether parametric fitting is appropriate.

;; ### Skewness
;;
;; Measures asymmetry around the mean:
;;
;; | Value | Interpretation |
;; |-------|----------------|
;; | ≈ 0 | Symmetric distribution |
;; | > 0 | Right-skewed (long right tail) - common for timing data |
;; | < 0 | Left-skewed (rare for timing) |
;;
;; Timing data is typically right-skewed because execution can be slower than
;; usual (GC, contention) but not faster than the optimal path.

;; ### Kurtosis
;;
;; Measures tail heaviness relative to a normal distribution:
;;
;; | Value | Interpretation |
;; |-------|----------------|
;; | ≈ 3 | Normal-like tails (mesokurtic) |
;; | > 3 | Heavy tails, more extreme values (leptokurtic) |
;; | < 3 | Light tails, fewer extreme values (platykurtic) |
;;
;; High kurtosis suggests occasional extreme outliers - investigate GC pauses
;; or system interference.

;; ### Coefficient of Variation (CV)
;;
;; The ratio of standard deviation to mean (σ/μ), expressing variability
;; relative to the typical value:
;;
;; | CV | Interpretation |
;; |----|----------------|
;; | < 0.1 | Low variability - stable benchmark |
;; | 0.1-0.5 | Moderate variability - typical |
;; | > 0.5 | High variability - investigate causes |
;;
;; High CV suggests measurement instability. Consider increasing warmup or
;; reducing system load.

;; ## Supported Distributions
;;
;; Criterium fits four distributions commonly seen in timing data:

;; ### Gamma Distribution
;;
;; - **Shape:** Right-skewed, always positive
;; - **Use case:** Often fits well for positive, right-skewed timing data
;; - **Parameters:** shape (α) and rate (β)
;;
;; Gamma is a natural choice when times result from multiple independent
;; sub-operations.

;; ### Log-Normal Distribution
;;
;; - **Shape:** Right-skewed, always positive, heavy right tail
;; - **Use case:** Natural for multiplicative processes
;; - **Parameters:** μ (log-scale mean) and σ (log-scale std dev)
;;
;; Log-normal fits when timing variability comes from many multiplicative
;; factors (cache effects, branch prediction, etc.).

;; ### Inverse Gaussian Distribution
;;
;; - **Shape:** Right-skewed, always positive
;; - **Use case:** Models first-passage times
;; - **Parameters:** μ (mean) and λ (shape)
;;
;; Theoretically appropriate when measuring time to reach a threshold
;; (e.g., completing a computation).

;; ### Weibull Distribution
;;
;; - **Shape:** Flexible - can be right-skewed, left-skewed, or symmetric
;; - **Use case:** Reliability analysis, flexible shape
;; - **Parameters:** shape (k) and scale (λ)
;;
;; Weibull's flexibility makes it a good diagnostic choice when other
;; distributions don't fit well.

;; ## Model Fitting (MLE)
;;
;; Criterium uses Maximum Likelihood Estimation (MLE) to fit parameters.
;; MLE finds parameter values that maximize the probability of observing
;; the collected samples.
;;
;; The fitting process:
;;
;; 1. Moment-matching prefilter checks parameter validity
;; 2. MLE optimization finds best parameters
;; 3. Information criteria (AIC/BIC) computed
;; 4. Goodness-of-fit tests performed
;; 5. Bootstrap CIs computed for best model

;; ### Customizing Distributions
;;
;; To fit only specific distributions, customize the bench plan:

(def gamma-lognormal-plan
  (-> bench-plans/distribution-analysis
      (assoc-in [:analyse 11]
                [:distribution-fit {:distributions [:gamma :lognormal]}])))

;; The `:distributions` option accepts a vector with any combination of:
;; `:gamma`, `:lognormal`, `:inverse-gaussian`, `:weibull`

;; ## Model Selection
;;
;; With multiple distributions fitted, how do you choose the best one?
;; Criterium uses information criteria that balance fit quality against
;; model complexity.

;; ### AIC and BIC
;;
;; - **AIC** (Akaike Information Criterion): Balances fit vs complexity
;; - **BIC** (Bayesian Information Criterion): Penalizes complexity more heavily
;; - **AICc**: Small-sample corrected AIC (important when n < 40)
;;
;; Lower values indicate better models. The best model by AIC is reported
;; as `:best-model`.

;; ### Interpreting Delta-AIC
;;
;; Delta-AIC (ΔAIC) is the difference from the best model's AIC:
;;
;; | ΔAIC | Interpretation |
;; |------|----------------|
;; | 0 | Best model |
;; | < 2 | Models are essentially equivalent |
;; | 2-7 | Some evidence for the better model |
;; | > 10 | Strong evidence for the better model |
;;
;; When multiple models have ΔAIC < 2, the simpler interpretation may be
;; preferred (gamma or log-normal over inverse-gaussian).

;; ## Goodness-of-Fit Testing
;;
;; Information criteria tell you which model is *best*, but not whether
;; any model actually *fits well*. Goodness-of-fit tests address this.

;; ### Kolmogorov-Smirnov (K-S) Test
;;
;; Measures the maximum distance between the empirical CDF (from samples)
;; and the theoretical CDF (from the fitted distribution).
;;
;; - Sensitive to differences in distribution shape
;; - Tests the null hypothesis: "samples come from the fitted distribution"

;; ### Cramér-von Mises (CvM) Test
;;
;; Integrates the squared difference between empirical and theoretical CDFs.
;;
;; - More sensitive to tail behavior than K-S
;; - Better at detecting deviations throughout the distribution

;; ### Interpreting P-Values
;;
;; | P-value | Interpretation |
;; |---------|----------------|
;; | > 0.05 | No strong evidence against the fit (acceptable) |
;; | < 0.05 | Evidence the distribution doesn't fit well |
;; | < 0.01 | Strong evidence of poor fit |
;;
;; **Important:** Low p-values for *all* distributions suggest the data has
;; unusual characteristics - possibly multimodal. Use the `histogram` bench
;; plan to investigate.

;; ## Visualizations
;;
;; The `:kindly` viewer provides three distribution fit charts:

;; ### PDF Overlay
;;
;; Shows fitted probability density functions overlaid on the kernel density
;; estimate. The best model (by AIC) appears as a solid line; others are dashed.
;;
;; Look for: How well the fitted curves match the KDE shape, especially at
;; peaks and tails.

;; ### CDF Overlay
;;
;; Shows fitted cumulative distribution functions overlaid on the empirical
;; CDF (step function).
;;
;; Look for: Systematic deviations where the fitted curve diverges from the
;; empirical steps, especially in the tails.

;; ### Q-Q Plot
;;
;; Quantile-quantile plot comparing sample quantiles to theoretical quantiles
;; from the best-fit distribution.
;;
;; - Points on the diagonal = perfect fit
;; - S-curve = different tail behavior than assumed
;; - Points curving away at ends = heavier or lighter tails than the model

;; ## Accessing Results Programmatically
;;
;; Use `last-bench` to access distribution fit results:

(do
  (bench/bench (reduce + (vec (range 20)))
               :bench-plan bench-plans/distribution-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        dist-fit (get-in data [:distribution-fit :fits [:elapsed-time]])]
    {:sample-size (:n dist-fit)
     :best-model (:best-model dist-fit)
     :distributions (keys (:distributions dist-fit))}))

;; ### Extracting Model Details

(do
  (bench/bench (reduce + (vec (range 20)))
               :bench-plan bench-plans/distribution-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        dist-fit (get-in data [:distribution-fit :fits [:elapsed-time]])
        best (:best-model dist-fit)
        best-result (get-in dist-fit [:distributions best])]
    {:model best
     :params (:params best-result)
     :aic (:aic best-result)
     :ks-pvalue (get-in best-result [:ks-test :p-value])
     :cvm-pvalue (get-in best-result [:cvm-test :p-value])}))

;; ### Comparing All Models

(do
  (bench/bench (reduce + (vec (range 20)))
               :bench-plan bench-plans/distribution-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        dist-fit (get-in data [:distribution-fit :fits [:elapsed-time]])
        distributions (:distributions dist-fit)]
    (->> distributions
         (map (fn [[dist result]]
                {:distribution dist
                 :delta-aic (:delta-aic result)
                 :ks-pvalue (get-in result [:ks-test :p-value])}))
         (sort-by :delta-aic))))

;; ### Parameter Confidence Intervals
;;
;; Bootstrap confidence intervals for the best model's parameters help assess
;; estimation uncertainty:

(do
  (bench/bench (reduce + (vec (range 20)))
               :bench-plan bench-plans/distribution-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        dist-fit (get-in data [:distribution-fit :fits [:elapsed-time]])
        best (:best-model dist-fit)]
    {:model best
     :parameter-cis (get-in dist-fit [:parameter-cis best])}))

;; Narrow CIs indicate stable parameter estimates. Wide CIs suggest:
;;
;; - Small sample size
;; - High variability in the data
;; - The distribution may not be a good fit

;; ## Small Sample Warning
;;
;; When sample size is below 30, distribution fitting becomes unreliable.
;; The output shows a `:small-sample` warning. In this case:
;;
;; - Results should be interpreted cautiously
;; - Consider increasing benchmark time via `:limit-time-s`
;; - Focus on basic statistics rather than distribution fitting

;; ## Configuration Options
;;
;; The `:distribution-fit` analysis step accepts:
;;
;; | Option | Default | Description |
;; |--------|---------|-------------|
;; | `:distributions` | all four | Vector of distributions to fit |
;; | `:outliers-id` | `:outliers` | Outlier analysis for filtering |
;; | `:n-bootstrap` | 200 | Bootstrap samples for parameter CIs |
;; | `:alpha` | 0.05 | Significance level for CIs |

;; ## Summary
;;
;; - Use `distribution-analysis` bench plan for parametric fitting
;; - **Shape statistics** identify distribution characteristics before fitting
;; - **Model selection** via AIC/BIC ranks competing distributions
;; - **Goodness-of-fit** tests verify the fit is adequate
;; - **Visualizations** reveal fit quality better than statistics alone
;; - Need 30+ samples for reliable fitting
;;
;; For distribution shape visualization without parametric assumptions, see
;; [Non-Parametric Analysis](./non_parametric_analysis.html).
;;
;; For tail behavior and worst-case latency analysis, see
;; [Tail Analysis](./tail_analysis.html).
