(ns criterium.distribution-fitting-notebook
  "Distribution fitting analysis for benchmark samples."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]))

;; # Distribution Fitting Analysis
;;
;; Distribution fitting helps understand the statistical nature of benchmark
;; timing variability by fitting parametric distributions to sample data.
;;
;; **Purpose:**
;; - **Diagnostic**: Understand the shape and nature of benchmark variability
;; - **Detection**: Identify if benchmark data is well-behaved vs problematic
;;
;; Criterium fits four distributions commonly seen in timing data:
;; - **Gamma**: Often fits well for positive, right-skewed timing data
;; - **Log-normal**: Natural for multiplicative processes
;; - **Inverse Gaussian**: Models first-passage times
;; - **Weibull**: Flexible shape, common in reliability analysis

;; ## Using the distribution-analysis Bench Plan
;;
;; The `distribution-analysis` bench plan includes distribution fitting along
;; with shape statistics (skewness, kurtosis, coefficient of variation).

(bench/bench (reduce + (range 1000))
             :bench-plan bench-plans/distribution-analysis
             :viewer :kindly)

;; ## Understanding the Output
;;
;; The distribution fit output includes several components:

;; ### Model Selection (AIC/BIC)
;;
;; Models are compared using information criteria:
;; - **AIC** (Akaike Information Criterion): Balances fit quality vs complexity
;; - **BIC** (Bayesian Information Criterion): Penalizes complexity more heavily
;; - **AICc**: Small-sample corrected AIC (important when n < 40)
;; - **ΔAIC**: Difference from best model (0 = best)
;;
;; **Interpretation:**
;; - ΔAIC < 2: Models are essentially equivalent
;; - ΔAIC 2-7: Some evidence for the better model
;; - ΔAIC > 10: Strong evidence for the better model

;; ### Goodness-of-Fit Tests
;;
;; Two tests assess how well distributions fit the data:
;;
;; **Kolmogorov-Smirnov (K-S) Test:**
;; - Measures maximum distance between empirical and theoretical CDFs
;; - Sensitive to differences in distribution shape
;;
;; **Cramér-von Mises (CvM) Test:**
;; - Integrates squared differences between CDFs
;; - More sensitive to tail behavior than K-S
;;
;; **Interpreting p-values:**
;; - p > 0.05: No strong evidence against the fit (acceptable)
;; - p < 0.05: Evidence the distribution doesn't fit well
;; - Low p-values for all distributions suggest non-standard timing behavior

;; ### Shape Statistics
;;
;; The output also shows shape statistics with interpretations:
;;
;; **Skewness:**
;; - Near 0: Symmetric distribution
;; - Positive: Right-skewed (long right tail) - common for timing data
;; - Negative: Left-skewed (rare for timing)
;;
;; **Kurtosis:**
;; - Near 3: Normal-like tails
;; - > 3: Heavy tails (more extreme values)
;; - < 3: Light tails
;;
;; **Coefficient of Variation (CV = σ/μ):**
;; - < 0.1: Low variability (stable benchmark)
;; - 0.1-0.5: Moderate variability
;; - > 0.5: High variability (investigate causes)

;; ## Visualizations
;;
;; The kindly viewer displays three distribution fit charts:
;;
;; - **PDF Overlay**: Fitted parametric PDFs overlaid on the kernel density estimate
;; - **CDF Overlay**: Fitted CDFs overlaid on the empirical CDF (step function)
;; - **Q-Q Plot**: Quantile-quantile plot comparing sample quantiles to theoretical
;;
;; The best model (by AIC) is shown with a solid line; other models are dashed.

;; ## Accessing Results Programmatically
;;
;; Use `last-bench` to access distribution fit results:

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/distribution-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        dist-fit (get-in data [:distribution-fit :fits [:elapsed-time]])]
    {:sample-size (:n dist-fit)
     :best-model (:best-model dist-fit)
     :distributions (keys (:distributions dist-fit))}))

;; ### Extracting Specific Model Results

(do
  (bench/bench (reduce + (range 1000))
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
  (bench/bench (reduce + (range 1000))
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

;; ## Parameter Confidence Intervals
;;
;; Bootstrap confidence intervals are computed for the best model's parameters.
;; These help assess parameter uncertainty:

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/distribution-analysis
               :viewer :none)
  (let [data (:data (bench/last-bench))
        dist-fit (get-in data [:distribution-fit :fits [:elapsed-time]])
        best (:best-model dist-fit)]
    {:model best
     :parameter-cis (get-in dist-fit [:parameter-cis best])}))

;; Narrow CIs indicate stable parameter estimates. Wide CIs suggest:
;; - Small sample size
;; - High variability in the data
;; - The distribution may not be a good fit

;; ## Small Sample Warning
;;
;; When sample size is below 30, distribution fitting becomes unreliable.
;; The output will show a warning. In this case:
;; - Take results with caution
;; - Consider increasing benchmark time for more samples
;; - Focus on basic statistics rather than distribution fitting

;; ## Customizing Distribution Fitting
;;
;; Create a custom bench plan to fit only specific distributions:

(def custom-distribution-plan
  (-> bench-plans/distribution-analysis
      (assoc :analyse
             [:transform-log
              [:quantiles {:quantiles [0.9 0.99]}]
              :outliers
              [:stats {}]
              [:stats {:samples-id :log-samples :id :log-stats}]
              :histogram
              :kde
              [:bootstrap-stats {:quantiles [0.99]
                                 :estimate-quantiles [0.025 0.975]}]
              ;; Fit only gamma and lognormal
              [:distribution-fit {:distributions [:gamma :lognormal]}]
              :event-stats])))

;; ### Available Options for distribution-fit
;;
;; - `:distributions` - List of distributions to fit (default: all four)
;; - `:outliers-id` - ID of outliers analysis for filtering (default: :outliers)
;; - `:n-bootstrap` - Number of bootstrap samples for parameter CIs

;; ## When to Use Distribution Fitting
;;
;; **Good use cases:**
;; - Investigating unexpected benchmark variability
;; - Comparing benchmark stability across implementations
;; - Understanding timing distribution shape for capacity planning
;; - Diagnosing JVM or system issues affecting benchmarks
;;
;; **Consider alternatives when:**
;; - You only need mean/variance (use default bench plan)
;; - You suspect multimodality (use `histogram` bench plan)
;; - Sample size is very small (< 30 samples)

;; ## Best Practices
;;
;; 1. **Start with the default plan** - Only use distribution fitting when
;;    you need to understand variability or diagnose issues.
;;
;; 2. **Check sample size** - Need at least 30 samples for reliable fitting.
;;    Increase benchmark time if needed.
;;
;; 3. **Compare AIC, not just p-values** - P-values tell you if a fit is
;;    adequate; AIC tells you which fit is best.
;;
;; 4. **Consider the context** - Timing data is often log-normal or gamma.
;;    If inverse-gaussian or weibull fit best, investigate why.
;;
;; 5. **Watch for poor fits** - If all distributions have low p-values,
;;    the data may be multimodal or have unusual characteristics.
;;
;; 6. **Use visualizations** - PDF/CDF overlays and Q-Q plots reveal
;;    fit quality better than statistics alone.

;; ## Running Examples

(comment
  ;; Basic distribution analysis
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/distribution-analysis)

  ;; Access results programmatically
  (let [data (:data (bench/last-bench))]
    (get-in data [:distribution-fit :fits [:elapsed-time]]))

  ;; With specific distributions only
  (bench/bench (reduce + (range 1000))
               :bench-plan custom-distribution-plan)

  ;; With Portal visualization
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/distribution-analysis
               :viewer :portal)

  ;; Compare with histogram for multimodal detection
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/histogram))
