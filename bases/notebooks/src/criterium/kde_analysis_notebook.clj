(ns criterium.kde-analysis-notebook
  "Kernel Density Estimation analysis for benchmark samples."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.measured :as measured]
   [criterium.notebook.helpers :refer [bench-display]]
   [scicloj.kindly.v4.kind :as kind]))

;; # KDE Analysis for Benchmark Samples
;;
;; Kernel Density Estimation (KDE) provides a smooth, continuous estimate of
;; the probability density function underlying benchmark samples. This is useful
;; for:
;;
;; - Visualizing sample distributions beyond histograms
;; - Detecting multimodality (multiple peaks indicating distinct performance modes)
;; - Identifying modes with confidence intervals
;; - Understanding the shape of timing distributions

;; ## Using the kde-histogram Bench Plan
;;
;; The `kde-histogram` bench plan includes histogram and KDE analysis.
;; This is not part of the default plan; use it explicitly when density
;; analysis is needed.

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan bench-plans/kde-histogram))

;; The output includes:
;; - Standard statistics (mean, standard deviation)
;; - Histogram visualization
;; - KDE density curve overlaid on the histogram

;; ## Using the kde-modes Bench Plan
;;
;; For statistically validated mode detection, use the `kde-modes` bench plan.
;; This includes multimodality testing to validate the number of distribution modes.

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan bench-plans/kde-modes))

;; The kde-modes plan adds:
;; - Mode detection with confidence intervals
;; - ACR test p-values for each k (number of modes)
;; - Validated mode count based on statistical significance

;; ## Understanding KDE Output
;;
;; KDE analysis produces several key outputs:

;; ### Bandwidth
;;
;; The bandwidth controls the smoothness of the density estimate. Criterium uses
;; ISJ (Improved Sheather-Jones) bandwidth selection, which automatically chooses
;; an optimal bandwidth based on the data. A smaller bandwidth captures more
;; detail but may be noisy; a larger bandwidth produces a smoother curve.

;; ### Density Curve
;;
;; The density curve shows the estimated probability density at each point.
;; Unlike histograms, KDE produces a smooth, continuous curve that doesn't
;; depend on bin boundaries.

;; ### Confidence Bands
;;
;; Bootstrap confidence bands show the uncertainty in the density estimate.
;; The shaded area represents the range where the true density likely falls.

;; ## Understanding Modes Output
;;
;; Mode analysis (from the `kde-modes` plan) provides statistically validated
;; peaks in the density curve:

;; ### Multimodality Testing Methods
;;
;; Criterium supports two methods for testing multimodality:
;;
;; **ACR Test (Default)**
;;
;; The ACR test (Ameijeiras-Alonso, Crujeiras, Rodríguez-Casal 2019) uses
;; excess mass as the test statistic. This provides better calibration than
;; mode counting, especially for detecting genuine multimodality vs noise.
;;
;; **Silverman's Test**
;;
;; Silverman's bootstrap test determines if the data supports k modes by
;; counting modes in bootstrap samples. For k=1, the Hall-York correction
;; is applied. However, mode counting can be sensitive to noise and may
;; be overly conservative.
;;
;; **Why ACR is Recommended**
;;
;; - Excess mass is more robust to small fluctuations in the density
;; - Better calibrated p-values across different sample sizes
;; - Less sensitive to bandwidth choice than mode counting
;;
;; Use `:method :silverman` if you specifically need backward compatibility.

;; ### Validated Mode Count
;;
;; The `n-modes` value in the output is the smallest k where we fail to reject
;; the null hypothesis (H0: at most k modes). This gives a statistically
;; supported mode count.

;; ### Mode Significance
;;
;; Each detected mode is marked with `significant?` indicating whether
;; the test supports its existence.

;; ### Understanding ACR Test Results
;;
;; The ACR test output includes:
;;
;; - `:method` - The test method used (`:acr` or `:silverman`)
;; - `:k-tested` - Vector of k values tested (e.g., [1 2 3])
;; - `:p-values` - Map of k to p-value (e.g., {1 0.02, 2 0.45})
;; - `:critical-bandwidths` - Map of k to h_k (smallest bandwidth giving ≤ k modes)
;; - `:excess-mass` - Map of k to excess mass statistic (ACR only)
;;
;; **Interpreting excess mass:**
;; - Larger values suggest stronger evidence for more than k modes
;; - The p-value is computed by comparing observed excess mass to bootstrap
;;   samples under the null hypothesis
;;
;; **Example interpretation:**
;; If `:p-values {1 0.02, 2 0.35}`:
;; - p=0.02 for k=1: Reject H0 (unimodal), evidence for >1 mode
;; - p=0.35 for k=2: Fail to reject H0 (≤2 modes)
;; - Conclusion: n-modes = 2 (bimodal distribution)

;; ## Accessing KDE and Modes Results Programmatically
;;
;; Use `last-bench` to access the full results:

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-modes
               :viewer :none)
  (let [data (:data (bench/last-bench))
        kde-result (:kde data)
        modes-result (:modes data)]
    {:bandwidth (get-in kde-result [:kdes [:elapsed-time] :bandwidth])
     :n-modes (get-in modes-result [:modes [:elapsed-time] :n-modes])
     :test-results (get-in modes-result [:modes [:elapsed-time] :test-results])
     :modes (get-in modes-result [:modes [:elapsed-time] :modes])}))

;; ## Customizing KDE Options
;;
;; The `kde` analysis step accepts several options via the bench plan:

;; ### n-points
;;
;; Grid size for density evaluation. Default is 512. Higher values give
;; smoother curves but take longer to compute.

;; ### n-bootstrap
;;
;; Number of bootstrap samples for confidence bands. Default is 200.
;; More samples give more accurate confidence intervals but take longer.

;; ### alpha
;;
;; Confidence level for intervals. Default is 0.05 (95% confidence).

;; ## Customizing Modes Options
;;
;; The `modes` analysis step has additional options:

;; ### max-modes
;;
;; Maximum number of modes to test. Default is 5. The test is run
;; for k=1 through max-modes.

;; ### method
;;
;; The multimodality test method. Options:
;; - `:acr` (default) - Uses excess mass statistic for better calibration
;; - `:silverman` - Uses bootstrap mode count (legacy method)

;; ### mode-method
;;
;; How mode locations are determined. Options:
;; - `:isj` (default) - Find modes from KDE density at ISJ bandwidth
;; - `:critical` - Find modes at critical bandwidth for validated k modes
;;
;; When using `:critical`, the output includes additional fields:
;; - `:antimodes` - Local minima between modes (useful for segmentation)
;; - `:mode-bandwidth` - The critical bandwidth used for mode finding
;;
;; The critical bandwidth approach finds the smallest bandwidth that gives
;; exactly k modes, which can provide more stable mode locations.

;; ### Custom Bench Plan Example
;;
;; Create a custom bench plan with modified options:

(def custom-kde-modes-plan
  (-> bench-plans/kde-modes
      (assoc :analyse
             [:transform-log
              [:quantiles {:quantiles [0.9 0.99]}]
              :outliers
              [:stats {}]
              [:stats {:samples-id :log-samples :id :log-stats}]
              :histogram
              [:kde {:n-points 256 :n-bootstrap 100}]
              :kde-stats
              [:modes {:max-modes 3 :n-bootstrap 100 :method :acr}]
              :event-stats])))

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan custom-kde-modes-plan))

;; ### Using Critical Bandwidth for Mode Finding
;;
;; The critical bandwidth approach can provide more stable mode locations:

(def critical-modes-plan
  (-> bench-plans/kde-modes
      (assoc :analyse
             [:transform-log
              [:quantiles {:quantiles [0.25 0.5 0.75]}]
              :outliers
              [:stats {}]
              [:stats {:samples-id :log-samples :id :log-stats}]
              :histogram
              [:kde {:n-points 512}]
              :kde-stats
              [:modes {:max-modes 3 :mode-method :critical}]
              :event-stats])))

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan critical-modes-plan))

;; ## Visualization with Portal and Kindly
;;
;; The `:portal` and `:kindly` viewers render the KDE as a Vega-Lite chart:
;; - Density curve overlaid on histogram bars
;; - Shaded confidence bands
;; - Mode markers with horizontal confidence interval lines (when modes data present)
;; - Color coding for significant vs non-significant modes

;; ### Portal Viewer
;;
;; To use with Portal, ensure Portal is connected:

(kind/code
 "(require '[portal.api :as p])
(def portal (p/open))
(add-tap #'p/submit)

(bench/bench (reduce + (range 1000))
             :bench-plan bench-plans/kde-modes
             :viewer :portal)")

;; ### Kindly Viewer
;;
;; For Clay notebooks, use `:viewer :kindly` to get embedded charts:

(bench/bench
 (reduce + (range 1000))
 :bench-plan bench-plans/kde-modes
 :viewer :kindly)

;; ## Detecting Multimodality
;;
;; Multimodal distributions indicate that benchmark execution times fall into
;; distinct groups. This can happen due to:
;;
;; - JIT compilation tiers
;; - GC pauses affecting some samples
;; - CPU frequency scaling
;; - Cache effects
;;
;; The ACR test provides statistical validation for multimodality. A low
;; p-value for k=1 suggests the distribution is not unimodal. The ACR test
;; uses the excess mass statistic, which measures how much the empirical
;; distribution exceeds what would be expected under k modes.

;; ### Example: Synthetic Multimodal Distribution
;;
;; This artificial example shows how multimodality appears:

(defn variable-work
  "Simulate variable-time work with occasional slow paths."
  [^long n slow?]
  (if slow?
    (reduce + (range (* n 3))) ; slow path
    (reduce + (range n)))) ; fast path

(bench/bench
 (variable-work 100 (zero? (long (mod (rand-int 100) 3))))
 :bench-plan bench-plans/kde-modes
 :viewer :kindly)

;; ### Comparing ACR and Silverman Results
;;
;; You can compare results from both test methods:

(defn compare-test-methods
  "Run both ACR and Silverman tests and compare results.
  Takes a measured (created with measured/callable or measured/expr)."
  [m]
  (let [silverman-plan (assoc-in bench-plans/kde-modes
                                 [:analyse 7] [:modes {:method :silverman}])]
    ;; Run with ACR (default method)
    (bench/bench-measured
     (bench/options->bench-plan :bench-plan bench-plans/kde-modes :viewer :none)
     m)
    (let [acr-result (get-in (:data (bench/last-bench))
                             [:modes :modes [:elapsed-time]])]
      ;; Run with Silverman
      (bench/bench-measured
       (bench/options->bench-plan :bench-plan silverman-plan :viewer :none)
       m)
      (let [silv-result (get-in (:data (bench/last-bench))
                                [:modes :modes [:elapsed-time]])]
        {:acr       {:n-modes     (:n-modes acr-result)
                     :p-values    (get-in acr-result [:test-results :p-values])
                     :excess-mass (get-in acr-result [:test-results :excess-mass])}
         :silverman {:n-modes  (:n-modes silv-result)
                     :p-values (get-in silv-result [:test-results :p-values])}}))))

;; Compare results on the multimodal example:
(compare-test-methods
 (measured/callable #(variable-work 100 (zero? (long (mod (rand-int 100) 3))))))

;; ## Comparing KDE to Histograms
;;
;; Both histogram and KDE visualize distributions, but they differ:
;;
;; | Aspect | Histogram | KDE |
;; |--------|-----------|-----|
;; | Output | Discrete bins | Continuous curve |
;; | Bin sensitivity | Depends on bin width | Depends on bandwidth |
;; | Mode detection | Visual inspection | Automatic with CIs |
;; | Interpretation | Counts per bin | Probability density |
;;
;; Use histograms for quick visual inspection; use KDE when you need
;; precise mode locations or smooth density estimates.

;; ## Best Practices
;;
;; 1. **Use kde-histogram for density visualization** - When you just need
;;    to see the distribution shape without statistical mode validation.
;;
;; 2. **Use kde-modes for mode detection** - When you need statistically
;;    validated mode counts with the ACR test.
;;
;; 3. **Check test p-values** - Low p-values for k=1 indicate evidence
;;    against unimodality. The `test-results` map contains p-values for each k.
;;
;; 4. **Use ACR over Silverman** - ACR provides better calibrated p-values
;;    and is less sensitive to noise in the density estimate.
;;
;; 5. **Consider :mode-method :critical** - When you need stable mode
;;    locations and want to identify antimodes (valleys between peaks).
;;
;; 6. **Consider computation cost** - Mode analysis involves multiple rounds
;;    of bootstrap resampling. Use `kde-histogram` when you only need the
;;    density curve.

;; ## Running Examples
;;
;; Execute benchmarks with KDE and mode analysis:

(comment
  ;; Basic KDE analysis (no mode testing)
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-histogram)

  ;; KDE with mode detection and ACR test (default)
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-modes)

  ;; Access modes results programmatically
  (let [data (:data (bench/last-bench))]
    {:kde-bandwidth (get-in data [:kde :kdes [:elapsed-time] :bandwidth])
     :n-modes       (get-in data [:modes :modes [:elapsed-time] :n-modes])
     :test-results  (get-in data [:modes :modes [:elapsed-time] :test-results])
     :excess-mass   (get-in data [:modes :modes [:elapsed-time] :test-results :excess-mass])})

  ;; Use Silverman test instead of ACR
  (bench/bench (reduce + (range 1000))
               :bench-plan (assoc-in bench-plans/kde-modes
                                     [:analyse 7] [:modes {:method :silverman}]))

  ;; Use critical bandwidth for mode finding
  (bench/bench (reduce + (range 1000))
               :bench-plan (assoc-in bench-plans/kde-modes
                                     [:analyse 7] [:modes {:mode-method :critical}]))

  ;; Custom options
  (bench/bench (reduce + (range 1000))
               :bench-plan custom-kde-modes-plan)

  ;; With Portal visualization
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-modes
               :viewer :portal))
