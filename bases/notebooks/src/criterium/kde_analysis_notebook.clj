(ns criterium.kde-analysis-notebook
  "Kernel Density Estimation analysis for benchmark samples."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
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
;; This includes Silverman's bootstrap test for multimodality.

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan bench-plans/kde-modes))

;; The kde-modes plan adds:
;; - Mode detection with confidence intervals
;; - Silverman's test p-values for each k (number of modes)
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

;; ### Silverman's Test
;;
;; Silverman's bootstrap test determines if the data supports k modes. The test
;; is run for k=1, 2, 3, ... up to max-modes. A low p-value indicates evidence
;; for more than k modes.
;;
;; For k=1, the Hall-York correction is applied for better calibration.

;; ### Validated Mode Count
;;
;; The `n-modes` value in the output is the smallest k where we fail to reject
;; the null hypothesis (H0: at most k modes). This gives a statistically
;; supported mode count.

;; ### Mode Significance
;;
;; Each detected mode is marked with `significant?` indicating whether
;; Silverman's test supports its existence.

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
     :silverman-p-values (get-in modes-result [:modes [:elapsed-time] :silverman :p-values])
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
;; Maximum number of modes to test. Default is 5. Silverman's test is run
;; for k=1 through max-modes.

;; ### Custom Bench Plan Example
;;
;; Create a custom bench plan with modified options:

(def custom-kde-modes-plan
  (-> bench-plans/kde-modes
      (assoc :analyse
             [:transform-log
              [:quantiles {:quantiles [0.9 0.99 0.99]}]
              :outliers
              [:stats {}]
              [:stats {:samples-id :log-samples :id :log-stats}]
              :histogram
              [:kde {:n-points 256 :n-bootstrap 100}]
              [:modes {:max-modes 3 :n-bootstrap 100}]
              :event-stats])))

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan custom-kde-modes-plan))

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
;; Silverman's test provides statistical validation for multimodality. A low
;; p-value for k=1 suggests the distribution is not unimodal.

;; ### Example: Synthetic Multimodal Distribution
;;
;; This artificial example shows how multimodality appears:

(defn variable-work
  "Simulate variable-time work with occasional slow paths."
  [^long n]
  (if (zero? (long (mod (rand-int 100) 5)))
    (reduce + (range (* n 10))) ; 20% slow path
    (reduce + (range n)))) ; 90% fast path

(bench/bench (variable-work 100)
             :bench-plan bench-plans/kde-modes
             :viewer :kindly)

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
;;    validated mode counts with Silverman's test.
;;
;; 3. **Check Silverman's p-values** - Low p-values for k=1 indicate
;;    evidence against unimodality.
;;
;; 4. **Consider computation cost** - Mode analysis with Silverman's test
;;    involves multiple rounds of bootstrap resampling. Use `kde-histogram`
;;    when you only need the density curve.

;; ## Running Examples
;;
;; Execute benchmarks with KDE and mode analysis:

(comment
  ;; Basic KDE analysis (no mode testing)
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-histogram)

  ;; KDE with mode detection and Silverman's test
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-modes)

  ;; Access modes results programmatically
  (let [data (:data (bench/last-bench))]
    {:kde-bandwidth (get-in data [:kde :kdes [:elapsed-time] :bandwidth])
     :n-modes (get-in data [:modes :modes [:elapsed-time] :n-modes])
     :p-values (get-in data [:modes :modes [:elapsed-time] :silverman :p-values])})

  ;; Custom options
  (bench/bench (reduce + (range 1000))
               :bench-plan custom-kde-modes-plan)

  ;; With Portal visualization
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-modes
               :viewer :portal))
