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
;; The `kde-histogram` bench plan includes both histogram and KDE analysis.
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
;; - Detected modes with confidence intervals

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

;; ### Modes
;;
;; Modes are the peaks of the density curve, representing the most likely
;; execution times. Each mode includes:
;; - Location: the x-value of the peak
;; - Density: the height of the peak
;; - Confidence interval: the range within which the mode location is estimated

;; ## Accessing KDE Results Programmatically
;;
;; Use `last-bench` to access the full KDE results:

(do
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-histogram
               :viewer :none)
  (let [data (:data (bench/last-bench))
        kde-result (:kde data)]
    {:bandwidth (get-in kde-result [:kdes [:elapsed-time] :bandwidth])
     :n-modes (count (get-in kde-result [:kdes [:elapsed-time] :modes]))
     :modes (get-in kde-result [:kdes [:elapsed-time] :modes])}))

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

;; ### n-modes
;;
;; Maximum number of modes to detect. Default is 5. Set higher if you
;; expect more distinct performance modes.

;; ### alpha
;;
;; Confidence level for intervals. Default is 0.05 (95% confidence).

;; ### Custom Bench Plan Example
;;
;; Create a custom bench plan with modified KDE options:

(def custom-kde-plan
  (-> bench-plans/kde-histogram
      (assoc :analyse
             [:transform-log
              [:quantiles {:quantiles [0.9 0.99 0.99]}]
              :outliers
              [:stats {}]
              [:stats {:samples-id :log-samples :id :log-stats}]
              :histogram
              [:kde {:n-points 256 :n-bootstrap 100 :n-modes 3}]
              :event-stats])))

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan custom-kde-plan))

;; ## Visualization with Portal and Kindly
;;
;; The `:portal` and `:kindly` viewers render the KDE as a Vega-Lite chart:
;; - Density curve overlaid on histogram bars
;; - Shaded confidence bands
;; - Mode markers with horizontal confidence interval lines

;; ### Portal Viewer
;;
;; To use with Portal, ensure Portal is connected:

(kind/code
 "(require '[portal.api :as p])
(def portal (p/open))
(add-tap #'p/submit)

(bench/bench (reduce + (range 1000))
             :bench-plan bench-plans/kde-histogram
             :viewer :portal)")

;; ### Kindly Viewer
;;
;; For Clay notebooks, use `:viewer :kindly` to get embedded charts:

(bench/bench (reduce + (range 1000))
             :bench-plan bench-plans/kde-histogram
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
;; When KDE detects multiple modes, investigate the cause to understand
;; your benchmark's behavior.

;; ### Example: Synthetic Multimodal Distribution
;;
;; This artificial example shows how multimodality appears:

(defn variable-work
  "Simulate variable-time work with occasional slow paths."
  [n]
  (if (zero? (mod (rand-int 100) 10))
    (reduce + (range (* n 10))) ; 10% slow path
    (reduce + (range n)))) ; 90% fast path

^:kindly/hide-code
(bench-display
 (bench/bench (variable-work 100)
              :bench-plan bench-plans/kde-histogram))

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
;; 1. **Use KDE when you need mode detection** - The automatic mode finding
;;    with confidence intervals is valuable for identifying performance modes.
;;
;; 2. **Combine with histograms** - The `kde-histogram` plan overlays KDE
;;    on histograms, giving you both discrete bin counts and smooth density.
;;
;; 3. **Check for multimodality** - Multiple modes may indicate that your
;;    benchmark is measuring different code paths or JIT states.
;;
;; 4. **Consider computation cost** - KDE with bootstrap confidence bands
;;    takes longer than basic statistics. Use the default plan for quick
;;    benchmarks; use `kde-histogram` when you need density analysis.

;; ## Running Examples
;;
;; Execute benchmarks with KDE analysis:

(comment
  ;; Basic KDE analysis
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-histogram)

  ;; Access KDE results programmatically
  (let [data (:data (bench/last-bench))]
    (get-in data [:kde :kdes [:elapsed-time] :modes]))

  ;; Custom KDE options
  (bench/bench (reduce + (range 1000))
               :bench-plan custom-kde-plan)

  ;; With Portal visualization
  (bench/bench (reduce + (range 1000))
               :bench-plan bench-plans/kde-histogram
               :viewer :portal))
