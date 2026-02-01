(ns non-parametric-analysis
  "Non-parametric analysis for benchmark samples: histogram, KDE, and mode detection."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]))

^:kindly/hide-code
(bench/set-default-viewer! :kindly)

;; # Non-Parametric Analysis
;;
;; Non-parametric analysis examines benchmark sample distributions without
;; assuming a specific shape (like normal or log-normal). This is essential
;; for benchmark data because:
;;
;; - JVM timing distributions are often skewed or multimodal
;; - Parametric assumptions can hide important details
;; - You want to see what actually happened, not what a model predicts
;;
;; Criterium provides three non-parametric techniques:
;;
;; - **Histogram** - Bin counts showing sample frequency
;; - **KDE** - Smooth density curve estimate
;; - **Mode detection** - Statistical identification of distinct peaks

;; ## When to Use Non-Parametric Analysis
;;
;; Use non-parametric analysis when:
;;
;; - You want to visualize the actual distribution shape
;; - You suspect multimodality (multiple execution time modes)
;; - Parametric summaries (mean, variance) seem misleading
;; - You need to identify distinct performance modes
;;
;; The `histogram` bench plan enables all three techniques:

(bench/bench (reduce + (vec (range 20)))
             :bench-plan bench-plans/histogram)

;; The output shows:
;;
;; - **Histogram** - Bar chart of sample counts per bin
;; - **KDE overlay** - Smooth density curve on the histogram
;; - **Mode count** - Statistically validated number of peaks
;; - **Mode locations** - Where each peak occurs with confidence intervals

;; ## Histogram Analysis
;;
;; Histograms group samples into bins and count occurrences. The key choice
;; is bin width - too narrow creates noise, too wide hides detail.

;; ### Knuth's Bayesian Binning (Default)
;;
;; Criterium defaults to Knuth's method, which uses Bayesian model selection
;; to find the optimal bin count. It balances detail against noise by
;; maximizing the posterior probability of the bin model.
;;
;; This works well for most benchmark data and adapts automatically to
;; sample size and distribution shape.

;; ### Freedman-Diaconis Method
;;
;; An alternative based on the interquartile range (IQR):
;;
;; ```
;; bin-width = 2 * IQR / n^(1/3)
;; ```
;;
;; Use Freedman-Diaconis when you want a rule-of-thumb approach or when
;; comparing with other tools that use this method:

(def fd-histogram-plan
  (-> bench-plans/histogram
      (assoc-in [:analyse 5] [:histogram {:method :freedman-diaconis}])))

(bench/bench (reduce + (vec (range 20)))
             :bench-plan fd-histogram-plan)

;; ### Configuration Options
;;
;; The `:histogram` analysis step accepts:
;;
;; | Option | Default | Description |
;; |--------|---------|-------------|
;; | `:method` | `:knuth` | Binning method (`:knuth` or `:freedman-diaconis`) |
;; | `:samples-id` | `:samples` | Source samples key |
;; | `:outliers-id` | `:outliers` | Outlier data for filtering |
;; | `:id` | `:histogram` | Output key in data map |

;; ## KDE (Kernel Density Estimation)
;;
;; KDE produces a smooth density curve by placing a kernel (small bump) at
;; each sample point and summing them. Unlike histograms, KDE doesn't depend
;; on bin boundaries and provides a continuous estimate.

;; ### Automatic Bandwidth Selection
;;
;; Criterium uses ISJ (Improved Sheather-Jones) bandwidth selection by
;; default. This data-driven method automatically finds a bandwidth that
;; balances smoothness against fidelity to the data.
;;
;; The bandwidth controls how smooth the curve is:
;;
;; - **Small bandwidth** - More detail, may show noise
;; - **Large bandwidth** - Smoother curve, may hide features

;; ### Manual Bandwidth Tuning
;;
;; When automatic selection isn't ideal, you can customize KDE options
;; in a custom bench plan. For example, to reduce grid resolution:
;;
;; ```clojure
;; (def custom-kde-plan
;;   (-> bench-plans/histogram
;;       (assoc-in [:analyse 6] [:kde {:n-points 256}])))
;; ```
;;
;; See [KDE Analysis](./criterium.kde_analysis_notebook.html) for advanced
;; bandwidth customization.

;; ### KDE Configuration Options
;;
;; | Option | Default | Description |
;; |--------|---------|-------------|
;; | `:n-points` | 512 | Grid size for density evaluation |
;; | `:n-bootstrap` | 200 | Bootstrap samples for confidence bands |
;; | `:alpha` | 0.05 | Confidence level (0.05 = 95% CI) |
;; | `:samples-id` | `:samples` | Source samples key |
;; | `:outliers-id` | `:outliers` | Outlier data for filtering |

;; ## Mode Detection
;;
;; Mode detection identifies distinct peaks in the distribution. This is
;; crucial for benchmarking because multiple modes often indicate:
;;
;; - JIT compilation tiers (fast vs slow code paths)
;; - GC pauses affecting some samples
;; - CPU frequency scaling
;; - Cache effects

;; ### The ACR Test
;;
;; Criterium uses the ACR test (Ameijeiras-Alonso, Crujeiras, Rodríguez-Casal
;; 2019) by default. The test determines if the data supports k modes by
;; computing an "excess mass" statistic and comparing against bootstrap
;; samples under the null hypothesis.
;;
;; The output shows:
;;
;; - **n-modes** - Validated mode count (smallest k where we fail to reject)
;; - **p-values** - For each k tested (low p-value rejects that mode count)
;; - **mode locations** - Where each peak occurs
;; - **significant?** - Whether each mode has statistical support

;; ### Interpreting ACR Results
;;
;; If p-values show `{1 0.02, 2 0.35}`:
;;
;; - p=0.02 for k=1: Reject unimodality (evidence for >1 mode)
;; - p=0.35 for k=2: Fail to reject (data consistent with ≤2 modes)
;; - Conclusion: n-modes = 2 (bimodal distribution)

;; ### Mode Detection Options
;;
;; | Option | Default | Description |
;; |--------|---------|-------------|
;; | `:max-modes` | 5 | Maximum modes to test |
;; | `:method` | `:acr` | Test method (`:acr` or `:silverman`) |
;; | `:mode-method` | `:isj` | How to find mode locations (`:isj` or `:critical`) |
;; | `:n-bootstrap` | 200 | Bootstrap samples for testing |

;; ### Why ACR Over Silverman?
;;
;; The older Silverman test counts modes in bootstrap samples, which can be
;; sensitive to noise. ACR uses excess mass, which is more robust to small
;; fluctuations in the density and provides better calibrated p-values.
;;
;; Use `:method :silverman` only for backward compatibility.

;; ## Outlier Filtering
;;
;; By default, outlier samples are included in histogram and KDE analysis.
;; This shows the full picture but can compress the main distribution
;; visually when outliers are far from the center.
;;
;; The `histogram` bench plan includes outlier detection (via IQR method).
;; Outliers appear in the visualization and are reported separately.
;;
;; To see how outliers affect the shape, compare runs with different
;; outlier characteristics. High outlier counts (>5%) suggest measurement
;; instability - consider increasing warmup or reducing system load.

;; ## Sample Size Guidance
;;
;; Non-parametric analysis quality depends on sample count:
;;
;; | Samples | Histogram | KDE | Mode Detection |
;; |---------|-----------|-----|----------------|
;; | <50 | Sparse, unreliable | Very rough | Not reliable |
;; | 50-200 | Usable | Reasonable | May detect strong modes |
;; | 200-500 | Good | Good | Reliable for 2-3 modes |
;; | >500 | Excellent | Excellent | Can detect subtle modes |
;;
;; Criterium's default collection aims for 200+ samples. For detailed
;; distribution analysis, consider increasing sample count with
;; `:limit-time-s` or custom collect plans.

;; ## Accessing Results Programmatically
;;
;; Use `last-bench` to access the data map after benchmarking:

(do
  (bench/bench (reduce + (vec (range 20)))
               :bench-plan bench-plans/histogram
               :viewer :none)
  (let [data (:data (bench/last-bench))]
    {:histogram-bins (get-in data [:histogram :elapsed-time :bin-count])
     :kde-bandwidth  (get-in data [:kde :kdes [:elapsed-time] :bandwidth])
     :n-modes        (get-in data [:modes :modes [:elapsed-time] :n-modes])
     :mode-locations (mapv :mode
                           (get-in data [:modes :modes [:elapsed-time] :modes]))}))

;; ## Summary
;;
;; - Use the `histogram` bench plan for distribution analysis
;; - **Histogram**: Knuth's method (default) adapts bin count automatically
;; - **KDE**: ISJ bandwidth selection works well for most data
;; - **Mode detection**: ACR test identifies statistically significant peaks
;; - More samples = more reliable analysis (aim for 200+)
;;
;; For advanced customization of analysis steps, see
;; [Analysis and View Options](./analysis_and_view_options.html).
;;
;; For the full technical details on KDE and mode testing methods, see
;; [KDE Analysis](./criterium.kde_analysis_notebook.html).

^:kindly/hide-code
(bench/set-default-viewer! :print)
