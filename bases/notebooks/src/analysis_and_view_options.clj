(ns analysis-and-view-options
  "Understanding how to configure analysis and view options in criterium."
  (:require
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.notebook.helpers :refer [bench-display]]
   [scicloj.kindly.v4.kind :as kind]))

;; # Analysis and View Options
;;
;; Criterium's benchmarking pipeline has three stages: **collection**, **analysis**,
;; and **viewing**. This notebook explains how to configure the analysis and view
;; stages to customize what statistics are computed and how results are displayed.
;;
;; For predefined bench plans that bundle common configurations, see
;; [Bench Options](./criterium.bench_options_notebook.html).

;; ## The Data-Map Model
;;
;; Benchmark results flow through the pipeline as a **data map**. Each analysis
;; step reads from the map, computes results, and adds them back under a new key:

;; ```
;; {:samples {...}}           ; After collection
;;        |
;;        v (transform-log)
;; {:samples {...}
;;  :log-samples {...}}       ; Added log-transformed samples
;;        |
;;        v (quantiles)
;; {:samples {...}
;;  :log-samples {...}
;;  :quantiles {...}}         ; Added quantile analysis
;;        |
;;        v (outliers)
;; {:samples {...}
;;  :log-samples {...}
;;  :quantiles {...}
;;  :outliers {...}}          ; Added outlier detection
;;        |
;;        v (stats)
;; {:samples {...}
;;  :log-samples {...}
;;  :quantiles {...}
;;  :outliers {...}
;;  :stats {...}}             ; Added statistics
;; ```

;; Each analysis function is independent - it reads specific keys and writes to
;; a specific output key. This makes the pipeline composable.

;; ### Viewing the Data Map
;;
;; Use `:viewer :pprint` to see the actual data structure:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))
              :viewer :pprint
              :collect-plan :one-shot
              :analyse [:event-stats]
              :view []))

;; The top-level keys show what's available. The `:samples` key contains the
;; raw measurements; other keys are added by analysis steps.

;; ## The ID Scheme
;;
;; Analysis steps use **IDs** to reference data in the map. Every analysis
;; function accepts options that control where it reads from and writes to:
;;
;; | Option | Purpose | Default |
;; |--------|---------|---------|
;; | `:id` | Key where results are stored | Analysis-specific (e.g., `:stats`) |
;; | `:samples-id` | Key for source samples | `:samples` |
;; | `:outliers-id` | Key for outlier data | `:outliers` |
;; | `:quantiles-id` | Key for quantile data | `:quantiles` |
;;
;; This scheme enables running the same analysis multiple times with different
;; inputs or outputs.

;; ### Example: Two Stats Analyses
;;
;; The default bench plan computes statistics twice - once on raw samples and
;; once on log-transformed samples:

(kind/code
 "[:stats {}]                              ; Reads :samples, writes :stats
[:stats {:samples-id :log-samples        ; Reads :log-samples
         :id :log-stats}]                ; Writes :log-stats")

;; Both produce statistics, but from different sample sets. The histogram view
;; then uses `:log-stats` for better visualization of timing data.

;; ## Argument Specification Syntax
;;
;; Analysis and view steps can be specified in two forms:
;;
;; **Keyword form** - uses all defaults:

(kind/code ":stats        ; Same as [:stats {}]
:outliers    ; Same as [:outliers {}]
:histogram   ; Same as [:histogram {}]")

;; **Vector form** - provides options:

(kind/code "[:stats {:samples-id :log-samples :id :log-stats}]
[:quantiles {:quantiles [0.5 0.9 0.99]}]
[:outliers {:outlier-method :standard}]")

;; The vector form is `[keyword options-map]`. Use it when you need to:
;; - Change which data the analysis reads from
;; - Change where results are stored
;; - Configure analysis-specific options

;; ## Analysis Functions
;;
;; Here are the commonly used analysis functions and their key options:

(kind/table
 {:column-names ["Analysis" "Key Options" "Purpose"]
  :row-vectors
  [[:transform-log ":id, :samples-id, :metric-ids" "Log-transform samples for visualization"]
   [:quantiles ":id, :samples-id, :quantiles" "Compute percentiles (default: 0.25, 0.5, 0.75)"]
   [:outliers ":id, :samples-id, :quantiles-id, :outlier-method" "Detect outliers via IQR"]
   [:stats ":id, :samples-id, :outliers-id" "Compute mean, variance, bounds"]
   [:histogram ":id, :samples-id, :outliers-id, :method" "Build histogram (:freedman-diaconis or :knuth)"]
   [:bootstrap-stats ":id, :samples-id, :outliers-id, :quantiles" "Bootstrap confidence intervals"]
   [:kde ":id, :samples-id, :outliers-id, :n-points" "Kernel density estimation"]
   [:modes ":id, :kde-id, :samples-id, :max-modes" "Detect distribution modes"]
   [:distribution-fit ":id, :samples-id, :outliers-id, :distributions" "Fit parametric distributions"]
   [:tail-analysis ":id, :samples-id, :threshold-quantile" "Extreme value analysis"]]})

;; ## View Functions
;;
;; View functions format analysis results for display. They also use the ID
;; scheme to reference the correct analysis output:

(kind/table
 {:column-names ["View" "Key Options" "Displays"]
  :row-vectors
  [[:stats ":stats-id, :metric-ids" "Mean with confidence interval"]
   [:bootstrap-stats ":bootstrap-stats-id" "Bootstrap CI for mean"]
   [:quantiles ":quantiles-id, :metric-ids" "Percentile values"]
   [:outlier-counts ":outliers-id, :show-medcouple" "Outlier counts by severity"]
   [:histogram ":histogram-id, :stats-id" "Sample distribution histogram"]
   [:samples ":samples-id" "Raw sample values"]
   [:extremes ":stats-id" "Min/max values"]
   [:collect-plan "" "Collection parameters"]]})

;; ## Building a Custom Bench Plan
;;
;; A bench plan is a map with `:analyse` and `:view` vectors. Let's build one
;; step by step.

;; ### Step 1: Define the Analysis Pipeline
;;
;; Start with what you want to compute. For a simple timing analysis:

(def my-analyse
  [:transform-log                              ; Log-transform for visualization
   [:quantiles {:quantiles [0.5 0.9 0.99]}]    ; Median, p90, p99
   :outliers                                   ; Detect outliers
   [:stats {}]                                 ; Stats on raw samples
   [:stats {:samples-id :log-samples           ; Stats on log samples
            :id :log-stats}]
   :histogram])                                ; For visualization

;; The order matters - each step may depend on previous ones:
;; - `:outliers` requires `:quantiles` to exist
;; - `:stats` can optionally filter using `:outliers`
;; - `:histogram` can optionally filter using `:outliers`

;; ### Step 2: Define the View Pipeline
;;
;; Choose what to display and connect views to the right analysis outputs:

(def my-view
  [[:stats {:metric-ids [:elapsed-time]}]      ; Show timing stats
   [:quantiles {:metric-ids [:elapsed-time]}]  ; Show percentiles
   :outlier-counts                             ; Show outlier summary
   [:histogram {:stats-id :log-stats}]         ; Use log-scale stats for overlay
   :collect-plan])                             ; Show collection parameters

;; ### Step 3: Assemble the Bench Plan

(def my-bench-plan
  {:collector-config bench-plans/default-collector-config
   :analyse my-analyse
   :view my-view
   :viewer :print})

;; ### Step 4: Use the Bench Plan

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :bench-plan my-bench-plan))

;; ## Inline Configuration
;;
;; You can also specify `:analyse` and `:view` directly in the `bench` call
;; without creating a separate bench plan:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 1000))
              :analyse [:transform-log
                        [:quantiles {:quantiles [0.5 0.95 0.99]}]
                        :outliers
                        [:stats {}]
                        :histogram]
              :view [[:stats {:metric-ids [:elapsed-time]}]
                     :quantiles
                     :outlier-counts
                     [:histogram {}]]))

;; ## Programmatic Access via last-bench
;;
;; After running a benchmark, use `last-bench` to access the full data map:

(do
  (bench/bench (reduce + (range 1000))
               :limit-time-s 2
               :analyse [:transform-log
                         [:quantiles {:quantiles [0.5 0.9 0.99]}]
                         :outliers
                         [:stats {}]]
               :view [])
  (let [data-map (:data (bench/last-bench))]
    {:mean (get-in data-map [:stats :elapsed-time :mean])
     :p99 (get-in data-map [:quantiles :elapsed-time 0.99])}))

;; The data map contains all analysis results keyed by their IDs. You can
;; access any computed value using standard Clojure map navigation.
;;
;; For in-situ measurement, see the [In-Situ](./in_situ.html) notebook which
;; shows how `instrument-fn` and `sampled-fn` produce compatible data maps.

;; ## Summary
;;
;; - The data map flows through analysis steps, accumulating results
;; - IDs (`:samples-id`, `:stats-id`, etc.) connect steps together
;; - Use keyword form for defaults, vector form for customization
;; - Build custom bench plans by composing `:analyse` and `:view` vectors
;; - For predefined configurations, see [Bench Options](./criterium.bench_options_notebook.html)
