(ns criterium.allocation-bench-notebook
  "Benchmarking with integrated allocation analysis."
  (:require
   [clojure.string :as str]
   [criterium.agent :as agent]
   [criterium.bench :as bench]
   [criterium.notebook.helpers :refer [bench-display]]
   [scicloj.kindly.v4.kind :as kind]))

;; # Allocation Analysis with bench
;;
;; The `:with-allocation-trace` option integrates allocation tracking directly
;; into the bench macro. This provides automatic collection, analysis, and
;; display of allocation data alongside timing metrics.

;; ## Agent Requirements
;;
;; Allocation tracing requires the native JVMTI agent. Check availability:

{:agent-attached? (agent/attached?)
 :agent-loaded?   (agent/loaded?)}

;; If the agent is not attached, add the JVM argument from `agent/jvm-opts`
;; when starting your REPL. See the
;; [Allocation Tracking notebook](./criterium.allocation_tracking_notebook.html)
;; for detailed setup instructions.

;; ## Basic Usage
;;
;; Add `:with-allocation-trace true` to any bench call:

^:kindly/hide-code
(bench-display
 (bench/bench (vec (range 100)) :with-allocation-trace true))

;; The output now includes three additional sections:
;; - **Allocation Summary** - Aggregate allocation statistics
;; - **Allocation Hotspots** - Top allocation sites by bytes/count
;; - **Allocations by Type** - Breakdown by object type

;; ## Understanding Allocation Summary
;;
;; The allocation summary provides aggregate statistics:

^:kindly/hide-code
(kind/table
 {:column-names ["Metric" "Description"]
  :row-vectors  [["Total Allocated" "Total bytes allocated during trace"]
                 ["Total Freed" "Bytes from objects garbage collected"]
                 ["Num Allocations" "Total object allocations"]
                 ["Num Freed" "Objects that were garbage collected"]]})

;; A high "freed" count indicates short-lived temporary objects, which may
;; signal optimization opportunities.

;; ## Interpreting Hotspots
;;
;; The hotspots section identifies where allocations originate:

^:kindly/hide-code
(bench-display
 (bench/bench (mapv str (range 50)) :with-allocation-trace true))

;; Each hotspot entry shows:
;; - **Call Site** - The file, class, method, and line triggering allocation
;; - **Count** - Number of allocations from this site
;; - **Bytes** - Total bytes allocated
;; - **Freed Count/Bytes** - How many were garbage collected

;; Hotspots are sorted by bytes allocated, making it easy to identify the
;; most memory-intensive code paths.

;; ## Analyzing by Type
;;
;; The by-type breakdown shows which object types are being created:

^:kindly/hide-code
(bench-display
 (bench/bench (into {} (map (fn [i] [(keyword (str i)) i]) (range 20)))
              :with-allocation-trace true))

;; This reveals:
;; - Primitive array allocations (e.g., `[J` for long arrays)
;; - Collection overhead (persistent data structure nodes)
;; - String allocations from conversions

;; ## Comparing Code Variants
;;
;; Allocation tracing is valuable for comparing implementation approaches.

;; ### String Building Comparison

^:kindly/hide-code
(kind/md "**Using str with apply:**")

^:kindly/hide-code
(bench-display
 (let [words ["the" "quick" "brown" "fox" "jumps"]]
   (bench/bench (apply str (interpose " " words))
                :with-allocation-trace true
                :collect-plan :one-shot)))

^:kindly/hide-code
(kind/md "**Using clojure.string/join:**")

^:kindly/hide-code
(bench-display
 (let [words ["the" "quick" "brown" "fox" "jumps"]]
   (bench/bench (str/join " " words)
                :with-allocation-trace true
                :collect-plan :one-shot)))

;; The `:one-shot` collect plan is useful for quick allocation comparisons
;; since allocation patterns are typically consistent across runs.

;; ### Collection Creation Comparison

^:kindly/hide-code
(kind/md "**Using vec on range:**")

^:kindly/hide-code
(bench-display
 (bench/bench (vec (range 100))
              :with-allocation-trace true
              :collect-plan :one-shot))

^:kindly/hide-code
(kind/md "**Using into []:**")

^:kindly/hide-code
(bench-display
 (bench/bench (into [] (range 100))
              :with-allocation-trace true
              :collect-plan :one-shot))

;; ## Graceful Degradation
;;
;; When the agent is not attached, `:with-allocation-trace` has no effect:
;; - The benchmark runs normally
;; - Timing and other metrics are collected as usual
;; - Allocation sections are simply omitted from output
;; - No errors are thrown

;; This means code using `:with-allocation-trace` works on any system,
;; with allocation data appearing only when the agent is available.

^:kindly/hide-code
(kind/code
 ";; Same code works with or without agent
(bench/bench (vec (range 100)) :with-allocation-trace true)
;; Without agent: shows timing only
;; With agent: shows timing + allocation analysis")

;; ## Using Different Viewers
;;
;; Allocation analysis works with all bench viewers.

;; ### :pprint Viewer
;;
;; Shows the raw data structures:

^:kindly/hide-code
(bench-display
 (bench/bench (vec (range 50))
              :with-allocation-trace true
              :viewer :pprint
              :collect-plan :one-shot))

;; ### :kindly Viewer
;;
;; Returns Kindly-annotated tables for Clay rendering:

(bench/bench (vec (range 50))
             :with-allocation-trace true
             :viewer :kindly
             :collect-plan :one-shot)

;; ### :portal Viewer
;;
;; Sends allocation tables to Portal for interactive exploration.
;; Ensure Portal is connected before using:

(comment
  (require '[portal.api :as p])
  (def p (p/open))
  (add-tap #'p/submit)

  (bench/bench (vec (range 100))
               :with-allocation-trace true
               :viewer :portal))

;; ## Accessing Results Programmatically
;;
;; The allocation data is available in `last-bench` results:

(do
  (bench/bench (mapv inc (range 100))
               :with-allocation-trace true
               :viewer :none)
  (let [data (:data (bench/last-bench))]
    {:has-trace?    (contains? data :allocation-trace)
     :has-summary?  (contains? data :allocation-summary)
     :has-hotspots? (contains? data :allocation-hotspots)
     :has-by-type?  (contains? data :allocation-by-type)}))

;; Access specific analysis results:

(do
  (bench/bench (vec (range 100))
               :with-allocation-trace true
               :viewer :none)
  (when-let [summary (get-in (bench/last-bench) [:data :allocation-summary])]
    {:total-allocated (:total-allocated summary)
     :num-allocations (:num-allocations summary)}))

;; ## Best Practices
;;
;; 1. **Use :one-shot for allocation comparisons** - Allocation patterns are
;;    consistent, so a single run suffices for comparing implementations.
;;
;; 2. **Focus on hotspots first** - The top allocation sites usually reveal
;;    the most impactful optimization opportunities.
;;
;; 3. **Watch the freed ratio** - High freed counts indicate temporary objects
;;    that add GC pressure.
;;
;; 4. **Compare similar workloads** - When comparing implementations, use
;;    identical input sizes for meaningful comparisons.
;;
;; 5. **Consider memory vs time tradeoffs** - Lower allocations often correlate
;;    with better performance, but not always.
