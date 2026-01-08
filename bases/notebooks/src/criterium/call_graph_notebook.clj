(ns criterium.call-graph-notebook
  "Call graph analysis with criterium.call-graph."
  (:require
   [criterium.call-graph :as call-graph]
   [criterium.call-graph.plans :as plans]
   [scicloj.kindly.v4.kind :as kind]))

;; # Call Graph Analysis
;;
;; The `criterium.call-graph` namespace provides a high-level API for tracing
;; method calls and visualizing call patterns. This is the recommended API
;; for understanding what code paths execute during expression evaluation.

;; ## When to Use Call Graphs
;;
;; Call graphs help you:
;; - Understand which methods are called during execution
;; - Identify frequently called methods (hot spots)
;; - Visualize call hierarchies with tree diagrams
;; - Explore call patterns with flame charts

;; ## Agent Requirements
;;
;; Call tracing requires the native JVMTI agent. The agent loads automatically
;; when available, or can be loaded explicitly at JVM startup:

(kind/code "clojure -J-agentpath:/path/to/libcriterium.dylib -M:dev")

;; Check if the agent is available:

(do
  (require 'criterium.agent)
  {:agent-attached? ((resolve 'criterium.agent/attached?))})

;; ## Basic Usage
;;
;; The `bench` macro traces method calls during expression evaluation
;; and displays the results:

(comment
  ;; Trace a simple computation
  (call-graph/bench (reduce + (range 100))))

;; The output includes:
;; - **Call tree**: Hierarchical view of method calls with counts
;; - **Flame chart**: Width represents call count
;; - **Most-called**: Table of methods sorted by call count

;; ## Viewing Options
;;
;; The `:viewer` option controls output format:

^:kindly/hide-code
(kind/table
 {:column-names ["Viewer" "Description"]
  :row-vectors [[":print" "ASCII text output (default)"]
                [":pprint" "Pretty-printed data structures"]
                [":portal" "Interactive Portal visualizations"]
                [":kindly" "Kindly-annotated visualizations"]]})

;; Set a different viewer:

(comment
  ;; Use Portal for interactive visualizations
  (call-graph/bench (reduce + (range 100)) :viewer :portal)

  ;; Use kindly for Clay notebooks
  (call-graph/bench (reduce + (range 100)) :viewer :kindly))

;; Set the default viewer for all subsequent calls:

(comment
  (call-graph/set-default-viewer! :kindly)
  (call-graph/bench (reduce + (range 100))) ; Uses :kindly
  (call-graph/default-viewer)) ; Returns :kindly

;; ## Customizing Views
;;
;; Control which views are displayed with the `:view` option:

(comment
  ;; Show only the call tree (no flame chart or most-called)
  (call-graph/bench (reduce + (range 100))
                    :view [:call-tree])

  ;; Show flame chart and most-called (no tree)
  (call-graph/bench (reduce + (range 100))
                    :view [:call-flame :most-called]))

;; Available view components:

^:kindly/hide-code
(kind/table
 {:column-names ["View" "Description"]
  :row-vectors [[":call-tree" "Hierarchical tree diagram"]
                [":call-flame" "Flame chart (width = call count)"]
                [":most-called" "Table/chart of most called methods"]]})

;; ## Using Plans
;;
;; Pre-configured plans simplify common use cases:

^:kindly/hide-code
(kind/table
 {:column-names ["Plan" "Views" "Analysis"]
  :row-vectors [["plans/default" ":call-tree, :call-flame, :most-called" ":most-called"]
                ["plans/tree-only" ":call-tree" "none"]
                ["plans/flame-only" ":call-flame" "none"]
                ["plans/most-called-only" ":most-called" ":most-called"]]})

;; Use a plan directly:

(comment
  ;; Show only the tree
  (call-graph/bench (reduce + (range 100))
                    :analyse (:analyse plans/tree-only)
                    :view (:view plans/tree-only))

  ;; Show only most-called methods
  (call-graph/bench (reduce + (range 100))
                    :analyse (:analyse plans/most-called-only)
                    :view (:view plans/most-called-only)))

;; ## Accessing Results
;;
;; Use `last-bench` to access results from the most recent trace:

(comment
  (call-graph/bench (reduce + (range 100)))

  ;; Get the complete results
  (call-graph/last-bench)

  ;; Access the raw call tree
  (-> (call-graph/last-bench) :data :call-tree)

  ;; Access most-called analysis
  (-> (call-graph/last-bench) :data :most-called :methods))

;; ## Filtering Call Trees
;;
;; Raw call trees include JDK internals and Clojure runtime details.
;; Use filters to focus on relevant code.

;; ### Predefined Filters

^:kindly/hide-code
(kind/table
 {:column-names ["Filter" "Effect"]
  :row-vectors [["jdk-filter" "Excludes java.*, javax.*, jdk.*, sun.*, com.sun.*"]
                ["clojure-core-boundary-filter" "Stops at clojure.core and clojure.lang."]]})

;; Access predefined filters:

call-graph/jdk-filter

call-graph/clojure-core-boundary-filter

;; ### Applying Filters

(comment
  (call-graph/bench (reduce + (range 100)))

  ;; Filter the call tree from last-bench
  (let [call-tree (-> (call-graph/last-bench) :data :call-tree)]
    (call-graph/filter-call-tree call-tree call-graph/jdk-filter)))

;; ### Custom Filters
;;
;; Create custom filters with these options:

^:kindly/hide-code
(kind/table
 {:column-names ["Option" "Type" "Effect"]
  :row-vectors [[":exclude-packages" "Set of strings" "Remove matching nodes, promote children"]
                [":stop-at-packages" "Set of strings" "Keep node, truncate children"]
                [":max-depth" "Integer" "Limit tree depth (1 = root only)"]]})

;; Example custom filter:

(def my-filter
  {:exclude-packages #{"java." "javax." "jdk." "sun." "com.sun."}
   :stop-at-packages #{"clojure.core" "clojure.lang."}
   :max-depth 10})

(comment
  (call-graph/bench (reduce + (range 100)))
  (let [call-tree (-> (call-graph/last-bench) :data :call-tree)]
    (call-graph/filter-call-tree call-tree my-filter)))

;; ### Combining Filters
;;
;; Merge filter maps to combine effects:

(def combined-filter
  (merge call-graph/jdk-filter
         call-graph/clojure-core-boundary-filter
         {:max-depth 8}))

combined-filter

;; ## Realistic Example
;;
;; Trace a more complex computation:

(defn process-data
  "Process items with multiple transformations."
  [items]
  (->> items
       (filter even?)
       (map #(* % %))
       (take 10)
       (reduce +)))

(comment
  ;; Trace the computation
  (call-graph/bench (process-data (range 100)))

  ;; Get results and filter
  (let [call-tree (-> (call-graph/last-bench) :data :call-tree)
        filtered (call-graph/filter-call-tree
                  call-tree
                  (merge call-graph/jdk-filter {:max-depth 5}))]
    {:original-depth (count (:children call-tree))
     :filtered-depth (count (:children filtered))}))

;; ## Most-Called Analysis
;;
;; The `:most-called` analysis aggregates methods by total call count.
;; Configure the limit:

(comment
  ;; Show top 10 most-called methods (default is 20)
  (call-graph/bench (reduce + (range 100)) :limit 10))

;; Access most-called data programmatically:

(comment
  (call-graph/bench (reduce + (range 100)))
  (let [most-called (-> (call-graph/last-bench) :data :most-called)]
    {:type (:type most-called)
     :method-count (count (:methods most-called))
     :top-method (first (:methods most-called))}))

;; ## Understanding the Call Tree Structure
;;
;; Each node in the call tree is a map:

^:kindly/hide-code
(kind/table
 {:column-names ["Key" "Type" "Description"]
  :row-vectors [[":class" "String" "Fully qualified class name"]
                [":method" "String" "Method name"]
                [":file" "String" "Source file (may be nil)"]
                [":line" "Integer" "Line number (-1 if unknown)"]
                [":call-count" "Integer" "Times this call path executed"]
                [":children" "Vector" "Child call nodes"]]})

;; ## Interpreting Results
;;
;; ### Call Tree
;; - Parent-child relationships show which methods call which
;; - Call counts at each level reflect calls from that specific parent
;; - Same method may appear multiple times with different parents
;;
;; ### Flame Chart
;; - Horizontal width represents proportion of total calls
;; - Stacked bars show call hierarchy
;; - Hover for method names and counts
;;
;; ### Most-Called
;; - Shows methods aggregated across all call sites
;; - Useful for identifying hot methods regardless of caller

;; ## Performance Notes
;;
;; Method tracing has significant overhead because it captures every method
;; entry and exit. Use call graphs for:
;; - Understanding code structure during development
;; - Identifying hot code paths
;; - Debugging unexpected behavior
;;
;; Do NOT use call graphs for:
;; - Production performance measurements
;; - Timing-sensitive benchmarks
;; - Long-running operations

;; ## Comparison with criterium.bench
;;
;; `criterium.bench/bench` measures execution time with statistical rigor.
;; `criterium.call-graph/bench` traces method calls for structural analysis.
;;
;; Use `criterium.bench` when you need:
;; - Accurate timing measurements
;; - Statistical confidence intervals
;; - Performance regression testing
;;
;; Use `criterium.call-graph` when you need:
;; - Understanding call patterns
;; - Identifying frequently called methods
;; - Visualizing code structure

;; ## Quick Reference
;;
;; ```clojure
;; ;; Basic usage
;; (call-graph/bench (my-function args))
;;
;; ;; With viewer
;; (call-graph/bench (my-function args) :viewer :portal)
;;
;; ;; Custom views
;; (call-graph/bench (my-function args) :view [:call-tree])
;;
;; ;; Limit most-called
;; (call-graph/bench (my-function args) :limit 10)
;;
;; ;; Access results
;; (call-graph/last-bench)
;;
;; ;; Filter results
;; (let [tree (-> (call-graph/last-bench) :data :call-tree)]
;;   (call-graph/filter-call-tree tree call-graph/jdk-filter))
;; ```
