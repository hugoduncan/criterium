(ns criterium.call-graph
  "Primary API for call tracing with call graph display.

  Provides functions and macros for tracing method calls during expression
  evaluation and displaying the resulting call graph.

  This is a standalone tracing feature (not integrated with benchmark
  collection), providing visibility into what code paths are exercised.

  Primary API:
  - bench             - Macro for tracing expressions and displaying call graphs
  - last-bench        - Access results from most recent trace
  - set-default-viewer! - Set default output viewer
  - default-viewer    - Get current default viewer

  Filter Functions (re-exported from criterium.agent):
  - filter-call-tree  - Apply filters to call tree data
  - jdk-filter        - Predefined filter excluding JDK packages
  - clojure-core-boundary-filter - Filter stopping at clojure.core boundary

  Example:
  (bench (my-function args))                    ; Basic usage
  (bench (my-function args) :viewer :portal)    ; With Portal visualization

  (let [results (last-bench)]
    (filter-call-tree (:call-tree (:data results)) jdk-filter))"
  (:require
   [criterium.agent :as agent]
   [criterium.benchmark :as benchmark]
   [criterium.call-graph.plans :as plans]
   [criterium.viewer.call-graph]
   [criterium.viewer.kindly]
   [criterium.viewer.portal]
   [criterium.viewer.pprint]
   [criterium.viewer.print]))

;;; Configuration

(def ^:dynamic *default-viewer*
  "Dynamic var for the default viewer to use when no explicit :viewer option
  is provided. Initial value is :print.

  This can be bound dynamically or altered via set-default-viewer!.

  Precedence (highest to lowest):
  1. Explicit :viewer option on bench call
  2. This dynamic default viewer
  3. Hard-coded fallback :print (if this var is nil)"
  :print)

(defn default-viewer
  "Returns the current default viewer.

  The default viewer is used when no explicit :viewer option is provided
  to bench calls. Initial value is :print."
  []
  *default-viewer*)

(defn set-default-viewer!
  "Set the default viewer for all bench calls that don't specify an explicit
  :viewer option.

  viewer - Keyword identifying the viewer,
           e.g. :print, :pprint, :portal, :kindly

  Example:
    (set-default-viewer! :kindly)
    (bench (my-function args))  ; Now uses :kindly viewer by default"
  [viewer]
  (alter-var-root #'*default-viewer* (constantly viewer)))

;;; Last Bench Storage

(def ^:private last-bench* (volatile! nil))

(defn last-bench
  "Returns the complete data from the most recent call trace.

  The returned data structure contains the call tree and configuration:
  {:data {:call-tree <nested-map>}
   :viewer <keyword>
   :analyse <vector>
   :view <vector>}

  Returns nil if no traces have been run in the current session.

  Example:
  (bench (my-function args))
  (let [results (last-bench)]
    (:call-tree (:data results)))"
  []
  @last-bench*)

(defn- last-bench!
  "Store the results of the last call trace execution."
  [results]
  (vreset! last-bench* results))

;;; Re-exported Filter Functions

(def filter-call-tree
  "Filter a call tree according to filter options.

  Options:
  - :exclude-packages - Set of package prefixes to exclude entirely.
    Nodes with matching classes are removed, their children promoted up.
  - :stop-at-packages - Set of package prefixes where traversal stops.
    Matching nodes are kept but their children are truncated.
  - :max-depth - Maximum depth to include
                 (1 = root only, 2 = root + children, etc.)

  Returns the filtered call tree, or nil if the root is excluded.

  Example:
  (filter-call-tree call-tree {:exclude-packages #{\"java.\" \"javax.\"}})"
  agent/filter-call-tree)

(def jdk-filter
  "Filter that excludes JDK internal packages.
  Use with filter-call-tree to remove JDK implementation details.

  Example:
  (filter-call-tree call-tree jdk-filter)"
  agent/jdk-filter)

(def clojure-core-boundary-filter
  "Filter that stops traversal at clojure.core and clojure.lang boundaries.
  Shows calls into Clojure core but not the internal implementation.

  Example:
  (filter-call-tree call-tree clojure-core-boundary-filter)"
  agent/clojure-core-boundary-filter)

;;; Plan Configuration

(defn options->call-graph-plan
  "Convert option arguments into a call-graph plan.

  Options:
  - :viewer  - Output format [:print, :pprint, :portal, :kindly]
  - :analyse - Vector of analysis steps (default: [:most-called])
  - :view    - Vector of view components
               (default: [:call-tree :call-flame :most-called])
  - :limit   - Maximum methods for most-called analysis (default: 20)

  Returns a plan map with :viewer, :analyse, :view, and :limit keys."
  [& {:as options}]
  (let [viewer (or (:viewer options) *default-viewer* :print)]
    {:viewer viewer
     :analyse (or (:analyse options) (:analyse plans/default))
     :view (or (:view options) (:view plans/default))
     :limit (or (:limit options) 20)}))

;;; Core Implementation

(defn- expand-analyse-spec
  "Expand an analysis spec with plan options.
  For :most-called, adds the :limit option from the plan."
  [spec plan]
  (if (= spec :most-called)
    (if-let [limit (:limit plan)]
      [:most-called {:limit limit}]
      spec)
    spec))

(defn bench-call-graph
  "Trace method calls during execution of a function and display the call graph.

  Takes a call-graph-plan and a zero-argument function to trace.

  The plan specifies:
  - :viewer  - Output format
  - :analyse - Analysis steps to apply
  - :view    - View components to display
  - :limit   - Maximum methods for most-called analysis (default: 20)

  Returns the value from calling f.
  The complete trace data is available via (last-bench)."
  [call-graph-plan f]
  (let [[call-tree result] (agent/with-call-tracing (f))
        ;; Build data map
        data-map {:call-tree call-tree}
        ;; Expand analysis specs with plan options
        analyse-specs (mapv #(expand-analyse-spec % call-graph-plan)
                            (:analyse call-graph-plan))
        ;; Apply analysis pipeline
        analyse-fn (benchmark/->analyse analyse-specs)
        data-map (if analyse-fn
                   (analyse-fn data-map)
                   data-map)
        ;; Apply view pipeline
        view-fn (benchmark/->view (:view call-graph-plan))
        viewer (:viewer call-graph-plan)
        _ (when view-fn
            (view-fn viewer data-map))
        ;; Store results
        results (assoc call-graph-plan :data data-map)]
    (last-bench! results)
    result))

(defmacro bench
  "Trace method calls during expression evaluation and display the call graph.

  Intended for use at the REPL to understand what code paths are exercised.

  Takes an expression to trace, and optional configuration options.

  Parameters:
    expr    - Expression to trace (may reference local bindings)
    options - Keyword/value pairs for configuration:
      :viewer  - Output format [:print, :pprint, :portal, :kindly]
                 Default can be set via (set-default-viewer! :kindly)
      :analyse - Vector of analysis steps (default: [:most-called])
      :view    - Vector of view components
                 (default: [:call-tree :call-flame :most-called])
      :limit   - Maximum methods for most-called analysis (default: 20)

  Returns:
  The value from evaluating the expression.
  Complete trace data available via (last-bench).

  Examples:
  ;; Basic usage
  (bench (reduce + (range 100)))

  ;; With Portal visualization
  (bench (my-function args) :viewer :portal)

  ;; With only tree view (no flame chart)
  (bench (my-function args) :view [:call-tree])

  ;; Access results after tracing
  (bench (my-function args))
  (-> (last-bench) :data :call-tree (filter-call-tree jdk-filter))

  Notes:
  - Method tracing has significant overhead; use for profiling, not production
  - The expression is evaluated twice: once for warmup, once while tracing
  - The call tree represents aggregated calls across all threads
  - If the agent is not attached, returns the expression value with nil
    call-tree"
  [expr & options]
  (let [options-map (apply hash-map options)]
    `(bench-call-graph
      (options->call-graph-plan ~options-map)
      (fn [] ~expr))))
