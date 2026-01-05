(ns criterium.call-tracing-notebook
  "Method call tracing with criterium's native agent."
  (:require
   [clojure.string :as str]
   [criterium.agent :as agent]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.kindly]
   [scicloj.kindly.v4.kind :as kind]))

;; # Call Tracing
;;
;; Criterium's native agent can trace method calls during code execution,
;; building a hierarchical call graph showing which methods call which.

;; ## When to Use Call Tracing
;;
;; Use call tracing when you want to:
;; - Understand call patterns in code execution
;; - Identify frequently called methods
;; - Visualize call hierarchies
;; - Debug unexpected code paths

;; ## Agent Requirements
;;
;; Call tracing uses the same native JVMTI agent as allocation tracking.
;;
;; Load the agent at JVM startup with -agentpath:

(kind/code "clojure -J-agentpath:/path/to/libcriterium.dylib -M:dev")

;; Use `(agent/jvm-opts)` to get the correct path for your platform.
;;
;; Check if the agent is currently available:

{:attached? (agent/attached?)}

;; ## Basic Usage
;;
;; The `with-call-tracing` macro captures method calls during execution
;; of its body. It returns a vector of `[call-tree result]`:

(let [[call-tree result] (agent/with-call-tracing
                           (reduce + (range 100)))]
  {:result           result
   :has-call-tree?   (some? call-tree)
   :agent-available? (agent/attached?)})

;; When the agent is unavailable, the macro gracefully degrades:
;; - The body still executes normally
;; - `call-tree` is nil instead of a map
;; - No error is thrown

;; ## Understanding the Call Tree
;;
;; Each node in the call tree is a map containing:

^:kindly/hide-code
(kind/table
 {:column-names ["Key" "Description"]
  :row-vectors  [[":class" "Fully qualified class name"]
                 [":method" "Method name"]
                 [":file" "Source file name (may be nil)"]
                 [":line" "Line number (-1 if unknown)"]
                 [":call-count" "Times this call path was executed"]
                 [":children" "Vector of child call nodes"]]})

;; An example - tracing a simple computation:

(let [[call-tree _] (agent/with-call-tracing
                      (mapv inc (range 5)))]
  (when call-tree
    {:class      (:class call-tree)
     :method     (:method call-tree)
     :call-count (:call-count call-tree)
     :children   (count (:children call-tree))}))

;; ## Viewing Call Trees
;;
;; ### Text Tree (`:print` viewer)
;;
;; The text viewer renders the call tree as an ASCII tree with box-drawing
;; characters. Each node shows the class.method, call count, and percentage
;; of total calls.

(let [[call-tree _] (agent/with-call-tracing
                      (reduce + (map #(* % %) (range 10))))]
  (when call-tree
    (println (call-graph/render-call-tree call-tree))))

;; ### Visual Tree (`:kindly` viewer)
;;
;; The Kindly viewer generates interactive Vega charts showing the call
;; hierarchy as a tree diagram and flame chart.

(let [[call-tree _] (agent/with-call-tracing
                      (reduce + (map #(* % %) (range 10))))]
  (when call-tree
    ((view/call-tree) :kindly {:call-tree call-tree})))

;; ## Filtering Call Trees
;;
;; Raw call trees can be noisy with JDK internals and Clojure runtime
;; details. Use `filter-call-tree` to focus on relevant code.

;; ### Available Filter Options

^:kindly/hide-code
(kind/table
 {:column-names ["Option" "Description"]
  :row-vectors  [[":exclude-packages" "Set of package prefixes to exclude entirely"]
                 [":stop-at-packages" "Packages where traversal stops (keeps node, removes children)"]
                 [":max-depth" "Maximum depth to include (1 = root only)"]]})

;; ### Predefined Filters
;;
;; Criterium provides common filters:

;; **`jdk-filter`** - Excludes JDK internal packages:

agent/jdk-filter

;; **`clojure-core-boundary-filter`** - Shows calls into Clojure core
;; but not its internal implementation:

agent/clojure-core-boundary-filter

;; ### Applying Filters

(let [[call-tree _] (agent/with-call-tracing
                      (reduce + (map #(* % %) (range 10))))]
  (when call-tree
    (let [filtered (agent/filter-call-tree call-tree agent/jdk-filter)]
      {:original-children (count (:children call-tree))
       :filtered-children (count (:children filtered))})))

;; ### Custom Filters
;;
;; Create custom filters by specifying package prefixes:

(def my-filter
  {:exclude-packages #{"java." "javax." "jdk." "sun." "com.sun."}
   :stop-at-packages #{"clojure.core" "clojure.lang."}
   :max-depth 5})

(let [[call-tree _] (agent/with-call-tracing
                      (reduce + (map #(* % %) (range 10))))]
  (when call-tree
    (let [filtered (agent/filter-call-tree call-tree my-filter)]
      (println (call-graph/render-call-tree filtered)))))

;; ### Combining Filters
;;
;; Merge filter maps to combine their effects:

(let [combined (merge agent/jdk-filter
                      agent/clojure-core-boundary-filter
                      {:max-depth 10})]
  combined)

;; ## Realistic Example
;;
;; Tracing a more complex operation - string manipulation:

(defn process-data
  "Process a collection of items with transformations."
  [items]
  (->> items
       (map str)
       (filter #(> (count %) 1))
       (map str/upper-case)
       (into [])))

(let [[call-tree result] (agent/with-call-tracing
                           (process-data (range 20)))]
  (when call-tree
    (let [filtered (agent/filter-call-tree
                    call-tree
                    (merge agent/jdk-filter {:max-depth 8}))]
      {:result result
       :tree   (call-graph/render-call-tree filtered)})))

;; ## Performance Considerations
;;
;; **Warning:** Method tracing has significant overhead because it captures
;; every method entry and exit. Use it for:
;; - Development and debugging
;; - Understanding code structure
;; - Profiling specific code paths
;;
;; Do NOT use it for:
;; - Production benchmarks
;; - Performance-critical code
;; - Long-running operations

;; ## Graceful Degradation
;;
;; When the agent cannot be loaded (unsupported platform, permission
;; issues, etc.), `with-call-tracing` returns nil for the call tree
;; but still executes the body:

(let [[call-tree result] (agent/with-call-tracing
                           (* 6 7))]
  {:result result
   :call-tree-available? (some? call-tree)})
