(ns criterium.allocation-tracking-notebook
  "Memory allocation analysis with criterium's native agent."
  (:require
   [clojure.string :as str]
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]
   [scicloj.kindly.v4.kind :as kind]))

;; # Allocation Tracking
;;
;; Criterium's optional native agent captures detailed allocation information
;; with minimal overhead.

;; ## When to Use Allocation Tracking
;;
;; Use allocation tracking when you want to:
;; - Measure memory pressure from code execution
;; - Identify allocation hotspots
;; - Compare allocation costs of different implementations
;; - Verify zero-allocation code paths

;; ## Agent Requirements
;;
;; Allocation tracking uses a native JVMTI agent.
;;
;; Load the agent at JVM startup with -agentpath.  Use `jvm-opts` to get the
;; correct JVM arguments:

(agent/jvm-opts)

;; Add the returned argument when starting your JVM:

(kind/code "clojure -J-agentpath:/path/to/libcriterium.dylib -M:dev")

;; Check if the agent is currently available:

{:attached? (agent/attached?)
 :loaded?   (agent/loaded?)}

;; ## Basic Usage
;;
;; The `with-allocation-tracing` macro captures all JVM allocations
;; during execution of its body. It returns a vector of
;; `[allocation-records result]`:

(let [[allocations result] (agent/with-allocation-tracing
                             (str "hello" " " "world"))]
  {:result           result
   :allocation-count (when allocations (count allocations))
   :agent-available? (some? allocations)})

;; When the agent is unavailable, the macro gracefully degrades:
;; - The body still executes normally
;; - `allocations` is nil instead of a vector
;; - No error is thrown

;; ## Understanding Allocation Records
;;
;; Each allocation record is a map containing:

^:kindly/hide-code
(kind/table
 {:column-names ["Key" "Description"]
  :row-vectors  [[":object-type" "Class of allocated object"]
                 [":object_size" "Size in bytes"]
                 [":thread" "Thread ID where allocation occurred"]
                 [":freed" "Whether object was garbage collected"]
                 [":call-class" "Class that triggered allocation"]
                 [":call-method" "Method that triggered allocation"]
                 [":call-file" "Source file of allocation site"]
                 [":call-line" "Line number of allocation site"]
                 [":alloc-class" "Class doing the actual allocation"]
                 [":alloc-method" "Method doing the allocation"]]})

;; An example
(let [[allocations _] (agent/with-allocation-tracing
                        (vec (range 10)))]
  (when allocations
    {:total-allocations (count allocations)
     :sample-record     (first allocations)
     :object-types      (->> allocations
                             (map :object-type)
                             frequencies)}))

;; ## Filtering by Thread
;;
;; Allocation tracking captures allocations from ALL JVM threads,
;; not just the current one. Background tasks, GC finalization,
;; and other threads contribute allocations.
;;
;; Use `allocation-on-thread?` to filter to a specific thread:

(let [current-thread  (jvm/current-thread-id)
      [allocations _] (agent/with-allocation-tracing
                        (mapv inc (range 100)))]
  (when allocations
    (let [all-count     (count allocations)
          thread-allocs (filter (agent/allocation-on-thread?) allocations)
          thread-count  (count thread-allocs)]
      {:current-thread     current-thread
       :all-allocations    all-count
       :thread-allocations thread-count
       :other-threads      (- all-count thread-count)})))

;; The predicate can also filter for a specific thread ID:

(defn filter-specific-thread
  "Filter allocations for a specific thread."
  [thread-id allocations]
  (filter (agent/allocation-on-thread? thread-id) allocations))

;; ## Checking Freed Objects
;;
;; The `:freed` field indicates whether an object was garbage collected
;; during the tracking session. Use `allocation-freed?` to check:

(let [[allocations _] (agent/with-allocation-tracing
                        (dotimes [_ 100]
                          (str "temp-" (rand-int 1000))))]
  (when allocations
    (let [thread-allocs (filter (agent/allocation-on-thread?) allocations)
          freed         (filter agent/allocation-freed? thread-allocs)
          retained      (remove agent/allocation-freed? thread-allocs)]
      {:total-tracked  (count thread-allocs)
       :freed-count    (count freed)
       :retained-count (count retained)})))

;; ## Summarizing Allocations
;;
;; The `allocations-summary` function provides aggregate statistics:

(let [[allocations _] (agent/with-allocation-tracing
                        (vec (repeatedly 50 #(str "item-" (rand-int 100)))))]
  (when allocations
    (let [thread-allocs (filter (agent/allocation-on-thread?) allocations)]
      (agent/allocations-summary thread-allocs))))

;; The summary includes:
;; - `:num-allocated` - Total objects allocated
;; - `:num-freed` - Objects that were garbage collected
;; - `:allocated-bytes` - Total bytes allocated
;; - `:freed-bytes` - Bytes from freed objects

;; ## Comparing Implementations
;;
;; Allocation tracking helps compare memory costs of different approaches.

;; ### String concatenation

(let [words ["the" "quick" "brown" "fox"]

      ;; Approach 1: str with apply
      [allocs1 _] (agent/with-allocation-tracing
                    (apply str (interpose " " words)))
      summary1    (when allocs1
                    (agent/allocations-summary
                     (filter (agent/allocation-on-thread?) allocs1)))

      ;; Approach 2: clojure.string/join
      [allocs2 _] (agent/with-allocation-tracing
                    (str/join " " words))
      summary2    (when allocs2
                    (agent/allocations-summary
                     (filter (agent/allocation-on-thread?) allocs2)))]

  {:apply-str-approach   summary1
   :string-join-approach summary2})

;; ### Collection creation

(let [;; vec from range
      [allocs1 _] (agent/with-allocation-tracing
                    (vec (range 100)))
      summary1    (when allocs1
                    (agent/allocations-summary
                     (filter (agent/allocation-on-thread?) allocs1)))

      ;; mapv
      [allocs2 _] (agent/with-allocation-tracing
                    (mapv identity (range 100)))
      summary2    (when allocs2
                    (agent/allocations-summary
                     (filter (agent/allocation-on-thread?) allocs2)))]

  {:vec-range     summary1
   :mapv-identity summary2})

;; ## Analyzing Allocation Patterns
;;
;; Group allocations by type to understand allocation patterns:

(let [[allocations _] (agent/with-allocation-tracing
                        (let [data (vec (range 1000))]
                          (reduce + data)))]
  (when allocations
    (let [thread-allocs (filter (agent/allocation-on-thread?) allocations)]
      (->> thread-allocs
           (group-by :object-type)
           (map (fn [[type allocs]]
                  {:type  type
                   :count (count allocs)
                   :bytes (reduce + (map :object_size allocs))}))
           (sort-by :bytes >)
           (take 10)
           vec))))

;; ## Graceful Degradation
;;
;; When the agent cannot be loaded (unsupported platform, permission
;; issues, etc.), `with-allocation-tracing` returns nil for allocations
;; but still executes the body:
