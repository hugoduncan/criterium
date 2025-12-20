(ns criterium.allocation
  "Allocation tracing for benchmarks.

  Provides a wrapper around the native agent's allocation tracing that
  produces allocation trace maps conforming to :criterium/allocation-trace.

  This namespace bridges the low-level agent API with the criterium
  pipeline architecture, capturing thread context and timing metadata
  alongside raw allocation records."
  (:require
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]
   [criterium.types :as types]))

(def trace?
  "Check if x is an allocation trace map with :type :criterium/allocation-trace.
  Re-exported from criterium.types."
  types/allocation-trace?)

(defn filter-thread
  "Filter allocation records to those from a specific thread.

  Parameters:
    records   - Sequence of allocation records from the agent
    thread-id - Optional thread ID to filter by. Defaults to current thread.

  Returns filtered sequence of records where :thread matches thread-id."
  ([records]
   (filter-thread records (jvm/current-thread-id)))
  ([records thread-id]
   (filter (agent/allocation-on-thread? thread-id) records)))

(defmacro with-allocation-trace
  "Execute body while tracing allocations, returning an allocation trace map.

  Wraps the native agent's allocation tracing and packages the results
  into a :criterium/allocation-trace map suitable for the analysis pipeline.

  Parameters:
    opts - Options map with:
           :eval-count - Number of evaluations (default 1)
    body - Forms to execute while tracing

  Returns a vector of [allocation-trace result] where:
    allocation-trace - Map with :type :criterium/allocation-trace containing:
                       :records      - Raw allocation records from agent
                       :thread-id    - Thread ID where body executed
                       :eval-count   - Number of evaluations
                       :elapsed-time - Execution time in nanoseconds
                       Returns nil if agent not attached.
    result           - Value returned by body forms

  Example:
    (with-allocation-trace {:eval-count 100}
      (dotimes [_ 100] (make-string)))
    => [{:type :criterium/allocation-trace
         :records [...] :thread-id 1 :eval-count 100 :elapsed-time 12345}
        nil]"
  [opts & body]
  `(let [thread-id#  (jvm/current-thread-id)
         eval-count# (:eval-count ~opts 1)
         start#      (jvm/timestamp)
         [records# result#] (agent/with-allocation-tracing ~@body)
         elapsed#    (jvm/elapsed-time start# (jvm/timestamp))]
     (if records#
       [{:type         :criterium/allocation-trace
         :records      records#
         :thread-id    thread-id#
         :eval-count   eval-count#
         :elapsed-time elapsed#}
        result#]
       [nil result#])))
