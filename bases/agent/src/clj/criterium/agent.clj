(ns criterium.agent
  "Agent API."
  (:require
   [criterium.agent.core :as core]
   [criterium.jvm :as jvm]))

(defn attached?
  "Predicate for whether the criterium agent is attached."
  []
  (core/attached?))

(defmacro with-allocation-tracing
  "A scope in which all allocations and releases are tracked.

  Note that the allocations tracked are not limited to the current
  thread.  Filter the returned records with `thread-allocations` if that
  is all you are concerned with."
  [& body]
  (if (attached?)
    (core/with-allocation-tracing-enabled body)
    (core/with-allocation-tracing-disabled body)))

(defn allocation-on-thread?
  "Return a predicate for the allocation record being on the given thread.
  The thread defaults to the current thread."
  ([] (core/allocation-on-thread? (jvm/current-thread-id)))
  ([thread-id] (core/allocation-on-thread? thread-id)))

(defn allocation-freed?
  "predicate for the allocation record having been freed."
  [record] (core/allocation-freed? record))

(defn allocations-summary
  "Return total counts and bytes for the given allocation records."
  [records]
  (core/allocations-summary records))
