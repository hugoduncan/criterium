(ns criterium.agent.wrapper
  "Direct access wrappers for Agent class methods.

  This namespace imports criterium.agent.Agent and provides Clojure
  functions that compile to direct field access and static method calls,
  avoiding reflection.

  This namespace will fail to load if the Agent class is not available."
  (:import
   [criterium.agent Agent]))

(defn read-state
  "Read Agent.state field directly."
  ^long []
  Agent/state)

(defn send-command
  "Call Agent.command directly."
  [^long cmd]
  (Agent/command cmd)
  nil)

(defn allocation-start-marker
  "Call Agent.allocation_start_marker directly."
  []
  (Agent/allocation_start_marker))

(defn allocation-finish-marker
  "Call Agent.allocation_finish_marker directly."
  []
  (Agent/allocation_finish_marker))

(defn method-tracing-start-marker
  "Call Agent.method_tracing_start_marker directly.
  Generates a method entry event that triggers transition to active state."
  []
  (Agent/method_tracing_start_marker))

(defn method-tracing-finish-marker
  "Call Agent.method_tracing_finish_marker directly.
  Generates a method entry event that triggers completion of tracing."
  []
  (Agent/method_tracing_finish_marker))

(defn set-handler
  "Call Agent.set_handler directly."
  [handler-fn]
  (Agent/set_handler handler-fn)
  nil)
