(ns criterium.agent.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.agent.core :as agent-core]))

;; JIT so zero allocation tests see jit'ed version
(dotimes [_ 10000] (agent-core/agent-state))

(deftest agent-state-test
  (is (keyword? (agent-core/agent-state))))
