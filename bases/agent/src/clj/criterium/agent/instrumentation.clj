(ns criterium.agent.instrumentation
  "A non-native agent to access the Instrumentation interface."
  (:import
   [java.lang.instrument
    Instrumentation])
  (:gen-class
   :methods
   [^:static [agentmain [String java.lang.instrument.Instrumentation] void]]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn -premain
  "Invoke when attached at startup"
  [args _inst]
  (println "Loading criterium.agent" args))

(defonce ^:private instrument (atom nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn -agentmain
  "Invoke when attached into running jvm"
  [^String _args ^Instrumentation instrumentation]
  (swap! instrument (constantly instrumentation)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn object-size
  ^long [x]
  (.getObjectSize ^Instrumentation @instrument x))
