(ns criterium.bench.impl
  "Internal implementation details for criterium.bench namespace.
   Not intended for direct use by consumers of the library.")

(def ^:private last-bench* (volatile! nil))

(defn last-bench!
  "Store the results of the last benchmark execution."
  [results]
  (vreset! last-bench* results))

(defn last-bench
  "Retrieve the results of the last benchmark execution.
   Internal use only."
  []
  @last-bench*)
