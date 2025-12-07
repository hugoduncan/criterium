(ns criterium.util.blackhole
  "Blackhole wrapper for preventing dead code elimination.
  Delegates to criterium.blackhole."
  (:require
   [criterium.blackhole :as bh]))

(defmacro consume
  "Consume a value to prevent dead code elimination."
  [x]
  `(bh/consume ~x))

(defn evaporate
  "Evaporate the blackhole, releasing any references it may contain."
  []
  (bh/evaporate))
