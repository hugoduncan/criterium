(ns criterium.util.t-digest.scale
  "Re-exports scale functions from stats component for backward compatibility."
  (:require [stats.t-digest.scale :as scale]))

(def finite? scale/finite?)

(def k scale/k)
(def q scale/q)
(def max-size scale/max-size)
(def normalizer scale/normalizer)

(def limit scale/limit)
(def bound scale/bound)

(def k0 scale/k0)
(def k1 scale/k1)
(def k2 scale/k2)

(def k1-q-limit-low scale/k1-q-limit-low)
(def k1-q-limit-high scale/k1-q-limit-high)
(def k1-k-limit-f-low scale/k1-k-limit-f-low)
(def k1-q-limit-f-high scale/k1-q-limit-f-high)
(def k1-x-limit-low scale/k1-x-limit-low)
(def k1-x-limit-high scale/k1-x-limit-high)
