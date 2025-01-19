(ns criterium.util.t-digest
  (:require
   [criterium.util.t-digest.merging-digest :as md]))

(defn new-digest []
  (md/new-digest))

(defn add-point
  "Add a single value into the digest"
  [digest value]
  (md/add-point digest value))

(defn compress
  "Merge any buffered points into the digest."
  [digest]
  (md/compress digest))

(defn quantile
  "Return estimated value at given quantile [0,1].
   Return nil if digest is empty."
  ^double [digest ^double x]
  (md/quantile digest x))

(defn cdf
  "Return the cumulative probability at x.
   Return NaN if digest is empty."
  ^double [digest ^double x]
  (md/cdf digest x))
