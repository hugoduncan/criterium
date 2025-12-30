(ns criterium.util.t-digest
  "Re-exports t-digest functionality from stats component for backward compatibility."
  (:require
   [stats.t-digest :as t-digest]))

(def new-digest
  "Creates a new t-digest with optional compression factor."
  t-digest/new-digest)

(defn add-point
  "Add a single value into the digest"
  ([digest value]
   (t-digest/add-point digest value))
  ([digest value weight]
   (t-digest/add-point digest value weight)))

(def compress
  "Merge any buffered points into the digest."
  t-digest/compress)

(def quantile
  "Return estimated value at given quantile [0,1].
   Return nil if digest is empty."
  t-digest/quantile)

(def cdf
  "Return the cumulative probability at x.
   Return NaN if digest is empty."
  t-digest/cdf)

(def sample-count t-digest/sample-count)

(def minimum t-digest/minimum)

(def maximum t-digest/maximum)

(def mean
  "Return the mean estimate.
   Return NaN if digest is empty."
  t-digest/mean)

(defn variance
  "Return the variance estimate.
   Return NaN if digest is empty."
  ([digest]
   (t-digest/variance digest))
  ([digest mean]
   (t-digest/variance digest mean)))

(def transform t-digest/transform)

(def centroid-means t-digest/centroid-means)

(def histogram t-digest/histogram)

(def filter-outliers t-digest/filter-outliers)
