(ns criterium.stats.t-digest
  "T-digest streaming quantile estimation.
   Provides a wrapper API over the merging-digest implementation."
  (:require
   [criterium.stats.t-digest.merging-digest :as md]))

(defn new-digest
  "Creates a new t-digest with optional compression factor."
  ([] (md/new-digest))
  ([compression] (md/new-digest compression))
  ([compression buffer-size] (md/new-digest compression buffer-size)))

(defn add-point
  "Add a single value into the digest"
  ([digest value]
   (md/add-point digest value))
  ([digest value weight]
   (md/add-point digest value weight)))

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

(defn sample-count
  ^double [digest]
  (md/sample-count digest))

(defn minimum
  ^double [digest]
  (md/minimum digest))

(defn maximum
  ^double [digest]
  (md/maximum digest))

(defn mean
  "Return the mean estimate.
   Return NaN if digest is empty."
  ^double [digest]
  (md/mean digest))

(defn variance
  "Return the variance estimate.
   Return NaN if digest is empty."
  (^double [digest]
   (md/variance digest))
  (^double [digest ^double mean]
   (md/variance digest mean)))

(defn transform
  [digest f]
  (md/transform digest f))

(defn centroid-means
  [digest]
  (md/centroid-means digest))

(defn histogram [digest iqr]
  (md/histogram digest iqr))

(defn filter-outliers
  [digest outliers]
  (md/filter-outliers digest outliers))
