(ns criterium.util.t-digest.merging-digest
  "Re-exports merging-digest functions from stats component for backward
  compatibility."
  (:require [criterium.stats.t-digest.merging-digest :as md]))

;; Re-export records for backward compatibility
(def ->Centroid md/->Centroid)
(def ->TDigest md/->TDigest)
(def map->Centroid md/map->Centroid)
(def map->TDigest md/map->TDigest)

;; Centroid accessors
(def centroid-weight md/centroid-weight)
(def centroid-mean md/centroid-mean)

;; Core API
(def new-digest md/new-digest)

(defn add-point
  ([digest value]
   (md/add-point digest value))
  ([digest value weight]
   (md/add-point digest value weight)))

(def compress md/compress)
(def quantile md/quantile)
(def cdf md/cdf)
(def sample-count md/sample-count)
(def minimum md/minimum)
(def maximum md/maximum)

(defn mean
  ^double [digest]
  (md/mean digest))

(defn variance
  (^double [digest]
   (md/variance digest))
  (^double [digest mean]
   (md/variance digest mean)))

(def transform md/transform)
(def centroid-means md/centroid-means)
(def histogram md/histogram)
(def filter-outliers md/filter-outliers)

;; Additional functions used in tests
(def compressed? md/compressed?)
(def interpolate-centroids md/interpolate-centroids)
(def vfirst md/vfirst)
(def vsecond md/vsecond)
(def vpeek md/vpeek)
