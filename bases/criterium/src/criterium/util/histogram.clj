(ns criterium.util.histogram
  "Histogram computation utilities using Freedman-Diaconis rule for binning.

  Re-exports from stats.interface for backward compatibility."
  (:require
   [stats.interface :as stats]))

(defn histogram
  "Compute histogram from vector of numeric values using Freedman-Diaconis rule.
   Optional pre-computed IQR can be provided.
   Returns map containing:
   - :counts - vector of bin counts
   - :centers - vector of bin centers
   - :width - bin width
   - :density - vector of probability density values
   - :n - total number of samples
   - :min - minimum value
   - :max - maximum value

   Throws:
   - ex-info {:error :histogram/no-values} for empty input
   - ex-info {:error :histogram/same-values} when all values are the same"
  ([values]
   (stats/histogram values))
  ([values precomputed-iqr]
   (stats/histogram values precomputed-iqr)))
