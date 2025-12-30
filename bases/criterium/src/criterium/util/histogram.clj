(ns criterium.util.histogram
  "Histogram computation utilities with multiple binning methods.

  Supports:
  - :freedman-diaconis (default) - Uses IQR-based bin width calculation
  - :knuth - Bayesian optimal bin count selection

  Re-exports from stats.interface for backward compatibility."
  (:require
   [stats.interface :as stats]))

(defn histogram
  "Compute histogram from vector of numeric values.

  Supports multiple binning methods via the :method option:
  - :freedman-diaconis (default) - Uses IQR-based bin width calculation
  - :knuth - Bayesian optimal bin count selection

  Options:
    :method   - Binning method (:freedman-diaconis or :knuth)
    :iqr      - Pre-computed IQR (only for :freedman-diaconis)
    :max-bins - Maximum bins to search (only for :knuth, default 50)

  Returns map containing:
    :type     - :criterium/histogram-fixed-width or :criterium/histogram-knuth
    :counts   - vector of bin counts
    :centers  - vector of bin centers
    :width    - bin width
    :density  - vector of probability density values
    :n        - total number of samples
    :num-bins - number of bins
    :min      - minimum value
    :max      - maximum value

  Additional keys for :knuth method:
    :optimal-bins  - optimal bin count M
    :log-posterior - log-posterior value at optimal M

  For backward compatibility, second argument can be a number (pre-computed IQR).

  Throws:
    ex-info {:error :histogram/no-values} for empty input
    ex-info {:error :histogram/same-values} when all values are the same"
  ([values]
   (stats/histogram values))
  ([values opts-or-iqr]
   (stats/histogram values opts-or-iqr)))
