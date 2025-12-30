(ns criterium.util.knuth
  "Knuth's Bayesian histogram binning algorithm.

   Implements optimal bin count selection by maximizing a log-posterior
   based on Knuth (2019) DOI: 10.1016/j.dsp.2019.102581

   Re-exports from stats.interface for backward compatibility."
  (:require
   [stats.interface :as stats]))

(def log-posterior
  "Compute Knuth's log-posterior for M bins given sample count and bin counts.

  F(M|x,I) = n·log(M) + logΓ(M/2) - M·logΓ(1/2) - logΓ((2n+M)/2) + Σₖ₌₁ᴹ logΓ(nₖ + 1/2)

  Parameters:
    n - total sample count
    bin-counts - sequence of counts per bin

  Returns the log-posterior value (higher is better)."
  stats/knuth-log-posterior)

(defn optimal-bins
  "Find optimal number of bins using Knuth's Bayesian method.

  Searches M ∈ [1, max-bins] for the value that maximizes the log-posterior.

  Parameters:
    samples - sequence of numeric values
    opts - optional map with:
      :max-bins - maximum M to search (default: 50)
      :min - pre-computed minimum value (avoids redundant scan)
      :max - pre-computed maximum value (avoids redundant scan)

  Returns map with:
    :optimal-bins - the optimal number of bins M
    :log-posterior - the log-posterior value at optimal M

  Throws:
    ex-info {:error :knuth/no-samples} for empty input
    ex-info {:error :knuth/same-values} when all values are identical"
  ([samples] (stats/knuth-optimal-bins samples))
  ([samples opts] (stats/knuth-optimal-bins samples opts)))
